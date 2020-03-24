{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -O0 #-}

{- |
Module           : $Header$
Description      : The symbolic simulator for LLVM-Symbolic programs
License          : BSD3
Stability        : provisional
Point-of-contact : jhendrix
-}
module Verifier.LLVM.Backend.SAW
  ( SAWBackend
  , SAWMemory
  , memState
  , createSAWBackend
  , createSAWBackend'
  , scLoadLLVMModule
  ) where

import Control.Exception (assert)
import Control.Lens hiding (op)
import Control.Monad hiding (fail)
import Control.Monad.IO.Class (liftIO)
import qualified Data.AIG as AIG
import Data.Bits
import Data.IORef
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.SBV.Dynamic
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Numeric.Natural (Natural)
import Prelude ()
import Prelude.Compat

import Verifier.SAW as SAW
import Verifier.SAW.Conversion
import Verifier.SAW.ParserUtils as SAW
import Verifier.SAW.Prelude
import Verifier.SAW.Cryptol.Prelude (scLoadCryptolModule)
import qualified Verifier.SAW.Recognizer as R
import Verifier.SAW.Rewriter
import qualified Verifier.SAW.Simulator.BitBlast as BB
import qualified Verifier.SAW.Simulator.SBV as SBVSim

import Verifier.LLVM.Backend as LLVM
import Verifier.LLVM.Backend.SAWImport
import Verifier.LLVM.Codebase.AST
import qualified Verifier.LLVM.MemModel as MM


preludeBVNatTermF :: TermF t
preludeBVNatTermF = FTermF $ GlobalDef (mkIdent preludeModuleName "bvNat")

scBitwidth :: SharedContext -> BitWidth -> IO Term
scBitwidth sc w = scNat sc (fromIntegral w)

asUnsignedBitvector :: BitWidth -> Term -> Maybe Integer
asUnsignedBitvector w s2 = do
  (s1, vt) <- R.asApp s2
  (s0, wt) <- R.asApp s1
  when (unwrapTermF  s0 /= preludeBVNatTermF) Nothing
  when (R.asNat wt /= Just (fromIntegral w)) Nothing
  toInteger <$> R.asNat vt

asSignedBitvector :: BitWidth -> Term -> Maybe Integer
asSignedBitvector w s2
    | w == 0 = error "Bad bitwidth"
    | otherwise = s2u <$> asUnsignedBitvector w s2
  where s2u v | v `testBit` (w-1) = v - 2^w
              | otherwise = v

scFloat :: SharedContext -> Float -> IO Term
scFloat sc x =
  do let (m, e) = decodeFloat x
     m' <- scIntegerConst sc m
     e' <- scIntegerConst sc (toInteger e)
     scGlobalApply sc "Prelude.mkFloat" [m', e']

scDouble :: SharedContext -> Double -> IO Term
scDouble sc x =
  do let (m, e) = decodeFloat x
     m' <- scIntegerConst sc m
     e' <- scIntegerConst sc (toInteger e)
     scGlobalApply sc "Prelude.mkDouble" [m', e']

-- | Create a vector from a term representing its element types and the element.
scVecLit :: SharedContext
         -> Term -- ^ Type of vector elements.
         -> V.Vector Term -- ^ Elements
         -> IO Term
scVecLit sc tp v = scTermF sc (FTermF (ArrayValue tp v))

scResizeTerm :: SharedContext
             -> BitWidth -- ^ Input width
             -> (BitWidth, Term) -- ^ Result bitwith and term representing it.
             -> Term
             -> IO Term
scResizeTerm sc iw (rw,rt) v
  | iw < rw = do
      let fn = scApplyLLVM_llvmZExt sc
      dt <- scBitwidth sc (rw - iw)
      fn dt rt v
  | iw > rw = do
      let fn = scApplyLLVM_llvmTrunc sc
      dt <- scBitwidth sc (iw - rw)
      fn dt rt v
  | otherwise = return v

-- | Create a bitvector from a constant.
scLLVMIntConst :: SharedContext
                 -> BitWidth -- ^ Result width with corresponding term.
                 -> Integer -- ^ Value of bitvector.
                 -> IO Term
scLLVMIntConst sc w v = do
  wt <- scBitwidth sc w
  scLLVMIntConst' sc (w,wt) v

-- | Create a bitvector from a constant.
scLLVMIntConst' :: SharedContext
                -> (BitWidth, Term) -- ^ Result width with corresponding term.
                -> Integer -- ^ Value of bitvector.
                -> IO Term
scLLVMIntConst' sc (w,wt) v = do
  let cfn = scApplyLLVM_llvmIntConstant sc
  cfn wt =<< scNat sc (fromInteger (v `mod` 2^(toInteger w)))

{-
Essay on how allocations are done:

We represent the result of an allocation as a fresh variable.  Arithmetic
on terms with these variables is designed to normalize the term into the
form "Var + offset" where feasible.

There are implicit assumptions on disjoinness between allocations.


Operations that attempt to exploit this normalization include:
  Integer arithmetic add, sub.
  Pointer add.
  UAddWithOverflow.
  Integer comparison operations.
-}

scFreshPtr :: SharedContext -> DataLayout -> IO Term
scFreshPtr sc dl = do
  let valueFn = scApplyLLVM_value sc
  let w = ptrBitwidth dl
  scFreshGlobal sc "_" =<< valueFn =<< scIntType sc w

-- | Set of shared term variables that refer to allocations.
type SharedTermSetRef = IORef (Set Term)

asApp2 :: Term -> Maybe (Term, Term, Term)
asApp2 t = do
  (t1,a2) <- R.asApp t
  (t0,a1) <- R.asApp t1
  return (t0,a1,a2)

data SAWBackendState
   = SBS { sbsDataLayout :: DataLayout
         , sbsContext :: SharedContext
           -- | Stores list of fresh variables and their associated terms with
           -- most recently allocated variables first.
         , sbsVars :: IORef [(BitWidth,VarIndex,ExtCns Term)]
           -- | Allocations added.
         , sbsAllocations :: SharedTermSetRef
           -- | Width of pointers in bits as a nat.
         , sbsPtrWidth :: Term
           -- | LLVM Type of a pointer
         , sbsPtrType    :: Term
           -- | LLVM Type of floats.
         , sbsFloatType  :: Term
           -- | LLVM Type for double
         , sbsDoubleType :: Term
           -- | Creates LLVM type for arrays
         , sbsArrayTypeFn :: Term -> Term -> IO Term
           -- | Creates LLVM type for vectors
         , sbsVecTypeFn :: Term -> Term -> IO Term
           -- | Creates LLVM type for structs
         , sbsStructTypeFn :: Term -> IO Term
         , sbsAdd :: BitWidth -> IO (Term -> Term -> IO Term)
         , sbsSub :: BitWidth -> IO (Term -> Term -> IO Term)
         , smGenerator :: MM.TermGenerator IO Term Term Term
         }

data DecomposeResult
   = BasePtr
   | OffsetPtr !Term !Term
   | SymbolicPtr

mkBackendState :: DataLayout
               -> SharedContext
               -> IO SAWBackendState
mkBackendState dl sc = do
  vars <- newIORef []
  allocs <- newIORef Set.empty
  ptrWidth <- scBitwidth sc (ptrBitwidth dl)
  let addFn = scApplyLLVM_llvmAdd sc

  let subFn = scApplyLLVM_llvmSub sc

  -- LLVM Type imports
  ptrType <- scApplyLLVM_PtrType sc =<< scNat sc (fromIntegral (dl^.ptrSize))
  llvmFloatType  <- scApplyLLVM_FloatType sc
  llvmDoubleType <- scApplyLLVM_DoubleType sc
  let arrayTypeFn  = scApplyLLVM_ArrayType sc
  let vecTypeFn    = scApplyLLVM_VectorType sc
  let structTypeFn = scApplyLLVM_StructType sc
  -- structFields <- scApplyLLVM_StructFields sc
  -- structIndex <- scApplyLLVM_StructIndex sc
  -- fieldType <- scApplyLLVM_fieldType sc

  trueTerm  <- scApplyPrelude_True  sc
  falseTerm <- scApplyPrelude_False sc

  ptrAddOp <- addFn ptrWidth
  let constPtr = scLLVMIntConst' sc (ptrBitwidth dl, ptrWidth)
  let decomposePtr ptr = do
        s <- readIORef allocs
        return $
          case asApp2 ptr of
            Just (f,b,o) | f == ptrAddOp ->
              if Set.member b s then OffsetPtr b o else SymbolicPtr
            _  | Set.member ptr s -> BasePtr
               | otherwise -> SymbolicPtr
  let ptrAdd x y = do
        mx <- decomposePtr x
        let addPrim = scApply2 sc ptrAddOp
        case mx of
          BasePtr -> addPrim x y
          OffsetPtr b o -> addPrim b =<< addPrim o y
          SymbolicPtr -> do
            my <- decomposePtr y
            case my of
              BasePtr -> addPrim y x
              OffsetPtr b o -> addPrim b =<< addPrim x o
              SymbolicPtr -> addPrim x y
  ptrSubOp <- subFn ptrWidth
  nullPtr <- constPtr 0
  let ptrSub x y = do
        mx <- decomposePtr x
        my <- decomposePtr y
        let subPrim = scApply2 sc ptrSubOp
        case (mx, my) of
          (BasePtr, BasePtr) | x == y -> return nullPtr
          (BasePtr, OffsetPtr by oy) | x == by -> subPrim nullPtr oy
          (OffsetPtr bx ox, BasePtr) | bx == y -> return ox
          (OffsetPtr bx ox, OffsetPtr by oy) | bx == by -> subPrim ox oy
          _ -> subPrim x y

  let decomposeOffset t = fmap toInteger <$> scIntAsConst' sc ptrWidth t

  let muxFn = scApplyPrelude_ite sc

  let andFn = scApplyPrelude_and sc
  let orFn  = scApplyPrelude_or  sc
  boolMuxFun <- scPrelude_Bool sc >>= return . muxFn
  let intTypeFn = scApplyLLVM_IntType sc
  let mkTypeTerm :: MM.Type -> IO Term
      mkTypeTerm tp0 =
        case MM.typeF tp0 of
          MM.Bitvector n -> intTypeFn =<< scNat sc (8*fromIntegral n)
          MM.Float  -> return llvmFloatType
          MM.Double -> return llvmDoubleType
          MM.Array n tp -> join $ arrayTypeFn <$> scNat sc (fromIntegral n) <*> mkTypeTerm tp
          MM.Struct flds -> join $ structTypeFn <$> fieldVFn flds
      fieldFn :: MM.Field MM.Type -> IO (Term, Natural)
      fieldFn f = (, fromIntegral (MM.fieldPad f)) <$> mkTypeTerm (f^.MM.fieldVal)
      fieldVFn :: V.Vector (MM.Field MM.Type) -> IO Term
      fieldVFn flds = scFieldInfo sc =<< traverse fieldFn (V.toList flds)

  --intToFloat  <- scApplyLLVM_llvmIntToFloat sc
  --intToDouble <- scApplyLLVM_llvmIntToDouble sc
  let intToFloat _ = error "Verifier.LLVM.Backend.SAW: intToFloat not implemented"
  let intToDouble _ = error "Verifier.LLVM.Backend.SAW: intToDouble not implemented"

  let ptrEqOp = scApplyLLVM_llvmIeqBool  sc
  let ptrLeOp = scApplyLLVM_llvmIuleBool sc

  let appendFn = scApplyPrelude_append sc
  boolTy <- scBoolType sc
  let appendInt m n x y = appendFn m n boolTy x y
  let sliceFn   = scApplyLLVM_llvmIntSlice sc
  let valueFn   = scApplyLLVM_value sc
  let tg = MM.TG
              { MM.tgPtrWidth = dl^.ptrSize
              , MM.tgPtrDecompose = \ptr -> do
                  mr <- decomposePtr ptr
                  case mr of
                    BasePtr -> return $ MM.ConcreteOffset ptr 0
                    OffsetPtr b o -> do
                      mo <- decomposeOffset o
                      return $ case mo of
                                 Just o' -> MM.ConcreteOffset b o'
                                 Nothing -> MM.SymbolicOffset b o
                    SymbolicPtr -> return $ MM.Symbolic ptr
              , MM.tgPtrSizeDecompose = decomposeOffset
              , MM.tgConstPtr = constPtr . fromIntegral
              , MM.tgAddPtr = ptrAdd
              , MM.tgCheckedAddPtr = \x y -> (trueTerm,) <$> ptrAdd x y
              , MM.tgSubPtr = ptrSub

              , MM.tgTrue  = trueTerm
              , MM.tgFalse = falseTerm
              , MM.tgPtrEq = ptrEqOp ptrWidth
              , MM.tgPtrLe = ptrLeOp ptrWidth
              , MM.tgAnd   = andFn
              , MM.tgOr    = orFn
              , MM.tgMuxCond = boolMuxFun

              , MM.tgConstBitvector = \w -> scLLVMIntConst sc (8*fromIntegral w)
              , MM.tgConstFloat  = scFloat sc
              , MM.tgConstDouble = scDouble sc
              , MM.tgApplyCtorF = \vcf ->
                 case vcf of
                   MM.ConcatBV xw x yw y -> do
                     xwt <- scNat sc $ fromIntegral xw `shiftL` 3
                     ywt <- scNat sc $ fromIntegral yw `shiftL` 3
                     case dl^.intLayout of
                       BigEndian    -> appendInt xwt ywt x y
                       LittleEndian -> appendInt ywt xwt y x
                   MM.BVToFloat x -> intToFloat x
                   MM.BVToDouble x -> intToDouble x
                   MM.ConsArray tp x n y -> do
                     let consFn = scApplyPrelude_ConsVec sc
                     tpt <- mkTypeTerm tp
                     nt <- scNat sc (fromInteger n)
                     consFn tpt x nt y
                   MM.AppendArray tp m x n y -> do
                     join $ appendFn <$> scNat sc (fromInteger m)
                                     <*> scNat sc (fromInteger n)
                                     <*> (valueFn =<< mkTypeTerm tp)
                                     ?? x
                                     ?? y
                   MM.MkArray tp v ->
                     join $ scVecLit sc <$> (valueFn =<< mkTypeTerm tp) ?? v
                   MM.MkStruct v -> do
                     ExprEvalFn eval <- createStructValue sc =<< (traverse . _1) fieldFn v
                     eval return
              , MM.tgApplyViewF = \vf -> do
                  let slice i n o v = do
                        join $ sliceFn
                                 <$> scNat sc (8*fromIntegral i)
                                 <*> scNat sc (8*fromIntegral n)
                                 <*> scNat sc (8*fromIntegral o)
                                 ?? v
                  case vf of
                    MM.SelectLowBV m n v -> do
                      case dl^.intLayout of
                        BigEndian    -> slice 0 m n v -- High order bits of v.
                        LittleEndian -> slice n m 0 v -- low order bits of v.
                    MM.SelectHighBV m n v -> do
                      case dl^.intLayout of
                        BigEndian    -> slice m n 0 v -- Low order bits of v.
                        LittleEndian -> slice 0 n m v -- High order bits of v.
                    MM.FloatToBV _v  -> error "Verifier.LLVM.Backend.SAW: floatToInt not implemented"
                                       --join $ scApplyLLVM_llvmFloatToInt sc ?? v
                    MM.DoubleToBV _v -> error "Verifier.LLVM.Backend.SAW: doubleToInt not implemented"
                                       --join $ scApplyLLVM_llvmDoubleToInt sc ?? v
                    MM.ArrayElt n tp o v -> do
                      let n' = fromIntegral n
                          o' = fromIntegral o
                          -- w  = 64 -- TODO: don't hard-code size
                      join $ (return $ scApplyPrelude_at sc)
                             <*> scNat sc n'
                             <*> (valueFn =<< mkTypeTerm tp)
                             <*> pure v
                             <*> scNat sc o'
                    MM.FieldVal tps i v -> do
                      f <- traverse fieldFn (V.toList tps)
                      (f', i') <- scStructIndex sc f (fromIntegral i)
                      join $ (return $ scApplyLLVM_llvmStructElt sc)
                               <*> pure f'
                               <*> pure v
                               <*> pure i'
              , MM.tgMuxTerm = \c tp x y -> do
                  tpt <- scApplyLLVM_value sc =<< mkTypeTerm tp
                  muxFn tpt c x y
              }

  return SBS { sbsDataLayout = dl
             , sbsContext = sc
             , sbsVars    = vars
             , sbsAllocations  = allocs
             , sbsPtrWidth     = ptrWidth
             , sbsPtrType      = ptrType
             , sbsFloatType    = llvmFloatType
             , sbsDoubleType   = llvmDoubleType
             , sbsArrayTypeFn  = arrayTypeFn
             , sbsVecTypeFn    = vecTypeFn
             , sbsStructTypeFn = structTypeFn
             , sbsAdd = \w ->
                 if w == ptrBitwidth dl then
                   return ptrAdd
                 else do
                   op <- addFn =<< scBitwidth sc w
                   return $ scApply2 sc op
             , sbsSub = \w ->
                 if w == ptrBitwidth dl then
                   return ptrSub
                 else do
                   op <- subFn =<< scBitwidth sc w
                   return $ scApply2 sc op
             , smGenerator = tg
             }

-- | Attempt to parse the second term as a constant integer.
-- The first term is the width of the term.
scIntAsConst' :: SharedContext -> Term -> Term -> IO (Maybe Natural)
scIntAsConst' sc w t =
  fmap R.asNat $ scApplyLLVM_llvmIntValueNat sc w t

type SAWMem = MM.Mem Term Term Term

data SAWMemory = SAWMemory { _memSymbols :: Map Term Symbol
                           , _memState :: MM.Mem Term Term Term
                           }

emptySAWMemory :: SAWMemory
emptySAWMemory = SAWMemory { _memSymbols = Map.empty
                           , _memState = MM.emptyMem
                           }

memSymbols :: Lens' SAWMemory (Map Term Symbol)
memSymbols = lens _memSymbols (\s v -> s { _memSymbols = v })

memState :: Lens' SAWMemory SAWMem
memState = lens _memState (\s v -> s { _memState = v })

smAddDefine :: DataLayout
            -> SharedContext
            -> SAWMemory
            -> Symbol
            -> [BlockLabel]
            -> IO (Maybe (Term, [Term], SAWMemory))
smAddDefine dl sc m sym lbls = do
  symt <- scFreshPtr sc dl
  lblst <- traverse (\_ -> scFreshPtr sc dl) lbls
  let m' = m & memSymbols . at symt ?~ sym
  return $ Just (symt, lblst, m')

-- | Return symbol associated with address if any.
smLookupSymbol :: SAWMemory -> Term -> LookupSymbolResult
smLookupSymbol m t =
  case m^.memSymbols^.at t of
    Just r -> Right r
    Nothing -> Left Indeterminate

smAlloc :: SAWBackendState
        -> MM.AllocType
        -> SAWMemory
        -> MemType
        -> BitWidth -- ^ Width of count.
        -> Term -- ^ Count
        -> Alignment
        -> IO (AllocResult SAWBackend)
smAlloc sbs atp m mtp w cnt _ = do
  let sc = sbsContext sbs
  -- Get size of tp in bytes.
  let dl = sbsDataLayout sbs
  let pw = ptrBitwidth dl
  let pwt = sbsPtrWidth sbs
  tpSize <- scLLVMIntConst' sc (pw,pwt) (toInteger (memTypeSize dl mtp))
  -- Convert count to have same bitwidth as pointer.
  cnt' <- scResizeTerm sc w (pw,pwt) cnt
  -- Get total number of bytes.
  let mulFn = scApplyLLVM_llvmMul sc
  mulOp <- mulFn pwt
  totalSize <- scApplyAll sc mulOp [cnt', tpSize]
  -- Get true predicate.
  t <- scApplyPrelude_True sc
  -- Return allocation.
  -- Create new variable for base address.
  base <- allocPtr sbs
  -- Return successful allocation.
  let m' = m & memState %~ MM.allocMem atp base totalSize
  return (AResult t base m')

allocPtr :: SAWBackendState -> IO Term
allocPtr sbs = do
  -- Create new variable for base address.
  s <- readIORef (sbsAllocations sbs)
  let nm = "lss__alloc" ++ show (Set.size s)
  let sc = sbsContext sbs
  tp <- scApplyLLVM_value sc (sbsPtrType sbs)
  base <- scFreshGlobal sc nm tp
  writeIORef (sbsAllocations sbs) $! Set.insert base s
  return base

smIsAllocated :: SAWBackendState
              -> SAWMemory
              -> Term -- ^ Pointer
              -> Term -- ^ Size
              -> IO Term
smIsAllocated sbs m p sz = MM.isAllocated (smGenerator sbs) p sz (m ^. memState)

mergeEq :: (Ord k, Eq a) => Map k a -> Map k a -> Map k a
mergeEq mx = Map.filterWithKey p
  where p k u = Map.lookup k mx == Just u

smMerge :: Term -> SAWMemory -> SAWMemory -> SAWMemory
smMerge c x y =
  SAWMemory { _memSymbols = mergeEq (x^.memSymbols) (y^.memSymbols)
            , _memState = MM.mergeMem c (x^.memState) (y^.memState)
            }

-- | Return term value, length of fields, and vector with the types of the fields.
sbsStructValue :: SAWBackendState
               -> V.Vector (FieldInfo, v)
               -> IO (ExprEvalFn v Term)
sbsStructValue sbs flds = do
  let fn fi = do
         (,fromIntegral (fiPadding fi)) <$> sbsMemType sbs (fiType fi)
  createStructValue (sbsContext sbs) =<< (traverse . _1) fn flds

-- | Return term value, length of fields, and vector with the types of the fields.
createStructValue :: forall v
                   . SharedContext
                     -- For each field, provide type, number of padding bytes, and value.
                  -> V.Vector ((Term, Natural), v)
                  -> IO (ExprEvalFn v Term)
createStructValue sc flds = do
  -- fieldType <- scApplyLLVM_fieldType sc
  let foldFn :: ((Term, Natural), v)
             -> (Natural, ExprEvalFn v Term, Term)
             -> IO (Natural, ExprEvalFn v Term, Term)
      foldFn ((mtp,pad),expr) (n,ExprEvalFn reval, rvtp) = do
        padding <- scNat sc pad
        let consStruct = scApplyLLVM_ConsStruct sc
        let cfn = consStruct mtp padding rvtp
        let reval' = ExprEvalFn $ \eval ->
                      join $ ((liftIO .) . cfn) <$> eval expr <*> reval eval
        let consFields = scApplyLLVM_ConsFields sc
        (n+1,reval',) <$> consFields mtp padding rvtp
  -- Get initial value and type.
  emptyStruct <- scApplyLLVM_EmptyStruct sc
  let eval0 = ExprEvalFn $ \_ -> return emptyStruct
  tp0 <- scApplyLLVM_EmptyFields sc
  view _2 <$> foldrMOf folded foldFn (0, eval0, tp0) flds

scApply2 :: SharedContext -> Term -> Term -> Term -> IO Term
scApply2 sc f x y = do
  g <- scApply sc f x
  scApply sc g y

convertMemType :: DataLayout -> MemType -> Maybe MM.Type
convertMemType dl tp0 =
  case tp0 of
    IntType w
        | r == 0 -> return (MM.bitvectorType (fromIntegral n))
        | otherwise -> Nothing
      where (n,r) = w `divMod` 8
    PtrType{} -> return (MM.bitvectorType (dl^.ptrSize))
    FloatType -> return MM.floatType
    DoubleType -> return MM.doubleType
    ArrayType n etp -> MM.arrayType (fromIntegral n) <$> convertMemType dl etp
    VecType n etp   -> MM.arrayType (fromIntegral n) <$> convertMemType dl etp
    StructType si   -> MM.mkStruct <$> traverse fldFn (siFields si)
      where fldFn f = (,fiPadding f) <$> convertMemType dl (fiType f)

-- | Apply an operation to leaves of a mux.
applyMuxToLeaves :: (Applicative m, Monad m)
                 => (Term -> a -> a -> m a) -- ^ Mux operation on result
                 -> (Term -> m a) -- ^ Action on leaves.
                 -> Term -- ^ Term to act on
                 -> m a
applyMuxToLeaves mux action = go
  where go t =
          case R.asMux t of
            Nothing -> action t
            Just (_ :*: b :*: x :*: y) -> join $ mux b <$> go x <*> go y

smLoad :: SAWBackendState
       -> SAWMemory
       -> MemType
       -> Term
       -> Alignment
       -> IO (Term, Term) -- Validity predicate and result.
smLoad sbs m tp0 ptr0 _a0 =
  case convertMemType (sbsDataLayout sbs) tp0 of
    Just tp -> applyMuxToLeaves mux action ptr0
      where mux c = MM.tgMuxPair (smGenerator sbs) c tp
            action ptr = MM.readMem (smGenerator sbs) ptr tp (m^.memState)
    Nothing -> fail "smLoad must be given types that are even byte size."

smStore :: SAWBackendState
        -> SAWMemory
        -> Term -- ^ Address to store value at.
        -> MemType      -- ^ Type of value
        -> Term -- ^ Value to store
        -> Alignment
        -> IO (Term, SAWMemory) -- Predicate and new memory.
smStore sbs m p mtp v _ = do
  case convertMemType (sbsDataLayout sbs) mtp of
    Nothing -> fail "memtype given to smStore must be an even byte size."
    Just tp -> do
      let tg = smGenerator sbs
          ms = m^.memState
      if isJust (runMatcher asBvNatLit p) then
        return (MM.tgFalse tg, m)
      else do
        sz <- MM.tgConstPtr tg (MM.typeEnd 0 tp)
        c  <- MM.isAllocated tg p sz ms
        ms' <- MM.writeMem' tg p tp v ms
        return (c,m & memState .~ ms')

-- | @memcpy mem dst src len align@ copies @len@ bytes from @src@ to @dst@,
-- both of which must be aligned according to @align@ and must refer to
-- non-overlapping regions.
smCopy :: SAWBackendState
       -> SAWMemory
       -> Term -- ^ Destination pointer
       -> Term -- ^ Source pointer
       -> BitWidth  -- ^ Bitwidth for counting number of bits.
       -> Term -- ^ Number of bytes to copy.
       -> Term -- ^ Alignment in bytes (should have 32-bit bits)
       -> IO (Term, SAWMemory)
smCopy sbs m dst src w sz0 _ = do
  sz <- scResizeTerm (sbsContext sbs) w
           (ptrBitwidth (sbsDataLayout sbs), sbsPtrWidth sbs) sz0
  (c,ms) <- MM.copyMem (smGenerator sbs) dst src sz (m^.memState)
  return (c, m & memState .~ ms)

data SAWBackend a = SAWBackend { runSAWBackend :: IO a }
  deriving (Functor)

type instance SBETerm SAWBackend = Term
type instance SBEPred SAWBackend = Term
type instance SBEMemory SAWBackend = SAWMemory

lift1 :: (x -> IO r) -> (x -> SAWBackend r)
lift1 = (SAWBackend .)

lift2 :: (x -> y -> IO r)
      -> (x -> y -> SAWBackend r)
lift2 = (lift1 .)

lift3 :: (x -> y -> z -> IO r)
      -> (x -> y -> z -> SAWBackend r)
lift3 = (lift2 .)

lift4 :: (w -> x -> y -> z -> IO r)
      -> (w -> x -> y -> z -> SAWBackend r)
lift4 = (lift3 .)

lift5 :: (v -> w -> x -> y -> z -> IO r)
      -> (v -> w -> x -> y -> z -> SAWBackend r)
lift5 = (lift4 .)

lift6 :: (u -> v -> w -> x -> y -> z -> IO r)
      -> (u -> v -> w -> x -> y -> z -> SAWBackend r)
lift6 = (lift5 .)

-- | Returns shared term of type LLVMType representing given state.
sbsMemType :: SAWBackendState -> MemType -> IO Term
sbsMemType sbs btp = do
  let sc = sbsContext sbs
  case btp of
    IntType w -> scIntType sc w
    FloatType  -> pure (sbsFloatType sbs)
    DoubleType -> pure (sbsDoubleType sbs)
    PtrType _  -> pure (sbsPtrType sbs)
    ArrayType n tp ->
      join $ sbsArrayTypeFn sbs <$> scNat sc (fromIntegral n)
                                <*> sbsMemType sbs tp
    VecType n tp ->
      join $ sbsVecTypeFn sbs <$> scNat sc (fromIntegral n)
                              <*> sbsMemType sbs tp
    StructType si ->
      join $ sbsStructTypeFn sbs <$> sbsFieldInfo sbs (siFields si)

-- | Returns shared term of type StructFields for the given field info.
sbsFieldInfo :: SAWBackendState
             -> V.Vector FieldInfo
             -> IO Term
sbsFieldInfo sbs flds = do
    flds' <- traverse go flds
    scFieldInfo (sbsContext sbs) (V.toList flds')
  where go fi = do (,fromIntegral (fiPadding fi)) <$> sbsMemType sbs (fiType fi)

-- | Returns shared term of type StructFields for the given field info.
scFieldInfo :: SharedContext
            -> [(Term, Natural)] -- ^ terms of type LLVMType with padding amounts
            -> IO Term
scFieldInfo sc [] = scApplyLLVM_EmptyFields sc
scFieldInfo sc ((tp, p) : tps) = do
  pt <- scNat sc p
  f <- scFieldInfo sc tps
  scApplyLLVM_ConsFields sc tp pt f

-- | Returns shared terms f :: StructFields and i :: StructIndex f
scStructIndex :: SharedContext
              -> [(Term, Natural)] -- ^ terms of type LLVMType with padding amounts
              -> Int
              -> IO (Term, Term)
scStructIndex _ [] _ = error "scStructIndex: index out of bounds"
scStructIndex sc ((tp, p) : tps) 0 = do
  pt <- scNat sc p
  f <- scFieldInfo sc tps
  f' <- scApplyLLVM_ConsFields sc tp pt f
  i' <- scApplyLLVM_ZeroIndex sc tp pt f
  return (f', i')
scStructIndex sc ((tp, p) : tps) n = do
  pt <- scNat sc p
  (f, i) <- scStructIndex sc tps (n - 1)
  f' <- scApplyLLVM_ConsFields sc tp pt f
  i' <- scApplyLLVM_SuccIndex sc tp pt f i
  return (f', i')

scIntType :: SharedContext -> BitWidth -> IO Term
scIntType sc w = scApplyLLVM_IntType sc =<< scBitwidth sc w

typedExprEvalFn :: forall v
                 . SAWBackendState
                -> TypedExpr v
                -> IO (ExprEvalFn v Term)
typedExprEvalFn sbs expr0 = do
  let dl = sbsDataLayout sbs
  let sc = sbsContext sbs
  let eval1 :: v
            -> (Term -> IO Term)
            -> ExprEvalFn v Term
      eval1 v fn = ExprEvalFn $ \eval -> liftIO . fn =<< eval v
  let evalBin x y op = evalBin' x y (scApply2 sc op)
      evalBin' x y f = ExprEvalFn $ \eval ->
         liftIO . uncurry f =<< both eval (x,y)
  let mkLLVMVecLit mtp v = do
        tp <- scApplyLLVM_value sc =<< sbsMemType sbs mtp
        return $ ExprEvalFn $ \eval -> liftIO . scVecLit sc tp =<< traverse eval v
  let constEvalFn v = ExprEvalFn $ \_ -> return v
      -- | Apply truncation or extension ops to term.
  let extOp :: (SharedContext -> Term -> Term -> Term -> IO Term)
            -> (SharedContext -> Term -> Term -> Term -> Term -> IO Term)
            -> OptVectorLength
            -> BitWidth -- ^ First constant argument to op
            -> BitWidth -- ^ Second constant width argument.
            -> v
            -> IO (ExprEvalFn v Term)
      extOp fn fnV mn dw rw v = do
        dt <- scBitwidth sc dw
        rt <- scBitwidth sc rw
        case fromIntegral <$> mn of
          Nothing -> do
            return $ eval1 v (fn sc dt rt)
          Just n  -> do
            nt <- scNat sc n
            return $ eval1 v (fnV sc nt dt rt)
  let resizeOp :: OptVectorLength
               -> BitWidth -- ^ Input width
               -> BitWidth -- ^ Result bitwith
               -> v
               -> IO (ExprEvalFn v Term)
      resizeOp mn iw rw v
        | iw < rw =
          extOp scApplyLLVM_llvmZExt  scApplyLLVM_llvmZExtV mn  (rw - iw) iw v
        | iw > rw =
          extOp scApplyLLVM_llvmTrunc scApplyLLVM_llvmTruncV mn (iw - rw) rw v
        | otherwise = return $ ExprEvalFn $ (\eval -> eval v)
  case expr0 of
    SValInteger w v  -> constEvalFn <$> scLLVMIntConst sc w v
    SValFloat  v     -> constEvalFn <$> scFloat sc v
    SValDouble v     -> constEvalFn <$> scDouble sc v
    SValNull{}       -> constEvalFn <$> scLLVMIntConst sc (ptrBitwidth dl) 0
    SValArray  mtp v -> mkLLVMVecLit mtp v
    SValVector mtp v -> mkLLVMVecLit mtp v
    SValStruct si vals -> assert (siFieldCount si == V.length vals) $ do
      sbsStructValue sbs (siFields si `V.zip` vals)
    IntArith iop mn w x y -> do
      case fromIntegral <$> mn of
        Nothing -> do
          let defOp :: (SharedContext -> Term -> IO Term)
                    -> BitWidth
                    -> IO (Term -> Term -> IO Term)
              defOp fn w' =
                fmap (scApply2 sc) (fn sc =<< scBitwidth sc w')
          evalBin' x y <$>
            case iop of
              Add{}  -> sbsAdd sbs w
              Sub{}  -> sbsSub sbs w
              Mul{}  -> defOp scApplyLLVM_llvmMul  w
              UDiv{} -> defOp scApplyLLVM_llvmUDiv w
              URem   -> defOp scApplyLLVM_llvmURem w
              SDiv{} | w > 0 -> defOp scApplyLLVM_llvmSDiv (w-1)
              SRem   | w > 0 -> defOp scApplyLLVM_llvmSRem (w-1)
              Shl{}  -> defOp scApplyLLVM_llvmShl  w
              Lshr{} -> defOp scApplyLLVM_llvmLShr w
              Ashr{} | w > 0 -> defOp scApplyLLVM_llvmAShr (w-1)
              And    -> defOp scApplyLLVM_llvmAnd  w
              Or     -> defOp scApplyLLVM_llvmOr   w
              Xor    -> defOp scApplyLLVM_llvmXor  w
              _ -> fail "Illegal arguments to intArith"
        Just n  -> do
          let defOp fn w' =
                do n_tm <- scNat sc n
                   w_tm <- scBitwidth sc w'
                   fn sc n_tm w_tm
          evalBin x y <$>
            case iop of
              Add{}  -> defOp scApplyLLVM_llvmAddV  w
              Sub{}  -> defOp scApplyLLVM_llvmSubV  w
              Mul{}  -> defOp scApplyLLVM_llvmMulV  w
              UDiv{} -> defOp scApplyLLVM_llvmUDivV w
              URem   -> defOp scApplyLLVM_llvmURemV w
              SDiv{} | w > 0 -> defOp scApplyLLVM_llvmSDivV (w-1)
              SRem   | w > 0 -> defOp scApplyLLVM_llvmSRemV (w-1)
              Shl{}          -> defOp scApplyLLVM_llvmShlV  w
              Lshr{}         -> defOp scApplyLLVM_llvmLShrV w
              Ashr{} | w > 0 -> defOp scApplyLLVM_llvmAShrV (w-1)
              And            -> defOp scApplyLLVM_llvmAndV  w
              Or             -> defOp scApplyLLVM_llvmOrV   w
              Xor            -> defOp scApplyLLVM_llvmXorV  w
              _ -> fail "Illegal arguments to intArith"
    PtrAdd x y ->
      return $ evalBin' x y (MM.tgAddPtr (smGenerator sbs))
    UAddWithOverflow w x y -> do
      let si = mkStructInfo dl False [IntType 1, IntType w]
      let [p0,p1] = V.toList $ fromIntegral <$> fiPadding <$> siFields si
      fmap (evalBin' x y) $
        (return $ scApplyLLVM_llvmAddWithOverflow sc)
              <*> scBitwidth sc w
              <*> scNat sc p0
              <*> scNat sc p1
    SAddWithOverflow w x y -> do
      let si = mkStructInfo dl False [IntType 1, IntType w]
      let [p0,p1] = V.toList $ fromIntegral <$> fiPadding <$> siFields si
      fmap (evalBin' x y) $
        (return $ scApplyLLVM_llvmSAddWithOverflow sc)
              <*> scBitwidth sc w
              <*> scNat sc p0
              <*> scNat sc p1
    BSwap nb x ->
      fmap (eval1 x) $ (return $ scApplyLLVM_llvmBSwap sc) <*> scNat sc (fromIntegral nb)
    ICmp op mn stp x y -> do
        -- Get scalar type bitwidth.
        let w = either id (const (ptrBitwidth dl)) stp
        case fromIntegral <$> mn of
          Nothing -> do
            let defOp mkFn w' = fmap (evalBin x y) (mkFn sc =<< scBitwidth sc w')
            case op of
              Ieq  -> defOp scApplyLLVM_llvmIeq  w
              Ine  -> defOp scApplyLLVM_llvmIne w
              Iugt -> defOp scApplyLLVM_llvmIugt w
              Iuge -> defOp scApplyLLVM_llvmIuge w
              Iult -> defOp scApplyLLVM_llvmIult w
              Iule -> defOp scApplyLLVM_llvmIule w
              Isgt | w > 0 -> defOp scApplyLLVM_llvmIsgt (w-1)
              Isge | w > 0 -> defOp scApplyLLVM_llvmIsge (w-1)
              Islt | w > 0 -> defOp scApplyLLVM_llvmIslt (w-1)
              Isle | w > 0 -> defOp scApplyLLVM_llvmIsle (w-1)
              _ -> fail "Illegal arguments to signed comparison"
          Just n  -> do
            let defOp mkFnV w' =
                  (evalBin x y) <$>
                  do n_tm <- scNat sc n
                     w_tm <- scBitwidth sc w'
                     mkFnV sc n_tm w_tm
            case op of
              Ieq  -> defOp scApplyLLVM_llvmIeqV  w
              Ine  -> defOp scApplyLLVM_llvmIneV  w
              Iugt -> defOp scApplyLLVM_llvmIugtV w
              Iuge -> defOp scApplyLLVM_llvmIugeV w
              Iult -> defOp scApplyLLVM_llvmIultV w
              Iule -> defOp scApplyLLVM_llvmIuleV w
              Isgt | w > 0 -> defOp scApplyLLVM_llvmIsgtV (w-1)
              Isge | w > 0 -> defOp scApplyLLVM_llvmIsgeV (w-1)
              Islt | w > 0 -> defOp scApplyLLVM_llvmIsltV (w-1)
              Isle | w > 0 -> defOp scApplyLLVM_llvmIsleV (w-1)
              _ -> fail "Illegal arguments to signed comparison"
    Trunc mn iw v rw -> assert (iw >= rw) $
      extOp scApplyLLVM_llvmTrunc scApplyLLVM_llvmTruncV mn (iw - rw) rw v
    ZExt mn iw v rw -> assert (iw <= rw) $
      extOp scApplyLLVM_llvmZExt  scApplyLLVM_llvmZExtV  mn (rw - iw) iw v
    SExt mn iw v rw -> assert (iw <= rw) $
      extOp scApplyLLVM_llvmSExt scApplyLLVM_llvmSExtV mn (rw - iw) (iw - 1) v
    PtrToInt mn _ v rw -> resizeOp mn (ptrBitwidth dl) rw v
    IntToPtr mn iw v _ -> resizeOp mn iw (ptrBitwidth dl) v
    Select mn c tp x y -> do
      fn <- case mn of
              Nothing -> return $ scApplyLLVM_llvmSelect sc
              Just n ->
                do n_tm <- scNat sc (fromIntegral n)
                   return $ scApplyLLVM_llvmSelectV sc n_tm
      mtp <- sbsMemType sbs tp
      return $ ExprEvalFn $ \eval -> do
         join $ (\cv xv yv -> liftIO $ fn mtp cv xv yv) <$> eval c <*> eval x <*> eval y
    GetStructField si v i -> assert (i < siFieldCount si) $ do
      let go fi = (, fromIntegral (fiPadding fi)) <$> sbsMemType sbs (fiType fi)
      flds <- traverse go (V.toList (siFields si))
      (tps, ft) <- scStructIndex sc flds (fromIntegral i)
      let fn = scApplyLLVM_llvmStructElt sc
      return $ ExprEvalFn $ \eval -> (\val -> liftIO $ fn tps val ft) =<< eval v
    GetConstArrayElt n tp a i -> assert (i < n) $ do
      let fn = scApplyPrelude_at sc
      nt <- scNat sc (fromIntegral n)
      mtp <- sbsMemType sbs tp
      it <- scNat sc (fromIntegral i)
      return $ ExprEvalFn $ \eval -> (\val -> liftIO $ fn nt mtp val it) =<< eval a

-- | Lambda abstract term @t@ over all symbolic variables.
abstract :: SAWBackendState -> Term -> IO Term
abstract sbs t = do
  -- NB: reverse the list because sbs is stored with most recent variables first.
  ecs <- map (\(_, _, ec) -> ec) . reverse <$> readIORef (sbsVars sbs)
  scAbstractExts (sbsContext sbs) ecs t

scTermSAT :: AIG.IsAIG l g =>
             AIG.Proxy l g -> SAWBackendState -> Term -> IO (AIG.SatResult)
scTermSAT proxy sbs t = do
  t' <- abstract sbs t
  BB.withBitBlastedPred proxy (sbsContext sbs) (\_ -> Map.empty) t' $ \be l _domTys ->
    AIG.checkSat be l

scWriteAiger :: AIG.IsAIG l g
             => AIG.Proxy l g
             -> SAWBackendState
             -> FilePath
             -> [(MemType,Term)]
             -> IO ()
scWriteAiger proxy sbs path terms = do
  let sc = sbsContext sbs
  let ts = map snd terms
  t <- scTuple sc ts
  t' <- abstract sbs t
  BB.withBitBlastedTerm proxy sc (\_ -> Map.empty) t' $ \be ls -> do
    AIG.writeAiger path (AIG.Network be (AIG.bvToList ls))

scWriteCNF :: AIG.IsAIG l g
           => AIG.Proxy l g
           -> SAWBackendState
           -> FilePath
           -> Term
           -> IO [Int]
scWriteCNF proxy sbs path t = do
  t' <- abstract sbs t
  BB.withBitBlastedPred proxy (sbsContext sbs) (\_ -> Map.empty) t' $ \be l _domTys -> do
  AIG.writeCNF be l path

scWriteSmtLib :: SharedContext
              -> FilePath
              -> BitWidth
              -> Term
              -> IO ()
scWriteSmtLib sc path w t = do
  let wn = fromIntegral w
  zero <- scBvConst sc wn 0
  wt <- scNat sc wn
  t' <- scBvEq sc wt t zero
  (_, lit) <- SBVSim.sbvSolve sc Map.empty [] t'
  writeFile path =<< generateSMTBenchmark True lit

-- Put in saw-core package?
getIfConds :: Term -> [Term]
getIfConds t = snd $ go (Set.empty, []) t
  where
    go acc@(idxs, conds) tm@(STApp{ stAppIndex = i })
      | Set.member i idxs = acc
      | otherwise         = termf (Set.insert i idxs, conds) tm
    go acc tm@(Unshared _) = termf acc tm

    termf acc@(idxs, conds) tm =
      case tm of
        (R.asMux -> Just (_ :*: ctm :*: ttm :*: etm)) ->
          let acc' = go (idxs, ctm : conds) ttm in go acc' etm
        STApp { stAppTermF = tf } -> foldl' go acc tf
        Unshared tf -> foldl' go acc tf

scSimplifyConds :: AIG.IsAIG l g =>
                   AIG.Proxy l g
                -> SAWBackendState
                -> SharedContext
                -> Term
                -> Term
                -> IO Term
scSimplifyConds proxy sbs sc assumptions t0 = do
  let conds = getIfConds t0
  -- Allow replacements only of conditions that do not contain locally
  -- bound variables, for simplicity.
  let closedConds = filter ((== emptyBitSet) . looseVars) conds
      andAssms = scAnd sc assumptions
      unsat c = (== Unsat) <$> scTermSAT proxy sbs c
  trueConds <- filterM (unsat <=< andAssms <=< scNot sc) closedConds
  falseConds <- filterM (unsat <=< andAssms) closedConds
  rules <- map ruleOfTerm <$> mapM (scTypeOfGlobal sc)
             [ "Prelude.ite_true"
             , "Prelude.ite_false"
             , "Prelude.ite_not"
             , "Prelude.ite_nest1"
             , "Prelude.ite_nest2"
             , "Prelude.ite_eq"
             , "Prelude.ite_bit_false_1"
             , "Prelude.ite_bit_true_1"
             , "Prelude.ite_bit"
             , "Prelude.not_not"
             , "Prelude.and_True1"
             , "Prelude.and_False1"
             , "Prelude.and_True2"
             , "Prelude.and_False2"
             , "Prelude.and_idem"
             , "Prelude.or_True1"
             , "Prelude.or_False1"
             , "Prelude.or_True2"
             , "Prelude.or_False2"
             , "Prelude.or_idem"
             , "Prelude.not_or"
             , "Prelude.not_and"
             ]
  let ss = addRules rules emptySimpset
  trueTerm <- scBool sc True
  falseTerm <- scBool sc False
  t <- rewriteSharedTerm sc ss t0
  t' <- foldM (\tcur c -> replaceTerm sc ss (c, trueTerm) tcur) t trueConds
  foldM (\tcur c -> replaceTerm sc ss (c, falseTerm) tcur) t' falseConds

intFromBV :: V.Vector Bool -> Integer
intFromBV v = go 0 0
  where n = V.length v
        go i r | i == n    = r
               | v V.! i   = go (i+1) $! r `setBit` i
               | otherwise = go (i+1) r

splitByWidths :: V.Vector a -> [BitWidth] -> Maybe [V.Vector a]
splitByWidths v [] | V.null v = Just []
                   | otherwise = Nothing
splitByWidths v (w:wl) = (i:) <$> splitByWidths v' wl
  where (i,v') = V.splitAt (fromIntegral w) v


scEvalTerm :: SAWBackendState -> [Bool] -> Term -> IO Term
scEvalTerm sbs inputs t = do
  -- NB: reverse the list because sbs is stored with most recent variables first.
  (widths,varIndices,_) <- unzip3 . reverse <$> readIORef (sbsVars sbs)
  case splitByWidths (V.fromList inputs) widths of
    Nothing -> fail "Incorrect number of inputs"
    Just wv -> do
      let sc = sbsContext sbs
      -- Make list of integers contants
      vals <- zipWithM (scLLVMIntConst sc) widths (intFromBV <$> wv)
      -- Create map of variable indices to integers.
      let m = Map.fromList (varIndices `zip` vals)
      -- Return instantiated t.
      scInstantiateExt sc m t

scTermMemPrettyPrinter :: MM.MemPrettyPrinter Term Term Term
scTermMemPrettyPrinter = pp
  where ppt _ = ppTerm defaultPPOpts
        pp = MM.PP { MM.ppPtr = ppt
                   , MM.ppCond = ppt
                   , MM.ppTerm = ppt
                   }


-- | Create a SAW backend.
createSAWBackend :: AIG.IsAIG l g
                 => AIG.Proxy l g
                 -> DataLayout
                 -> IO (SBE SAWBackend, SAWMemory)
createSAWBackend proxy dl = do
  sc0 <- mkSharedContext
  scLoadPreludeModule sc0
  scLoadCryptolModule sc0
  scLoadLLVMModule sc0
  (sbe, mem, _) <- createSAWBackend' proxy dl sc0
  return (sbe, mem)

createSAWBackend' :: AIG.IsAIG l g
                  => AIG.Proxy l g
                  -> DataLayout
                  -> SharedContext
                  -> IO (SBE SAWBackend, SAWMemory, SharedContext)
createSAWBackend' proxy dl sc0 = do
  modmap <- scGetModuleMap sc0
  let activeDefs = filter defPred $ allModuleDefs modmap
        where defPred d = defIdent d `Set.notMember` excludedDefs
              excludedDefs = Set.fromList
                [ "Prelude.append"
                , "Prelude.bvAdd"
                , "Prelude.bvAddWithCarry"
                , "Prelude.bvSub"
                , "Prelude.bvMul"
                , "Prelude.bvUDiv"
                , "Prelude.bvURem"
                , "Prelude.bvSDiv"
                , "Prelude.bvSRem"
                , "Prelude.bvShl"
                , "Prelude.bvShr"
                , "Prelude.bvSShr"
                , "Prelude.bvNot"
                , "Prelude.bvAnd"
                , "Prelude.bvOr"
                , "Prelude.bvXor"
                , "Prelude.bvEq"
                , "Prelude.bvugt"
                , "Prelude.bvuge"
                , "Prelude.bvult"
                , "Prelude.bvule"
                , "Prelude.bvsgt"
                , "Prelude.bvsge"
                , "Prelude.bvslt"
                , "Prelude.bvsle"
                , "Prelude.bvTrunc"
                , "Prelude.bvUExt"
                , "Prelude.bvSExt"
                , "Prelude.take"
                , "Prelude.drop"
                , "Prelude.ite"
                , "Prelude.and"
                , "Prelude.or"
                , "Prelude.not"
                , "Prelude.addNat"
                , "Prelude.mulNat"
                ]
  let eqs = [ "Prelude.ite_true"
            , "Prelude.ite_false"
            , "Prelude.ite_not"
            , "Prelude.ite_fold_not"
            , "Prelude.ite_nest1"
            , "Prelude.ite_nest2"
            , "Prelude.not_True"
            , "Prelude.not_False"
            , "Prelude.not_not"
            , "Prelude.and_True1"
            , "Prelude.and_False1"
            , "Prelude.and_True2"
            , "Prelude.and_False2"
            , "Prelude.or_True1"
            , "Prelude.or_False1"
            , "Prelude.or_True2"
            , "Prelude.or_False2"
            , "Prelude.take0"
            , "Prelude.drop0"
            , "Prelude.bveq_sameL"
            , "Prelude.bveq_sameR"
            , "Prelude.bveq_same2"
            , "Prelude.bvAddZeroL"
            , "Prelude.bvAddZeroR"
            , "LLVM.ite_same"
            , "LLVM.ite_false_false"
            , "LLVM.and_true2"
            , "LLVM.bvEq_commute_ite1"
            , "LLVM.bvule_sameL"
            , "LLVM.bvule_sameR"
            , "LLVM.bvule_same2"
            ]
  let conversions =
        natConversions
        ++ bvConversions
        ++ vecConversions
        ++ [ remove_ident_coerce
           , remove_ident_unsafeCoerce]
  simpSet <- scSimpset sc0 activeDefs eqs conversions
  let sc = rewritingSharedContext sc0 simpSet
  sbs <- mkBackendState dl sc

  boolType <- scApplyPrelude_Bool sc
  trueTerm <- scApplyPrelude_True sc
  let pNot = scApplyPrelude_not sc
  let pAnd = scApplyPrelude_and sc
  let iteFn = scApplyPrelude_ite sc

  let apply_bvEq = scApplyLLVM_llvmIeqBool sc

  let valueFn   = scApplyLLVM_value sc
  let intTypeFn = scApplyLLVM_IntType sc

  let sbe = SBE { sbeTruePred = trueTerm
                , applyIEq = \w x y -> SAWBackend $ do
                   join $ apply_bvEq <$> scBitwidth sc w ?? x ?? y
                , applyAnd  = lift2 pAnd
                , applyBNot = lift1 pNot
                , applyPredIte = lift3 (iteFn boolType)
                , applyIte = \tp x y z -> SAWBackend $ do
                    tpt <- scApplyLLVM_value sc =<< sbsMemType sbs tp
                    Right <$> iteFn tpt x y z
                , LLVM.asBool = R.asBool
                , prettyPredD = ppTerm defaultPPOpts
                , evalPred = \inputs t -> SAWBackend $ do
                    t' <- scEvalTerm sbs inputs t
                    case R.asBool t' of
                      Just b -> return b
                      Nothing ->
                        fail $ "Could not evaluate predicate as Boolean:\n" ++ showTerm t'
                , freshInt = \w -> SAWBackend $ do
                    vtp <- valueFn =<< intTypeFn =<< scBitwidth sc w
                    i <- scFreshGlobalVar sc
                    let ec = EC i "_" vtp
                    t <- scFlatTermF sc (ExtCns ec)
                    modifyIORef' (sbsVars sbs) ((w,i,ec):)
                    return t
                , simplifyConds = lift2 (scSimplifyConds proxy sbs sc)
                , typedExprEval = typedExprEvalFn sbs
                , applyTypedExpr = \expr -> SAWBackend $ do
                    ExprEvalFn fn <- typedExprEvalFn sbs expr
                    fn return
                , prettyTermD = ppTerm defaultPPOpts
                , asUnsignedInteger = asUnsignedBitvector
                , asSignedInteger   = asSignedBitvector
                , asConcretePtr     = asUnsignedBitvector (ptrBitwidth dl)
                , memDump      = \m _ -> SAWBackend $ do
                    print $ MM.ppMem scTermMemPrettyPrinter (m^.memState)
                , memLoad      = lift4 (smLoad sbs)
                , memStore     = lift5 (smStore sbs)
                , memCopy      = lift6 (smCopy sbs)
                , memAddDefine = lift3 (smAddDefine dl sc)
                , memInitGlobal =  \m mtp v -> SAWBackend $ do
                   case convertMemType (sbsDataLayout sbs) mtp of
                     Nothing -> fail "memtype given to smStore must be an even byte size."
                     Just tp -> do
                       ptr <- allocPtr sbs
                       let tg = smGenerator sbs
                       Just . (ptr,) <$> memState (MM.allocAndWriteMem tg MM.HeapAlloc ptr tp v) m
                , codeLookupSymbol = ((SAWBackend . return) .) . smLookupSymbol

                , stackAlloc     = lift5 (smAlloc sbs MM.StackAlloc)
                , heapAlloc      = lift5 (smAlloc sbs MM.HeapAlloc)
                , isAllocated    = lift3 (smIsAllocated sbs)

                , stackPushFrame = SAWBackend . return . (trueTerm,)
                                   . over memState MM.pushStackFrameMem
                , stackPopFrame  = SAWBackend . return . (memState %~ MM.popStackFrameMem)
                , memBranch      = SAWBackend . return . (memState %~ MM.branchMem)
                , memBranchAbort = SAWBackend . return . (memState %~ MM.branchAbortMem)
                , memMerge = \c x y -> SAWBackend $ return $ smMerge c x y

                , termSAT    = lift1 (scTermSAT proxy sbs)
                , writeAiger = lift2 (scWriteAiger proxy sbs)
                , writeCnf   = Just (lift2 (scWriteCNF proxy sbs))

                , writeSAWCore = Just $ \nm t -> SAWBackend $ do
                    writeFile nm (scWriteExternal t)
                , writeSmtLib = Just (lift3 (scWriteSmtLib sc))

                , evalAiger  = \inputs _ t -> SAWBackend $ scEvalTerm sbs inputs t
                , sbeRunIO   = runSAWBackend
                }
  return (sbe, emptySAWMemory, sc0)

_unused :: a
_unused = undefined
  scApplyLLVM_LLVMType
  scApplyLLVM_StructValue
  scApplyLLVM_arithmeticWithOverflowResult
  scApplyLLVM_binFn
  scApplyLLVM_binRel
  scApplyLLVM_ConsFields
  scApplyLLVM_EmptyFields
  scApplyLLVM_singleField
  scApplyLLVM_sbvVecZipWith
  scApplyLLVM_mkOverflowResult
  scApplyLLVM_liftVecBVRel
  scApplyLLVM_binVRel
  scApplyLLVM_bvMap
  scApplyLLVM_bvVecZipWith
  scApplyLLVM_liftBVRel
  scApplyLLVM_and_true2
  scApplyLLVM_bvEq_commute_ite1
  scApplyLLVM_bvule_same2
  scApplyLLVM_bvule_sameL
  scApplyLLVM_bvule_sameR
  scApplyLLVM_ite_false_false
  scApplyLLVM_ite_same
  scApplyLLVM_mkArrayType
  scApplyLLVM_mkDoubleType
  scApplyLLVM_mkFloatType
  scApplyLLVM_mkIntType
  scApplyLLVM_mkPtrType
  scApplyLLVM_StructFields
  scLLVM_StructIndex
  scApplyLLVM_getStructField
