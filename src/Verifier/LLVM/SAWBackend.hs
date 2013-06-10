{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -O0 #-}
module Verifier.LLVM.SAWBackend
  ( SAWBackend
  , SAWMemory
  , createSAWBackend
  ) where

import Control.Applicative hiding (empty)
import Control.Exception (assert)
import Control.Lens hiding (op, iact)
import Control.Monad
import Control.Monad.Maybe
import Control.Monad.State
import Data.Bits (setBit, shiftL)
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as LV
--import Text.PrettyPrint.Leijen hiding ((<$>))

import Verifier.SAW as SAW
import Verifier.SAW.BitBlast
import Verifier.SAW.Conversion
import qualified Verifier.SAW.Export.SMT.Version1 as SMT1
import qualified Verifier.SAW.Export.SMT.Version2 as SMT2
import Verifier.SAW.ParserUtils as SAW
import Verifier.SAW.Prelude
import Verifier.SAW.Prim
import qualified Verifier.SAW.Recognizer as R
import Verifier.SAW.Rewriter

import Verifier.LLVM.AST
import Verifier.LLVM.Backend as LLVM
import qualified Verifier.LLVM.MemModel as MM

import Verinf.Symbolic.Lit

#if !MIN_VERSION_base(4,6,0)
-- | Strict version of modifyIORef
-- Added for compatibility with GHC base 4.5 and 4.6
modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' r f = do
  v <- readIORef r
  writeIORef r $! f v 
#endif

preludeBVNatTermF :: TermF t
preludeBVNatTermF = FTermF $ GlobalDef (mkIdent preludeModuleName "bvNat")

nyi :: String -> a
nyi nm = error $ "(SAW backend) Not yet implemented: " ++ show nm

scBitwidth :: SharedContext s -> BitWidth -> IO (SharedTerm s)
scBitwidth sc w = scNat sc (toInteger w)

asUnsignedBitvector :: BitWidth -> SharedTerm s -> Maybe Integer
asUnsignedBitvector w s2 = do
  (s1, vt) <- R.asApp s2
  (s0, wt) <- R.asApp s1
  when (unwrapTermF  s0 /= preludeBVNatTermF) Nothing
  when (R.asNatLit wt /= Just (toInteger w)) Nothing
  R.asNatLit vt

scFloat :: SharedContext s -> Float -> IO (SharedTerm s)
scFloat sc v = scTermF sc (FTermF (FloatLit v))

scDouble :: SharedContext s -> Double -> IO (SharedTerm s)
scDouble sc v = scTermF sc (FTermF (DoubleLit v))

-- | Create a vector from a term representing its element types and the element.
scVecLit :: SharedContext s
         -> SharedTerm s -- ^ Type of vector elements.
         -> V.Vector (SharedTerm s) -- ^ Elements
         -> IO (SharedTerm s)
scVecLit sc tp v = scTermF sc (FTermF (ArrayValue tp v))

$(runDecWriter $ do
    prelude <- defineImport [|preludeModule|] preludeModule
    llvm <- defineModuleFromFile [prelude] "llvmModule" "saw/LLVM.sawcore"
    declareDefTermF prelude "ite"
    declareSharedModuleFns "LLVM" (decVal llvm)
 )

scResizeTerm :: SharedContext s
             -> BitWidth -- ^ Input width
             -> (BitWidth, SharedTerm s) -- ^ Result bitwith and term representing it.
             -> SharedTerm s
             -> IO (SharedTerm s)
scResizeTerm sc iw (rw,rt) v
  | iw < rw = do
      fn <- scApplyLLVMLlvmZExt sc
      dt <- scBitwidth sc (rw - iw)
      fn dt rt v
  | iw > rw = do
      fn <- scApplyLLVMLlvmTrunc sc
      dt <- scBitwidth sc (iw - rw)
      fn dt rt v
  | otherwise = return v

-- | Create a bitvector from a constant.
scLLVMIntConst :: SharedContext s
                 -> BitWidth -- ^ Result width with corresponding term.
                 -> Integer -- ^ Value of bitvector.
                 -> IO (SharedTerm s)
scLLVMIntConst sc w v = do
  wt <- scBitwidth sc w
  scLLVMIntConst' sc (w,wt) v

-- | Create a bitvector from a constant.
scLLVMIntConst' :: SharedContext s
                  -> (BitWidth, SharedTerm s) -- ^ Result width with corresponding term.
                  -> Integer -- ^ Value of bitvector.
                  -> IO (SharedTerm s)
scLLVMIntConst' sc (w,wt) v = do
  cfn <- scApplyLLVMLlvmIntConstant sc 
  cfn wt =<< scNat sc (v `mod` 2^(toInteger w))

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

scFreshPtr :: SharedContext s -> DataLayout -> IO (SharedTerm s)
scFreshPtr sc dl = do
  valueFn <- scApplyLLVMValue sc
  let w = ptrBitwidth dl
  scFreshGlobal sc "_" =<< valueFn =<< scIntType sc w 

-- | Set of shared term variables that refer to allocations.
type SharedTermSetRef s = IORef (Set (SharedTerm s))

asApp2 :: SharedTerm s -> Maybe (SharedTerm s, SharedTerm s, SharedTerm s)
asApp2 t = do
  (t1,a2) <- R.asApp t
  (t0,a1) <- R.asApp t1
  return (t0,a1,a2)

data SAWBackendState s l =
       SBS { sbsDataLayout :: DataLayout
           , sbsContext :: SharedContext s
             -- | Cache used for bitblasting shared terms.
           , sbsBCache :: BCache l
             -- | Stores list of fresh variables and their associated terms with
             -- most recently allocated variables first.
           , sbsVars :: IORef [(BitWidth,VarIndex, BValue l)]
             -- | Allocations added.
           , sbsAllocations :: SharedTermSetRef s
             -- | Width of pointers in bits as a nat.
           , sbsPtrWidth :: SharedTerm s
             -- | LLVM Type of a pointer
           , sbsPtrType    :: SharedTerm s
             -- | LLVM Type of floats.
           , sbsFloatType  :: SharedTerm s
             -- | LLVM Type for double
           , sbsDoubleType :: SharedTerm s
              -- | Creates LLVM type for arrays
           , sbsArrayTypeFn :: SharedTerm s -> SharedTerm s -> IO (SharedTerm s)
              -- | Creates LLVM type for vectors
           , sbsVecTypeFn :: SharedTerm s -> SharedTerm s -> IO (SharedTerm s)
              -- | Creates LLVM type for structs
           , sbsStructTypeFn :: SharedTerm s -> SharedTerm s -> IO (SharedTerm s)
              -- | Fieldtype constant
           , sbsFieldType :: SharedTerm s
           , sbsAdd :: BitWidth -> IO (SharedTerm s -> SharedTerm s -> IO (SharedTerm s))
           , sbsSub :: BitWidth -> IO (SharedTerm s -> SharedTerm s -> IO (SharedTerm s))
           , smGenerator :: MM.TermGenerator IO (SharedTerm s) (SharedTerm s) (SharedTerm s)
           }

data DecomposeResult s
   = BasePtr
   | OffsetPtr !(SharedTerm s) !(SharedTerm s)
   | SymbolicPtr

mkBackendState :: forall s l .
                  DataLayout
               -> BitEngine l
               -> SharedContext s
               -> IO (SAWBackendState s l)
mkBackendState dl be sc = do
  bc <- newBCache be
  vars <- newIORef []
  allocs <- newIORef Set.empty
  ptrWidth <- scBitwidth sc (ptrBitwidth dl)
  addFn <- scApplyLLVMLlvmAdd sc

  subFn <- scApplyLLVMLlvmSub sc

  -- LLVM Type imports
  ptrType <- join $ scApplyLLVMPtrType sc <*> scNat sc (toInteger (dl^.ptrSize))
  llvmFloatType  <- scApplyLLVMFloatType sc
  llvmDoubleType <- scApplyLLVMDoubleType sc
  arrayTypeFn  <- scApplyLLVMArrayType sc 
  vecTypeFn    <- scApplyLLVMVectorType sc 
  structTypeFn <- scApplyLLVMStructType sc
  fieldType <- scApplyLLVMFieldType sc

  trueTerm  <- scApplyPreludeTrue  sc
  falseTerm <- scApplyPreludeFalse sc

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

  let decomposeOffset = scIntAsConst' sc ptrWidth

  muxOp <- scTermF sc iteTermF

  andFn <- scApplyPreludeAnd sc
  orFn  <- scApplyPreludeOr  sc
  boolMuxOp <- join $ scApply sc muxOp <$> scPreludeBool sc
  intTypeFn <- scApplyLLVMIntType sc
  let mkTypeTerm :: MM.Type -> IO (SharedTerm s)
      mkTypeTerm tp0 =
        case MM.typeF tp0 of
          MM.Bitvector n -> intTypeFn =<< scNat sc (8*toInteger n)
          MM.Float  -> return llvmFloatType
          MM.Double -> return llvmDoubleType
          MM.Array n tp -> join $ arrayTypeFn <$> scNat sc (toInteger n) <*> mkTypeTerm tp
          MM.Struct flds -> join $ structTypeFn
                              <$> scNat sc (toInteger (V.length flds))
                              <*> fieldVFn flds
      fieldFn :: MM.Field MM.Type -> IO (SharedTerm s, Integer)
      fieldFn f = (, toInteger (MM.fieldPad f)) <$> mkTypeTerm (f^.MM.fieldVal)
      fieldVFn flds = scFieldInfo sc fieldType =<< traverse fieldFn flds

  intToFloat  <- scApplyLLVMLlvmIntToFloat sc
  intToDouble <- scApplyLLVMLlvmIntToDouble sc

  ptrEqOp <- scApplyLLVMLlvmIeqBool  sc
  ptrLeOp <- scApplyLLVMLlvmIuleBool sc

  appendInt <- scApplyLLVMLlvmAppendInt sc
  sliceFn <- scApplyLLVMLlvmIntSlice sc
  valueFn   <- scApplyLLVMValue sc
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
              , MM.tgMuxCond = scApply3 sc boolMuxOp

              , MM.tgConstBitvector = \w -> scLLVMIntConst sc (8*fromIntegral w)
              , MM.tgConstFloat  = scFloat sc
              , MM.tgConstDouble = scDouble sc
              , MM.tgApplyCtorF = \vcf ->
                 case vcf of
                   MM.ConcatBV xw x yw y -> do
                     xwt <- scNat sc $ toInteger xw `shiftL` 3
                     ywt <- scNat sc $ toInteger yw `shiftL` 3
                     case dl^.intLayout of
                       BigEndian    -> appendInt xwt ywt x y
                       LittleEndian -> appendInt ywt xwt y x         
                   MM.BVToFloat x -> intToFloat x 
                   MM.BVToDouble x -> intToDouble x
                   MM.ConsArray tp x n y -> do
                     consFn <- scApplyPreludeConsVec sc
                     tpt <- mkTypeTerm tp
                     nt <- scNat sc n
                     consFn tpt x nt y
                   MM.AppendArray tp m x n y -> do
                     appendFn <- scApplyPreludeAppend sc
                     join $ appendFn <$> scNat sc m
                                     <*> scNat sc n
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
                                 <$> scNat sc (8*toInteger i)
                                 <*> scNat sc (8*toInteger n)
                                 <*> scNat sc (8*toInteger o)
                                 ?? v
                  case vf of
                    MM.SelectLowBV m n v -> do
                      case dl^.intLayout of
                        BigEndian    -> slice n m 0 v -- High order bits of v.
                        LittleEndian -> slice 0 m n v -- low order bits of v.
                    MM.SelectHighBV m n v -> do
                      case dl^.intLayout of
                        BigEndian    -> slice 0 n m v -- Low order bits of v.
                        LittleEndian -> slice m n 0 v -- High order bits of v.
                    MM.FloatToBV v  -> join $ scApplyLLVMLlvmFloatToInt sc ?? v
                    MM.DoubleToBV v -> join $ scApplyLLVMLlvmDoubleToInt sc ?? v
                    MM.ArrayElt n tp o v -> do
                      join $ scApplyPreludeGet sc
                             <*> scNat sc (toInteger n)
                             <*> (valueFn =<< mkTypeTerm tp)
                             <*> pure v
                             <*> scFinConst sc (toInteger o) (toInteger n)
                    MM.FieldVal tps i v -> do
                      let n = toInteger (V.length tps)
                      join $ scApplyLLVMLlvmStructElt sc
                               <*> scNat sc n
                               <*> fieldVFn tps
                               <*> pure v
                               <*> scFinConst sc (toInteger i) n
              , MM.tgMuxTerm = \c tp x y -> do
                  tpt <- join $ scApplyLLVMValue sc <*> mkTypeTerm tp
                  scApply4 sc muxOp tpt c x y
              }

  return SBS { sbsDataLayout = dl
             , sbsContext = sc
             , sbsBCache  = bc
             , sbsVars    = vars
             , sbsAllocations  = allocs
             , sbsPtrWidth     = ptrWidth
             , sbsPtrType      = ptrType
             , sbsFloatType    = llvmFloatType
             , sbsDoubleType   = llvmDoubleType
             , sbsArrayTypeFn  = arrayTypeFn
             , sbsVecTypeFn    = vecTypeFn
             , sbsStructTypeFn = structTypeFn
             , sbsFieldType    = fieldType
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
scIntAsConst' :: SharedContext s -> SharedTerm s -> SharedTerm s -> IO (Maybe Integer)
scIntAsConst' sc w t =
  fmap R.asNatLit $ join $
    scApplyLLVMLlvmIntValueNat sc ?? w ?? t

type SAWMem s = MM.Mem (SharedTerm s) (SharedTerm s) (SharedTerm s)

data SAWMemory s = SAWMemory { _memSymbols :: Map (SharedTerm s) Symbol
                             , _memState :: MM.Mem (SharedTerm s) (SharedTerm s) (SharedTerm s)
                             }

emptySAWMemory :: SAWMemory s
emptySAWMemory = SAWMemory { _memSymbols = Map.empty
                           , _memState = MM.emptyMem
                           }

memSymbols :: Simple Lens (SAWMemory s) (Map (SharedTerm s) Symbol)
memSymbols = lens _memSymbols (\s v -> s { _memSymbols = v })

memState :: Simple Lens (SAWMemory s) (SAWMem s)
memState = lens _memState (\s v -> s { _memState = v })

smAddDefine :: DataLayout
            -> SharedContext s
            -> SAWMemory s
            -> Symbol
            -> [BlockLabel]
            -> IO (Maybe (SharedTerm s, [SharedTerm s], SAWMemory s))
smAddDefine dl sc m sym lbls = do
  symt <- scFreshPtr sc dl
  lblst <- traverse (\_ -> scFreshPtr sc dl) lbls
  let m' = m & memSymbols . at symt ?~ sym
  return $ Just (symt, lblst, m')

-- | Return symbol associated with address if any.
smLookupSymbol :: SAWMemory s -> SharedTerm s -> LookupSymbolResult
smLookupSymbol m t = 
  case m^.memSymbols^.at t of
    Just r -> LookupResult r
    Nothing -> Indeterminate

smAlloc :: SAWBackendState s l
        -> MM.AllocType
        -> SAWMemory s
        -> MemType
        -> BitWidth -- ^ Width of count.
        -> SharedTerm s -- ^ Count
        -> Alignment
        -> IO (AllocResult (SAWBackend s l))
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
  mulFn <- scApplyLLVMLlvmMul sc
  mulOp <- mulFn pwt
  totalSize <- scApplyAll sc mulOp [cnt', tpSize]
  -- Get true predicate.
  t <- scApplyPreludeTrue sc  
  -- Return allocation.
  -- Create new variable for base address.
  base <- allocPtr sbs
  -- Return successful allocation.
  let m' = m & memState %~ MM.allocMem atp base totalSize
  return (AResult t base m')

allocPtr :: SAWBackendState s l -> IO (SharedTerm s)
allocPtr sbs = do
  -- Create new variable for base address.
  s <- readIORef (sbsAllocations sbs)
  let nm = "$alloc" ++ show (Set.size s)
  base <- scFreshGlobal (sbsContext sbs) nm (sbsPtrType sbs)
  writeIORef (sbsAllocations sbs) $! Set.insert base s
  return base

mergeEq :: (Ord k, Eq a) => Map k a -> Map k a -> Map k a
mergeEq mx = Map.filterWithKey p
  where p k u = Map.lookup k mx == Just u

smMerge :: SharedTerm s -> SAWMemory s -> SAWMemory s -> SAWMemory s
smMerge c x y =
  SAWMemory { _memSymbols = mergeEq (x^.memSymbols) (y^.memSymbols)
            , _memState = MM.mergeMem c (x^.memState) (y^.memState)
            } 

-- | Return term value, length of fields, and vector with the types of the fields.
sbsStructValue :: SAWBackendState s l
               -> V.Vector (FieldInfo, v)
               -> IO (ExprEvalFn v (SharedTerm s))
sbsStructValue sbs flds = do
  let fn fi = do
         (,toInteger (fiPadding fi)) <$> sbsMemType sbs (fiType fi)
  createStructValue (sbsContext sbs) =<< (traverse . _1) fn flds

-- | Return term value, length of fields, and vector with the types of the fields.
createStructValue :: forall s v 
                   . SharedContext s
                     -- For each field, provide type, number of padding bytes, and value.
                  -> V.Vector ((SharedTerm s, Integer), v)
                  -> IO (ExprEvalFn v (SharedTerm s))
createStructValue sc flds = do
  fieldType <- scApplyLLVMFieldType sc
  let foldFn :: ((SharedTerm s, Integer), v)
             -> (Integer, ExprEvalFn v (SharedTerm s), SharedTerm s)
             -> IO (Integer, ExprEvalFn v (SharedTerm s), SharedTerm s)
      foldFn ((mtp,pad),expr) (n,ExprEvalFn reval, rvtp) = do
        nt <- scNat sc n
        padding <- scNat sc pad
        consStruct <- scApplyLLVMConsStruct sc
        let cfn = consStruct mtp padding nt rvtp
        let reval' = ExprEvalFn $ \eval ->
                      join $ ((liftIO .) . cfn) <$> eval expr <*> reval eval
        consVecFn <- scApplyPreludeConsVec sc
        entry <- scTuple sc [mtp,padding] 
        (n+1,reval',) <$> consVecFn fieldType entry nt rvtp
  -- Get initial value and type.
  emptyStruct <- scApplyLLVMEmptyStruct sc
  let eval0 = ExprEvalFn $ \_ -> return emptyStruct
  emptyFn <- scApplyPreludeEmptyVec sc
  tp0 <- emptyFn fieldType
  view _2 <$> foldrMOf folded foldFn (0, eval0, tp0) flds

scApply2 :: SharedContext s
         -> SharedTerm s
         -> SharedTerm s
         -> SharedTerm s
         -> IO (SharedTerm s)
scApply2 sc f x y = do
  g <- scApply sc f x 
  scApply sc g y

scApply3 :: SharedContext s
         -> SharedTerm s
         -> SharedTerm s
         -> SharedTerm s
         -> SharedTerm s
         -> IO (SharedTerm s)
scApply3 sc f x y z = do
  g <- scApply2 sc f x y 
  scApply sc g z

scApply4 :: SharedContext s
         -> SharedTerm s
         -> SharedTerm s
         -> SharedTerm s
         -> SharedTerm s
         -> SharedTerm s
         -> IO (SharedTerm s)
scApply4 sc f w x y z = do
  g <- scApply3 sc f w x y 
  scApply sc g z

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
applyMuxToLeaves :: (Applicative m, Monad m, Termlike t)
                 => (t -> a -> a -> m a) -- ^ Mux operation on result
                 -> (t -> m a) -- ^ Action on leaves.   
                 -> t -- ^ Term to act on
                 -> m a
applyMuxToLeaves mux action = go
  where go t =
          case R.asMux t of
            Nothing -> action t
            Just (_ :*: b :*: x :*: y) -> join $ mux b <$> go x <*> go y

smLoad :: forall s l .
          SAWBackendState s l
       -> SAWMemory s
       -> MemType
       -> SharedTerm s
       -> Alignment
       -> IO (SharedTerm s, SharedTerm s) -- Validity predicate and result.
smLoad sbs m tp0 ptr0 _a0 =
  case convertMemType (sbsDataLayout sbs) tp0 of
    Just tp -> applyMuxToLeaves mux action ptr0
      where mux c = MM.tgMuxPair (smGenerator sbs) c tp
            action ptr = MM.readMem (smGenerator sbs) ptr tp (m^.memState)
--            action ptr = trace (show msg) $ MM.readMem (smGenerator sbs) ptr tp (m^.memState)
--              where msg = text "Loading" <+> scPrettyTermDoc ptr <$$>
--                          MM.ppMem scTermMemPrettyPrinter (m^.memState)
    Nothing -> fail "smLoad must be given types that are even byte size."

smStore :: SAWBackendState s l
        -> SAWMemory s
        -> SharedTerm s -- ^ Address to store value at. 
        -> MemType      -- ^ Type of value
        -> SharedTerm s -- ^ Value to store
        -> Alignment
        -> IO (SharedTerm s, SAWMemory s) -- Predicate and new memory.
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
smCopy :: SAWBackendState s l
       -> SAWMemory s
       -> SharedTerm s -- ^ Destination pointer
       -> SharedTerm s -- ^ Source pointer
       -> BitWidth  -- ^ Bitwidth for counting number of bits.
       -> SharedTerm s -- ^ Number of bytes to copy.
       -> SharedTerm s -- ^ Alignment in bytes (should have 32-bit bits)
       -> IO (SharedTerm s, SAWMemory s)
smCopy sbs m dst src w sz0 _ = do
  sz <- scResizeTerm (sbsContext sbs) w
           (ptrBitwidth (sbsDataLayout sbs), sbsPtrWidth sbs) sz0 
  (c,ms) <- MM.copyMem (smGenerator sbs) dst src sz (m^.memState)
  return (c, m & memState .~ ms)

data SAWBackend s l a = SAWBackend { runSAWBackend :: IO a }
  deriving (Functor)

type instance SBETerm (SAWBackend s l) = SharedTerm s
type instance SBEPred (SAWBackend s l) = SharedTerm s
type instance SBEMemory (SAWBackend s l) = SAWMemory s

lift1 :: (x -> IO r) -> (x -> SAWBackend s l r)
lift1 = (SAWBackend .)

lift2 :: (x -> y -> IO r)
      -> (x -> y -> SAWBackend s l r)
lift2 = (lift1 .)

lift3 :: (x -> y -> z -> IO r)
      -> (x -> y -> z -> SAWBackend s l r)
lift3 = (lift2 .)

lift4 :: (w -> x -> y -> z -> IO r)
      -> (w -> x -> y -> z -> SAWBackend s l r)
lift4 = (lift3 .)

lift5 :: (v -> w -> x -> y -> z -> IO r)
      -> (v -> w -> x -> y -> z -> SAWBackend s l r)
lift5 = (lift4 .)

lift6 :: (u -> v -> w -> x -> y -> z -> IO r)
      -> (u -> v -> w -> x -> y -> z -> SAWBackend s l r)
lift6 = (lift5 .)

-- | Returns share term representing given state.
sbsMemType :: SAWBackendState s l -> MemType -> IO (SharedTerm s)
sbsMemType sbs btp = do
  let sc = sbsContext sbs
  case btp of
    IntType w -> scIntType sc w
    FloatType  -> pure (sbsFloatType sbs)
    DoubleType -> pure (sbsDoubleType sbs)
    PtrType _  -> pure (sbsPtrType sbs)
    ArrayType n tp -> 
      join $ sbsArrayTypeFn sbs <$> scNat sc (toInteger n)
                                <*> sbsMemType sbs tp
    VecType n tp ->
      join $ sbsVecTypeFn sbs <$> scNat sc (toInteger n)
                              <*> sbsMemType sbs tp
    StructType si ->
      join $ sbsStructTypeFn sbs 
               <$> scNat sc (toInteger (siFieldCount si))
               <*> sbsFieldInfo sbs (siFields si)

-- | Returns term (tp,padding) for the given field info. 
sbsFieldInfo :: SAWBackendState s l
             -> V.Vector FieldInfo
             -> IO (SharedTerm s)
sbsFieldInfo sbs flds = do
    flds' <- traverse go flds
    scFieldInfo (sbsContext sbs) (sbsFieldType sbs) flds'
  where go fi = do (,toInteger (fiPadding fi)) <$> sbsMemType sbs (fiType fi)

-- | Returns term (tp,padding) for the given field info. 
scFieldInfo :: SharedContext s
              -> SharedTerm s -- ^ Field type function
              -> V.Vector (SharedTerm s, Integer)
              -> IO (SharedTerm s)
scFieldInfo sc ftp flds = scVecLit sc ftp =<< traverse go flds
  where go (tp,p) = scNat sc p >>= \pt -> scTuple sc [tp,pt]


scFinConst :: SharedContext s
           -> Integer -- ^ Index
           -> Integer -- ^ Bound n
           -> IO (SharedTerm s)
scFinConst sc i n | i < n = do
  fv <- scApplyPreludeFinVal sc
  join $ fv <$> scNat sc i <*> scNat sc (n - (i + 1))
scFinConst _ _ _ = error "illegal arguments to scFinConst"

scIntType :: SharedContext s -> BitWidth -> IO (SharedTerm s)
scIntType sc w = join $ scApplyLLVMIntType sc <*> scBitwidth sc w

typedExprEvalFn :: forall s l v 
                 . SAWBackendState s l
                -> TypedExpr v
                -> IO (ExprEvalFn v (SharedTerm s))
typedExprEvalFn sbs expr0 = do
  let dl = sbsDataLayout sbs
  let sc = sbsContext sbs
  let eval1 :: v
            -> (SharedTerm s -> IO (SharedTerm s))
            -> ExprEvalFn v (SharedTerm s)
      eval1 v fn = ExprEvalFn $ \eval -> liftIO . fn =<< eval v       
  let evalBin x y op = evalBin' x y (scApply2 sc op)
      evalBin' x y f = ExprEvalFn $ \eval ->
         liftIO . uncurry f =<< both eval (x,y)
  let mkVecLit mtp v = do
        tp <- join $ scApplyLLVMValue sc <*> sbsMemType sbs mtp
        return $ ExprEvalFn $ \eval -> liftIO . scVecLit sc tp =<< traverse eval v
  let constEvalFn v = ExprEvalFn $ \_ -> return v
      -- | Apply truncation or extension ops to term. 
  let extOp :: (SharedContext s
                    -> IO (SharedTerm s -> SharedTerm s
                                        -> SharedTerm s
                                        -> IO (SharedTerm s)))
            -> (SharedContext s
                    -> IO (SharedTerm s -> SharedTerm s
                                        -> SharedTerm s
                                        -> SharedTerm s
                                        -> IO (SharedTerm s)))
            -> OptVectorLength
            -> BitWidth -- ^ First constant argument to op
            -> BitWidth -- ^ Second constant width argument.
            -> v
            -> IO (ExprEvalFn v (SharedTerm s))
      extOp fn fnV mn dw rw v = do
        dt <- scBitwidth sc dw
        rt <- scBitwidth sc rw
        case mn of
          Nothing -> do
            f <- fn sc
            return $ eval1 v (f dt rt)
          Just n  -> do
            f <- fnV sc
            nt <- scNat sc (toInteger n)
            return $ eval1 v (f nt dt rt)
  let resizeOp :: OptVectorLength
               -> BitWidth -- ^ Input width
               -> BitWidth -- ^ Result bitwith
               -> v
               -> IO (ExprEvalFn v (SharedTerm s))
      resizeOp mn iw rw v
        | iw < rw =
          extOp scApplyLLVMLlvmZExt  scApplyLLVMLlvmZExtV mn  (rw - iw) iw v
        | iw > rw =
          extOp scApplyLLVMLlvmTrunc scApplyLLVMLlvmTruncV mn (iw - rw) rw v
        | otherwise = return $ ExprEvalFn $ (\eval -> eval v)
  case expr0 of
    SValInteger w v  -> constEvalFn <$> scLLVMIntConst sc w v
    SValFloat  v     -> constEvalFn <$> scFloat sc v
    SValDouble v     -> constEvalFn <$> scDouble sc v
    SValNull{}       -> constEvalFn <$> scLLVMIntConst sc (ptrBitwidth dl) 0
    SValArray  mtp v -> mkVecLit mtp v
    SValVector mtp v -> mkVecLit mtp v
    SValStruct si vals -> assert (siFieldCount si == V.length vals) $ do
      sbsStructValue sbs (siFields si `V.zip` vals)
    IntArith iop mn w x y -> do
      case mn of
        Nothing -> do
          let defOp :: (SharedContext s -> IO (SharedTerm s -> IO (SharedTerm s)))
                    -> BitWidth
                    -> IO (SharedTerm s -> SharedTerm s -> IO (SharedTerm s))
              defOp fn w' =
                fmap (scApply2 sc) $ join $ fn sc <*> scBitwidth sc w'
          evalBin' x y <$>
            case iop of
              Add{}  -> sbsAdd sbs w
              Sub{}  -> sbsSub sbs w
              Mul{}  -> defOp scApplyLLVMLlvmMul  w
              UDiv{} -> defOp scApplyLLVMLlvmUDiv w
              URem   -> defOp scApplyLLVMLlvmURem w
              SDiv{} | w > 0 -> defOp scApplyLLVMLlvmSDiv (w-1)
              SRem   | w > 0 -> defOp scApplyLLVMLlvmSRem (w-1)
              Shl{}  -> defOp scApplyLLVMLlvmShl  w
              Lshr{} -> defOp scApplyLLVMLlvmLShr w
              Ashr{} | w > 0 -> defOp scApplyLLVMLlvmAShr (w-1)
              And    -> defOp scApplyLLVMLlvmAnd  w
              Or     -> defOp scApplyLLVMLlvmOr   w
              Xor    -> defOp scApplyLLVMLlvmXor  w
              _ -> fail "Illegal arguments to intArith"
        Just n  -> do
          let defOp fn w' =
                join $ fn sc <*> scNat sc (toInteger n) <*> scBitwidth sc w'
          evalBin x y <$>
            case iop of
              Add{}  -> defOp scApplyLLVMLlvmAddV  w
              Sub{}  -> defOp scApplyLLVMLlvmSubV  w
              Mul{}  -> defOp scApplyLLVMLlvmMulV  w
              UDiv{} -> defOp scApplyLLVMLlvmUDivV w
              URem   -> defOp scApplyLLVMLlvmURemV w
              SDiv{} | w > 0 -> defOp scApplyLLVMLlvmSDivV (w-1)
              SRem   | w > 0 -> defOp scApplyLLVMLlvmSRemV (w-1)
              Shl{}          -> defOp scApplyLLVMLlvmShlV  w
              Lshr{}         -> defOp scApplyLLVMLlvmLShrV w
              Ashr{} | w > 0 -> defOp scApplyLLVMLlvmAShrV (w-1)
              And            -> defOp scApplyLLVMLlvmAndV  w
              Or             -> defOp scApplyLLVMLlvmOrV   w
              Xor            -> defOp scApplyLLVMLlvmXorV  w
              _ -> fail "Illegal arguments to intArith"
    PtrAdd x y ->
      return $ evalBin' x y (MM.tgAddPtr (smGenerator sbs))
    UAddWithOverflow w x y -> do
      let si = mkStructInfo dl False [IntType 1, IntType w]
      let [p0,p1] = V.toList $ fiPadding <$> siFields si
      fmap (evalBin' x y) $
        scApplyLLVMLlvmAddWithOverflow sc
              <*> scBitwidth sc w
              <*> scNat sc (toInteger p0)
              <*> scNat sc (toInteger p1)
    ICmp op mn stp x y -> do
        -- Get scalar type bitwidth.
        let w = either id (const (ptrBitwidth dl)) stp
        case mn of
          Nothing -> do
            let defOp mkFn w' = fmap (evalBin x y) $ join $ mkFn sc <*> scBitwidth sc w'
            case op of
              Ieq  -> defOp scApplyLLVMLlvmIeq  w
              Ine  -> defOp scApplyLLVMLlvmIne w
              Iugt -> defOp scApplyLLVMLlvmIugt w
              Iuge -> defOp scApplyLLVMLlvmIuge w 
              Iult -> defOp scApplyLLVMLlvmIult w 
              Iule -> defOp scApplyLLVMLlvmIule w
              Isgt | w > 0 -> defOp scApplyLLVMLlvmIsgt (w-1)
              Isge | w > 0 -> defOp scApplyLLVMLlvmIsge (w-1)
              Islt | w > 0 -> defOp scApplyLLVMLlvmIslt (w-1)
              Isle | w > 0 -> defOp scApplyLLVMLlvmIsle (w-1)
              _ -> fail "Illegal arguments to signed comparison"
          Just n  -> do
            let defOp mkFnV w' = fmap (evalBin x y) $ join $
                   mkFnV sc <*> scNat sc (toInteger n)
                            <*> scBitwidth sc w'
            case op of
              Ieq  -> defOp scApplyLLVMLlvmIeqV  w
              Ine  -> defOp scApplyLLVMLlvmIneV  w
              Iugt -> defOp scApplyLLVMLlvmIugtV w
              Iuge -> defOp scApplyLLVMLlvmIugeV w
              Iult -> defOp scApplyLLVMLlvmIultV w
              Iule -> defOp scApplyLLVMLlvmIuleV w
              Isgt | w > 0 -> defOp scApplyLLVMLlvmIsgtV (w-1)
              Isge | w > 0 -> defOp scApplyLLVMLlvmIsgeV (w-1)
              Islt | w > 0 -> defOp scApplyLLVMLlvmIsltV (w-1)
              Isle | w > 0 -> defOp scApplyLLVMLlvmIsleV (w-1)
              _ -> fail "Illegal arguments to signed comparison"
    Trunc mn iw v rw -> assert (iw >= rw) $
      extOp scApplyLLVMLlvmTrunc scApplyLLVMLlvmTruncV mn (iw - rw) rw v
    ZExt mn iw v rw -> assert (iw <= rw) $
      extOp scApplyLLVMLlvmZExt  scApplyLLVMLlvmZExtV  mn (rw - iw) iw v
    SExt mn iw v rw -> assert (iw <= rw) $
      extOp scApplyLLVMLlvmSExt scApplyLLVMLlvmSExtV mn (rw - iw) (iw - 1) v
    PtrToInt mn _ v rw -> resizeOp mn (ptrBitwidth dl) rw v
    IntToPtr mn iw v _ -> resizeOp mn iw (ptrBitwidth dl) v
    Select mn c tp x y -> do 
      fn <- case mn of 
              Nothing -> scApplyLLVMLlvmSelect sc
              Just n -> do
                fn <- scApplyLLVMLlvmSelectV sc
                fn <$> scNat sc (toInteger n)
      mtp <- sbsMemType sbs tp       
      return $ ExprEvalFn $ \eval -> do
         join $ (\cv xv yv -> liftIO $ fn mtp cv xv yv) <$> eval c <*> eval x <*> eval y 
    GetStructField si v i -> assert (i < siFieldCount si) $ do
      let n = toInteger (siFieldCount si)

      nt <- scNat sc n
      tps <- sbsFieldInfo sbs (siFields si)
      ft <- scFinConst sc (toInteger i) (toInteger (siFieldCount si))
      fn <- scApplyLLVMLlvmStructElt sc
      return $ ExprEvalFn $ \eval -> (\val -> liftIO $ fn nt tps val ft) =<< eval v
    GetConstArrayElt n tp a i -> assert (i < n) $ do
      fn <- scApplyLLVMLlvmArrayElt sc
      nt <- scNat sc (toInteger n)
      mtp <- sbsMemType sbs tp
      it <- scFinConst sc (toInteger i) (toInteger n)
      return $ ExprEvalFn $ \eval -> (\val -> liftIO $ fn nt mtp val it) =<< eval a

-- | Returns value and rest out of construct.
asConsStruct :: (Monad m, Termlike t) => t -> m (t, t)
asConsStruct t = do
  ("LLVM.ConsStruct", [_, _, _, _, v, r]) <- R.asCtor t
  return (v,r)

structElt :: SharedTerm s -> Nat -> Maybe (SharedTerm s)
structElt t 0 = fst <$> asConsStruct t
structElt t i = assert (i > 0) $ do
  (_,r) <- asConsStruct t
  structElt r (i-1)

getStructElt :: Conversion (SharedTerm s)
getStructElt = Conversion $ 
    thenMatcher (asGlobalDef "LLVM.llvmStructElt" 
                   <:> asAny
                   <:> asAny       -- Types
                   <:> asAny   -- Struct 
                   <:> asFinValLit -- Index
                  )
                (\(_ :*: s :*: i) -> 
                   return <$> structElt s (finVal i))


evalAppendInt :: Conversion (SharedTerm s)
evalAppendInt = Conversion $
  thenMatcher (asGlobalDef "LLVM.llvmAppendInt"
                <:> asAnyNatLit
                <:> asAnyNatLit
                <:> asBvNatLit
                <:> asBvNatLit)
              (\(_ :*: u :*: v :*: x :*: y) ->
                  let r = (unsigned x `shiftL` fromIntegral v) + unsigned y
                    in Just (mkBvNat (u + v) r))

scWriteAiger :: (Eq l, Storable l)
             => SAWBackendState s l
             -> FilePath
             -> [(MemType,SharedTerm s)]
             -> IO ()
scWriteAiger sbs path terms = do
  let bc = sbsBCache sbs
  mbits <- runMaybeT $ mapM (MaybeT . bitBlastWith bc . snd) terms
  case mbits of
    Nothing -> fail "Could not write Aig as term could not be bitblasted."
    Just bits -> do
      inputValues <- fmap (view _3) <$> readIORef (sbsVars sbs)
      let inputs  = LV.concat $ flattenBValue <$> inputValues
      let outputs = LV.concat $ flattenBValue <$> bits
      beWriteAigerV (bcEngine bc) path inputs outputs

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


scEvalTerm :: SAWBackendState s l -> [Bool] -> SharedTerm s -> IO (SharedTerm s)
scEvalTerm sbs inputs t = do
  (widths,varIndices,_) <- unzip3 <$> readIORef (sbsVars sbs)
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

runStateFromRef :: IORef t -> StateT t IO a -> IO a
runStateFromRef r a = do
  s <- readIORef r
  (v,s') <- runStateT a s
  writeIORef r $! s'
  return v

-- | Gets elements in list from lens, and clears list.
getWarnings :: (Applicative m, MonadState s m) => Simple Lens s [w] -> m [w]
getWarnings l = use l <* assign l []

scTermMemPrettyPrinter :: MM.MemPrettyPrinter (SharedTerm s) (SharedTerm s) (SharedTerm s)
scTermMemPrettyPrinter = pp
  where ppt _ = scPrettyTermDoc 
        pp = MM.PP { MM.ppPtr = ppt
                   , MM.ppCond = ppt
                   , MM.ppTerm = ppt
                   }


remove_ident_coerce :: (Eq t, Termlike t) => Conversion t
remove_ident_coerce = Conversion $ thenMatcher pat action
  where pat = asGlobalDef "Prelude.coerce" <:> asAny <:> asAny <:> asAny <:> asAny
        action (() :*: t :*: f :*: _prf :*: x)
          | t == f = return (return x)
          | otherwise = fail "Cannot remove coerce."

remove_ident_unsafeCoerce :: (Eq t, Termlike t) => Conversion t
remove_ident_unsafeCoerce = Conversion $ thenMatcher pat action
  where pat = asGlobalDef "Prelude.unsafeCoerce" <:> asAny <:> asAny <:> asAny
        action (() :*: t :*: f :*: x)
          | t == f = return (return x)
          | otherwise = fail "Cannot remove unsafeCoerce."

-- | Create a SAW backend.
createSAWBackend :: (Eq l, Storable l)
                 => BitEngine l
                 -> DataLayout
                 -> MemGeom
                 -> IO (SBE (SAWBackend s l), SAWMemory s)
createSAWBackend be dl _mg = do
  sc0 <- mkSharedContext llvmModule
  let activeDefs = filter defPred $ allModuleDefs llvmModule
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
                , "Prelude.bvMbit"
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
                , "Prelude.vTake"
                , "Prelude.vDrop"
                , "LLVM.llvmAppendInt"
                ]
  let eqs = [ "Prelude.ite_not"
            , "Prelude.vTake0"
            , "Prelude.vDrop0"
            , "LLVM.ite_same"
            , "LLVM.ite_false_false"
            , "LLVM.and_true2"
            , "LLVM.bvEq_commute_ite1"
            , "LLVM.bvule_sameL"
            , "LLVM.bvule_sameR"
            , "LLVM.bvule_same2"
            , "LLVM.bveq_sameL"
            , "LLVM.bveq_sameR"
            , "LLVM.bveq_same2"
            ]
  let conversions =
        natConversions
        ++ bvConversions
        ++ vecConversions
        ++ [ remove_ident_coerce
           , remove_ident_unsafeCoerce]
        ++ [ getStructElt
           , evalAppendInt
           ]
  

  simpSet <- scSimpset sc0 activeDefs eqs conversions
  let sc = rewritingSharedContext sc0 simpSet
  sbs <- mkBackendState dl be sc

  boolType <- scPreludeBool sc
  true <- scApplyPreludeTrue sc
  pNot <- scApplyPreludeNot sc
  pAnd <- scApplyPreludeAnd sc
  iteFn <- scApplyPreludeIte sc

  apply_bvEq <- scApplyLLVMLlvmIeqBool sc

  valueFn   <- scApplyLLVMValue sc
  intTypeFn <- scApplyLLVMIntType sc
  
  let sbe = SBE { sbeTruePred = true
                , applyIEq = \w x y -> SAWBackend $
                   join $ apply_bvEq <$> scBitwidth sc w ?? x ?? y
                , applyAnd  = lift2 pAnd
                , applyBNot = lift1 pNot
                , applyPredIte = lift3 (iteFn boolType)
                , applyIte = \tp x y z -> SAWBackend $ do
                    tpt <- join $ scApplyLLVMValue sc <*> sbsMemType sbs tp
                    Right <$> iteFn tpt x y z
                , LLVM.asBool = R.asBool
                , prettyPredD = scPrettyTermDoc
                , evalPred = \inputs t -> SAWBackend $ do
                    t' <- scEvalTerm sbs inputs t
                    case R.asBool t' of
                      Just b -> return b
                      Nothing ->
                        fail $ "Could not evaluate predicate as Boolean:\n" ++ show t'
                , freshInt = \w -> SAWBackend $ do
                    vtp <- valueFn =<< intTypeFn =<< scBitwidth sc w
                    i <- scFreshGlobalVar sc
                    t <- scFlatTermF sc (ExtCns (EC i "_" vtp))
                    Just lits <- bitBlastWith (sbsBCache sbs) t
                    modifyIORef' (sbsVars sbs) ((w,i,lits):)                    
                    return t
                , typedExprEval = typedExprEvalFn sbs
                , applyTypedExpr = \expr -> SAWBackend $ do
                    ExprEvalFn fn <- typedExprEvalFn sbs expr
                    fn return
                , prettyTermD = scPrettyTermDoc
                , asUnsignedInteger = asUnsignedBitvector
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

                , stackPushFrame = SAWBackend . return . (true,) 
                                   . over memState MM.pushStackFrameMem
                , stackPopFrame  = SAWBackend . return . (memState %~ MM.popStackFrameMem)
                , memBranch      = SAWBackend . return . (memState %~ MM.branchMem)
                , memBranchAbort = SAWBackend . return . (memState %~ MM.branchAbortMem)
                , memMerge = \c x y -> SAWBackend $ return $ smMerge c x y

                -- TODO: SAT checking for SAW backend
                , termSAT    = nyi "termSAT"
                , writeAiger = lift2 (scWriteAiger sbs)
                , writeCnf   = do
                    nyi "writeCnf"
                , createSMTLIB1Script = Just $ \nm -> SAWBackend $ do
                    ref <- newIORef $ SMT1.qf_aufbv_WriterState sc (fromString nm)
                    let runSMTLIB1 a = SAWBackend $ do
                          wl <- runStateFromRef ref $ a >> getWarnings SMT1.warnings
                          unless (null wl) $ do
                            putStrLn "Errors occurred during SMTLIB generation:"
                            forM_ wl $ \w -> do
                              let wd = SMT1.ppWarning (scPrettyTermDoc <$> w)
                              putStrLn $ "  " ++ show wd
                    return SMTLIB1Script {
                              addSMTLIB1Assumption =
                                runSMTLIB1 . SMT1.writeAssumption
                            , addSMTLIB1Formula  =
                                runSMTLIB1 . SMT1.writeFormula
                            , writeSMTLIB1ToFile = \p -> do
                                writeFile p . SMT1.render =<< readIORef ref 
                            }
                , createSMTLIB2Script = Just $ SAWBackend $ do
                    ref <- newIORef $ SMT2.qf_aufbv_WriterState sc
                    let runSMTLIB2 a = SAWBackend $ do
                          wl <- runStateFromRef ref $ a >> getWarnings SMT2.warnings
                          unless (null wl) $ do
                            putStrLn "Errors occurred during SMTLIB generation:"
                            forM_ wl $ \w -> do
                              let wd = SMT2.ppWarning (scPrettyTermDoc <$> w)
                              putStrLn $ "  " ++ show wd
                    return SMTLIB2Script {
                              addSMTLIB2Assert   = runSMTLIB2 . SMT2.assert
                            , addSMTLIB2CheckSat = runSMTLIB2 SMT2.checkSat
                            , writeSMTLIB2ToFile = \p -> do
                                writeFile p . SMT2.render =<< readIORef ref 
                            }
                , evalAiger  = \inputs _ t -> SAWBackend $ scEvalTerm sbs inputs t
                , sbeRunIO   = runSAWBackend
                }
  return (sbe, emptySAWMemory)

_unused :: a
_unused = undefined
  scLLVMLLVMType
  scLLVMStructValue
  scApplyLLVMArithmeticWithOverflowResult
  scApplyLLVMBinFn
  scApplyLLVMBinRel
  scApplyLLVMConsFieldType
  scApplyLLVMEmptyFields
  scApplyLLVMSingleField
  scApplyLLVMSbvVecZipWith
  scApplyLLVMMkOverflowResult
  scApplyLLVMLiftVecBVRel
  scApplyLLVMLiftVecSBVRel
  scApplyLLVMBinVRel
  scApplyLLVMBvMap
  scApplyLLVMBvVecZipWith
  scApplyLLVMLiftBVRel
  scApplyLLVMLiftSBVRel
  scApplyLLVMAnd_true2
  scApplyLLVMBvEq_commute_ite1
  scApplyLLVMBveq_same2
  scApplyLLVMBveq_sameL
  scApplyLLVMBveq_sameR
  scApplyLLVMBvule_same2
  scApplyLLVMBvule_sameL
  scApplyLLVMBvule_sameR
  scApplyLLVMIte_false_false
  scApplyLLVMIte_same
