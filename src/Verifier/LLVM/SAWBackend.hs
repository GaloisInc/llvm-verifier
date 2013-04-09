{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{- LANGUAGE DoAndIfThenElse #-}
{- LANGUAGE ImplicitParams #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Verifier.LLVM.SAWBackend
  ( SAWBackend
  , SAWMemory
  , createSAWBackend
  ) where

import Control.Applicative hiding (empty)
import Control.Exception (assert)
import Control.Lens hiding (op, iact)
import Control.Monad
import Control.Monad.State
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V

import Verifier.LLVM.AST
import Verifier.LLVM.Backend
import Verifier.LLVM.MemModel

import Verifier.SAW
import Verifier.SAW.ParserUtils
import Verifier.SAW.Prelude

#if __GLASGOW_HASKELL__ < 706
-- | Strict version of modifyIORef
-- Added for compatibility with GHC base 4.5 and 4.6
modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' r f = do
  v <- readIORef r
  writeIORef r $! f v 
#endif

preludeTrueTermF :: TermF t
preludeTrueTermF = FTermF $ CtorApp (mkIdent preludeModuleName "True") []

preludeFalseTermF :: TermF t
preludeFalseTermF = FTermF $ CtorApp (mkIdent preludeModuleName "False") []

preludeBVNatTermF :: TermF t
preludeBVNatTermF = FTermF $ GlobalDef (mkIdent preludeModuleName "bvNat")

nyi :: String -> a
nyi nm = error $ "Not yet implemented: " ++ show nm

scBitwidth :: SharedContext s -> BitWidth -> IO (SharedTerm s)
scBitwidth sc w = scNat sc (toInteger w)

scBitvectorType :: SharedContext s -> SharedTerm s -> IO (SharedTerm s)
scBitvectorType sc wt = ($ wt) =<< scApplyPreludeBitvector sc

scFreshInt :: SharedContext s -> BitWidth -> IO (SharedTerm s)
scFreshInt sc w =
  scFreshGlobal sc "_" =<< scBitvectorType sc =<< scBitwidth sc w

termAsBool :: SharedTerm s -> Maybe Bool
termAsBool (asTermF -> Just a)
  | a == preludeTrueTermF  = return True
  | a == preludeFalseTermF = return False
termAsBool _ = Nothing

asUnsignedBitvector :: BitWidth -> SharedTerm s -> Maybe Integer
asUnsignedBitvector w s2 = do
  (s1, vt) <- asApp s2
  (s0, wt) <- asApp s1
  when (asTermF  s0 /= Just preludeBVNatTermF) Nothing
  when (asNatLit wt /= Just (toInteger w)) Nothing
  asNatLit vt

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
    declareDefTermF llvm "llvmAdd"
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
scBitvectorConst :: SharedContext s
                 -> BitWidth -- ^ Result width with corresponding term.
                 -> Integer -- ^ Value of bitvector.
                 -> IO (SharedTerm s)
scBitvectorConst sc w v = do
  wt <- scBitwidth sc w
  scBitvectorConst' sc (w,wt) v

-- | Create a bitvector from a constant.
scBitvectorConst' :: SharedContext s
                  -> (BitWidth, SharedTerm s) -- ^ Result width with corresponding term.
                  -> Integer -- ^ Value of bitvector.
                  -> IO (SharedTerm s)
scBitvectorConst' sc (w,wt) v = do
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
scFreshPtr sc dl = scFreshInt sc (ptrBitwidth dl)

-- | Set of shared term variables that refer to allocations.
type Allocations s = IORef (Set (SharedTerm s))

asApp2 :: SharedTerm s -> Maybe (SharedTerm s, SharedTerm s, SharedTerm s)
asApp2 t = do
  (t1,a2) <- asApp t
  (t0,a1) <- asApp t1
  return (t0,a1,a2)

data SAWBackendState s =
       SBS { sbsDataLayout :: DataLayout
           , sbsContext :: SharedContext s
           , sbsAllocations :: Allocations s
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
           , smGenerator :: TermGenerator IO (SharedTerm s) (SharedTerm s) (SharedTerm s)
           }

mkBackendState :: forall s . DataLayout -> SharedContext s -> IO (SAWBackendState s)
mkBackendState dl sc = do
  ptrWidth <- scBitwidth sc (ptrBitwidth dl)
  addFn <- scApplyLLVMLlvmAdd sc

  subFn <- scApplyLLVMLlvmSub sc
  allocs <- newIORef Set.empty

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
  let decomposePtr ptr =
        case asApp2 ptr of
          Just (f,b,o) | f == ptrAddOp -> do
            s <- readIORef allocs
            return $ if Set.member b s then Just (b,o) else Nothing
          _ -> return Nothing

  let ptrAdd x y = do
        mx <- decomposePtr x
        let addPrim = scApply2 sc ptrAddOp
        case mx of
          Just (b,o) -> addPrim b =<< addPrim o y
          Nothing -> do
            my <- decomposePtr y
            case my of
              Just (b,o) -> addPrim b =<< addPrim x o
              Nothing -> addPrim x y
  ptrSubOp <- subFn ptrWidth
  let ptrSub x y = do
        mx <- decomposePtr x
        my <- decomposePtr y
        let subPrim = scApply2 sc ptrSubOp
        case (,) <$> mx <*> my of
          Just ((bx,ox),(by,oy)) | bx == by -> subPrim ox oy
          _ -> subPrim x y



  let decomposeOffset = scIntAsConst' sc ptrWidth

  muxOp <- scTermF sc iteTermF

  andFn <- scApplyPreludeAnd sc
  orFn  <- scApplyPreludeOr  sc
  boolMuxOp <- join $ scApply sc muxOp <$> scPreludeBool sc

  let mkTypeTerm :: Type -> IO (SharedTerm s)
      mkTypeTerm tp0 =
        case typeF tp0 of
          Bitvector n -> scIntType sc (8*toInteger n)
          Float  -> return llvmFloatType
          Double -> return llvmDoubleType
          Array n tp -> join $ arrayTypeFn <$> scNat sc (toInteger n) <*> mkTypeTerm tp
          Struct flds -> join $ structTypeFn
                           <$> scNat sc (toInteger (V.length flds))
                           <*> fieldVFn flds
      fieldFn :: Field Type -> IO (SharedTerm s, Integer)
      fieldFn f = (, toInteger (fieldPad f)) <$> mkTypeTerm (f^.fieldVal)
      fieldVFn flds = scFieldInfo sc fieldType =<< traverse fieldFn flds

  intToFloat  <- scApplyLLVMLlvmIntToFloat sc
  intToDouble <- scApplyLLVMLlvmIntToDouble sc

  ptrEqOp <- join $ scApplyLLVMLlvmIeq  sc ?? ptrWidth
  ptrLeOp <- join $ scApplyLLVMLlvmIule sc ?? ptrWidth

  appendInt <- scApplyLLVMLlvmAppendInt sc
  sliceFn <- scApplyLLVMLlvmIntSlice sc
  valueFn   <- scApplyLLVMValue sc
  let tg = TG { tgPtrWidth = dl^.ptrSize
              , tgPtrDecompose = \ptr -> do
                  mr <- decomposePtr ptr
                  case mr of
                    Nothing -> return $ Symbolic ptr
                    Just (b,o) -> do
                      mo <- decomposeOffset o
                      return $ case mo of
                                 Just o' -> ConcreteOffset b o'
                                 Nothing -> SymbolicOffset b o
              , tgPtrSizeDecompose = decomposeOffset
              , tgConstPtr = scBitvectorConst' sc (ptrBitwidth dl, ptrWidth) . fromIntegral
              , tgAddPtr = ptrAdd
              , tgCheckedAddPtr = \x y -> (trueTerm,) <$> ptrAdd x y
              , tgSubPtr = ptrSub

              , tgTrue  = trueTerm
              , tgFalse = falseTerm
              , tgPtrEq = scApply2 sc ptrEqOp
              , tgPtrLe = scApply2 sc ptrLeOp
              , tgAnd   = andFn
              , tgOr    = orFn
              , tgMuxCond = scApply3 sc boolMuxOp

              , tgConstBitvector = \w -> scBitvectorConst sc (8*fromIntegral w)
              , tgConstFloat  = scFloat sc
              , tgConstDouble = scDouble sc
              , tgApplyCtorF = \vcf ->
                 case vcf of
                   ConcatBV xw x yw y -> do
                     xwt <- scNat sc (toInteger xw)
                     ywt <- scNat sc (toInteger yw)
                     case dl^.intLayout of
                       BigEndian    -> appendInt xwt x ywt y
                       LittleEndian -> appendInt ywt y xwt x         
                   BVToFloat x -> intToFloat x 
                   BVToDouble x -> intToDouble x
                   ConsArray tp x n y -> do
                     consFn <- scApplyPreludeConsVec sc
                     tpt <- mkTypeTerm tp
                     nt <- scNat sc n
                     consFn tpt x nt y
                   AppendArray tp m x n y -> do
                     appendFn <- scApplyPreludeAppend sc
                     join $ appendFn <$> scNat sc m
                                     <*> scNat sc n
                                     <*> (valueFn =<< mkTypeTerm tp)
                                     ?? x
                                     ?? y
                   MkArray tp v ->
                     join $ scVecLit sc <$> (valueFn =<< mkTypeTerm tp) ?? v 
                   MkStruct v -> do
                     ExprEvalFn eval <- createStructValue sc =<< (traverse . _1) fieldFn v
                     eval return
              , tgApplyViewF = \vf -> do
                  let slice i n o v = do
                        join $ sliceFn
                                 <$> scNat sc (8*toInteger i)
                                 <*> scNat sc (8*toInteger n)
                                 <*> scNat sc (8*toInteger o)
                                 ?? v
                  case vf of
                    SelectLowBV m n v -> do
                      case dl^.intLayout of
                        BigEndian    -> slice n m 0 v -- High order bits of v.
                        LittleEndian -> slice 0 m n v -- low order bits of v.
                    SelectHighBV m n v -> do
                      case dl^.intLayout of
                        BigEndian    -> slice 0 n m v -- Low order bits of v.
                        LittleEndian -> slice m n 0 v -- High order bits of v.
                    FloatToBV v  -> join $ scApplyLLVMLlvmFloatToInt sc ?? v
                    DoubleToBV v -> join $ scApplyLLVMLlvmDoubleToInt sc ?? v
                    ArrayElt n tp o v -> do
                      join $ scApplyPreludeGet sc
                             <*> scNat sc (toInteger n)
                             <*> (valueFn =<< mkTypeTerm tp)
                             <*> pure v
                             <*> scFinConst sc (toInteger o) (toInteger n)
                    FieldVal tps i v -> do
                      let n = toInteger (V.length tps)
                      join $ scApplyLLVMLlvmStructElt sc
                               <*> scNat sc n
                               <*> fieldVFn tps
                               <*> pure v
                               <*> scFinConst sc (toInteger i) n
              , tgMuxTerm = \c tp x y -> do
                  tpt <- mkTypeTerm tp
                  scApply4 sc muxOp tpt c x y
              }

  return SBS { sbsDataLayout = dl
             , sbsContext = sc
             , sbsAllocations = allocs
             , sbsPtrWidth = ptrWidth
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

{-
sbsMkPtrConstant :: SAWBackendState s -> Integer -> IO (SharedTerm s)
sbsMkPtrConstant sbs =
  scBitvectorConst' (sbsContext sbs)
                    (ptrBitwidth (sbsDataLayout sbs), sbsPtrWidth sbs)

-- | Attempts to decompose a term into an allocation and an offset.
sbsDecomposePtr :: SAWBackendState s -> SharedTerm s -> IO (Maybe (SharedTerm s, SharedTerm s))
sbsDecomposePtr sbs ptr =
  case asApp2 ptr of
    Just (f,b,o) | f == sbsPtrAddFn sbs -> do
      s <- readIORef (sbsAllocations sbs)
      return $ if Set.member b s then Just (b,o) else Nothing
    _ -> return Nothing

-- | Attempt to parse the term as a constant integer.
sbsIntAsConst :: SAWBackendState s -> BitWidth -> SharedTerm s -> IO (Maybe Integer)
sbsIntAsConst sbs w t = scBitwidth sc w >>= \wt -> scIntAsConst' sc wt t
  where sc = sbsContext sbs
-}

-- | Attempt to parse the second term as a constant integer.
-- The first term is the width of the term.
scIntAsConst' :: SharedContext s -> SharedTerm s -> SharedTerm s -> IO (Maybe Integer)
scIntAsConst' sc w t =
  fmap asNatLit $ join $
    scApplyLLVMLlvmIntValueNat sc ?? w ?? t

{-
type AllocInfo s = Maybe (SharedTerm s, SharedTerm s, SharedTerm s)
-}

{-
-- | Attempt to decompose range.
sbsAllocInfo :: SAWBackendState s -> SharedTerm s -> SharedTerm s -> IO (AllocInfo s)
sbsAllocInfo sbs ptr sizeTerm = do
  -- Attempt to decompose pointer.
  dptr <- sbsDecomposePtr sbs ptr
  -- Get allocation info if pointer can be decomposed
  case dptr of
    Just (b,o) -> do
      (\e -> Just (b,o,e)) <$> sbsPtrAdd sbs o sizeTerm
    Nothing -> return Nothing
-}

{-
adjustAlignment :: Int -> Alignment -> Alignment
adjustAlignment off a = checkAlign 0
  where checkAlign i | i == a = i
                       -- Stop if offset is set at this bit. 
                     | off `testBit` fromIntegral i = i
                     | otherwise = checkAlign (i+1)
-}

type SAWMem s = Mem (SharedTerm s) (SharedTerm s) (SharedTerm s)

data SAWMemory s = SAWMemory { _memSymbols :: Map (SharedTerm s) Symbol
                             , _memState :: Mem (SharedTerm s) (SharedTerm s) (SharedTerm s)
                             }

emptySAWMemory :: SAWMemory s
emptySAWMemory = SAWMemory { _memSymbols = Map.empty
                           , _memState = emptyMem
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

smAlloc :: SAWBackendState s
        -> AllocType
        -> SAWMemory s
        -> MemType
        -> BitWidth -- ^ Width of count.
        -> SharedTerm s -- ^ Count
        -> Alignment
        -> IO (AllocResult (SAWBackend s))
smAlloc sbs atp m mtp w cnt _ = do
  let sc = sbsContext sbs
  -- Create new variable for base address.
  base <- scFreshGlobal sc "_" (sbsPtrType sbs)
  modifyIORef' (sbsAllocations sbs) (Set.insert base)
  -- Get size of tp in bytes.
  let dl = sbsDataLayout sbs
  let pw = ptrBitwidth dl
  let pwt = sbsPtrWidth sbs
  tpSize <- scBitvectorConst' sc (pw,pwt) (toInteger (memTypeSize dl mtp))
  -- Convert count to have same bitwidth as pointer.
  cnt' <- scResizeTerm sc w (pw,pwt) cnt
  -- Get total number of bytes.
  mulFn <- scApplyLLVMLlvmMul sc
  mulOp <- mulFn pwt
  totalSize <- scApplyAll sc mulOp [cnt', tpSize]
  -- Get true predicate.
  t <- scApplyPreludeTrue sc
  -- Return memory with new change.
  let m' = m & memState %~ allocMem atp base totalSize
  -- Return successful allocation.
  return $ AResult t base m'   

mergeEq :: (Ord k, Eq a) => Map k a -> Map k a -> Map k a
mergeEq mx = Map.filterWithKey p
  where p k u = Map.lookup k mx == Just u

smMerge :: SharedTerm s -> SAWMemory s -> SAWMemory s -> SAWMemory s
smMerge c x y =
  SAWMemory { _memSymbols = mergeEq (x^.memSymbols) (y^.memSymbols)
            , _memState = mergeMem c (x^.memState) (y^.memState)
            } 

-- | Return term value, length of fields, and vector with the types of the fields.
sbsStructValue :: forall s v
                   . SAWBackendState s
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
  empty <- scApplyLLVMEmptyStruct sc
  let eval0 = ExprEvalFn $ \_ -> return empty
  emptyFn <- scApplyPreludeEmptyVec sc
  tp0 <- emptyFn fieldType
  view _2 <$> foldrMOf folded foldFn (0, eval0, tp0) flds

{-
-- | Create a dummy value (of all zeros) for the given memtype.
dummyValue :: SAWBackendState s -> MemType -> IO (SharedTerm s)
dummyValue sbs tp0 = do
  let dl = sbsDataLayout sbs
  let sc = sbsContext sbs
  let vecFn n tp = do
       fn <- scApplyPreludeReplicate sc
       join $ fn <$> scNat sc (toInteger n)
                 <*> sbsMemType sbs tp
                 <*> dummyValue sbs tp
  case tp0 of
    IntType w -> scBitvectorConst sc w 0
    FloatType -> scFloat sc 0
    DoubleType -> scDouble sc 0
    PtrType{} -> scBitvectorConst sc (ptrBitwidth dl) 0
    ArrayType n tp -> vecFn n tp
    VecType n tp -> vecFn n tp
    StructType si -> do
      let valueFn fi = (fi, fiType fi)
      ExprEvalFn evalFn <- createStructValue sbs (valueFn <$> siFields si)
      evalFn (dummyValue sbs)
-}

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

{-
scLazyIte :: SharedContext s
          -> SharedTerm s -- ^ Condition
          -> IO (SharedTerm s) -- ^ Type of result
          -> IO (SharedTerm s) -- ^ Result if condition is true.
          -> IO (SharedTerm s) -- ^ Result if condition is false.
          -> IO (SharedTerm s) -- ^ Result if condition is false.
scLazyIte sc c mtp mx my = do
  case scViewAsBool c of
    Just True -> mx
    Just False -> my
    Nothing -> join $ scApplyPreludeIte sc <*> mtp <*> pure c <*> mx <*> my

-- | Convert bits to bytes.
bitsToBytes :: BitWidth -> Integer
bitsToBytes w | w .&. 0x7 == 0 = toInteger w `shiftR` 3
              | otherwise = error "SAW Backend only supports full byte memory accesses."
-}

{-
-- | Returns true if two types have compatible SAWCore types.
compatTypes :: DataLayout -> MemType -> MemType -> Bool
compatTypes dl tp0 tp1 =
  case (tp0, tp1) of
    (IntType w0, IntType w1) -> w0 == w1
    (IntType w0, PtrType{}) -> w0 == ptrBitwidth dl
    (PtrType{}, IntType w1) -> ptrBitwidth dl == w1
    (PtrType{}, PtrType{}) -> True
    (FloatType, FloatType) -> True
    (DoubleType, DoubleType) -> True
    (ArrayType m tp0', ArrayType n tp1') -> m == n && compatTypes dl tp0' tp1'
    (ArrayType m tp0',   VecType n tp1') -> m == n && compatTypes dl tp0' tp1'
    (VecType m tp0',   ArrayType n tp1') -> m == n && compatTypes dl tp0' tp1'
    (VecType m tp0',     VecType n tp1') -> m == n && compatTypes dl tp0' tp1'
    (StructType si0, StructType si1) -> V.and $ V.zipWith fieldFn (siFields si0) (siFields si1)
      where fieldFn fi0 fi1 = fiOffset fi0 == fiOffset fi1
                           && compatTypes dl (fiType fi0) (fiType fi1)
                           && fiPadding fi0 == fiPadding fi1
    _ -> False

scIntValueType :: SharedContext s -> BitWidth -> IO (SharedTerm s) 
scIntValueType sc w = 
  join $ scApplyLLVMValue sc <*> scIntType sc w
-}

convertMemType :: DataLayout -> MemType -> Maybe Type
convertMemType dl tp0 =
  case tp0 of
    IntType w
        | r == 0 -> return (bitvectorType (fromIntegral n))
        | otherwise -> Nothing
      where (n,r) = w `divMod` 8
    PtrType{} -> return (bitvectorType (dl^.ptrSize))
    FloatType -> return floatType
    DoubleType -> return doubleType
    ArrayType n etp -> arrayType (fromIntegral n) <$> convertMemType dl etp
    VecType n etp   -> arrayType (fromIntegral n) <$> convertMemType dl etp
    StructType si   -> mkStruct <$> traverse fldFn (siFields si)
      where fldFn f = (,fiPadding f) <$> convertMemType dl (fiType f)

smLoad :: forall s .
          SAWBackendState s
       -> SAWMemory s
       -> MemType
       -> SharedTerm s
       -> Alignment
       -> IO (SharedTerm s, SharedTerm s) -- Validity predicate and result.
smLoad sbs m tp0 ptr0 _a0 =
  case convertMemType (sbsDataLayout sbs) tp0 of
    Just tp -> readMem (smGenerator sbs) ptr0 tp (m^.memState)
    Nothing -> fail "smLoad must be given types that are even byte size."

smStore :: SAWBackendState s
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
      (c,ms) <- writeMem (smGenerator sbs) p tp v (m^.memState)
      return (c,m & memState .~ ms)

-- | @memcpy mem dst src len align@ copies @len@ bytes from @src@ to @dst@,
-- both of which must be aligned according to @align@ and must refer to
-- non-overlapping regions.
smCopy :: SAWBackendState s
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
  (c,ms) <- copyMem (smGenerator sbs) dst src sz (m^.memState)
  return (c, m & memState .~ ms)

data SAWBackend s a = SAWBackend { runSAWBackend :: IO a }
  deriving (Functor)

type instance SBETerm (SAWBackend s) = SharedTerm s
type instance SBEPred (SAWBackend s) = SharedTerm s
type instance SBEMemory (SAWBackend s) = SAWMemory s

lift1 :: (x -> IO r) -> (x -> SAWBackend s r)
lift1 = (SAWBackend .)

lift2 :: (x -> y -> IO r)
      -> (x -> y -> SAWBackend s r)
lift2 = (lift1 .)

lift3 :: (x -> y -> z -> IO r)
      -> (x -> y -> z -> SAWBackend s r)
lift3 = (lift2 .)

lift4 :: (w -> x -> y -> z -> IO r)
      -> (w -> x -> y -> z -> SAWBackend s r)
lift4 = (lift3 .)

lift5 :: (v -> w -> x -> y -> z -> IO r)
      -> (v -> w -> x -> y -> z -> SAWBackend s r)
lift5 = (lift4 .)

lift6 :: (u -> v -> w -> x -> y -> z -> IO r)
      -> (u -> v -> w -> x -> y -> z -> SAWBackend s r)
lift6 = (lift5 .)

-- | Returns share term representing given state.
sbsMemType :: SAWBackendState s -> MemType -> IO (SharedTerm s)
sbsMemType sbs btp = do
  let sc = sbsContext sbs
  case btp of
    IntType w -> scIntType sc (toInteger w)
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
sbsFieldInfo :: SAWBackendState s
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

scIntType :: SharedContext s -> Integer -> IO (SharedTerm s)
scIntType sc w = join $ scApplyLLVMIntType sc <*> scNat sc w

typedExprEvalFn :: forall s v 
                 . SAWBackendState s
                -> TypedExpr v
                -> IO (ExprEvalFn v (SharedTerm s))
typedExprEvalFn sbs expr0 = do
  let dl = sbsDataLayout sbs
  let sc = sbsContext sbs
  let eval1 :: v
            -> (SharedTerm s -> IO (SharedTerm s))
            -> ExprEvalFn v (SharedTerm s)
      eval1 v fn = ExprEvalFn $ \eval -> liftIO . fn =<< eval v       
  let evalBin x y op = ExprEvalFn $ \eval ->
         liftIO . uncurry (scApply2 sc op) =<< both eval (x,y)
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
    SValInteger w v ->      
      constEvalFn <$> scBitvectorConst sc w v
    SValFloat v  -> constEvalFn <$> scFloat sc v
    SValDouble v -> constEvalFn <$> scDouble sc v
    SValNull{} -> do
      nullPtrFn <- scApplyLLVMLlvmNullPtr sc
      constEvalFn <$> (nullPtrFn =<< scNat sc (toInteger (dl^.ptrSize)))
    SValArray  mtp v -> mkVecLit mtp v
    SValVector mtp v -> mkVecLit mtp v
    SValStruct si vals -> assert (siFieldCount si == V.length vals) $ do
      sbsStructValue sbs (siFields si `V.zip` vals)
    IntArith iop mn w x y -> do
      case mn of
        Nothing -> do
          let defOp :: (SharedContext s -> IO (SharedTerm s -> IO (SharedTerm s)))
                    -> IO (SharedTerm s -> SharedTerm s -> IO (SharedTerm s))
              defOp fn = do
                fmap (scApply2 sc) $ join $ fn sc <*> scBitwidth sc w
          mkFn <-
            case iop of
              Add{}  -> sbsAdd sbs w
              Sub{}  -> sbsSub sbs w
              Mul{}  -> defOp scApplyLLVMLlvmMul
              UDiv{} -> defOp scApplyLLVMLlvmUDiv
              SDiv{} -> defOp scApplyLLVMLlvmSDiv
              URem   -> defOp scApplyLLVMLlvmURem
              SRem   -> defOp scApplyLLVMLlvmSRem
              Shl{}  -> defOp scApplyLLVMLlvmShl
              Lshr{} -> defOp scApplyLLVMLlvmLShr
              Ashr{} -> defOp scApplyLLVMLlvmAShr
              And    -> defOp scApplyLLVMLlvmAnd
              Or     -> defOp scApplyLLVMLlvmOr
              Xor    -> defOp scApplyLLVMLlvmXor
          return $ ExprEvalFn $ \eval -> do
            liftIO . uncurry mkFn =<< both eval (x,y)          
        Just n  -> do
          let mkFnv =
                case iop of
                  Add{}  ->  scApplyLLVMLlvmAddV
                  Sub{}  ->  scApplyLLVMLlvmSubV
                  Mul{}  ->  scApplyLLVMLlvmMulV
                  UDiv{} -> scApplyLLVMLlvmUDivV
                  SDiv{} -> scApplyLLVMLlvmSDivV
                  URem   -> scApplyLLVMLlvmURemV
                  SRem   -> scApplyLLVMLlvmSRemV
                  Shl{}  ->  scApplyLLVMLlvmShlV
                  Lshr{} -> scApplyLLVMLlvmLShrV
                  Ashr{} -> scApplyLLVMLlvmAShrV
                  And    ->  scApplyLLVMLlvmAndV
                  Or     ->   scApplyLLVMLlvmOrV
                  Xor    ->  scApplyLLVMLlvmXorV   
          fmap (evalBin x y) $ join $
            mkFnv sc <*> scNat sc (toInteger n)
                     <*> scBitwidth sc w
    PtrAdd x y -> do
      let mkFn = scApplyLLVMLlvmAddPtr
      fmap (evalBin x y) $ join $
        mkFn sc <*> scNat sc (toInteger (dl^.ptrSize))
    UAddWithOverflow w x y -> do
      let si = mkStructInfo dl False [IntType 1, IntType w]
      let [p0,p1] = V.toList $ fiPadding <$> siFields si
      fn <- scApplyLLVMLlvmAddWithOverflow sc
              <*> scBitwidth sc w
              <*> scNat sc (toInteger p0)
              <*> scNat sc (toInteger p1)
      return $ ExprEvalFn $ \eval -> join $ (\xv yv -> liftIO $ fn xv yv) <$> eval x <*> eval y
    IntCmp op mn w x y -> do
        case mn of
          Nothing ->
            fmap (evalBin x y) $ join $ mkFn sc <*> scBitwidth sc w
          Just n  ->
            fmap (evalBin x y) $ join $
              mkFnV sc <*> scNat sc (toInteger n)
                       <*> scBitwidth sc w
      where (mkFn, mkFnV) =
              case op of 
                Ieq  -> (scApplyLLVMLlvmIeq,  scApplyLLVMLlvmIeqV)
                Ine  -> (scApplyLLVMLlvmIne,  scApplyLLVMLlvmIneV)
                Iugt -> (scApplyLLVMLlvmIugt, scApplyLLVMLlvmIugtV)
                Iuge -> (scApplyLLVMLlvmIuge, scApplyLLVMLlvmIugeV)
                Iult -> (scApplyLLVMLlvmIult, scApplyLLVMLlvmIultV)
                Iule -> (scApplyLLVMLlvmIule, scApplyLLVMLlvmIuleV)
                Isgt -> (scApplyLLVMLlvmIsgt, scApplyLLVMLlvmIsgtV)
                Isge -> (scApplyLLVMLlvmIsge, scApplyLLVMLlvmIsgeV)
                Islt -> (scApplyLLVMLlvmIslt, scApplyLLVMLlvmIsltV)
                Isle -> (scApplyLLVMLlvmIsle, scApplyLLVMLlvmIsleV)
    Trunc mn iw v rw -> assert (iw >= rw) $
      extOp scApplyLLVMLlvmTrunc scApplyLLVMLlvmTruncV mn (iw - rw) rw v
    ZExt mn iw v rw -> assert (iw <= rw) $
      extOp scApplyLLVMLlvmZExt  scApplyLLVMLlvmZExtV  mn (rw - iw) iw v
    SExt mn iw v rw -> assert (iw <= rw) $
      extOp scApplyLLVMLlvmSExt scApplyLLVMLlvmSExtV mn (rw - iw) iw v
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

-- | Create a SAW backend.
createSAWBackend :: DataLayout
                 -> MemGeom
                 -> IO (SBE (SAWBackend s), SAWMemory s)
createSAWBackend dl _mg = do

  sc <- mkSharedContext llvmModule
  boolType <- scPreludeBool sc
  t    <- scApplyPreludeTrue sc
  pNot <- scApplyPreludeNot sc
  pAnd <- scApplyPreludeAnd sc
  iteFn <- scApplyPreludeIte sc

  let pIte = iteFn boolType

  bvEq <- scApplyPreludeBvEq sc
  sbs <- mkBackendState dl sc
  let sbe = SBE { sbeTruePred = t
                , applyIEq = \w x y -> SAWBackend $
                   join $ bvEq <$> scBitwidth sc w ?? x ?? y
                , applyAnd  = lift2 pAnd
                , applyBNot = lift1 pNot
                , applyPredIte = lift3 pIte
                , applyIte = \tp x y z -> SAWBackend $ do
                    fmap Right $ join $
                      iteFn <$> (join $ scApplyLLVMValue sc <*> sbsMemType sbs tp) 
                            ?? x
                            ?? y
                            ?? z
                , asBool = termAsBool
                , prettyPredD = nyi "prettyPredD"
                , evalPred = nyi "evalPred"
                , freshInt = SAWBackend . scFreshInt sc
                , typedExprEval = typedExprEvalFn sbs
                , applyTypedExpr = \expr -> SAWBackend $ do
                    ExprEvalFn fn <- typedExprEvalFn sbs expr
                    fn return
                , prettyTermD = nyi "prettyTermD"
                , asUnsignedInteger = asUnsignedBitvector
                , asConcretePtr     = asUnsignedBitvector (ptrBitwidth dl)
                , memDump      = nyi "memDump"
                , memLoad      = lift4 (smLoad sbs)
                , memStore     = lift5 (smStore sbs)
                , memCopy      = lift6 (smCopy sbs)
                , memAddDefine = lift3 (smAddDefine dl sc) 
                , memInitGlobal = nyi "memInitGlobal"
                , codeLookupSymbol = ((SAWBackend . return) .) . smLookupSymbol
                , stackAlloc     = lift5 (smAlloc sbs StackAlloc)
                , stackPushFrame = \m ->
                             SAWBackend $ return (t, m & memState %~ pushStackFrameMem)
                , stackPopFrame  = SAWBackend . return . (memState %~ popStackFrameMem)
                , heapAlloc      = lift5 (smAlloc sbs HeapAlloc)
                , memBranch      = SAWBackend . return . (memState %~ branchMem)
                , memBranchAbort = SAWBackend . return . (memState %~ branchAbortMem)
                , memMerge = \c x y -> SAWBackend $ return $ smMerge c x y
                , writeAiger = nyi "writeAiger"
                , writeCnf   = nyi "writeCnf"
                , evalAiger  = nyi "evalAiger"
                , sbeRunIO   = runSAWBackend
                }
  return (sbe, emptySAWMemory)