{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Verifier.LLVM.SAWBackend where

import Control.Applicative
import Control.Exception (assert)
import Control.Lens 
import Control.Monad
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector as V

import Verifier.LLVM.AST
import Verifier.LLVM.Backend

import Verifier.SAW
import Verifier.SAW.ParserUtils
import Verifier.SAW.Prelude


nyi :: String -> a
nyi nm = error $ "Not yet implemented: " ++ show nm

scFreshInt :: SharedContext s -> BitWidth -> IO (SharedTerm s)
scFreshInt sc w = do
  wt <- scNat sc (toInteger w)
  bvFn <- scApplyPreludeBitvector sc
  scFreshGlobal sc "_" =<< bvFn wt
  
scAsBool :: SharedTerm s -> Maybe Bool
scAsBool = nyi "scAsBool"

scFloat :: SharedContext s -> Float -> IO (SharedTerm s)
scFloat = nyi "scFloat"

scDouble :: SharedContext s -> Double -> IO (SharedTerm s)
scDouble = nyi "scDouble"

-- | Create a vector from a term representing its element types and the element.
scVecLit :: SharedContext s
         -> SharedTerm s -- ^ Type of vector elments.
         -> V.Vector (SharedTerm s) -- ^ Elements
         -> IO (SharedTerm s)
scVecLit = nyi "scVecLit"

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


-- | The allocation state of memory.
-- Type parameters are for merge predicate and changes respectively.
data MemState p d
  = -- | Represents initial memory and changes since then.
    EmptyMem [d]
    -- | Represents a push of a stack frame, and allocation since that stack push.
  | StackFrame [d] (MemState p d)
    -- | Represents a push of a branch frame, and allocations since that branch.
  | BranchFrame [d] (MemState p d)
    -- | Represents a merge of two frames.
  | MergeFrame p [d] [d] (MemState p d)

data MemAlloc s
    -- | Represents a stack allocation with base, alignment of base, and number of bytes.
  = StackAlloc (SharedTerm s) Alignment (SharedTerm s)
    -- | Represents a heap allocation with base, alignment of base, and number of bytes.
  | HeapAlloc (SharedTerm s) Alignment (SharedTerm s)

type MemAllocState s = MemState (SharedTerm s) (MemAlloc s)

data MemWrite s
    -- | @MemCopy dst src len a@ represents a copy from [src..len) to
    -- [dst..len).  Both source and dest can be assumed to satisfy the
    -- given alignment. 
  = MemCopy (SharedTerm s) (SharedTerm s) (SharedTerm s) Alignment
    -- | Memstore is given address, type of value, value, and alignment. 
  | MemStore (SharedTerm s) MemType (SharedTerm s) Alignment

type MemWriteState s = MemState (SharedTerm s) (MemAlloc s)

data SAWMemory s = SAWMemory { _memSymbols :: Map (SharedTerm s) Symbol
                             , _allocState :: MemAllocState s
                             , _valueState :: MemWriteState s
                             }

emptyMemory :: SAWMemory s
emptyMemory = SAWMemory { _memSymbols = Map.empty
                        , _allocState = EmptyMem []
                        , _valueState = EmptyMem []
                        }

memSymbols :: Simple Lens (SAWMemory s) (Map (SharedTerm s) Symbol)
memSymbols = lens _memSymbols (\s v -> s { _memSymbols = v })

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

{-
szResizeTerm :: SharedContext s
             -> BitWidth -- ^ Input width
             -> BitWidth -- ^ Result bitwith
             -> SharedTerm s
             -> IO (SharedTerm s)
szResizeTerm sc iw rw v
  | iw < rw =
      extOp scApplyLLVMLlvmZExt  scApplyLLVMLlvmZExtV mn  (rw - iw) iw v
        | iw > rw =
          extOp scApplyLLVMLlvmTrunc scApplyLLVMLlvmTruncV mn (iw - rw) rw v
        | otherwise = return (\eval -> eval v)

smStackAlloc :: DataLayout
             -> SharedContext s
             -> SAWMemory s
             -> MemType
             -> BitWidth
             -> SharedTerm s
             -> Alignment
             -> IO (SharedTerm s)
smStackAlloc dl sc m tp w cnt a = do
  ptrBitwidth dl
 


-}


data SAWBackend s a = SAWBackend { runSAWBackend :: IO a }
  deriving (Functor)

type instance SBETerm (SAWBackend s) = SharedTerm s
type instance SBEPred (SAWBackend s) = SharedTerm s
type instance SBEMemory (SAWBackend s) = SAWMemory s

$(runDecWriter $ do
    prelude <- importExp [|preludeModule|] preludeModule
    llvm <- mkDecModule [prelude] "llvmModule" "saw/LLVM.sawcore"
    decSharedModuleFns "LLVM" (decVal llvm)
 )

lift1 :: (x -> IO r) -> x -> SAWBackend s r
lift1 fn x = SAWBackend (fn x)

lift2 :: (x -> y -> IO r) -> x -> y -> SAWBackend s r
lift2 fn x y = SAWBackend (fn x y)

lift3 :: (x -> y -> z -> IO r) -> x -> y -> z -> SAWBackend s r
lift3 fn x y z = SAWBackend (fn x y z)

-- | Returns term (tp,padding) for the given field info. 
scFieldInfo :: SharedContext s
            -> (MemType -> IO (SharedTerm s))
            -> FieldInfo
            -> IO (SharedTerm s)
scFieldInfo sc scMemType fi = do
  tp <- scMemType (fiType fi)
  p <- scNat sc (toInteger (fiPadding fi))
  scTuple sc [tp,p]

scFinConst :: SharedContext s
           -> Integer -- ^ Index
           -> Integer -- ^ Bound n
           -> IO (SharedTerm s)
scFinConst sc i n | i < n = do
  fv <- scApplyPreludeFinVal sc
  join $ fv <$> scNat sc i <*> scNat sc (n - (i + 1))
scFinConst _ _ _ = error "illegal arguments to scFinConst"

mkEvalMemType :: forall s . DataLayout
              -> SharedContext s
              -> IO (MemType -> IO (SharedTerm s))
mkEvalMemType dl sc = do
  intTypeFn  <- scApplyLLVMIntType sc
  floatType  <- scApplyLLVMFloatType sc
  doubleType <- scApplyLLVMDoubleType sc
  ptrTypeFn <- scApplyLLVMPtrType sc
  ptrType   <- ptrTypeFn =<< scNat sc (toInteger (dl^.ptrSize))
  arrayTypeFn  <- scApplyLLVMArrayType sc 
  vecTypeFn    <- scApplyLLVMVectorType sc 
  structTypeFn <- scApplyLLVMStructType sc
  fieldType <- scApplyLLVMFieldType sc
  let scMemType (IntType w) = intTypeFn =<< scNat sc (toInteger w)
      scMemType FloatType   = return floatType
      scMemType DoubleType  = return doubleType
      scMemType (PtrType _) = return ptrType
      scMemType (ArrayType n tp) = 
        join $ arrayTypeFn <$> scNat sc (toInteger n) <*> scMemType tp
      scMemType (VecType n tp) = 
        join $ vecTypeFn <$> scNat sc (toInteger n) <*> scMemType tp
      scMemType (StructType si) = do
        nt <- scNat sc (toInteger (siFieldCount si))
        fldV <- traverse (scFieldInfo sc scMemType) (siFields si)
        structTypeFn nt =<< scVecLit sc fieldType fldV
  return scMemType

type EvalFn v s = (v -> IO (SharedTerm s)) -> IO (SharedTerm s)

typedExprEvalFn :: forall s v 
                 . DataLayout
                -> SharedContext s
                -> TypedExpr v
                -> IO (ExprEvalFn v (SharedTerm s))
typedExprEvalFn dl sc expr0 = do
  let eval1 :: v
            -> (SharedTerm s -> IO (SharedTerm s))
            -> ExprEvalFn v (SharedTerm s)
      eval1 v fn = ExprEvalFn $ \eval -> liftIO . fn =<< eval v       
  let evalBin x y op = ExprEvalFn $ \eval -> liftIO . scApplyAll sc op =<< traverse eval [x,y]
  scMemType <- mkEvalMemType dl sc
  let mkVecLit mtp v = do
        mtpt <- scMemType mtp
        valueFn <- scApplyLLVMValue sc
        tp <- valueFn mtpt
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
        dt <- scNat sc (toInteger dw)
        rt <- scNat sc (toInteger rw)
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
    SValInteger w v -> do
      cfn <- scApplyLLVMLlvmIntConstant sc
      wt <- scNat sc $ toInteger w
      vt <- scNat sc $ v `mod` 2^(toInteger w)
      constEvalFn <$> cfn wt vt 
    SValFloat v  -> constEvalFn <$> scFloat sc v
    SValDouble v -> constEvalFn <$> scDouble sc v
    SValNull{} -> do
      nullPtrFn <- scApplyLLVMLlvmNullPtr sc
      constEvalFn <$> (nullPtrFn =<< scNat sc (toInteger (dl^.ptrSize)))
    SValArray  mtp v -> mkVecLit mtp v
    SValVector mtp v -> mkVecLit mtp v
    SValStruct si vals -> assert (siFieldCount si == V.length vals) $ do
      fieldType <- scApplyLLVMFieldType sc
      -- Return term value, length of fields, and vector with the types of the fields.
      let procFields :: [(FieldInfo, v)]
                     -> IO (Integer, ExprEvalFn v (SharedTerm s), SharedTerm s)
          -- Return (EmptyStruct, EmptyVec fieldType)
          procFields [] = do
            (0,,) <$> (do empty <- scApplyLLVMEmptyStruct sc
                          return $ ExprEvalFn $ \_ -> return empty)
                  <*> (do emptyFn <- scApplyPreludeEmptyVec sc
                          emptyFn fieldType)
          procFields ((fi,expr):flds) = do
            (n,ExprEvalFn reval,rvtp) <- procFields flds
            -- Function fo r
            mtp <- scMemType (fiType fi)
            padding <- scNat sc n
            nt <- scNat sc (toInteger (length flds))
            (n+1,,) <$> (do consStruct <- scApplyLLVMConsStruct sc
                            let cfn = consStruct mtp padding nt rvtp
                            return $ ExprEvalFn $ \eval ->
                              join $ (\c r -> liftIO (cfn c r)) <$> eval expr <*> reval eval)
                    <*> (do consVecFn <- scApplyPreludeConsVec sc
                            entry <- scTuple sc [mtp,padding]
                            consVecFn fieldType entry nt rvtp)
      view _2 <$> procFields (V.toList (siFields si `V.zip` vals))
    IntArith op mn w x y -> do
        case mn of
          Nothing ->
            fmap (evalBin x y) $ join $ mkFn sc <*> scNat sc (toInteger w)
          Just n  ->
            fmap (evalBin x y) $ join $
              mkFnv sc <*> scNat sc (toInteger n)
                       <*> scNat sc (toInteger w)
      where (mkFn, mkFnv) =
              case op of
                Add{}  -> (scApplyLLVMLlvmAdd,  scApplyLLVMLlvmAddV)
                Sub{}  -> (scApplyLLVMLlvmSub,  scApplyLLVMLlvmSubV)
                Mul{}  -> (scApplyLLVMLlvmMul,  scApplyLLVMLlvmMulV)
                UDiv{} -> (scApplyLLVMLlvmUDiv, scApplyLLVMLlvmUDivV)
                SDiv{} -> (scApplyLLVMLlvmSDiv, scApplyLLVMLlvmSDivV)
                URem   -> (scApplyLLVMLlvmURem, scApplyLLVMLlvmURemV)
                SRem   -> (scApplyLLVMLlvmSRem, scApplyLLVMLlvmSRemV)
                Shl{}  -> (scApplyLLVMLlvmShl,  scApplyLLVMLlvmShlV)
                Lshr{} -> (scApplyLLVMLlvmLShr, scApplyLLVMLlvmLShrV)
                Ashr{} -> (scApplyLLVMLlvmAShr, scApplyLLVMLlvmAShrV)
                And    -> (scApplyLLVMLlvmAnd,  scApplyLLVMLlvmAndV)
                Or     -> (scApplyLLVMLlvmOr,   scApplyLLVMLlvmOrV)
                Xor    -> (scApplyLLVMLlvmXor,  scApplyLLVMLlvmXorV)   
    PtrAdd x y -> do
      let mkFn = scApplyLLVMLlvmAddPtr
      fmap (evalBin x y) $ join $
        mkFn sc <*> scNat sc (toInteger (dl^.ptrSize))
    UAddWithOverflow w x y -> do
      let si = mkStructInfo dl False [IntType 1, IntType w]
      let [p0,p1] = V.toList $ fiPadding <$> siFields si
      fn <- scApplyLLVMLlvmAddWithOverflow sc
              <*> scNat sc (toInteger w)
              <*> scNat sc (toInteger p0)
              <*> scNat sc (toInteger p1)
      return $ ExprEvalFn $ \eval -> join $ (\xv yv -> liftIO $ fn xv yv) <$> eval x <*> eval y
    IntCmp op mn w x y -> do
        case mn of
          Nothing ->
            fmap (evalBin x y) $ join $ mkFn sc <*> scNat sc (toInteger w)
          Just n  ->
            fmap (evalBin x y) $ join $
              mkFnV sc <*> scNat sc (toInteger n)
                       <*> scNat sc (toInteger w)
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
      mtp <- scMemType tp       
      return $ ExprEvalFn $ \eval -> do
         join $ (\cv xv yv -> liftIO $ fn mtp cv xv yv) <$> eval c <*> eval x <*> eval y 
    GetStructField si v i -> assert (i < siFieldCount si) $ do
      fn <- scApplyLLVMLlvmStructElt sc
      nt <- scNat sc (toInteger (siFieldCount si))
      fieldType <- scApplyLLVMFieldType sc
      flds <- traverse (scFieldInfo sc scMemType) (siFields si)
      tps <- scVecLit sc fieldType flds
      -- Get index
      ft <- scFinConst sc (toInteger i) (toInteger (siFieldCount si))
      return $ ExprEvalFn $ \eval -> (\val -> liftIO $ fn nt tps val ft) =<< eval v
    GetConstArrayElt n tp a i -> assert (i < n) $ do
      fn <- scApplyLLVMLlvmArrayElt sc
      nt <- scNat sc (toInteger n)
      mtp <- scMemType tp
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
  scMemType <- mkEvalMemType dl sc

  let sbe = SBE { sbeTruePred = t
                , applyIEq = \w x y -> SAWBackend $
                   join $ bvEq <$> scNat sc (toInteger w) ?? x ?? y
                , applyAnd  = lift2 pAnd
                , applyBNot = lift1 pNot
                , applyPredIte = lift3 pIte
                , applyIte = \tp x y z -> SAWBackend $
                    Right <$> join (iteFn <$> scMemType tp ?? x ?? y ?? z)
                , asBool = scAsBool
                , prettyPredD = nyi "prettyPredD"
                , evalPred = nyi "evalPred"
                , freshInt = SAWBackend . scFreshInt sc
                , typedExprEval = \expr -> do
                    typedExprEvalFn dl sc expr
                , applyTypedExpr = \expr -> SAWBackend $ do
                    ExprEvalFn fn <- typedExprEvalFn dl sc expr
                    fn return
                , prettyTermD = nyi "prettyTermD"
                , asUnsignedInteger = nyi "asUnsignedInteger"
                , asConcretePtr = nyi "asConcretePtr"
                , memDump = nyi "memDump"
                , memLoad = nyi "memLoad"
                , memStore = nyi "memStore"
                , memAddDefine = lift3 (smAddDefine dl sc) 
                , memInitGlobal = nyi "memInitGlobal"
                , codeLookupSymbol = \m t -> SAWBackend $ do
                    return (smLookupSymbol m t)
                , stackAlloc = nyi "stackAlloc"
                , stackPushFrame = nyi "stackPushFrame"
                , stackPopFrame = nyi "stackPopFrame"
                , heapAlloc = nyi "heapAlloc"
                , memCopy = nyi "memCopy"
                , memBranch = SAWBackend . return
                , memBranchAbort = SAWBackend . return
                , memMerge = nyi "memMerge"
                , writeAiger = nyi "writeAiger"
                , writeCnf = nyi "writeCnf"
                , evalAiger = nyi "evalAiger"
                , sbeRunIO = runSAWBackend
                }
  return (sbe, emptyMemory)