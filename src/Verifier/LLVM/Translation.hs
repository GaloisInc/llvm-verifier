{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ViewPatterns #-}
-- | This module defines the translation from LLVM IR to Symbolic IR.
--
-- Translation into symbolic IR requires post-dominator information about the
-- LLVM IR.  This information is analyzed and generated during translation.
--
-- In addition to branches, call and phi non-terminal instructions require
-- special support:
--
-- [Call Statements]
--    To simplify the simulator, the symbolic representation splits blocks with
--    calls into multiple basic blocks with each basic block except the last
--    terminated with the call.
--
-- [Phi Statements]
--   The value of a Phi statement in LLVM depends on which previous block was
--   executed.  Since phi statements must appear at the top of the block, we can
--   move phi statements to execute during the transition from the previous
--   block to the new block.
module Verifier.LLVM.Translation
  ( module Verifier.LLVM.AST
  , liftDefine
  , liftTypedValue
  , liftValue
  , MaybeVectorType(..)
  , asMaybePtrVectorType
  , asMaybeIntVectorType
  ) where

import Control.Applicative ((<$>))
import Control.Monad (unless, zipWithM_)
import Control.Monad.State.Strict (State, execState, modify)
import           Data.Traversable
import qualified Data.LLVM.CFG              as CFG
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import qualified Data.Vector                as V
import qualified Text.LLVM                  as L
import           Text.LLVM.AST              (Stmt'(..), Stmt, Typed (..))
import           Text.PrettyPrint.HughesPJ

import Prelude hiding (mapM)


import           Verifier.LLVM.AST
import           Verifier.LLVM.LLVMContext
import           Verifier.LLVM.Utils

import Debug.Trace

-- Utility {{{1

-- | This function is called whenever lifting fails due to an internal error.
liftError :: Doc -> a
liftError d = error (render d)

-- LLVMTranslationInfo {{{1
-- | Information about basic block and control-flow graph used during
-- code generation.
newtype LLVMTranslationInfo = LTI CFG.CFG

-- | Build the translation info using the given CFG's dominator analysis
mkLTI :: CFG.CFG -> LLVMTranslationInfo
mkLTI = LTI

blockIsDummyExit :: L.BlockLabel -> Bool
blockIsDummyExit (L.Named (L.Ident nm)) = nm == CFG.dummyExitName
blockIsDummyExit _ = False

-- | Returns basic blocks excluding dummy exit block.
ltiBlocks :: LLVMTranslationInfo -> [CFG.BB]
ltiBlocks (LTI cfg) = [ bb
                      | bb <- CFG.allBBs cfg
                      , not (blockIsDummyExit (snd (L.bbLabel bb))) 
                      ]

-- | @ltiImmediatePostDominator lti bb@ returns the immediate post dominator
-- of @bb@ or @Nothing@ if it has no post-dominator.
ltiImmediatePostDominator :: LLVMTranslationInfo
                            -> L.BlockLabel
                            -> Maybe L.BlockLabel
ltiImmediatePostDominator (LTI cfg) bb =
  case CFG.ipdom cfg (CFG.asId cfg bb) of
    Nothing    -> Nothing
    Just apdId | blockIsDummyExit n -> Nothing
               | otherwise -> Just n
      where n = CFG.asName cfg apdId

-- Block generator Monad {{{1

type TranslationWarning = Doc

data BlockGeneratorState =
     BlockGeneratorState { bgBlocks :: [SymBlock]
                         , bgRevWarnings :: [TranslationWarning]
                         }

type BlockGenerator a = State BlockGeneratorState a

runBlockGenerator :: BlockGenerator () -> ([TranslationWarning], [SymBlock])
runBlockGenerator m = (reverse (bgRevWarnings s1), bgBlocks s1)
  where s0 = BlockGeneratorState { bgBlocks = [] 
                                 , bgRevWarnings = []
                                 }
        s1 = execState m s0

mkSymBlock :: SymBlockID -> [SymStmt] -> SymBlock
mkSymBlock sbid stmts = SymBlock { sbId = sbid, sbStmts = stmts }

addWarning :: Doc -> BlockGenerator ()
addWarning d = modify (\s -> s { bgRevWarnings = d:bgRevWarnings s })

-- | Define block with given identifier.
defineBlock :: SymBlockID -> [SymStmt] -> BlockGenerator ()
defineBlock sbid stmts = modify fn
  where fn s = s { bgBlocks = mkSymBlock sbid stmts : bgBlocks s }

-- Phi instruction parsing {{{1

type PhiInstr = (L.Ident, L.Type, Map L.BlockLabel L.Value)

-- Define init block that pushes post dominator frames then jumps to first
-- block.
parsePhiStmts :: [Stmt] -> [PhiInstr]
parsePhiStmts sl =
  [ (r, tp, valMap)
  | L.Result r (L.Phi tp vals) _ <- sl
  , let valMap = Map.fromList [(b, v) | (v,b) <- vals]]

-- | Maps LLVM Blocks to associated phi instructions.
blockPhiMap :: [CFG.BB] -> Map L.BlockLabel [PhiInstr]
blockPhiMap blocks =
  Map.fromList
    [ (l, parsePhiStmts sl)
    | L.BasicBlock { L.bbLabel = (_bbid, l), L.bbStmts = sl } <- blocks ]

data MaybeVectorType v = ScalarType v
                       | VectorType Int v
                       | NotMaybeVector
  deriving (Functor)

lookupAlias :: (?lc :: LLVMContext) => L.Ident -> Maybe L.Type
lookupAlias i = Map.lookup i (llvmTypeAliasMap ?lc)

addrWidth :: (?lc :: LLVMContext) => BitWidth
addrWidth = llvmAddrWidthBits ?lc

asPtrType :: (?lc :: LLVMContext) => L.Type -> Maybe L.Type
asPtrType = go
  where go (L.Alias a)  = go =<< lookupAlias a
        go (L.PtrTo tp) = return tp
        go _ = Nothing

asIntType :: (?lc :: LLVMContext) => L.Type -> Maybe BitWidth
asIntType = go
  where go (L.Alias a)  = go =<< lookupAlias a
        go (L.PrimType (L.Integer w)) = return (fromIntegral w)
        go _ = Nothing

asMaybeVectorType :: (?lc :: LLVMContext)
                  => (L.Type -> Maybe a)
                  -> L.Type
                  -> MaybeVectorType a
asMaybeVectorType fn = go
  where lkup f a = maybe NotMaybeVector f (lookupAlias a)
        go (L.Alias a)  = lkup go a
        go (L.Vector n (L.Alias a))  = lkup (go . L.Vector n) a
        go (L.Vector n tp) =
          maybe NotMaybeVector (VectorType (fromIntegral n)) (fn tp)
        go tp = maybe NotMaybeVector ScalarType (fn tp)

asMaybePtrVectorType :: (?lc :: LLVMContext) => L.Type -> MaybeVectorType L.Type
asMaybePtrVectorType = asMaybeVectorType asPtrType

asMaybeIntVectorType :: (?lc :: LLVMContext) => L.Type -> MaybeVectorType BitWidth
asMaybeIntVectorType = asMaybeVectorType asIntType

-- | Check that two types are equal, returning simplied type if they are
-- and nothing otherwise.
typesEq :: (?lc :: LLVMContext) => L.Type -> L.Type -> Maybe L.Type
typesEq ltp _ = return ltp

liftTypedValue :: (?lc :: LLVMContext) => L.Typed L.Value -> Maybe TypedSymValue
liftTypedValue (L.Typed tp v) = liftValue tp v

returnTypedValue :: (?lc :: LLVMContext) => L.Typed L.Value -> Maybe (L.Typed TypedSymValue)
returnTypedValue (L.Typed tp v) = L.Typed tp <$> liftValue tp v

liftValue :: (?lc :: LLVMContext) => L.Type -> L.Value -> Maybe TypedSymValue
liftValue (L.Alias i) v = do
  tp <- lookupAlias i
  liftValue tp v
liftValue (L.PrimType (L.Integer w)) (L.ValInteger x) =
  return $ SValExpr $ SValInteger (fromIntegral w) x
liftValue (L.PrimType (L.Integer 1)) (L.ValBool x) =
  return $ SValExpr $ SValInteger 1 (if x then 1 else 0)
liftValue _ (L.ValFloat x) =
  return $ SValExpr $ SValFloat x
liftValue (L.PrimType (L.FloatType L.Double)) (L.ValDouble d) =
  return $ SValExpr $ SValDouble d
-- TODO: Figure out how to check ident types.
liftValue _ (L.ValIdent i) =
   return $ SValIdent i
-- TODO: Figure out how to check symbol types.
liftValue _ (L.ValSymbol sym) =
  return $ SValSymbol sym
liftValue (L.PtrTo etp) L.ValNull =
  return $ SValExpr $ SValNull (llvmAddrWidthBits ?lc) etp
liftValue (L.Array len _) (L.ValArray vtp el0)
    | fromIntegral len == V.length el
    = SValExpr . SValArray vtp <$> mapM (liftValue vtp) el
  where el = V.fromList el0
liftValue (L.Vector len _) (L.ValVector vtp el0)
    | fromIntegral len == V.length el
    = SValExpr . SValArray vtp <$> mapM (liftValue vtp) el
  where el = V.fromList el0
liftValue _ (L.ValStruct fldvs) =
    SValExpr . SValStruct si <$> mapM liftTypedValue (V.fromList fldvs)
  where si = mkStructInfo False (typedType <$> fldvs)
liftValue _ (L.ValPackedStruct fldvs) =
    SValExpr . SValStruct si <$> mapM liftTypedValue (V.fromList fldvs)
  where si = mkStructInfo True (typedType <$> fldvs)
liftValue (L.Array len (L.PrimType (L.Integer 8))) (L.ValString str) = do
  unless (fromIntegral len == length str) $ fail "Incompatible types"
  return (sValString str)
liftValue tp (L.ValConstExpr ce)  =
  case (tp,ce) of
    (_, L.ConstGEP inbounds (base:il)) -> do
      SValExpr <$> liftGEP inbounds base il
    --  evalGEP (GEP inbounds ptr idxs)
    (tp0, L.ConstConv op (L.Typed itp t) tp1) -> do
      rtp <- typesEq tp0 tp1
      v <- liftValue itp t
      case op of
        L.PtrToInt -> do
          ptrType <- asPtrType itp
          w <- asIntType rtp
          return $ SValExpr (PtrToInt Nothing ptrType v w)
        L.IntToPtr -> do
          w <- asIntType itp
          ptrType <- asPtrType rtp
          return $ SValExpr (IntToPtr Nothing w v ptrType)
        L.BitCast ->
          return $ SValExpr (Bitcast itp v rtp)
        _ -> fail "Could not interpret constant expression"
    _ -> fail "Could not interpret constant expression"
liftValue tp L.ValUndef = zeroValue tp
liftValue _ L.ValLabel{} = fail "Could not interpret label"
liftValue tp L.ValZeroInit = zeroValue tp
liftValue _ L.ValAsm{} = fail "Could not interpret asm."
liftValue _ L.ValMd{} = fail "Could not interpret metadata."
liftValue _ _ = fail "Could not interpret LLVM value"

zeroValue :: (?lc :: LLVMContext) => L.Type -> Maybe TypedSymValue
zeroValue = fmap SValExpr . zeroExpr

zeroExpr :: (?lc :: LLVMContext) => L.Type -> Maybe (TypedExpr TypedSymValue)
zeroExpr tp0 =
  case tp0 of
    L.PrimType pt -> 
      case pt of
        L.Integer w -> return $ SValInteger (fromIntegral w) 0
        L.FloatType L.Float -> return $ SValFloat 0
        L.FloatType L.Double -> return $ SValDouble 0
        _ -> fail "Bad prim type"
    L.Alias i -> zeroExpr =<< lookupAlias i
    L.Array n tp -> SValArray tp . V.replicate (fromIntegral n) <$> zeroValue tp
    L.FunTy{} -> fail "Bad function type"
    L.PtrTo tp -> return $ SValNull addrWidth tp
    L.Struct l -> SValStruct si <$> mapM zeroValue (V.fromList l)
      where si = mkStructInfo False l
    L.PackedStruct l -> SValStruct si <$> mapM zeroValue (V.fromList l)
      where si = mkStructInfo True l
    L.Vector n tp -> SValVector tp . V.replicate (fromIntegral n) <$> zeroValue tp
    L.Opaque -> fail "Opaque"

unsupportedStmt :: L.Stmt -> BlockGenerator SymStmt
unsupportedStmt stmt = do
  addWarning $ text "Unsupported instruction: " <+> L.ppStmt stmt
  return (BadSymStmt stmt)

trySymStmt :: Stmt
           -> Maybe SymStmt
           -> BlockGenerator SymStmt
trySymStmt stmt ms =
  case ms of
    Just s -> return s
    Nothing -> unsupportedStmt stmt

liftGEP :: (?lc :: LLVMContext)
        => Bool 
        -> L.Typed L.Value
        -> [L.Typed L.Value]
        -> Maybe (TypedExpr TypedSymValue)
liftGEP inbounds (Typed initType initValue) = go [] initType
  where gepFailure tp = trace ("GEPFailure: " ++ tp) fail "Could not parse GEP Value"
        go :: [GEPOffset TypedSymValue]
           -> L.Type
           -> [L.Typed L.Value]
           -> Maybe (TypedExpr TypedSymValue)
        go args tp [] = do
          sv <- liftValue initType initValue
          return $ GEP inbounds sv (reverse args) tp
        go args (L.Alias a) l = do
           tp <- lookupAlias a
           go args tp l
        go args (L.Array _ etp) r = goArray args etp r
        go args (L.PtrTo etp) r   = goArray args etp r
        go args (L.Struct tpl) r      = goStruct args (mkStructInfo False tpl) r
        go args (L.PackedStruct tpl) r = goStruct args (mkStructInfo True tpl) r
        go _ tp _ = gepFailure ("go " ++ show tp)
        goArray args etp (tv : r)= do
          w <- asIntType (typedType tv)
          sv <- liftTypedValue tv
          let sz = llvmStoreSizeOf ?lc etp
          go (ArrayElement sz w sv:args) etp r
        goArray _ _ _ = gepFailure "goArray"
        goStruct args
                 si 
                 (L.Typed (L.PrimType (L.Integer 32)) (L.ValInteger i) : r) = do       
          let idx = fromIntegral i
          unless (0 <= idx && idx < V.length (structFields si)) $
            gepFailure "idxRange"
          let (tp,_) = structFields si V.! idx
          go (StructField si idx:args) tp r
        goStruct _ _ _ = gepFailure "goStruct"
        

liftStmt :: (?lc :: LLVMContext) => L.Stmt -> BlockGenerator SymStmt
liftStmt stmt = do
  case stmt of
    Effect (L.Store ptr addr malign) _ ->
      trySymStmt stmt $ do
        tptr <- returnTypedValue ptr
        taddr <- liftTypedValue addr 
        return (Store tptr taddr malign)
    Effect{} -> unsupportedStmt stmt
    Result r app _ -> trySymStmt stmt $ do
      -- Return an assignemnt statement for the value.
      let retExpr :: Monad m => L.Type -> SymExpr -> m SymStmt
          retExpr tp e = return (Assign (L.Typed tp r) e)
      let retTExpr :: Monad m => TypedExpr TypedSymValue -> m SymStmt
          retTExpr v = retExpr (typedExprType v) (Val (SValExpr v))
      let retIntArith op tp u v = do
            x <- liftValue tp u
            y <- liftValue tp v
            case asMaybeIntVectorType tp of
              ScalarType w -> retTExpr (IntArith op Nothing w x y)
              VectorType n w -> retTExpr (IntArith op (Just n) w x y)
              _ -> fail "Could not parse argument type"
      case app of
        L.Arith llvmOp (L.Typed tp u) v   -> do
          let retTotal op = retIntArith op tp u v
          let retPartial op = retIntArith op tp u v
          case llvmOp of
            L.Add nuw nsw -> retTotal (Add nuw nsw)
            L.Sub nuw nsw -> retTotal (Sub nuw nsw)
            L.Mul nuw nsw -> retTotal (Mul nuw nsw)
            L.UDiv exact  -> retPartial (UDiv exact)
            L.SDiv exact  -> retPartial (SDiv exact)
            L.URem        -> retPartial URem
            L.SRem        -> retPartial SRem
            _ -> fail "Do not support floating point operations"
        L.Bit llvmOp (L.Typed tp u) v   -> retIntArith op tp u v
          where op = case llvmOp of
                       L.Shl nuw nsw -> Shl nuw nsw
                       L.Lshr exact -> Lshr exact
                       L.Ashr exact -> Ashr exact
                       L.And -> And
                       L.Or  -> Or
                       L.Xor -> Xor
        L.Conv op (L.Typed itp e) rtp -> do
          sv <- liftValue itp e
          let intConv :: (BitWidth -> BitWidth -> Bool)
                      -> (Maybe Int -> BitWidth -> TypedSymValue -> BitWidth
                                    -> TypedExpr TypedSymValue)
                      -> Maybe SymStmt
              intConv cond fn =
                case (asMaybeIntVectorType itp, asMaybeIntVectorType rtp) of
                  (ScalarType iw, ScalarType rw) | cond iw rw ->
                    retTExpr (fn Nothing iw sv rw)
                  (VectorType n iw, VectorType nr rw) | n == nr && cond iw rw ->
                    retTExpr (fn (Just n) iw sv rw)
                  _ -> fail "Could not parse conversion types"
          case op of
            L.Trunc -> intConv (\iw rw -> iw > rw) Trunc
            L.ZExt -> intConv (\iw rw -> iw < rw) ZExt
            L.SExt -> intConv (\iw rw -> iw < rw) SExt
            L.PtrToInt ->
              case (asMaybePtrVectorType itp, asMaybeIntVectorType rtp) of
                (ScalarType ptr, ScalarType w) ->
                  retTExpr (PtrToInt Nothing ptr sv w)
                (VectorType n ptr, VectorType nr w) | n == nr ->
                  retTExpr (PtrToInt (Just n) ptr sv w)
                _ -> fail "Could not parse conversion types"
            L.IntToPtr ->
              case (asMaybeIntVectorType itp, asMaybePtrVectorType rtp) of
                (ScalarType w, ScalarType ptr) ->
                  retTExpr (IntToPtr Nothing w sv ptr)
                (VectorType n w, VectorType nr ptr) | n == nr ->
                  retTExpr (IntToPtr (Just n) w sv ptr)
                _ -> fail "Could not parse conversion types"
            L.BitCast -> retTExpr (Bitcast itp sv rtp)
            _ -> fail "Unsupported conversion operator"
        L.Alloca tp Nothing mi -> retExpr (L.PtrTo tp) $ Alloca tp Nothing mi
        L.Alloca tp (Just (L.Typed szTp sz)) mi -> do
          w <- asIntType szTp
          v <- liftValue szTp sz
          retExpr (L.PtrTo tp) $ Alloca tp (Just (w,v)) mi 
        L.Load (L.Typed tp ptr) malign -> do
          etp <- asPtrType tp
          v <- liftValue tp ptr
          retExpr etp (Load v etp malign)
        L.ICmp op (L.Typed tp u) v -> do
          x <- liftValue tp u
          y <- liftValue tp v
          case asMaybeIntVectorType tp of
            ScalarType w -> retTExpr (IntCmp op Nothing w x y)
            VectorType n w -> retTExpr (IntCmp op (Just n) w x y)
            _ -> fail "Could not parse argument type"
        L.GEP ib tp tpvl     -> retTExpr =<< liftGEP ib tp tpvl
        L.Select (L.Typed tpc c') (L.Typed tpv v1') v2' -> do
          c <- liftValue tpc c'
          v1 <- liftValue tpv v1'
          v2 <- liftValue tpv v2'
          case (asMaybeIntVectorType tpc, asMaybeVectorType Just tpv) of
            (ScalarType w,_) | w == 1 ->
              retTExpr $ Select Nothing c tpv v1 v2
            (VectorType n w, VectorType nr tpe) | w == 1 && n == nr ->
              retTExpr $ Select (Just n) c tpe v1 v2
            _  -> fail "Could not parse select intruction."
        L.ExtractValue (L.Typed vtp v) il -> go vtp il =<< liftValue vtp v
          where go :: L.Type
                   -> [Int32]
                   -> TypedSymValue
                   -> Maybe SymStmt
                go tp [] sv = retExpr tp (Val sv)
                go (L.Alias a) l sv = lookupAlias a >>= \t -> go t l sv
                go (L.Struct ftys) (i0 : is) sv
                   | 0 <= i && i < length ftys =
                       go (ftys !! i) is (SValExpr (GetStructField si sv i))
                   | otherwise = fail "Illegal index"
                  where i = fromIntegral i0
                        si = mkStructInfo False ftys
                go (L.PackedStruct ftys) (i0 : is) sv
                    | 0 <= i && i < length ftys =
                       go (ftys !! i) is (SValExpr (GetStructField si sv i))
                    | otherwise = fail "Illegal index"
                  where i = fromIntegral i0
                        si = mkStructInfo True ftys
                go (L.Array n tp) (i : is) sv
                    | 0 <= i && i < n = go tp is (SValExpr (GetConstArrayElt tp sv i))
                    | otherwise = fail "Illegal index"
                go _ _ _ = fail "non-composite type in extractvalue"
        _ -> fail "Unsupported instruction"

-- Lift LLVM basic block to symbolic block {{{1
--
-- Invariants assumed by block:
-- * When jumping from the current block to a new block,
--   * The current block must ensure that the correct post-dominator merge frames are added.
--   * The current block must set the phi value registers.
liftBB :: (?lc :: LLVMContext)
       => LLVMTranslationInfo -- ^ Translation information from analysis
       -> Map L.BlockLabel [PhiInstr] -- ^ Maps block identifiers to phi instructions for block.
       -> CFG.BB -- ^ Basic block to generate.
       -> BlockGenerator ()
liftBB lti phiMap bb = do
  let llvmId = CFG.blockName bb
      -- Block for post dominator
      pd = flip symBlockID 0 <$> ltiImmediatePostDominator lti llvmId
      blockName :: Int -> SymBlockID
      blockName = symBlockID llvmId
      -- | Returns set block instructions for jumping to a particular target.
      -- This includes setting the current block and executing any phi
      -- instructions.
      phiInstrs :: L.BlockLabel -> [SymStmt]
      phiInstrs tgt =
          [ Assign (L.Typed tp r) val
            | (r, tp, valMap) <- case Map.lookup tgt phiMap of
                Nothing -> error "AST xlate: missing tgt entry in phiMap"
                Just x  -> x
            , let val = case liftValue tp =<< Map.lookup llvmId valMap of
                          Nothing -> error $
                            "AST xlate: expected to find "
                            ++ show (L.ppLabel llvmId)
                            ++ " as a phi operand of block"
                            ++ " labeled " ++ show (L.ppLabel tgt)
                          Just x -> Val x
          ]
      -- @brSymInstrs tgt@ returns the code for jumping to the target block.
      -- Observations:
      --  * A post-dominator of a post-dominator of the current block is itself
      --    the post-dominator of the current block.
      --    Consequently, branching to a post-dominator of the current block
      --    cannot result in new nodes being needed.
      -- Behavior:
      --   For unconditional branches to tgt, do the following:
      --     If tgt is the dominator
      --       Set current block in state to branch target.
      --       Merge this state with dominator state.
      --       Clear current execution
      --     Else
      --       Set current block in state to branch target.
      brSymInstrs :: L.BlockLabel -> [SymStmt]
      brSymInstrs tgt =
        SetCurrentBlock (symBlockID tgt 0) : phiInstrs tgt
      -- | Sequentially process statements.
      impl :: [L.Stmt] -- ^ Remaining statements
           -> Int -- ^ Index of symbolic block that we are defining.
           -> [SymStmt] -- ^ Previously generated statements in reverse order.
           -> BlockGenerator ()
      impl [] idx il = liftError $
                       text "Missing terminal instruction in block" <+>
                       int idx <+>
                       text "after generating the following statements:" $$
                       (nest 2 . vcat . map ppSymStmt $ il)
      impl [stmt@(Effect (L.Ret tpv) _)] idx il = do
        symStmt <- 
          case liftTypedValue tpv of
            Just v -> return $ Return (Just v)
            Nothing -> unsupportedStmt stmt   
        defineBlock (blockName idx) (reverse il ++ [symStmt])
      impl [Effect L.RetVoid _] idx il =
        defineBlock (blockName idx) (reverse il ++ [Return Nothing])     
      -- For function calls, we:
      -- * Allocate block for next block after call.
      -- * Define previous block to end with pushing call frame.
      -- * Process rest of instructions.
      -- * TODO: Handle invoke instructions
      impl (stmt@(Result reg (L.Call _b tp v tpvl) _):r) idx il = do
        -- NB: The LLVM bitcode parser always types call
        -- instructions with the full ptr-to-fun type in order to
        -- present a consistent form that also handles varargs.  We
        -- just extract the return type here for now, but will
        -- likely need to support varargs more explicitly
        -- later. TODO.
        let mstmt = do
              sv <- liftValue tp v
              L.FunTy rty _args _va <- asPtrType tp
              svl <- mapM liftTypedValue tpvl
              let res = Typed { typedType = rty, typedValue = reg }
              return $ PushCallFrame sv svl (Just res) (blockName (idx + 1))
        symStmt <- maybe (unsupportedStmt stmt) return mstmt
        defineBlock (blockName idx) $ reverse (symStmt:il)
        impl r (idx+1) []
      -- Function call that does not return a value (see comment for other call case).
      impl (stmt@(Effect (L.Call _b tp v tpvl) _):r) idx il = do
        let mstmt = do
              sv <- liftValue tp v
              svl <- mapM liftTypedValue tpvl
              return $ PushCallFrame sv svl Nothing (blockName (idx+1))
        symStmt <- maybe (unsupportedStmt stmt) return mstmt
        defineBlock (blockName idx) $ reverse (symStmt:il)
        impl r (idx+1) []
      impl [Effect (L.Jump tgt) _] idx il = do
        defineBlock (blockName idx) $
          reverse il ++ brSymInstrs tgt
      impl [stmt@(Effect (L.Switch (Typed tp v) def cases) _)] idx il = do
          case (asIntType tp, liftValue tp v) of
            (Just w, Just tsv) -> do
              let mkCase (cv, bid) rest =
                    [ PushPendingExecution bid (HasConstValue tsv w cv) pd rest ]
              let symbolicCases = foldr mkCase (brSymInstrs def)
                                $ zip consts caseBlockIds
              defineBlock (blockName idx) $ reverse il ++ symbolicCases
              zipWithM_ defineBlock caseBlockIds (brSymInstrs <$> targets)
            _ -> do
              symStmt <- unsupportedStmt stmt
              defineBlock (blockName idx) $ reverse (symStmt:il)
        where -- Get values and targets
              (consts,targets) = unzip cases
              caseBlockIds     = blockName <$> [(idx + 1)..(idx + length cases)]
      impl [stmt@(Effect (L.Br (Typed tp c) tgt1 tgt2) _)] idx il =
        case liftValue tp c of
          Just tc | tp == i1 -> do
            let suspendSymBlockID = blockName (idx + 1)
            defineBlock (blockName idx) $ reverse il ++
              [ PushPendingExecution suspendSymBlockID
                                    (HasConstValue tc 1 0)
                                    pd
                                    (brSymInstrs tgt1) ]
            -- Define block for suspended thread.
            defineBlock suspendSymBlockID  (brSymInstrs tgt2)
          _ -> do
            ss <- unsupportedStmt stmt
            defineBlock (blockName idx) $ reverse (ss:il)
      impl [Effect L.Unreachable _] idx il = do
        defineBlock (blockName idx) (reverse (Unreachable : il))
      impl [stmt@(Effect L.Unwind _)] idx il = do
        ss <- unsupportedStmt stmt
        defineBlock (blockName idx) (reverse (ss : il))
      -- | Phi statements are handled by initial blocks.
      impl (Result _id (L.Phi _ _) _:r) idx il = impl r idx il
      impl (Effect (L.Comment _) _:r) idx il = impl r idx il
      impl (stmt:rest) idx il = do
        s' <- liftStmt stmt
        impl rest idx (s' : il)
   in impl (L.bbStmts bb) 0 []

-- Lift LLVM definition to symbolic definition {{{1
liftDefine :: LLVMContext -> L.Define -> ([TranslationWarning], SymDefine)
liftDefine lc d = (warnings,sd)
  where cfg            = CFG.buildCFG (L.defBody d)
        lti            = mkLTI cfg
        blocks         = ltiBlocks lti
        initBlock      = CFG.bbById cfg (CFG.entryId cfg)
        initBlockLabel = CFG.blockName initBlock
        initSymBlock =
          mkSymBlock initSymBlockID
                     [SetCurrentBlock (symBlockID initBlockLabel 0)]
        phiMap = blockPhiMap blocks
        (warnings,symBlocks) = runBlockGenerator (mapM_ (liftBB lti phiMap) blocks)
          where ?lc = lc
        sd = SymDefine { sdName = L.defName d
                       , sdArgs = L.defArgs d
                       , sdVarArgs = L.defVarArgs d
                       , sdRetType = L.defRetType d
                       , sdBody = Map.fromList
                                    [ (sbId b,b) | b <- initSymBlock : symBlocks ]
                       }

-- Test code {{{1
{-
-- | Translate the given module
testTranslate :: L.Module -> IO ()
testTranslate mdl = do
  putStrLn $ render $ L.ppModule mdl
  putStrLn $ replicate 80 '-'
  let lc = undefined
  forM_ (L.modDefines mdl) $ \def -> do
    let (_,sd) = liftDefine lc def
    putStrLn $ render $ ppSymDefine sd
    putStrLn ""
-}