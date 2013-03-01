{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
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
  , liftValue
  ) where

import Control.Applicative
import Control.Lens hiding (op)
import Control.Monad (unless, zipWithM_)
import Control.Monad.State.Strict (State, execState, modify)
import qualified Data.LLVM.CFG              as CFG
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import qualified Data.Vector                as V
import qualified Text.LLVM                  as L
import           Text.LLVM.AST              (Stmt'(..), Stmt, Typed (..))
import           Text.PrettyPrint.HughesPJ

import           Verifier.LLVM.AST

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

liftTypedValue :: (?lc :: LLVMContext) => L.Typed L.Value -> Maybe TypedSymValue
liftTypedValue (L.Typed tp v) = flip liftValue v =<< liftMemType tp

liftValue :: (?lc :: LLVMContext) => MemType -> L.Value -> Maybe TypedSymValue
liftValue (IntType w) (L.ValInteger x) =
  return $ SValExpr $ SValInteger w x
liftValue (IntType 1) (L.ValBool x) =
  return $ SValExpr $ SValInteger 1 (if x then 1 else 0)
liftValue FloatType  (L.ValFloat x) =
  return $ SValExpr $ SValFloat x
liftValue DoubleType (L.ValDouble d) =
  return $ SValExpr $ SValDouble d
-- TODO: Figure out how to check ident types.
liftValue _ (L.ValIdent i) =
   return $ SValIdent i
-- TODO: Figure out how to check symbol types.
liftValue _ (L.ValSymbol sym) =
  return $ SValSymbol sym
liftValue (PtrType etp) L.ValNull =
  return $ SValExpr $ SValNull etp
liftValue (ArrayType len etp) (L.ValArray _ el0)
    | fromIntegral len == V.length el
    = SValExpr . SValArray etp <$> traverse (liftValue etp) el
  where el = V.fromList el0
liftValue (VecType len etp) (L.ValVector _ el0)
    | fromIntegral len == V.length el
    = SValExpr . SValVector etp <$> traverse (liftValue etp) el
  where el = V.fromList el0
liftValue (StructType si) (L.ValStruct fldvs) =
    SValExpr . SValStruct si <$> traverse liftTypedValue (V.fromList fldvs)
liftValue (StructType si) (L.ValPackedStruct fldvs) =
    SValExpr . SValStruct si <$> traverse liftTypedValue (V.fromList fldvs)
liftValue (ArrayType len (IntType 8)) (L.ValString str) = do
  unless (fromIntegral len == length str) $ fail "Incompatible types"
  return (sValString str)
liftValue rtp (L.ValConstExpr ce)  =
  case ce of
    L.ConstGEP inbounds (base:il) -> do
      SValExpr . snd <$> liftGEP inbounds base il
    L.ConstConv op (L.Typed itp0 t) _tp1 -> do
      itp <- liftMemType itp0
      v <- liftValue itp t
      case (op,itp,rtp) of
        (L.PtrToInt, PtrType ptrType, IntType w) -> do
          return $ SValExpr (PtrToInt Nothing ptrType v w)
        (L.IntToPtr, IntType w, PtrType ptrType) -> do
          return $ SValExpr (IntToPtr Nothing w v ptrType)
        (L.BitCast,_,_) -> liftBitcast itp v rtp
        _ -> fail "Could not interpret constant expression"
    _ -> fail "Could not interpret constant expression"
liftValue tp L.ValUndef = return $ zeroValue tp
liftValue _ L.ValLabel{} = fail "Could not interpret label"
liftValue tp L.ValZeroInit = return $ zeroValue tp
liftValue _ L.ValAsm{} = fail "Could not interpret asm."
liftValue _ L.ValMd{} = fail "Could not interpret metadata."
liftValue _ _ = fail "Could not interpret LLVM value"


-- | Lift a bitcast expression.
liftBitcast :: (?lc :: LLVMContext)
            => MemType -- ^ Input argument type
            -> TypedSymValue -- ^ Input argument expression.
            -> MemType -- ^ Result argument type
            -> Maybe TypedSymValue
liftBitcast PtrType{} v PtrType{} = return v
liftBitcast itp v rtp | itp == rtp = return v
{-
liftBitcast itp v (ArrayType rc rtp) =
    SValExpr . SValArray rtp <$> generateM rc fn
  where fn i =
          case itp of
            ArrayType ic itp ->
              where cc = gcd ic rc
                    rm = rc `div` cc
-}

liftBitcast _ _ _ = fail "Symbolic simulator does not support bitcast."

zeroValue :: MemType -> TypedSymValue
zeroValue = SValExpr . zeroExpr

zeroExpr :: MemType -> (TypedExpr TypedSymValue)
zeroExpr tp0 =
  case tp0 of
    IntType w  -> SValInteger (fromIntegral w) 0
    FloatType  -> SValFloat 0
    DoubleType -> SValDouble 0
    PtrType tp -> SValNull tp
    ArrayType n tp -> SValArray tp $ V.replicate (fromIntegral n) $ zeroValue tp
    VecType n tp  -> SValVector tp $ V.replicate (fromIntegral n) $ zeroValue tp
    StructType si -> SValStruct si $ zeroValue <$> siFieldTypes si

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
        -> Maybe (MemType, TypedExpr TypedSymValue)
liftGEP _inbounds (Typed initType0 initValue) args0 = do
     rtp <- liftMemType initType0
     let fn (L.Typed tp v) = (,v) <$> liftMemType tp
     go (SValExpr (SValInteger aw 0)) rtp =<< traverse fn args0
  where gepFailure = fail "Could not parse GEP Value"
        pdl = llvmDataLayout ?lc
        aw :: BitWidth
        aw = ptrBitwidth pdl 
        mn = Nothing
       
        go :: TypedSymValue
           -> MemType
           -> [(MemType, L.Value)]
           -> Maybe (MemType, TypedExpr TypedSymValue)
        go args tp [] = do
          initType <- liftMemType initType0
          sv <- liftValue initType initValue
          return (tp, PtrAdd sv args)
        go args (ArrayType _ etp) r = goArray args etp r
        go args (PtrType tp) r = do
          mtp <- asMemType (llvmAliasMap ?lc) tp
          goArray args mtp r
        go args (StructType si) r = goStruct args si r
        go _ _ _ = gepFailure

        mergeAdd (SValExpr (SValInteger _ 0)) y = y
        mergeAdd x (SValExpr (SValInteger _ 0)) = x
        mergeAdd (SValExpr (SValInteger _ i)) (SValExpr (SValInteger _ j)) =
          SValExpr (SValInteger aw (i+j))
        mergeAdd x y = SValExpr (IntArith (Add False False) mn aw x y)

        goArray args etp ((IntType w, v0) : r)= do
          v1 <- liftValue (IntType w) v0
          let v2 | SValExpr (SValInteger _ i) <- v1 = SValExpr (SValInteger aw i)
                 | aw == w   = v1
                 | aw >  w   = SValExpr $ ZExt  mn w v1 aw
                 | otherwise = SValExpr $ Trunc mn w v1 aw
          let sz = fromIntegral $ memTypeSize pdl etp
          let v3 | SValExpr (SValInteger _ i) <- v2 = SValExpr (SValInteger aw (sz*i))
                 | sz == 1 = v2
                 | otherwise = SValExpr $ IntArith (Mul False False) 
                                                   mn
                                                   aw
                                                   (SValExpr (SValInteger aw sz))
                                                   v2
          go (mergeAdd args v3) etp r
        goArray _ _ _ = gepFailure

        goStruct args si  ((IntType 32, L.ValInteger i) : r) = do       
          fi <- siFieldInfo si (fromIntegral i)
          let val = SValExpr (SValInteger aw (toInteger (fiOffset fi)))
          go (mergeAdd args val) (fiType fi) r
        goStruct _ _ _ = gepFailure

liftStmt :: (?lc :: LLVMContext) => L.Stmt -> BlockGenerator SymStmt
liftStmt stmt = do
  case stmt of
    Effect (L.Store (L.Typed tp0 v) addr malign) _ ->
      trySymStmt stmt $ do
        tp <- liftMemType tp0
        tptr <- liftValue tp v
        taddr <- liftTypedValue addr 
        return $ Store tp tptr taddr malign
    Effect{} -> unsupportedStmt stmt
    Result r app _ -> trySymStmt stmt $ do
      -- Return an assignemnt statement for the value.
      let retExpr :: Monad m => MemType -> SymExpr -> m SymStmt
          retExpr tp e = return (Assign r tp e)
      let retTExpr :: Monad m => MemType -> TypedExpr TypedSymValue -> m SymStmt
          retTExpr tp v = retExpr tp (Val (SValExpr v))
      let retIntArith :: IntArithOp -> L.Type -> L.Value -> L.Value -> Maybe SymStmt
          retIntArith op tp0 u v = do
            tp <- liftMemType tp0 
            x <- liftValue tp u
            y <- liftValue tp v
            case tp of
              IntType w   -> retTExpr tp (IntArith op Nothing w x y)
              VecType n (IntType w) -> retTExpr tp (IntArith op (Just n) w x y)
              _ -> fail "Could not parse argument type"
      case app of
        L.Arith llvmOp (L.Typed tp u) v -> do
           op <- case llvmOp of
                   L.Add nuw nsw -> return (Add nuw nsw)
                   L.Sub nuw nsw -> return (Sub nuw nsw)
                   L.Mul nuw nsw -> return (Mul nuw nsw)
                   L.UDiv exact  -> return (UDiv exact)
                   L.SDiv exact  -> return (SDiv exact)
                   L.URem        -> return URem
                   L.SRem        -> return SRem
                   _ -> fail "Do not support floating point operations"
           retIntArith op tp u v
        L.Bit llvmOp (L.Typed tp u) v   -> retIntArith op tp u v
          where op = case llvmOp of
                       L.Shl nuw nsw -> Shl nuw nsw
                       L.Lshr exact -> Lshr exact
                       L.Ashr exact -> Ashr exact
                       L.And -> And
                       L.Or  -> Or
                       L.Xor -> Xor
        L.Conv op (L.Typed itp0 e) rtp0 -> do
          itp <- liftMemType itp0 
          rtp <- liftMemType rtp0
          sv <- liftValue itp e
          let intConv :: (BitWidth -> BitWidth -> Bool)
                      -> (Maybe Int -> BitWidth -> TypedSymValue -> BitWidth
                                    -> TypedExpr TypedSymValue)
                      -> Maybe SymStmt
              intConv cond fn =
                case (itp, rtp) of
                  (IntType iw, IntType rw) | cond iw rw ->
                    retTExpr rtp (fn Nothing iw sv rw)
                  (VecType n (IntType iw), VecType nr (IntType rw)) | n == nr && cond iw rw ->
                    retTExpr rtp (fn (Just n) iw sv rw)
                  _ -> fail "Could not parse conversion types"
          case op of
            L.Trunc -> intConv (\iw rw -> iw > rw) Trunc
            L.ZExt -> intConv (\iw rw -> iw < rw) ZExt
            L.SExt -> intConv (\iw rw -> iw < rw) SExt
            L.PtrToInt ->
              case (itp, rtp) of
                (PtrType ptr, IntType w) ->
                  retTExpr rtp (PtrToInt Nothing ptr sv w)
                ( VecType n (PtrType ptr), VecType nr (IntType w)) | n == nr ->
                  retTExpr rtp (PtrToInt (Just n) ptr sv w)
                _ -> fail "Could not parse conversion types"
            L.IntToPtr ->
              case (itp, rtp) of
                ( IntType w, PtrType ptr) ->
                  retTExpr rtp (IntToPtr Nothing w sv ptr)
                ( VecType n (IntType w), VecType nr (PtrType ptr)) | n == nr ->
                  retTExpr rtp (IntToPtr (Just n) w sv ptr)
                _ -> fail "Could not parse conversion types"
            L.BitCast -> do
              retExpr rtp . Val =<< liftBitcast itp sv rtp
            _ -> fail "Unsupported conversion operator"
        L.Alloca tp0 msz mi -> do
          tp <- liftMemType tp0
          ssz <- case msz of
                   Nothing -> return Nothing
                   Just (L.Typed szTp0 sz) -> do
                     IntType w <- liftMemType szTp0
                     v <- liftValue (IntType w) sz
                     return (Just (w,v))
          retExpr (PtrType (MemType tp)) $ Alloca tp ssz mi 
        L.Load (L.Typed tp0 ptr) malign -> do
          tp@(PtrType etp0) <- liftMemType tp0
          etp <- asMemType (llvmAliasMap ?lc) etp0
          v <- liftValue tp ptr
          retExpr etp (Load v etp malign)
        L.ICmp op (L.Typed tp0 u) v -> do
          tp <- liftMemType tp0
          x <- liftValue tp u
          y <- liftValue tp v
          case tp of
            IntType w ->
              retTExpr (IntType 1) (IntCmp op Nothing w x y)
            VecType n (IntType w) ->
              retTExpr (VecType n (IntType 1)) (IntCmp op (Just n) w x y)
            _ -> fail "Could not parse argument type"
        L.GEP ib tp tpvl     -> uncurry retTExpr =<< liftGEP ib tp tpvl
        L.Select (L.Typed tpc0 c') (L.Typed tpv0 v1') v2' -> do
          tpc <- liftMemType tpc0
          tpv <- liftMemType tpv0
          c <- liftValue tpc c'
          v1 <- liftValue tpv v1'
          v2 <- liftValue tpv v2'
          case (tpc, tpv) of
            (IntType w,_) | w == 1 ->
              retTExpr tpv $ Select Nothing c tpv v1 v2
            (VecType n (IntType w), VecType nr tpe) | w == 1 && n == nr ->
              retTExpr tpv $ Select (Just n) c tpe v1 v2
            _  -> fail "Could not parse select intruction."
        L.ExtractValue (L.Typed vtp v) il -> do
            vmtp <- liftMemType vtp
            go vmtp il =<< liftValue vmtp v
          where go :: MemType
                   -> [Int32]
                   -> TypedSymValue
                   -> Maybe SymStmt
                go tp [] sv = retExpr tp (Val sv)
                go (StructType si) (i0 : is) sv =
                    case fiType <$> siFieldInfo si i of
                      Nothing -> fail "Illegal index"
                      Just tp -> go tp is (SValExpr (GetStructField si sv i))
                  where i = fromIntegral i0
                go (ArrayType n tp) (i : is) sv
                    | 0 <= i && i < fromIntegral n = go tp is (SValExpr expr)
                    | otherwise = fail "Illegal index"
                  where expr = GetConstArrayElt (fromIntegral n) tp sv (fromIntegral i)
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
          [ Assign r (undefined tp) val
            | (r, tp, valMap) <-
                case Map.lookup tgt phiMap of
                  Nothing -> error "AST xlate: missing tgt entry in phiMap"
                  Just x  -> x
            , let mval = do mtp <- liftMemType tp
                            v <- Map.lookup llvmId valMap
                            liftValue mtp v
            , let val = case mval of
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
              mtp@(PtrType (FunType (fdRetType -> Just rty))) <- liftMemType tp 
              sv <- liftValue mtp v
              svl <- traverse liftTypedValue tpvl
              return $ PushCallFrame sv svl (Just (rty, reg)) (blockName (idx + 1))
        symStmt <- maybe (unsupportedStmt stmt) return mstmt
        defineBlock (blockName idx) $ reverse (symStmt:il)
        impl r (idx+1) []
      -- Function call that does not return a value (see comment for other call case).
      impl (stmt@(Effect (L.Call _b tp v tpvl) _):r) idx il = do
        let mstmt = do
              mtp <- liftMemType tp
              sv <- liftValue mtp v
              svl <- traverse liftTypedValue tpvl
              return $ PushCallFrame sv svl Nothing (blockName (idx+1))
        symStmt <- maybe (unsupportedStmt stmt) return mstmt
        defineBlock (blockName idx) $ reverse (symStmt:il)
        impl r (idx+1) []
      impl [Effect (L.Jump tgt) _] idx il = do
        defineBlock (blockName idx) $
          reverse il ++ brSymInstrs tgt
      impl [stmt@(Effect (L.Switch (Typed tp v) def cases) _)] idx il = do
         let mcases = do
               mtp@(IntType w) <- liftMemType tp
               tsv <- liftValue mtp v
               let mkCase (cv, bid) rest =
                     [ PushPendingExecution bid (HasConstValue tsv w cv) pd rest ]
               return $ foldr mkCase (brSymInstrs def)
                      $ zip consts caseBlockIds
         case mcases of
            Just symbolicCases -> do
              defineBlock (blockName idx) $ reverse il ++ symbolicCases
              zipWithM_ defineBlock caseBlockIds (brSymInstrs <$> targets)
            _ -> do
              symStmt <- unsupportedStmt stmt
              defineBlock (blockName idx) $ reverse (symStmt:il)
        where -- Get values and targets
              (consts,targets) = unzip cases
              caseBlockIds     = blockName <$> [(idx + 1)..(idx + length cases)]
      impl [stmt@(Effect (L.Br (Typed tp c) tgt1 tgt2) _)] idx il = do
        let mres = do 
              IntType 1 <- liftMemType tp
              liftValue (IntType 1) c
        case mres of
          Just tc -> do
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
liftDefine :: (?lc :: LLVMContext) => L.Define -> Either Doc ([TranslationWarning], SymDefine)
liftDefine d
    | L.defVarArgs d =
       Left (text "Unsupported var args function" <+> symd <> char '.')
    | otherwise =
       case mfd of
         Just (FunDecl rtp args _) -> Right (warnings, sd)
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
                  sd = SymDefine { sdName = L.defName d
                                 , sdArgs = zip (L.typedValue <$> L.defArgs d) args
                                 , sdRetType = rtp
                                 , sdBody = Map.fromList
                                              [ (sbId b,b) | b <- initSymBlock : symBlocks ]
                                 }
         Nothing -> Left (text "Unsupported type for function" <+> symd <> char '.')
  where mfd = liftA3 FunDecl
                     (liftRetType (L.defRetType d))
                     (traverse liftMemType (L.typedType <$> L.defArgs d))
                     (return False) 
        symd = L.ppSymbol (L.defName d)

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