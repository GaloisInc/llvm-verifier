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
module Data.LLVM.Symbolic.Translation
  ( liftDefine
  , liftTypedValue
  , liftValue
  , MaybeVectorType(..)
  , asMaybePtrVectorType
  , asMaybeIntVectorType
  ) where

import Control.Applicative ((<$>))
import           Control.Monad.State.Strict
import Data.LLVM.TargetData
import           Data.LLVM.Symbolic.AST
import           Data.Map                   (Map)
import           LSS.LLVMUtils
import           Text.LLVM.AST              (Stmt'(..), Stmt, Typed (..))
import           Text.PrettyPrint.HughesPJ
import qualified Control.Exception          as CE
import qualified Data.LLVM.CFG              as CFG
import qualified Data.List                  as L
import qualified Data.Map                   as Map
import qualified Data.Vector                as V
import qualified Text.LLVM                  as LLVM
import qualified Text.LLVM                  as L

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

blockIsDummyExit :: LLVM.BlockLabel -> Bool
blockIsDummyExit (LLVM.Named (LLVM.Ident nm)) = nm == CFG.dummyExitName
blockIsDummyExit _ = False

-- | Returns basic blocks excluding dummy exit block.
ltiBlocks :: LLVMTranslationInfo -> [CFG.BB]
ltiBlocks (LTI cfg) = [ bb
                      | bb <- CFG.allBBs cfg
                      , not (blockIsDummyExit (snd (LLVM.bbLabel bb))) 
                      ]
           
-- | @ltiPostDominators lti bb@ returns the post dominators obtained by
-- jumping to @bb@.  Entries are stored so that post-dominators visited
-- later are earlier in the list (e.g., the last entry is the immediate
-- post-dominator).
ltiPostDominators :: LLVMTranslationInfo -> LLVM.BlockLabel -> [LLVM.BlockLabel]
ltiPostDominators (LTI cfg) (CFG.asId cfg -> aid) =
  case lookup aid (CFG.pdoms cfg) of
    Nothing   -> []
    Just apds -> [ CFG.asName cfg apd 
                 | apd <- apds 
                 , not (blockIsDummyExit (CFG.asName cfg apd)) ]

-- | @ltiIsImmediatePostDominator lti bb n@ returns true if @n@ is the immediate
-- post-dominator of @bb@.
ltiIsImmediatePostDominator :: LLVMTranslationInfo
                            -> LLVM.BlockLabel
                            -> LLVM.BlockLabel
                            -> Bool
ltiIsImmediatePostDominator (LTI cfg) (CFG.asId cfg -> aid) (CFG.asId cfg -> bid) =
  case CFG.ipdom cfg aid of
    Nothing    -> False
    Just apdId -> apdId == bid

-- | @ltiNewPostDominators lti bb n@ returns the new post dominators obtained by
-- jumping from @bb@ to @n@.  Entries are stored so that post-dominators
-- visited later are earlier in the list (e.g., the last entry is the
-- immediate post-dominator).
ltiNewPostDominators :: LLVMTranslationInfo -> LLVM.BlockLabel -> LLVM.BlockLabel -> [LLVM.BlockLabel]
ltiNewPostDominators lti a b = ltiPostDominators lti b L.\\ ltiPostDominators lti a

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

type PhiInstr = (LLVM.Ident, LLVM.Type, Map LLVM.BlockLabel LLVM.Value)

-- Define init block that pushes post dominator frames then jumps to first
-- block.
parsePhiStmts :: [Stmt] -> [PhiInstr]
parsePhiStmts sl =
  [ (r, tp, valMap)
  | LLVM.Result r (LLVM.Phi tp vals) _ <- sl
  , let valMap = Map.fromList [(b, v) | (v,b) <- vals]]

-- | Maps LLVM Blocks to associated phi instructions.
blockPhiMap :: [CFG.BB] -> Map LLVM.BlockLabel [PhiInstr]
blockPhiMap blocks =
  Map.fromList
    [ (l, parsePhiStmts sl)
    | LLVM.BasicBlock { LLVM.bbLabel = (_bbid, l), LLVM.bbStmts = sl } <- blocks ]

data MaybeVectorType v = ScalarType v
                       | VectorType Int v
                       | NotMaybeVector
  deriving (Functor)

lookupAlias :: (?lc :: LLVMContext) => L.Ident -> Maybe L.Type
lookupAlias i = Map.lookup i (llvmTypeAliasMap ?lc)

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

asMaybePtrVectorType :: (?lc :: LLVMContext) => L.Type -> MaybeVectorType L.Type
asMaybePtrVectorType = go
  where lkup f a = maybe NotMaybeVector f (lookupAlias a)
        go (L.Alias a)  = lkup go a
        go (L.PtrTo tp) = ScalarType tp
        go (L.Vector n (L.Alias a))  = lkup (go . L.Vector n) a
        go (L.Vector n (L.PtrTo tp)) = VectorType (fromIntegral n) tp
        go _ = NotMaybeVector

asMaybeIntVectorType :: (?lc :: LLVMContext) => L.Type -> MaybeVectorType BitWidth
asMaybeIntVectorType = go
  where lkup f a = maybe NotMaybeVector f (lookupAlias a)
        go (L.Alias a) = lkup go a
        go (L.PrimType (L.Integer w)) = ScalarType (fromIntegral w)
        go (L.Vector n (L.Alias a)) = lkup (go . L.Vector n) a
        go (L.Vector n (L.PrimType (L.Integer w))) =
          VectorType (fromIntegral n) (fromIntegral w)
        go _ = NotMaybeVector

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
liftValue  (L.PrimType (L.Integer w)) (L.ValInteger x) =
  return $ SValInteger (fromIntegral w) x
liftValue (L.PrimType (L.Integer 1)) (L.ValBool x) =
  return $ SValInteger 1 (if x then 1 else 0)
liftValue (L.PtrTo etp) L.ValNull =
  return $ SValNull etp
liftValue tp (L.ValConstExpr ce) = do
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
          return $ SValExpr (PtrToInt ptrType v w)
        L.IntToPtr -> do
          w <- asIntType itp
          ptrType <- asPtrType rtp
          return $ SValExpr (IntToPtr w v ptrType)
        L.BitCast ->
          return $ SValExpr (Bitcast itp v rtp)
        _ -> fail "Could not interpret constant expression"
    _ -> fail "Could not interpret constant expression"
liftValue (L.Array len (L.Alias a)) v = do
  tp <- lookupAlias a
  liftValue (L.Array len tp) v
liftValue (L.Array len etp) (L.ValArray vtp el) = do
  rtp <- typesEq etp vtp
  unless (fromIntegral len == length el) $ fail "Incompatible array types"
  vl <- mapM (liftValue etp) el
  return (SValArray rtp vl)
liftValue (L.Array len (L.PrimType (L.Integer 8))) (L.ValString str) = do
  unless (fromIntegral len == length str) $ fail "Incompatible types"
  return (sValString str)
liftValue tp (L.ValStruct fldvs) = do
  _ <- typesEq tp (L.Struct (typedType <$> fldvs))
  SValStruct <$> mapM returnTypedValue fldvs
liftValue (L.PtrTo (L.Alias a)) v = do
  tp <- lookupAlias a
  liftValue (L.PtrTo tp) v
-- TODO: Figure out how to check symbol types.
liftValue (L.PtrTo (L.FunTy _ argTps _)) (L.ValSymbol sym) =
  return $ SValSymbol sym (Just argTps)
liftValue _ (L.ValSymbol sym) =
  return $ SValSymbol sym Nothing
liftValue (L.PrimType (L.FloatType L.Double)) (L.ValDouble d) =
  return $ SValDouble d
-- TODO: Figure out how to check ident types.
liftValue _ (L.ValIdent i) =
   return $ SValIdent i
liftValue tp L.ValUndef =
  return (SValUndef tp)
liftValue tp L.ValZeroInit =
  return (SValZeroInit tp)
liftValue _ _ = Nothing

unsupportedStmt :: L.Stmt -> BlockGenerator SymStmt
unsupportedStmt stmt = do
  addWarning $ text "Unsupported instruction: " <+> LLVM.ppStmt stmt
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
    Effect (L.Store v addr malign) _ ->
      return (Store v addr malign)
    Effect{} -> unsupportedStmt stmt
    Result r app _ -> trySymStmt stmt $ do
      let retExpr :: Monad m => SymExpr -> m SymStmt
          retExpr e = return (Assign r e)
      let retTExpr :: Monad m => TypedExpr TypedSymValue -> m SymStmt
          retTExpr v = retExpr (TypedExpr (typedExprType v) v)
      case app of
        L.Arith op tpv1 v2   -> retExpr $ Arith op tpv1 v2
        L.Bit   op tpv1 v2   -> retExpr $ Bit   op tpv1 v2
        L.Conv op (L.Typed itp e) rtp -> do
          sv <- liftValue itp e
          let intConv :: (BitWidth -> BitWidth -> Bool)
                      -> (BitWidth -> TypedSymValue -> BitWidth -> TypedExpr TypedSymValue)
                      -> (Int -> BitWidth -> TypedSymValue -> BitWidth
                              -> TypedExpr TypedSymValue) 
                      -> Maybe SymStmt
              intConv cond sfn vfn =
                case (asMaybeIntVectorType itp, asMaybeIntVectorType rtp) of
                  (ScalarType iw, ScalarType rw) | cond iw rw ->
                    retTExpr (sfn iw sv rw)
                  (VectorType n iw, VectorType nr rw) | n == nr && cond iw rw ->
                    retTExpr (vfn n iw sv rw)
                  _ -> fail "Could not parse conversion types"
          case op of
            L.Trunc -> intConv (\iw rw -> iw > rw) Trunc TruncV
            L.ZExt -> intConv (\iw rw -> iw < rw) ZExt ZExtV
            L.SExt -> intConv (\iw rw -> iw < rw) SExt SExtV
            L.PtrToInt ->
              case (asMaybePtrVectorType itp, asMaybeIntVectorType rtp) of
                (ScalarType ptr, ScalarType w) ->
                  retTExpr (PtrToInt ptr sv w)
                (VectorType n ptr, VectorType nr w) | n == nr ->
                  retTExpr (PtrToIntV n ptr sv w)
                _ -> fail "Could not parse conversion types"
            L.IntToPtr ->
              case (asMaybeIntVectorType itp, asMaybePtrVectorType rtp) of
                (ScalarType w, ScalarType ptr) ->
                  retTExpr (IntToPtr w sv ptr)
                (VectorType n w, VectorType nr ptr) | n == nr ->
                  retTExpr (IntToPtrV n w sv ptr)
                _ -> fail "Could not parse conversion types"
            L.BitCast -> retTExpr (Bitcast itp sv rtp)
            _ -> fail "Unsupported conversion operator"
        L.Alloca tp mtpv mi  -> retExpr $ Alloca tp mtpv mi
        L.Load tpv malign    -> retExpr $ Load tpv malign
        L.ICmp op tpv1 v2    -> retExpr $ ICmp op tpv1 v2
        L.FCmp op tpv1 v2    -> retExpr $ FCmp op tpv1 v2
        L.GEP ib tp tpvl     -> do
          retTExpr =<< liftGEP ib tp tpvl
        L.Select tpc tpv1 v2 -> retExpr $ Select tpc tpv1 v2
        L.ExtractValue tpv i -> retExpr $ ExtractValue tpv i
        L.InsertValue tpv tpa i -> retExpr $ InsertValue tpv tpa i
        _ -> fail "Unsupported instruction"

-- Lift LLVM basic block to symbolic block {{{1
--
-- Invariants assumed by block:
-- * When jumping from the current block to a new block,
--   * The current block must ensure that the correct post-dominator merge frames are added.
--   * The current block must set the phi value registers.
liftBB :: (?lc :: LLVMContext)
       => LLVMTranslationInfo -- ^ Translation information from analysis
       -> Map LLVM.BlockLabel [PhiInstr] -- ^ Maps block identifiers to phi instructions for block.
       -> CFG.BB -- ^ Basic block to generate.
       -> BlockGenerator ()
liftBB lti phiMap bb = do
  let llvmId = CFG.blockName bb
      blockName :: Int -> SymBlockID
      blockName = symBlockID llvmId
      -- | Returns set block instructions for jumping to a particular target.
      -- This includes setting the current block and executing any phi
      -- instructions.
      phiInstrs :: LLVM.BlockLabel -> [SymStmt]
      phiInstrs tgt =
          [ Assign r (Val Typed { typedType = tp, typedValue = val })
            | (r, tp, valMap) <- case Map.lookup tgt phiMap of
                Nothing -> error "AST xlate: missing tgt entry in phiMap"
                Just x  -> x
            , let val = case Map.lookup llvmId valMap of
                          Nothing -> error $
                            "AST xlate: expected to find "
                            ++ show (LLVM.ppLabel llvmId)
                            ++ " as a phi operand of block"
                            ++ " labeled " ++ show (LLVM.ppLabel tgt)
                          Just x -> x
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
      brSymInstrs :: LLVM.BlockLabel -> [SymStmt]
      brSymInstrs tgt =
        SetCurrentBlock (symBlockID tgt 0) : phiInstrs tgt ++
          (if ltiIsImmediatePostDominator lti llvmId tgt
             then [ MergePostDominator (symBlockID tgt 0) ]
             else map (\d -> PushPostDominatorFrame (symBlockID d 0))
                      (ltiNewPostDominators lti llvmId tgt))
      -- | Sequentially process statements.
      impl :: [LLVM.Stmt] -- ^ Remaining statements
           -> Int -- ^ Index of symbolic block that we are defining.
           -> [SymStmt] -- ^ Previously generated statements in reverse order.
           -> BlockGenerator ()
      impl [] idx il = liftError $
                       text "Missing terminal instruction in block" <+>
                       int idx <+>
                       text "after generating the following statements:" $$
                       (nest 2 . vcat . map ppSymStmt $ il)
      impl [Effect (LLVM.Ret tpv) _] idx il =
        defineBlock (blockName idx) (reverse il ++ [MergeReturn (Just tpv)])
      impl [Effect LLVM.RetVoid _] idx il =
        defineBlock (blockName idx) (reverse il ++ [MergeReturn Nothing])      
      -- For function calls, we:
      -- * Allocate block for next block after call.
      -- * Define previous block to end with pushing call frame.
      -- * Process rest of instructions.
      -- * TODO: Handle invoke instructions
      impl (Result reg (LLVM.Call _b tp v tpvl) _:r) idx il = do
        let res = case LLVM.elimFunPtr tp of
                    -- NB: The LLVM bitcode parser always types call
                    -- instructions with the full ptr-to-fun type in order to
                    -- present a consistent form that also handles varargs.  We
                    -- just extract the return type here for now, but will
                    -- likely need to support varargs more explicitly
                    -- later. TODO.
                    Nothing -> error "internal: malformed call instruction type"
                    Just (rty, _, _) -> Typed { typedType = rty, typedValue = reg}
        defineBlock (blockName idx) $ reverse il ++
          [ PushCallFrame v tpvl (Just res) (blockName (idx + 1)) ]
        impl r (idx+1) []
      -- Function call that does not return a value (see comment for other call case).
      impl (Effect (LLVM.Call _b _tp v tpvl) _:r) idx il = do
        defineBlock (blockName idx) $ reverse il ++
          [ PushCallFrame v tpvl Nothing (blockName (idx+1)) ]
        impl r (idx+1) []
      impl [Effect (LLVM.Jump tgt) _] idx il = do
        defineBlock (blockName idx) $
          reverse il ++ brSymInstrs tgt
      impl [Effect (LLVM.Switch tpv def cases) _] idx il = do
        defineBlock (blockName idx) $ reverse il ++ go cases
        mapM_ (uncurry defineBlock) caseDefs
          where go []            = [IfThenElse (NotConstValues tpv vs)
                                               (brSymInstrs def)
                                               symbolicCases]
                go ((i, l) : cs) = [IfThenElse (HasConstValue tpv i)
                                               (brSymInstrs l)
                                               (go cs)]
                symbolicCases    = concatMap mkCase caseConds
                                   ++ brSymInstrs def
                vs               = map fst cases
                caseBlockIds     = [(idx + 1)..(idx + length cases)]
                casePairs        = zip caseBlockIds cases
                caseDefs         = map
                                   (\(bid, (_, l)) ->
                                      (blockName bid, brSymInstrs l))
                                   casePairs
                caseConds        = map
                                   (\(bid, (cv, _)) -> (blockName bid, cv))
                                   casePairs
                mkCase (bid, cv) = [ SetCurrentBlock bid
                                   , PushPendingExecution (HasConstValue tpv cv)
                                   ]
      impl [Effect (LLVM.Br tc@(Typed tp _) tgt1 tgt2) _] idx il = do
        CE.assert (tp == i1) $ return ()
        let suspendSymBlockID = blockName (idx + 1)
        -- Define end of current block:
        --   If c is true:
        --     Treat as unconditional branch to tgt1.
        --   Else if c if false:
        --     Treat as unconditional branch to tgt2.
        --   Else
        --     Add pending execution for false branch, and execute true branch.
        --   Else
        defineBlock (blockName idx) $ reverse il ++
          [ IfThenElse (HasConstValue tc 1)
               (brSymInstrs tgt1)
               [IfThenElse (HasConstValue tc 0)
                           (brSymInstrs tgt2)
                           ([ SetCurrentBlock suspendSymBlockID
                            , PushPendingExecution (HasConstValue tc 0)
                            ]
                              ++ brSymInstrs tgt1)]]
        -- Define block for suspended thread.
        defineBlock suspendSymBlockID  (brSymInstrs tgt2)
      impl [Effect LLVM.Unreachable _] idx il = do
        defineBlock (blockName idx) (reverse (Unreachable : il))
      impl [Effect LLVM.Unwind _] idx il = do
        defineBlock (blockName idx) (reverse (Unwind : il))
      -- | Phi statements are handled by initial blocks.
      impl (Result _id (LLVM.Phi _ _) _:r) idx il = impl r idx il
      impl (Effect (LLVM.Comment _) _:r) idx il = impl r idx il
      impl (stmt:rest) idx il = do
        s' <- liftStmt stmt
        impl rest idx (s' : il)
   in impl (LLVM.bbStmts bb) 0 []

-- Lift LLVM definition to symbolic definition {{{1
liftDefine :: LLVMContext -> LLVM.Define -> ([TranslationWarning], SymDefine)
liftDefine lc d = (warnings,sd)
  where cfg            = CFG.buildCFG (LLVM.defBody d)
        lti            = mkLTI cfg
        blocks         = ltiBlocks lti
        initBlock      = CFG.bbById cfg (CFG.entryId cfg)
        initBlockLabel = CFG.blockName initBlock
        initSymBlock =
          mkSymBlock initSymBlockID
                     ([ PushPostDominatorFrame (symBlockID dom 0)
                        | dom <- ltiPostDominators lti initBlockLabel]
                      ++ [SetCurrentBlock (symBlockID initBlockLabel 0)])
        phiMap = blockPhiMap blocks
        (warnings,symBlocks) = runBlockGenerator (mapM_ (liftBB lti phiMap) blocks)
          where ?lc = lc
        sd = SymDefine { sdName = LLVM.defName d
                       , sdArgs = LLVM.defArgs d
                       , sdVarArgs = LLVM.defVarArgs d
                       , sdRetType = LLVM.defRetType d
                       , sdBody = Map.fromList
                                    [ (sbId b,b) | b <- initSymBlock : symBlocks ]
                       }

-- Test code {{{1
{-
-- | Translate the given module
testTranslate :: LLVM.Module -> IO ()
testTranslate mdl = do
  putStrLn $ render $ LLVM.ppModule mdl
  putStrLn $ replicate 80 '-'
  let lc = undefined
  forM_ (LLVM.modDefines mdl) $ \def -> do
    let (_,sd) = liftDefine lc def
    putStrLn $ render $ ppSymDefine sd
    putStrLn ""
-}