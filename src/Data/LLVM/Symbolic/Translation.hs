{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | This module defines the translation from LLVM IR to Symbolic IR.
--
-- In addition, to the LLVM IR, translation into symbolic IR requires post-dominator information
-- about the LLVM IR.  This information during translation to add
--
-- In addition, it has call and phi non-terminal instructions which may require special support.
-- N.B. call and invoke can be given pointers to functions to support indirect calls.
--
-- [Call Statements]
--    If call statements remain in the IR, then the symbolic simulator will need to remember
--    which instruction was being executing when returning from a method call.  To simplify the
--    simulator, the symbolic representation splits blocks with calls into multiple basic
--    blocks with each basic block except the last terminated with the call.
--
-- [Phi Statements]
--   The value of a Phi statement in LLVM depends on which previous block was executed.  To
--   deal with these statements, we can either explicitly track the previous block, or perform
--   a SSA destruction step to replace the phi instructions with explicitly reads and writes to
--   registers.  Tracking the previous block is quite simple.  However, we may want to replace
--   the SSA values with registers for efficiency purposes anyways.
module Data.LLVM.Symbolic.Translation (LLVMTranslationInfo(..), liftDefine) where

import Control.Monad.State.Strict
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Text.LLVM.AST as LLVM
import Text.LLVM.AST (Stmt(..))
import Text.PrettyPrint.HughesPJ

import Data.LLVM.Symbolic.AST

-- | Information about basic block and control-flow graph used during
-- code generation.
data LLVMTranslationInfo = LTI {
    -- | @ltiPostDominators bb@ returns the post dominators obtained by
    -- jumping to @bb@.  Entries are stored so that post-dominators visited
    -- later are earlier in the list (e.g., the last entry is the immediate
    -- post-dominator).
    ltiPostDominators :: Maybe LLVM.Ident -> [LLVM.Ident]
    -- | @ltiIsImmediatePostDominator bb n@ returns true if @n@ is the immediate
    -- post-dominator of @bb@.
  , ltiIsImmediatePostDominator :: Maybe LLVM.Ident -> LLVM.Ident -> Bool
    -- | @ltiNewPostDominators bb n@ returns the new post dominators obtained by
    -- jumping from @bb@ to @n@.  Entries are stored so that post-dominators
    -- visited later are earlier in the list (e.g., the last entry is the
    -- immediate post-dominator).
  , ltiNewPostDominators :: Maybe LLVM.Ident -> LLVM.Ident -> [LLVM.Ident]
  }

-- | This function is called whenever lifting fails due to an internal error.
liftError :: Doc -> a
liftError d = error (render d)

data BGState = BGS { lsBlocks :: !(Map SymBlockID SymBlock) }

newtype BlockGenerator a = BG (State BGState a)
  deriving (Functor, Monad)

-- | Define block with given identifier.
defineBlock :: SymBlockID -> [SymStmt] -> BlockGenerator ()
defineBlock sbid stmts =
  let b = SymBlock { sbId = sbid, sbStmts = stmts }
   in BG $ modify $ \s -> s { lsBlocks = Map.insert sbid b (lsBlocks s) }

-- | Run block generator.
runBlockGenerator :: LLVM.Symbol
                  -> [Typed Reg]
                  -> LLVM.Type
                  -> BlockGenerator ()
                  -> SymDefine
runBlockGenerator nm args res (BG bg) =
  let initState = BGS { lsBlocks = Map.empty }
      finalState = execState bg initState
   in SymDefine {
          sdName = nm
        , sdArgs = args
        , sdRetType = res
        , sdBody = lsBlocks finalState
        }

type PhiInstr = (LLVM.Ident, LLVM.Type, Map (Maybe LLVM.Ident) LLVM.Value)

-- Generates symbolic procedures for a LLVM basic block.
--
-- Invariants assumed by block:
-- * When jumping from the current block to a new block,
--   * The current block must ensure that the correct post-dominator merge frames are added.
--   * The current block must set the phi value registers.
liftBB :: LLVMTranslationInfo -- ^ Translation information from analysis
       -> (LLVM.Ident -> [PhiInstr]) -- ^ Returns phi instructions in block with given id.
       -> LLVM.BasicBlock -- ^ Basic block to generate.
       -> BlockGenerator ()
liftBB lti phiFn bb = do
  let llvmId = LLVM.bbLabel bb
      blockName :: Int -> SymBlockID
      blockName = symBlockID llvmId
      -- | Returns set block instructions for jumping to a particular target.
      -- This includes setting the current block and executing any phi instructions.
      phiInstrs :: LLVM.Ident -> [SymStmt]
      phiInstrs tgt =
          [ Assign r (Val Typed { typedType = tp, typedValue = valMap Map.! llvmId })
            | (r, tp, valMap) <- phiFn tgt ]
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
      brSymInstrs :: LLVM.Ident -> [SymStmt]
      brSymInstrs tgt =
        SetCurrentBlock (symBlockID (Just tgt) 0) : phiInstrs tgt ++
          (if ltiIsImmediatePostDominator lti llvmId tgt
             then [ MergePostDominator (symBlockID (Just tgt) 0) TrueSymCond
                  , ClearCurrentExecution ]
             else map (\d -> PushPostDominatorFrame (symBlockID (Just d) 0))
                      (ltiNewPostDominators lti llvmId tgt))
      -- | Sequentially process statements.
      impl :: [LLVM.Stmt] -- ^ Remaining statements
           -> Int -- ^ Index of symbolic block that we are defining.
           -> [SymStmt] -- ^ Statements for previous nonterminals in reverse order.
           -> BlockGenerator ()
      impl [] _ _ = liftError $ text "Missing terminal instruction."
      impl [Effect (LLVM.Ret tpv)] idx il =
        defineBlock (blockName idx) (reverse il ++ [MergeReturnAndClear tpv])
      impl [Effect LLVM.RetVoid] idx il =
        defineBlock (blockName idx) (reverse il ++ [MergeReturnVoidAndClear])
      -- For function calls, we:
      -- * Allocate block for next block after call.
      -- * Define previous block to end with pushing call frame.
      -- * Process rest of instructions.
      impl (Result reg (LLVM.Call _b tp v tpvl):r) idx il = do
        let res = Typed { typedType = tp, typedValue = reg }
        defineBlock (blockName idx) $ reverse il ++
          [ SetCurrentBlock (blockName (idx+1))
          , PushCallFrame v tpvl (Just res)]
        impl r (idx+1) []
      -- Function call that does not return a value (see comment for other call case).
      impl (Effect (LLVM.Call _b _tp v tpvl):r) idx il = do
        defineBlock (blockName idx) $ reverse il ++
          [ SetCurrentBlock (blockName (idx+1))
          , PushCallFrame v tpvl Nothing ]
        impl r (idx+1) []
      impl [Effect (LLVM.Jump tgt)] idx il = do
        defineBlock (blockName idx) $
          reverse il ++ brSymInstrs tgt
      impl [Effect (LLVM.Br Typed { typedValue = c } tgt1 tgt2)] idx il = do
        let suspendSymBlockID = blockName (idx + 1)
        -- Define end of current block:
        --   If c is true:
        --     Treat as unconditional branch to tgt1.
        --   Else if c if false:
        --     Treat as unconditional branch to tgt2.
        --   Else
        --     Add pending execution for false branch, and keep executing true branch.
        --   Else
        defineBlock (blockName idx) $ reverse il ++
          [ IfThenElse (HasConstValue c 1)
               (brSymInstrs tgt1)
               [IfThenElse (HasConstValue c 0)
                           (brSymInstrs tgt2)
                           ([ SetCurrentBlock suspendSymBlockID
                            , PushPendingExecution (HasConstValue c 0)
                              -- TODO Add phi instructions for d
                            , AddPathConstraint (HasConstValue c 1)]
                              ++ brSymInstrs tgt1)]]
        -- Define block for suspended thread.
        defineBlock suspendSymBlockID  (brSymInstrs tgt2)
      impl [Effect LLVM.Unreachable] idx il = do
        defineBlock (blockName idx) (reverse (Unreachable : il))
      impl [Effect LLVM.Unwind] idx il = do
        defineBlock (blockName idx) (reverse (Unwind : il))
      -- | Phi statements are handled by initial blocks.
      impl (Result _id (LLVM.Phi _ _):r) idx il = impl r idx il
      impl (Effect (LLVM.Comment _):r) idx il = impl r idx il
      impl (stmt:rest) idx il =
        let s' = case stmt of
                   Result r (LLVM.Arith op tpv1 v2)   -> Assign r (Arith op tpv1 v2)
                   Result r (LLVM.Bit   op tpv1 v2)   -> Assign r (Bit   op tpv1 v2)
                   Result r (LLVM.Conv  op tpv tp)    -> Assign r (Conv  op tpv tp)
                   Result r (LLVM.Alloca tp mtpv mi)  -> Assign r (Alloca tp mtpv mi)
                   Result r (LLVM.Load tpv)           -> Assign r (Load tpv)
                   Effect   (LLVM.Store a v)          -> Store a v
                   Result r (LLVM.ICmp op tpv1 v2)    -> Assign r (ICmp op tpv1 v2)
                   Result r (LLVM.FCmp op tpv1 v2)    -> Assign r (FCmp op tpv1 v2)
                   Result r (LLVM.GEP tp tpvl)        -> Assign r (GEP tp tpvl)
                   Result r (LLVM.Select tpc tpv1 v2) -> Assign r (Select tpc tpv1 v2)
                   Result r (LLVM.ExtractValue tpv i) -> Assign r (ExtractValue tpv i)
                   Result r (LLVM.InsertValue tpv tpa i) -> Assign r (InsertValue tpv tpa i)
                   _ | null rest -> liftError $ text "Unsupported instruction: " <+> LLVM.ppStmt stmt
                   _ -> liftError $
                          text "Terminal instruction found before end of block: "
                            <+> LLVM.ppStmt stmt
         in impl rest idx (s' : il)
   in impl (LLVM.bbStmts bb) 0 []


liftDefine :: LLVM.Define -> SymDefine
liftDefine d = liftDefine' (mkLTI d) d

liftDefine' :: LLVMTranslationInfo
            -> LLVM.Define
            -> SymDefine
liftDefine' lti d =
  runBlockGenerator (LLVM.defName d) (LLVM.defArgs d) (LLVM.defRetType d) $ do
    let blocks@(initBlock:_) = LLVM.defBody d
    let initBlockLabel = LLVM.bbLabel initBlock
    -- Define init block that pushes post dominator frames then jumps to first
    -- block.
    defineBlock initSymBlockID $
       (map (\dom -> PushPostDominatorFrame (symBlockID (Just dom) 0))
            (ltiPostDominators lti initBlockLabel))
         ++ [SetCurrentBlock (symBlockID initBlockLabel 0)]
    let parsePhiStmts :: [Stmt] -> [PhiInstr]
        parsePhiStmts sl =
          [ (r, tp, valMap)
          | LLVM.Result r (LLVM.Phi tp vals) <- sl
          , let valMap = Map.fromList [(Just b, v) | (v,b) <- vals]]
    let blockMap :: Map LLVM.Ident [PhiInstr]
        blockMap = Map.fromList
                    [ (l, parsePhiStmts sl)
                    | LLVM.BasicBlock { LLVM.bbLabel = Just l, LLVM.bbStmts = sl } <- blocks ]
    let phiFn :: LLVM.Ident -> [PhiInstr]
        phiFn i = blockMap Map.! i
    mapM_ (\bb -> liftBB lti phiFn bb) blocks

-- STUB TODO
mkLTI :: LLVM.Define -> LLVMTranslationInfo
mkLTI _def = LTI (\_ -> []) (\_ _ -> False) (\_ _ -> [])
