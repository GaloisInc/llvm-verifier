{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns               #-}
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
  , testTranslate
  ) where

import           Control.Monad.State.Strict
import           Data.LLVM.Symbolic.AST
import           Data.Map                   (Map)
import           Text.LLVM.AST              (Stmt'(..), Stmt, Typed (..))
import           Text.PrettyPrint.HughesPJ
import qualified Control.Exception          as CE
import qualified Data.LLVM.CFG              as CFG
import qualified Data.List                  as L
import qualified Data.Map                   as Map
import qualified Text.LLVM                  as LLVM

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

-- | @ltiPostDominators lti bb@ returns the post dominators obtained by
-- jumping to @bb@.  Entries are stored so that post-dominators visited
-- later are earlier in the list (e.g., the last entry is the immediate
-- post-dominator).
ltiPostDominators :: LLVMTranslationInfo -> LLVM.BlockLabel -> [LLVM.BlockLabel]
ltiPostDominators (LTI cfg) (CFG.asId cfg -> aid) =
  case lookup aid (CFG.pdoms cfg) of
    Nothing   -> []
    Just apds -> map (CFG.asName cfg) apds

-- | @ltiIsImmediatePostDominator lti bb n@ returns true if @n@ is the immediate
-- post-dominator of @bb@.
ltiIsImmediatePostDominator :: LLVMTranslationInfo -> LLVM.BlockLabel -> LLVM.BlockLabel -> Bool
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
type BlockGenerator a = State [SymBlock] a

mkSymBlock :: SymBlockID -> [SymStmt] -> SymBlock
mkSymBlock sbid stmts = SymBlock { sbId = sbid, sbStmts = stmts }

-- | Define block with given identifier.
defineBlock :: SymBlockID -> [SymStmt] -> BlockGenerator ()
defineBlock sbid stmts = modify (mkSymBlock sbid stmts:)

-- Phi instruction parsing {{{1

type PhiInstr = (LLVM.Ident, LLVM.Type, Map LLVM.BlockLabel LLVM.Value)

-- Define init block that pushes post dominator frames then jumps to first
-- block.
parsePhiStmts :: [Stmt] -> [PhiInstr]
parsePhiStmts sl =
  [ (r, tp, valMap)
  | LLVM.Result r (LLVM.Phi tp vals) <- sl
  , let valMap = Map.fromList [(b, v) | (v,b) <- vals]]

-- | Maps LLVM Blocks to associated phi instructions.
blockPhiMap :: [CFG.BB] -> Map LLVM.BlockLabel [PhiInstr]
blockPhiMap blocks =
  Map.fromList
    [ (l, parsePhiStmts sl)
    | LLVM.BasicBlock { LLVM.bbLabel = (_bbid, l), LLVM.bbStmts = sl } <- blocks ]


-- Lift LLVM basic block to symbolic block {{{1
--
-- Invariants assumed by block:
-- * When jumping from the current block to a new block,
--   * The current block must ensure that the correct post-dominator merge frames are added.
--   * The current block must set the phi value registers.
liftBB :: LLVMTranslationInfo -- ^ Translation information from analysis
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
            | (r, tp, valMap) <- phiMap Map.! tgt
            , let val = valMap Map.! llvmId ]
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
             then [ MergePostDominator (symBlockID tgt 0) TrueSymCond
                  , ClearCurrentExecution ]
             else map (\d -> PushPostDominatorFrame (symBlockID d 0))
                      (ltiNewPostDominators lti llvmId tgt))
      -- | Sequentially process statements.
      impl :: [LLVM.Stmt] -- ^ Remaining statements
           -> Int -- ^ Index of symbolic block that we are defining.
           -> [SymStmt] -- ^ Previously generated statements in reverse order.
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
      -- * TODO: Handle invoke instructions
      impl (Result reg (LLVM.Call _b tp v tpvl):r) idx il = do
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
          [ SetCurrentBlock (blockName (idx+1))
          , PushCallFrame v tpvl (Just res)
          ]
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
      impl [Effect (LLVM.Br (Typed tc c) tgt1 tgt2)] idx il = do
        CE.assert (tc == LLVM.iT 1) $ return ()
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
                   Result r (LLVM.Arith op tpv1 v2)    -> Assign r (Arith op tpv1 v2)
                   Result r (LLVM.Bit   op tpv1 v2)    -> Assign r (Bit   op tpv1 v2)
                   Result r (LLVM.Conv  op tpv tp)     -> Assign r (Conv  op tpv tp)
                   Result r (LLVM.Alloca tp mtpv mi)   -> Assign r (Alloca tp mtpv mi)
                   Result r (LLVM.Load tpv malign)     -> Assign r (Load tpv malign)
                   Effect   (LLVM.Store v addr malign) -> Store v addr malign
                   Result r (LLVM.ICmp op tpv1 v2)     -> Assign r (ICmp op tpv1 v2)
                   Result r (LLVM.FCmp op tpv1 v2)     -> Assign r (FCmp op tpv1 v2)
                   Result r (LLVM.GEP tp tpvl)         -> Assign r (GEP tp tpvl)
                   Result r (LLVM.Select tpc tpv1 v2)  -> Assign r (Select tpc tpv1 v2)
                   Result r (LLVM.ExtractValue tpv i)  -> Assign r (ExtractValue tpv i)
                   Result r (LLVM.InsertValue tpv tpa i) -> Assign r (InsertValue tpv tpa i)
                   _ | null rest -> liftError $ text "Unsupported instruction: " <+> LLVM.ppStmt stmt
                   _ -> liftError $
                          text "Terminal instruction found before end of block: "
                            <+> LLVM.ppStmt stmt
         in impl rest idx (s' : il)
   in impl (LLVM.bbStmts bb) 0 []

-- Lift LLVM definition to symbolic definition {{{1
liftDefine :: LLVM.Define -> SymDefine
liftDefine d =
  let cfg            = CFG.buildCFG (LLVM.defBody d)
      lti            = mkLTI cfg
      blocks         = CFG.allBBs cfg
      initBlock      = CFG.bbById cfg (CFG.entryId cfg)
      initBlockLabel = CFG.blockName initBlock
      initSymBlock =
        mkSymBlock initSymBlockID
                   ([ PushPostDominatorFrame (symBlockID dom 0)
                        | dom <- ltiPostDominators lti initBlockLabel]
                    ++ [SetCurrentBlock (symBlockID initBlockLabel 0)])
      phiMap = blockPhiMap blocks
      symBlocks = initSymBlock : execState (mapM_ (liftBB lti phiMap) blocks) []
   in SymDefine {
          sdName = LLVM.defName d
        , sdArgs = LLVM.defArgs d
        , sdVarArgs = LLVM.defVarArgs d
        , sdRetType = LLVM.defRetType d
        , sdBody = Map.fromList [ (sbId b,b) | b <- symBlocks ]
        }

-- Test code {{{1
-- | Translate the given module
testTranslate :: LLVM.Module -> IO ()
testTranslate mdl = do
  putStrLn $ render $ LLVM.ppModule mdl
  putStrLn $ replicate 80 '-'
  forM_ (LLVM.modDefines mdl) $ \def -> do
    putStrLn $ render $ ppSymDefine $ liftDefine def
    putStrLn ""
