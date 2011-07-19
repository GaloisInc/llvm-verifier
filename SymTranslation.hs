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
--    simulator, we will normalize IR by introducing extra basic blocks, so that call statements
--    are always immediately followed by an unconditional branch.
--  
-- [Phi Statements]
--   The value of a Phi statement in LLVM depends on which previous block was executed.  To deal
--   with these statements, we can either explicitly track the previous block, or perform a
--   SSA destruction step to replace the phi instructions with explicitly reads and writes to
--   registers.  Tracking the previous block is quite simple.  However, we may want to replace
--   the SSA values with registers for efficiency purposes anyways.
module SymTranslation where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Text.LLVM.AST as LLVM
import Text.LLVM.AST (Stmt(..))
import Text.PrettyPrint.HughesPJ

import SymAST

-- | Information about basic block and control-flow graph used during
-- code generation.
data LLVMTranslationInfo = LTI {
    -- Returns post-dominator for given basic block, or none if block does
    -- does not have a post-dominator within this procedure (e.g., terminated with
    -- a ret or unwind statement).
    ltiPostDominator :: Maybe LLVM.Ident -> [LLVM.Ident]
  }

-- | This function is called whenever lifting fails due to an internal error.
liftError :: Doc -> a
liftError d = error (render d)

data LiftState = LS 
  { lsNextBlock :: SymBlockID
  , lsBlocks :: Map SymBlockID SymBlock
  }

newtype BlockGenerator a = BG (State LiftState a)
  deriving (Functor, Monad)

-- | Returns new block identifier.
newBlockID :: String -> BlockGenerator SymBlockID
newBlockID _name = undefined

-- | Define block with given identifier.
defineBlock :: SymBlockID -> [SymStmt] -> BlockGenerator ()
defineBlock _id _instrs = undefined

-- | Run block generator.
runBlockGenerator :: LLVM.Symbol -> [Typed Reg] -> LLVM.Type -> BlockGenerator () -> SymDefine
runBlockGenerator nm args res (BG bg) = 
  let initState = LS { lsNextBlock = 0, lsBlocks = Map.empty }
      finalState = execState bg initState
   in SymDefine {
          sdName = nm
        , sdArgs = args
        , sdReturn = res
        , sdBody = V.generate (fromInteger (lsNextBlock finalState)) $
                     (\i -> lsBlocks finalState Map.! (toInteger i))
        }

-- | Returns symbolic block for LLVM block with given identifier.
type SymBlockFn = Maybe LLVM.Ident -> SymBlockID

type BBId = Maybe LLVM.Ident

type PhiInstr = (LLVM.Ident, LLVM.Type, Maybe LLVM.Ident -> LLVM.Value)

-- Generates symbolic procedures for a LLVM basic block.
--
-- Invariants assumed by block:
-- * When jumping from the current block to a new block, 
--   * The current block must ensure that the correct post-dominator merge frames are added.
--   * The current block must set the phi value registers.
liftBB :: LLVMTranslationInfo -- ^ Translation information from analysis
       -> SymBlockFn -- ^ Maps LLVM block identifiers to symbolic block identifier.
       -> (LLVM.Ident -> [PhiInstr]) -- ^ Returns phi instructions in block with given id.
       -> LLVM.BasicBlock -- ^ Basic block to generate.
       -> BlockGenerator ()
liftBB lti blockFn phiFn bb = do
  let llvmId = LLVM.bbLabel bb
  let blockName = maybe "%_UNKNOWN" (\(LLVM.Ident n) -> '%' : n) llvmId
  let -- | @isImmediatePostDominator bb n@ returns true if @n@ is an immediate
      -- post-dominator of @bb@.
      isImmediatePostDominator :: Maybe LLVM.Ident -> LLVM.Ident -> Bool
      isImmediatePostDominator fromId toId = undefined
      -- | @newPostDominators bb n@ returns the new post dominators obtained by
      -- jumping from @bb@ to @n@.  The first entry is the last new post-dominator
      -- post-dominator and the last is the immediate post-dominator.
      newPostDominators :: Maybe LLVM.Ident -> LLVM.Ident -> [LLVM.Ident]
      newPostDominators = undefined
      -- | Returns set block instructions for jumping to a particular target.
      -- This includes setting the current block and executing any phi instructions.
      phiInstrs :: LLVM.Ident -> [SymStmt]
      phiInstrs tgt = 
          [ Assign r (Val Typed { typedType = tp, typedValue = valFn llvmId })
            | (r, tp, valFn) <- phiFn tgt ]
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
        SetCurrentBlock (blockFn (Just tgt)) : phiInstrs tgt ++
          (if isImmediatePostDominator llvmId tgt
             then [ MergePostDominator (blockFn (Just tgt)) TrueSymCond
                  , ClearCurrentExecution ]
             else map (\d -> PushPostDominatorFrame (blockFn (Just d)))  
                      (newPostDominators llvmId tgt))
      -- | Sequentially process statements.
      impl :: [LLVM.Stmt] -- ^ Remaining statements
           -> (SymBlockID,Int) -- ^ Id and index of symbolic block that we are defining.
           -> [SymStmt] -- ^ Statements for previous nonterminals in reverse order.
           -> BlockGenerator ()
      impl [] _ _ = liftError $ text "Missing terminal instruction."
      impl [Effect (LLVM.Ret tpv)] (id,_) il = 
        defineBlock id (reverse il ++ [MergeReturnAndClear tpv])
      impl [Effect LLVM.RetVoid] (id,_) il =
        defineBlock id (reverse il ++ [MergeReturnVoidAndClear])
      -- For function calls, we:
      -- * Allocate block for next block after call.
      -- * Define previous block to end with pushing call frame.
      -- * Process rest of instructions.
      impl (Result reg (LLVM.Call _b tp v tpvl):r) (id,idx) il = do
        nid <- newBlockID (blockName ++ '.' : show idx)
        defineBlock id $ reverse il ++ 
          [ SetCurrentBlock nid
          , PushCallFrame (Just reg) tp v tpvl ]
        impl r (nid,idx+1) []
      -- Function call that does not return a value (see comment for other call case).
      impl (Effect (LLVM.Call _b tp v tpvl):r) (id,idx) il = do
        nid <- newBlockID (blockName ++ '.' : show idx)
        defineBlock id $ reverse il ++ 
          [ SetCurrentBlock nid
          , PushCallFrame Nothing tp v tpvl ]
        impl r (nid,idx+1) []
      impl [Effect (LLVM.Jump tgt)] (id,_) il = do
        defineBlock id $ reverse il ++ brSymInstrs tgt
      impl [Effect (LLVM.Br Typed { typedValue = c } tgt1 tgt2)] (id,idx) il = do
        suspendSymBlockID <- newBlockID (blockName ++ '.' : show idx)
        -- Define end of current block:
        --   If c is true:
        --     Treat as unconditional branch to tgt1.   
        --   Else if c if false:
        --     Treat as unconditional branch to tgt2.
        --   Else 
        --     Add pending execution for false branch, and keep executing true branch.
        --   Else
        defineBlock id $ reverse il ++
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
        defineBlock suspendSymBlockID (brSymInstrs tgt2)
      impl [Effect LLVM.Unreachable] (id,_) il = do
        defineBlock id (reverse (Unreachable : il))
      impl [Effect LLVM.Unwind] (id,_) il = do
        defineBlock id (reverse (Unwind : il))
      -- | Phi statements are handled by initial blocks.
      impl (Result _id (LLVM.Phi _ _):r) id il = impl r id il
      impl (Effect (LLVM.Comment _):r) id il = impl r id il
      impl (stmt:r) id il =
        let s' = case stmt of
                   Result id (LLVM.Arith op tpv1 v2)   -> Assign id (Arith op tpv1 v2)
                   Result id (LLVM.Bit   op tpv1 v2)   -> Assign id (Bit   op tpv1 v2)
                   Result id (LLVM.Conv  op tpv tp)    -> Assign id (Conv  op tpv tp)
                   Result id (LLVM.Alloca tp mtpv mi)  -> Assign id (Alloca tp mtpv mi)
                   Result id (LLVM.Load tpv)           -> Assign id (Load tpv)
                   Effect    (LLVM.Store a v)          -> Store a v
                   Result id (LLVM.ICmp op tpv1 v2)    -> Assign id (ICmp op tpv1 v2)
                   Result id (LLVM.FCmp op tpv1 v2)    -> Assign id (FCmp op tpv1 v2)
                   Result id (LLVM.GEP tp tpvl)        -> Assign id (GEP tp tpvl)
                   Result id (LLVM.Select tpc tpv1 v2) -> Assign id (Select tpc tpv1 v2)
                   _ | null r -> liftError $ text "Unsupported instruction: " <+> LLVM.ppStmt stmt
                   _ -> liftError $ 
                          text "Terminal instruction found before end of block: "
                            <+> LLVM.ppStmt stmt
         in impl r id (s' : il)
  impl (LLVM.bbStmts bb) (blockFn llvmId, 1) []

liftDefine :: LLVMTranslationInfo
           -> LLVM.Define
           -> SymDefine
liftDefine _lti d =
  runBlockGenerator (LLVM.defName d) (LLVM.defArgs d) (LLVM.defRetType d) $ do
    initID <- newBlockID "init"
    undefined
