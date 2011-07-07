{-# LANGUAGE EmptyDataDecls #-}
module Abstract where

import Data.Map (Map)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Text.LLVM.AST as LLVM

-- | Identifier for a function.
type FuncID = LLVM.Symbol

-- | Identifier for a basic block.
type BlockID = LLVM.Ident

-- | Identies a named value in a function.
-- TODO: Figure out if LLVM.Ident is the right type and if this can be
-- changed to an integer (for efficiency purposes).
type Reg = LLVM.Ident


-- LLVM Background {{{1
-- 
-- Instruction support {{{2
--
-- LLVM has the following block terminator functions: 
--
--   * ret
--   * br (conditional and unconditional)
--   * switch 
--   * indirectbr
--   * invoke
--   * unwind
--   * unreachable
--
-- In addition, it has call and phi non-terminal instructions which may require special support.
-- N.B. call and invoke can be given pointers to functions to support indirect calls.
--
-- [Call Statements]
--    If call statements remain in the IR, then the symbolic simulator will need to remember
--    which instruction was being executing when returnning from a method call.  If they are
--    eliminated and replaced with invoke, then extra basic blocks will need to be created,
--    but we can always assume that we will resume with the next branch.
--  
--    N.B. invoke statements have exception handlers, which we may not want to have to introduce.
--    One possibility would be to keep call statements in, but require that they are the last
--    statement in a basic block, and are followed by an unconditional branch.
--
-- [Phi Statements]
--   The value of a Phi statement in LLVM depends on which previous block was executed.  To deal
--   with these statements, we can either explicitly track the previous block, or perform a
--   SSA destruction step to replace the phi instructions with explicitly reads and writes to
--   registers.  Tracking the previous block is quite simple.  However, we may want to replace
--   the SSA values with registers for efficiency purposes anyways.

-- Intrinsic support {{{2
-- 
-- LLVM includes many intrinsic functions with semantics different from simple calls.  Support
-- needs to be added carefully.  Two sources for details are:
--
-- * http://llvm.org/docs/LangRef.html
-- * http://llvm.org/docs/ExceptionHandling.html

-- Symbolic State {{{1
-- The Symbolic state along a single path of execution contains values in the 

-- | Frame in program
data Frame val = Frame {
    frameFn :: FuncID
  , frameBlock :: BlockID 
  , frameRegisters :: Map Reg val
  }

-- | Symbolic state with path constraints.
data SState val =
  SState {
    frames :: [Frame val]
  -- | Contains pointer to exception structure or Nothing if not handling
  -- an exception
  , exception :: Maybe val
  -- | Constaints on paths.
  , pathConstraints :: val
  }

-- Merge frames {{{1
-- 
-- To enable divergent symbolic execution paths to be incrementally remerged during execution,
-- the symbolic simulator will maintain maintain a non-empty stack of "merge frames"
-- that identify potential locations where multiple symbolic execution traces may
-- join into a single frame.
--
-- Each merge frame may contain:
-- * A set of pending execution states that eventually reach this merge frame.
-- * One or more resume states containing executions that must continue once
--   the current frame and all pending frames are finished.
--
-- There are the following types of entries:
--
-- * The bottom-most entry corresponds to program exit.
-- * Return frames correspond to function invocation, and may contain a normal
--   path, and exception path.
-- * Intermediate merge frames correspond to nodes that are the immediate dominator
--   for another node.
-- 
-- Entries are pushed on the stack {{{2
-- * When starting the simulation, the program exit frame is added.
-- * When calling a function, a return frame is pushed on the stack.
-- * When jumping from a block to a block with a new immediate dominator, a merge
--   frame for the new dominator is added.
--
-- The top-entry is updated (and possibly popped when) {{{2
-- * Jumping to an immediate dominator.
-- * Returning from a function (or invoking an exception).
-- * Exiting the program.
--
-- When the update occurs, the current state is merged with the appropriate
-- state at the top frame.  If there is a next pending state in the merge frame,
-- then it is removed and made the current state.  If not, then the top frame is
-- popped and the merge states on it are merged onto the top of the stack.

-- When a block is executed the following steps are performed.
-- * Execute non-terminator instructions in block.
-- * Choose one of following actions:
--   * If block terminator is a ret:
--     * The top of the merge frame stack must be a return frame or exit frame.
--     * Pop call frame from symbolic state.
--     * Merge (state, return value) with normal state on return or exit frame.
--     * Discard current state.
--   * If block terminator is an unconditional branch or a conditional branch
--     where the branch target can be statically determined:
--     * Set current block in state to branch target.
--     * If branch target is post-dominator for current block:
--       * The top of the merge frame stack must be a post-dominator frame.
--       * Merge state with post-dominator merge state.
--       * Discard current state.
--   * If block terminiator is an indeterminate conditional branch:
--     * At least one branch target must not be the post-dominator.
--     * If one of the branch targets is the post-dominator block:
--       * Set block of current state to post-dominator block.
--       * Merge state with post-dominator merge state after adding branch
--         condition for post-dominator target.
--       * Set block of current state to other branch target.
--       * Add branch condition for other branch target.
--     * Otherwise:
--       * Set block of current state to first branch.
--       * Push copy of current state to pending states on control state.
--       * Add branch condition of first branch to copy of current state.
--       * Set block of current state to second branch.
--   * TODO: Handle switch, indirectbr, invoke, unwind, and unreachable.
 
-- | Symbolic state and return value (if any).
type SReturnState sym = (SState sym, Maybe sym)
        
data MergeFrame sym
  -- | Intra-procedural intermediate merge point including label to merge at
  -- and symbolic state if any.
  = PostDominatorFrame {
        -- | Label to merge state for.
        _mergeLabel :: BlockID 
        -- | For intermediate merge points.
      , _mergedState :: Maybe (SState sym)
      }
  -- | Return frame (exception handlers are only added at function invocation points).
  | ReturnFrame {
        -- | Register to store return value (if any).
        _returnReg :: Maybe Reg
        -- | Label for normal path.
      , _normalLabel :: BlockID
        -- | Label for exception path.
      , _exceptLabel :: Maybe BlockID
        -- | Merged potential state and possible return value after function call.
      , _normalState :: Maybe (SReturnState sym)
        -- | Merged exception state after function call.
      , _exceptionState :: Maybe (SState sym)
      }
  -- | Frame at program exit with symbolic value for exit code.
  | ExitFrame (Maybe (SReturnState sym))


-- | A control stack consists of merge frames along with next states to execute beneath that frame.
type ControlStack val = [(MergeFrame val,[SState val])]

-- CProgram {{{1
