-- | This module defines the main data types for the AST used directly by the symbolic
-- simulator.  This AST data type is the interface between the symbolic execution and
-- the LLVM lifting operating.
--
-- The Symbolic IR is similar to the LLVM IR, but includes several differences:
--
-- * The Symbolic IR includes explicit instructions for pushing and popping frames from
--   the merge frame stack.  
-- * It allows IfThenElse instructions to appear directly within blocks.
-- * 
module SymAST 
  ( FuncID
  , SymBlockID 
  , initSymBlockID
  , symBlockID
  , ppSymBlockID
  , Reg
  , LLVM.Typed(..)
  , SymValue(..)
  , SymExpr(..)
  , SymCond(..)
  , SymStmt(..)
  , SymBlock(..)
  , SymDefine(..)
  , ppSymDefine
  ) where

import Data.Map (Map)
import Data.Vector (Vector)
import qualified Text.LLVM.AST as LLVM
import Text.PrettyPrint.HughesPJ

-- | Identifier for a function.
type FuncID = LLVM.Symbol

-- | Identifier for a basic block.
data SymBlockID
  -- | Identifier for initial block that sets up initial
  -- post-dominator frames.
  = InitBlock
  -- | Identifier for blocks derived from LLVM blocks.
  | NamedBlock !(LLVM.Ident) !Int
  -- | Identifier for blocks derived from LLVM blocks.
  | UnnamedBlock !Int
  deriving (Eq, Ord, Show)

-- | Return init symbolic block id.
initSymBlockID :: SymBlockID
initSymBlockID = InitBlock

-- | Create new block id for block with given name and unique integer.
-- The first block is for the entry point to the LLVM block.
symBlockID :: Maybe LLVM.Ident -> Int -> SymBlockID
symBlockID (Just id) = NamedBlock id
symBlockID Nothing = UnnamedBlock

-- | Pretty print SymBlockID
ppSymBlockID :: SymBlockID -> Doc
ppSymBlockID InitBlock = text "init"
ppSymBlockID (NamedBlock id n) =
  text "%N" <> LLVM.ppIdent id <> char '.' <> int n
ppSymBlockID (UnnamedBlock n) =
  text "%U." <> int n

-- | Identies a named value in a function.
-- TODO: Figure out if LLVM.Ident is the right type and if this can be
-- changed to an integer (for efficiency purposes).
type Reg = LLVM.Ident

type Typed v = LLVM.Typed v

-- | Represents a value in the symbolic simulator.
--TODO: Figure out if LLVM.Value if sufficient or we need something else.
type SymValue = LLVM.Value

-- | Expression in Symbolic instruction set.
-- | TODO: Make this data-type strict.
data SymExpr
  -- | Statement for arithmetic operation.
  = Arith LLVM.ArithOp (Typed SymValue) SymValue
  -- | Statement for bit operation.
  | Bit LLVM.BitOp (Typed SymValue) SymValue
  -- | Statement for conversion operation.
  -- TODO: See if type information is needed.
  | Conv LLVM.ConvOp (Typed SymValue) LLVM.Type
  | Alloca LLVM.Type (Maybe (Typed SymValue)) (Maybe Int) 
  | Load (Typed SymValue)
  | ICmp LLVM.ICmpOp (Typed SymValue) SymValue
  | FCmp LLVM.FCmpOp (Typed SymValue) SymValue
  -- | A copy of a value.
  | Val (Typed SymValue)
  -- | GetElementPointer instruction.
  | GEP (Typed SymValue) [Typed SymValue]
  | Select (Typed SymValue) (Typed SymValue) SymValue

-- | Predicates in symbolic simulator context.
data SymCond
  -- | @HasConstValue v i@ holds if @v@ corresponds to the constant @i@.
  = HasConstValue SymValue Integer
  | Not SymCond
  | TrueSymCond

-- | Instruction in symbolic level.
data SymStmt
  -- | Clear current execution path.
  = ClearCurrentExecution
  -- | @PushCallFrame mr @:
  -- 1. Pushes a call merge frame to the merge frame stack that has the return location @n@
  -- (exceptions are not caught by calls).
  -- 2. Pushes a call frame to the current execution path's stack with the given aguments.
  -- 3. When the frame is popped the return value is stored in @mr@ if @mr@ is defined.
  | PushCallFrame (Maybe Reg) LLVM.Type SymValue [Typed SymValue]
  -- | @PushInvokeFrame (n,v) e@ pushes a invoke frame to the merge frame stack that has the 
  -- normal return basic block @n@, normal return value to assign @v@, and exception path @e@.
  | PushInvokeFrame SymBlockID (Maybe Reg) SymBlockID LLVM.Type SymValue [Typed SymValue]
  -- | @PushPostDominatorFrame@ pushes a new frame to the merge frame stack for a post-dominator
  -- at the given block.  This instruction is used when we jump into a block that has a different
  -- immediate post-dominator than its parent.
  | PushPostDominatorFrame SymBlockID
  -- | Merge current state to post-dominator return path under the given condition.
  -- N.B. The current state must be unchanged.  However, the current block of the merged
  -- state must be the post-dominator block.
  | MergePostDominator SymBlockID SymCond
  -- | @MergeReturnVoidAndClear@ pops top call frame from path, merges current path
  -- with call frame, and clears current path.
  | MergeReturnVoidAndClear
  -- | @PushPendingExecution c@ make the current state a pending execution in the top-most
  -- merge frame with the additional path constraint c.
  | PushPendingExecution SymCond
  -- | @MergeReturnAndClear@ pops top call frame from path, merges (current path return value)
  -- with call frame, and clears current path.
  | MergeReturnAndClear (Typed SymValue)
  -- | Sets the block to the given location.
  | SetCurrentBlock SymBlockID
  -- | Add an additional constraint to the current execution path.
  | AddPathConstraint SymCond
  -- | Assign result of instruction to register.
  | Assign Reg SymExpr
  -- | @Store addr v@ stores value @v@ in @addr@.
  | Store (Typed SymValue) (Typed SymValue)
  -- | Conditional execution.
  | IfThenElse SymCond [SymStmt] [SymStmt]
  -- | Print out an error message if we reach an unreachable.
  | Unreachable 
  -- | Unwind exception path.
  | Unwind
  -- TODO: Support all exception handling.

data SymBlock = SymBlock {
         sbId :: SymBlockID -- ^ Identifier for block (unique within definition).
       , sbStmts :: [SymStmt]
       }

-- | Symbolically lifted version of a LLVM definition.
data SymDefine = SymDefine {
         sdName :: LLVM.Symbol
       , sdArgs :: [Typed Reg]
       , sdReturn :: LLVM.Type
       , sdBody :: Map SymBlockID SymBlock
       }

ppSymDefine :: SymDefine -> Doc
ppSymDefine _ = error "ppSymDefine undefined"
