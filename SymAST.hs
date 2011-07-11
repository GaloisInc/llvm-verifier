-- | This module defines the main data types for the AST used directly by the symbolic
-- simulator.  This AST data type is the interface between the symbolic execution and
-- the LLVM lifting operating.
module SymAST 
  ( FuncID
  , BlockID 
  , Reg
  , BlockLocation
  , SymValue
  , SymExpr
  , SymCond
  , SymStmt
  , SymBlock
  , SymDefine
  ) where

import Data.Map (Map)
import qualified Text.LLVM.AST as LLVM

-- | Identifier for a function.
type FuncID = LLVM.Symbol

-- | Identifier for a basic block.
type BlockID = LLVM.Ident

-- | Identies a named value in a function.
-- TODO: Figure out if LLVM.Ident is the right type and if this can be
-- changed to an integer (for efficiency purposes).
type Reg = LLVM.Ident

-- | Uniquely identifies a block in the program by function and block identifier.
type BlockLocation = (FuncID,BlockID)

-- | Represents a value in the symbolic simulator.
--TODO: Figure out if LLVM.Value if sufficient or we need something else.
type SymValue = LLVM.Value

type Typed v = LLVM.Typed v

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
  | Phi [(SymValue, BlockID)]
  -- | GetElementPointer instruction.
  | GEP (Typed SymValue) [Typed SymValue]
  | Select (Typed SymValue) (Typed SymValue) SymValue

-- | Predicates in symbolic simulator context.
data SymCond
  -- | @HasConstValue v i@ holds if @v@ corresponds to the constant @i@.
  = HasConstValue SymValue Integer
  -- | @IsPrevBlock b@ holds if @b@ was the previous block executed along the current exection
  -- path.
  | IsPrevBlock BlockID

-- | Instruction in symbolic level.
data SymStmt
  -- | Clear current execution path.
  = ClearCurrentExecution
  -- | @PushCallFrame n@ pushes a invoke frame to the merge frame stack that has the 
  -- return location @n@ (exceptions are not caught by calls).
  | PushCallFrame (BlockLocation, Maybe Reg)
  -- | @PushInvokeFrame (n,v) e@ pushes a invoke frame to the merge frame stack that has the 
  -- normal return basic block @n@, normal return value to assign @v@, and exception path @e@.
  | PushInvokeFrame (BlockLocation, Maybe Reg) BlockLocation
  -- | @PushPostDominatorFrame@ pushes a new frame to the merge frame stack for a post-dominator
  -- at the given block.  This instruction is used when we jump into a block that has a different
  -- immediate post-dominator than its parent.
  | PushPostDominatorFrame BlockID
  -- | Merge current state to post-dominator return path.
  -- N.B. The current state must be unchanged.  However, the current block of the merged
  -- state must be the post-dominator block.
  | MergePostDominator BlockID
  -- | @MergeVoidReturnAndClear@ merges current path and clears current path.
  | MergeReturnVoidAndClear
  -- | @MergeReturnAndClear@ merges current path and return value with state,
  -- and clears current path.
  | MergeReturnAndClear SymValue
  -- | Sets the block to the given location.
  | SetCurrentBlock BlockID
  -- | Assign result of instruction to register.
  | Assign Reg SymExpr
  -- | @Store addr v@ stores value @v@ in @addr@.
  | Store (Typed SymValue) (Typed SymValue)
  -- | Conditional execution.
  | IfThenElse SymCond SymBlock SymBlock
  -- TODO: Support exception handling and unwind and unreachable.

type SymBlock = [SymStmt]

-- | Symbolically lifted version of a LLVM definition.
data SymDefine = SymDefine {
    sdName :: LLVM.Symbol
  , sdArgs :: [Typed LLVM.Ident]
  , sdBody :: Map BlockID SymBlock
  }
