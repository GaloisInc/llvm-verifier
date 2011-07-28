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
module Data.LLVM.Symbolic.AST
  ( FuncID
  , SymBlockID
  , initSymBlockID
  , symBlockID
  , ppSymBlockID
  , Reg
  , LLVM.Typed(..)
  , SymValue
  , SymExpr(..)
  , SymCond(..)
  , SymStmt(..)
  , SymBlock(..)
  , SymDefine(..)
  , ppSymDefine
  ) where

import Data.Int (Int32)
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Text.LLVM.AST as LLVM
import Text.PrettyPrint.HughesPJ

-- | Intersperse commas into document.
commas :: [Doc] -> Doc
commas ds = hcat (intersperse (comma <> space) ds)

-- | Identifier for a function.
type FuncID = LLVM.Symbol

-- | Identifier for a basic block.
data SymBlockID
  -- | Identifier for initial block that sets up initial
  -- post-dominator frames.
  = InitBlock
  -- | Identifier for blocks derived from LLVM blocks.
  | NamedBlock !(LLVM.Ident) !Int
  deriving (Eq, Ord, Show)

-- | Return init symbolic block id.
initSymBlockID :: SymBlockID
initSymBlockID = InitBlock

-- | Create new block id for block with given name and unique integer.
-- The first block is for the entry point to the LLVM block.
symBlockID :: LLVM.Ident -> Int -> SymBlockID
symBlockID i = NamedBlock i

-- | Pretty print SymBlockID
ppSymBlockID :: SymBlockID -> Doc
ppSymBlockID InitBlock = text "init"
ppSymBlockID (NamedBlock i n) = LLVM.ppIdent i <> char '.' <> int n

-- | Identies a named value in a function.
-- TODO: Figure out if LLVM.Ident is the right type and if this can be
-- changed to an integer (for efficiency purposes).
type Reg = LLVM.Ident

ppReg :: Reg -> Doc
ppReg = LLVM.ppIdent

type Typed v = LLVM.Typed v

-- | Represents a value in the symbolic simulator.
--TODO: Figure out if LLVM.Value if sufficient or we need something else.
type SymValue = LLVM.Value

ppSymValue :: SymValue -> Doc
ppSymValue = LLVM.ppValue

ppTypedValue :: Typed SymValue -> Doc
ppTypedValue = LLVM.ppTyped ppSymValue

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
  | ExtractValue (Typed SymValue) Int32
  | InsertValue (Typed SymValue) (Typed SymValue) Int32


-- | Pretty print symbolic expression.
ppSymExpr :: SymExpr -> Doc
ppSymExpr (Arith op l r) = LLVM.ppArithOp op <+> ppTypedValue l <> comma <+> ppSymValue r
ppSymExpr (Bit op l r) = LLVM.ppBitOp op <+> ppTypedValue l <> comma <+> ppSymValue r
ppSymExpr (Conv op l r) = LLVM.ppConvOp op <+> ppTypedValue l <> comma <+> LLVM.ppType r
ppSymExpr (Alloca ty len align) = LLVM.ppAlloca ty len align
ppSymExpr (Load ptr) = text "load" <+> ppTypedValue ptr
ppSymExpr (ICmp op l r) = text "icmp" <+> LLVM.ppICmpOp op <+> ppTypedValue l <> comma <+> ppSymValue r
ppSymExpr (FCmp op l r) = text "fcmp" <+> LLVM.ppFCmpOp op <+> ppTypedValue l <> comma <+> ppSymValue r
ppSymExpr (Val v) = ppTypedValue v
ppSymExpr (GEP ptr ixs) = text "getelementptr" <+> commas (map (ppTypedValue) (ptr:ixs))
ppSymExpr (Select c t f) = text "select" <+> ppTypedValue c
                         <> comma <+> ppTypedValue t
                         <> comma <+> LLVM.ppType (LLVM.typedType t) <+> ppSymValue f
ppSymExpr (ExtractValue v i) = text "extractvalue" <+> ppTypedValue v
                             <> comma <+> integer (toInteger i)
ppSymExpr (InsertValue a v i) = text "insertvalue" <+> ppTypedValue a
                              <> comma <+> ppTypedValue v
                              <> comma <+> integer (toInteger i)
-- | Predicates in symbolic simulator context.
data SymCond
  -- | @HasConstValue v i@ holds if @v@ corresponds to the constant @i@.
  = HasConstValue SymValue Integer
  | TrueSymCond

-- | Pretty print symbolic condition.
ppSymCond :: SymCond -> Doc
ppSymCond (HasConstValue v i) = ppSymValue v <+> text "==" <+> integer i
ppSymCond TrueSymCond = text "true"

-- | Instruction in symbolic level.
data SymStmt
  -- | Clear current execution path.
  = ClearCurrentExecution
  -- | @PushInvokeFrame fn args res@ pushes a invoke frame to the merge frame stack
  -- that will call @fn@ with @args@, and store the result in @res@ if the function
  -- returns normally.
  | PushCallFrame SymValue [Typed SymValue] (Maybe (Typed Reg))
  -- | @PushInvokeFrame fn args res e@ pushes a invoke frame to the merge frame stack
  -- that will call @fn@ with @args@.
  -- If the function returns normally, then the result, if any, will be stored in @res@.
  -- If the function throws an exception, then the current block will be set to @e@.
  | PushInvokeFrame SymValue [Typed SymValue] (Maybe (Typed Reg)) SymBlockID
  -- | @PushPostDominatorFrame@ pushes a new frame to the merge frame stack for
  -- a post-dominator at the given block.  This instruction is used when we
  -- jump into a block that has a different immediate post-dominator than its
  -- parent.
  | PushPostDominatorFrame SymBlockID
  -- | Merge current state to post-dominator return path under the given condition.
  -- N.B. The current state must be unchanged.  However, the current block of the merged
  -- state must be the post-dominator block.
  | MergePostDominator SymBlockID SymCond
  -- | @MergeReturnVoidAndClear@ pops top call frame from path, merges current path
  -- with call frame, and clears current path.
  | MergeReturnVoidAndClear
  -- | @MergeReturnAndClear@ pops top call frame from path, merges (current path return value)
  -- with call frame, and clears current path.
  | MergeReturnAndClear (Typed SymValue)
  -- | @PushPendingExecution c@ make the current state a pending execution in the top-most
  -- merge frame with the additional path constraint c.
  | PushPendingExecution SymCond
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

ppSymStmt :: SymStmt -> Doc
ppSymStmt ClearCurrentExecution = text "clearCurrentExecution"
ppSymStmt (PushCallFrame fn args res)
  = text "pushCallFrame " <+> ppSymValue fn
  <> parens (commas (map ppTypedValue args))
  <+> maybe (text "void") (LLVM.ppTyped ppReg) res
ppSymStmt (PushInvokeFrame fn args res e)
  = text "pushInvokeFrame " <+> ppSymValue fn
  <> parens (commas (map ppTypedValue args))
  <+> maybe (text "void") (LLVM.ppTyped ppReg) res
  <+> ppSymBlockID e
ppSymStmt (PushPostDominatorFrame b) = text "pushPostDominatorFrame" <+> ppSymBlockID b
ppSymStmt (MergePostDominator b c) = text "mergePostDominator" <+> ppSymBlockID b <> comma <+> ppSymCond c
ppSymStmt MergeReturnVoidAndClear = text "mergeReturnVoidAndClear"
ppSymStmt (MergeReturnAndClear v) = text "mergeReturnAndClear" <+> ppTypedValue v
ppSymStmt (PushPendingExecution c) = text "pushPendingExecution" <+> ppSymCond c
ppSymStmt (SetCurrentBlock b) = text "setCurrentBlock" <+> ppSymBlockID b
ppSymStmt (AddPathConstraint c) = text "addPathConstraint" <+> ppSymCond c
ppSymStmt (Assign v e) = ppReg v <+> char '=' <+> ppSymExpr e
ppSymStmt (Store addr v) = text "store" <+> ppTypedValue addr <> comma <+> ppTypedValue v
ppSymStmt (IfThenElse c t f) =
  text "if" <+> ppSymCond c <> char '{'
    $+$ nest 2 (vcat (map ppSymStmt t))
    $+$ text "} else {"
    $+$ nest 2 (vcat (map ppSymStmt f))
    $+$ char '}'
ppSymStmt Unreachable = text "unreachable"
ppSymStmt Unwind = text "unwind"

data SymBlock = SymBlock {
         sbId :: SymBlockID -- ^ Identifier for block (unique within definition).
       , sbStmts :: [SymStmt]
       }

ppSymBlock :: SymBlock -> Doc
ppSymBlock sb = ppSymBlockID (sbId sb) $+$ nest 2 (vcat (map ppSymStmt (sbStmts sb)))

-- | Symbolically lifted version of a LLVM definition.
data SymDefine = SymDefine {
         sdName :: LLVM.Symbol
       , sdArgs :: [Typed Reg]
       , sdRetType :: LLVM.Type
       , sdBody :: Map SymBlockID SymBlock
       }

ppSymDefine :: SymDefine -> Doc
ppSymDefine d = text "define"
              <+> LLVM.ppType (sdRetType d)
              <+> LLVM.ppSymbol (sdName d)
              <> parens (commas (map (LLVM.ppTyped LLVM.ppIdent) (sdArgs d)))
              <+> char '{'
              $+$ vcat (map ppSymBlock (Map.elems (sdBody d)))
              $+$ char '}'
