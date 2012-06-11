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
  , Reg
  , SymValue
  , SymExpr(..)
  , SymCond(..)
  , SymStmt(..)
  , SymBlock(..)
  , SymDefine(..)
  , entrySymbol
  , entryRetNormalID
  , initSymBlockID
  , lookupSymBlock
  , ppSymBlock
  , ppSymBlockID
  , ppSymCond
  , ppSymDefine
  , ppSymExpr
  , ppSymStmt
  , symBlockID
  , symBlockLabel
  ) where

import Data.Int
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Text.LLVM.AST as LLVM
import Text.PrettyPrint.HughesPJ

-- | A fake entry label to represent the function that calls a user function.
entrySymbol :: LLVM.Symbol
entrySymbol = LLVM.Symbol "__galois_entry"

-- | A fake sentinel SymBlockID to represent a fictitious target block for after
-- a normal return from a toplevel function invocation.
entryRetNormalID :: SymBlockID
entryRetNormalID = NamedBlock (LLVM.Named $ LLVM.Ident "__galois_entry_ret_normal") (-1)

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
  | NamedBlock !(LLVM.BlockLabel) !Int
  deriving (Eq, Ord, Show)

-- | Return init symbolic block id.
initSymBlockID :: SymBlockID
initSymBlockID = InitBlock

-- | Create new block id for block with given name and unique integer.
-- The first block is for the entry point to the LLVM block.
symBlockID :: LLVM.BlockLabel -> Int -> SymBlockID
symBlockID i = NamedBlock i

symBlockLabel :: SymBlockID -> Maybe LLVM.BlockLabel
symBlockLabel (NamedBlock i _) = Just i
symBlockLabel _                = Nothing

-- | Pretty print SymBlockID
ppSymBlockID :: SymBlockID -> Doc
ppSymBlockID InitBlock = text "init"
ppSymBlockID (NamedBlock b n) = LLVM.ppLabel b <> char '.' <> int n

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
  -- | @Alloca tp sz align@  allocates a new pointer to @sz@ elements of type
  -- @tp@ with alignment @align@.
  | Alloca LLVM.Type (Maybe (Typed SymValue)) (Maybe Int)
  | Load (Typed SymValue) (Maybe LLVM.Align)
  | ICmp LLVM.ICmpOp (Typed SymValue) SymValue
  | FCmp LLVM.FCmpOp (Typed SymValue) SymValue
  -- | A copy of a value.
  | Val (Typed SymValue)
  -- | GetElementPointer instruction.
  | GEP (Typed SymValue) [Typed SymValue]
  | Select (Typed SymValue) (Typed SymValue) SymValue
  | ExtractValue (Typed SymValue) [Int32]
  | InsertValue (Typed SymValue) (Typed SymValue) [Int32]


-- | Pretty print symbolic expression.
ppSymExpr :: SymExpr -> Doc
ppSymExpr (Arith op l r) = LLVM.ppArithOp op <+> ppTypedValue l <> comma <+> ppSymValue r
ppSymExpr (Bit op l r) = LLVM.ppBitOp op <+> ppTypedValue l <> comma <+> ppSymValue r
ppSymExpr (Conv op l r) = LLVM.ppConvOp op <+> ppTypedValue l <> comma <+> LLVM.ppType r
ppSymExpr (Alloca ty len align) = LLVM.ppAlloca ty len align
ppSymExpr (Load ptr malign) = text "load" <+> ppTypedValue ptr <> LLVM.ppAlign malign
ppSymExpr (ICmp op l r) = text "icmp" <+> LLVM.ppICmpOp op <+> ppTypedValue l <> comma <+> ppSymValue r
ppSymExpr (FCmp op l r) = text "fcmp" <+> LLVM.ppFCmpOp op <+> ppTypedValue l <> comma <+> ppSymValue r
ppSymExpr (Val v) = ppTypedValue v
ppSymExpr (GEP ptr ixs) = text "getelementptr" <+> commas (map (ppTypedValue) (ptr:ixs))
ppSymExpr (Select c t f) = text "select" <+> ppTypedValue c
                         <> comma <+> ppTypedValue t
                         <> comma <+> LLVM.ppType (LLVM.typedType t) <+> ppSymValue f
ppSymExpr (ExtractValue v is) = text "extractvalue" <+> ppTypedValue v
                              <> comma <+> text (show is)
ppSymExpr (InsertValue a v is) = text "insertvalue" <+> ppTypedValue a
                               <> comma <+> ppTypedValue v
                               <> comma <+> text (show is)

-- | Predicates in symbolic simulator context.
data SymCond
  -- | @HasConstValue v i@ holds if @v@ corresponds to the constant @i@.
  = HasConstValue (Typed SymValue) Integer
  -- | @NotConstValues v is@ holds if @v@ does not correspond to any
  -- of the constants in @is@.
  | NotConstValues (Typed SymValue) [Integer]
  -- | @TrueSymCond@ always holds.
  | TrueSymCond

-- | Pretty print symbolic condition.
ppSymCond :: SymCond -> Doc
ppSymCond (HasConstValue v i) = ppTypedValue v <+> text "==" <+> integer i
ppSymCond (NotConstValues v is) = ppTypedValue v <+> text "not in" <+>
                                  brackets (commas (map integer is))
ppSymCond TrueSymCond = text "true"

-- | Instruction in symbolic level.
data SymStmt
  -- | @PushInvokeFrame fn args res@ pushes a invoke frame to the merge frame stack
  -- that will call @fn@ with @args@, and store the result in @res@ if the function
  -- returns normally.
  = PushCallFrame SymValue [Typed SymValue] (Maybe (Typed Reg))
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
  -- | Merge current path state to post-dominator return path under the given condition,
  -- and clear the current path state.
  -- N.B. The current state must be unchanged.  However, the current block of the merged
  -- state must be the post-dominator block.
  | MergePostDominator SymBlockID 
  -- | @MergeReturn@ pops top call frame from path, merges (current path return value)
  -- with call frame, and clears current path.
  | MergeReturn (Maybe (Typed SymValue))
  -- | @PushPendingExecution c@ make the current state a pending execution in the top-most
  -- merge frame with the additional path constraint c.
  | PushPendingExecution SymCond
  -- | Sets the block to the given location.
  | SetCurrentBlock SymBlockID
  -- | Assign result of instruction to register.
  | Assign Reg SymExpr
  -- | @Store v addr@ stores value @v@ in @addr@.
  | Store (Typed SymValue) (Typed SymValue) (Maybe LLVM.Align)
  -- | Conditional execution.
  | IfThenElse SymCond [SymStmt] [SymStmt]
  -- | Print out an error message if we reach an unreachable.
  | Unreachable
  -- | Unwind exception path.
  | Unwind
  -- TODO: Support all exception handling.

ppSymStmt :: SymStmt -> Doc
ppSymStmt (PushCallFrame fn args res)
  = text "pushCallFrame" <+> ppSymValue fn
  <> parens (commas (map ppTypedValue args))
  <+> maybe (text "void") (LLVM.ppTyped ppReg) res
ppSymStmt (PushInvokeFrame fn args res e)
  = text "pushInvokeFrame " <+> ppSymValue fn
  <> parens (commas (map ppTypedValue args))
  <+> maybe (text "void") (LLVM.ppTyped ppReg) res
  <+> ppSymBlockID e
ppSymStmt (PushPostDominatorFrame b) = text "pushPostDominatorFrame" <+> ppSymBlockID b
ppSymStmt (MergePostDominator b) = text "mergePostDominator" <+> ppSymBlockID b 
ppSymStmt (MergeReturn mv) = text "mergeReturn" <+> maybe empty ppTypedValue mv
ppSymStmt (PushPendingExecution c) = text "pushPendingExecution" <+> ppSymCond c
ppSymStmt (SetCurrentBlock b) = text "setCurrentBlock" <+> ppSymBlockID b
ppSymStmt (Assign v e) = ppReg v <+> char '=' <+> ppSymExpr e
ppSymStmt (Store v addr malign) = text "store" <+> ppTypedValue v <> comma <+> ppTypedValue addr
                                  <> LLVM.ppAlign malign
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
       , sdVarArgs :: Bool
       , sdRetType :: LLVM.Type
       , sdBody :: Map SymBlockID SymBlock
       }

lookupSymBlock :: SymDefine -> SymBlockID -> SymBlock
lookupSymBlock sd sid =
  case Map.lookup sid (sdBody sd) of
    Nothing  -> error $ "Failed to locate symblock " ++ show (ppSymBlockID sid)
    Just blk -> blk

ppSymDefine :: SymDefine -> Doc
ppSymDefine d = text "define"
              <+> LLVM.ppType (sdRetType d)
              <+> LLVM.ppSymbol (sdName d)
              <> parens (commas (map (LLVM.ppTyped LLVM.ppIdent) (sdArgs d)))
              <+> char '{'
              $+$ vcat (map ppSymBlock (Map.elems (sdBody d)))
              $+$ char '}'
