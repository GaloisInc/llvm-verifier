{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ImplicitParams #-}
-- | This module defines the main data types for the AST used directly by the symbolic
-- simulator.  This AST data type is the interface between the symbolic execution and
-- the LLVM lifting operating.
--
-- The Symbolic IR is similar to the LLVM IR, but includes several differences:
--
-- * The Symbolic IR includes explicit instructions for pushing and popping frames from
--   the merge frame stack.
module Verifier.LLVM.AST
  ( FuncID
  , SymBlockID
  , sValString
  , TypedSymValue(..)
  , ppTypedSymValue
  , BitWidth
  , NUWFlag
  , NSWFlag
  , ExactFlag
  , IntArithOp(..)
  , ppIntArithOp
  , OptVectorLength
  , TypedExpr(..)
  , StructInfo(..)
  , Int32
  , GEPOffset(..)
  , SymExpr(..)
  , SymCond(..)
  , MergeLocation
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
  , module Verifier.LLVM.LLVMContext
  , L.commas
  ) where

import Control.Applicative ((<$>))
import Data.Foldable
import Data.Traversable
import Data.Int
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Text.LLVM.AST as L
import Text.PrettyPrint.HughesPJ

import Verifier.LLVM.LLVMContext
import Verifier.LLVM.Utils

-- | A fake entry label to represent the function that calls a user function.
entrySymbol :: L.Symbol
entrySymbol = L.Symbol "__galois_entry"

-- | A fake sentinel SymBlockID to represent a fictitious target block for after
-- a normal return from a toplevel function invocation.
entryRetNormalID :: SymBlockID
entryRetNormalID = NamedBlock (L.Named $ L.Ident "__galois_entry_ret_normal") (-1)

-- | Intersperse commas into document.
commas :: [Doc] -> Doc
commas ds = hcat (intersperse (comma <> space) ds)

-- | Identifier for a function.
type FuncID = L.Symbol

-- | Identifier for a basic block.
data SymBlockID
  -- | Identifier for initial block that sets up initial
  -- post-dominator frames.
  = InitBlock
  -- | Identifier for blocks derived from LLVM blocks.
  | NamedBlock !(L.BlockLabel) !Int
  deriving (Eq, Ord, Show)

-- | Return init symbolic block id.
initSymBlockID :: SymBlockID
initSymBlockID = InitBlock

-- | Create new block id for block with given name and unique integer.
-- The first block is for the entry point to the LLVM block.
symBlockID :: L.BlockLabel -> Int -> SymBlockID
symBlockID i = NamedBlock i

symBlockLabel :: SymBlockID -> Maybe L.BlockLabel
symBlockLabel (NamedBlock i _) = Just i
symBlockLabel _                = Nothing

-- | Pretty print SymBlockID
ppSymBlockID :: SymBlockID -> Doc
ppSymBlockID InitBlock = text "init"
ppSymBlockID (NamedBlock b n) = L.ppLabel b <> char '.' <> int n

-- | Offset in GEP address calculation.
data GEPOffset v
     -- | Returns the address of a field in a struct.
   = StructField StructInfo Int
     -- | @ArrayElement etp w i@ denotes the ith value in an array with
     -- elements of type @etp@.  The value @i@ has @w@ bits.
   | ArrayElement MemType BitWidth v
  deriving (Functor, Foldable, Traversable)

ppGEPOffset :: (v -> Doc) -> GEPOffset v -> Doc
ppGEPOffset _ (StructField _ i) = text "i32" <+> int i 
ppGEPOffset pp (ArrayElement _ w v) =
  ppIntType w <> integer (toInteger w) <+> pp v

-- | NUW flag used with addition, subtraction, multiplication, and left shift to
-- indicates that unsigned overflow is undefined.
type NUWFlag = Bool

-- | NSW flag used with addition, subtraction, multiplication, and left shift to
-- indicates that signed overflow is undefined.
type NSWFlag = Bool

-- | Exact flag used on division, remainder and right-shift operation to indicate that
-- operation should not have any remainder.  Otherwise the value is undefined.
type ExactFlag = Bool

-- | Binary operation on integers.
data IntArithOp
  = Add NUWFlag NSWFlag
  | Sub NUWFlag NSWFlag
  | Mul NUWFlag NSWFlag
  | UDiv ExactFlag | SDiv ExactFlag
  | URem           | SRem
  | Shl NUWFlag NSWFlag
  | Lshr ExactFlag
  | Ashr ExactFlag
  | And
  | Or
  | Xor

ppIntArithOp :: IntArithOp -> Doc
ppIntArithOp (Add nuw nsw) = text "add" <+> L.ppSignBits nuw nsw
ppIntArithOp (Sub nuw nsw) = text "sub" <+> L.ppSignBits nuw nsw
ppIntArithOp (Mul nuw nsw) = text "mul" <+> L.ppSignBits nuw nsw
ppIntArithOp (UDiv e)      = text "udiv" <+> L.ppExact e
ppIntArithOp (SDiv e)      = text "sdiv" <+> L.ppExact e
ppIntArithOp URem          = text "urem"
ppIntArithOp SRem          = text "srem"
ppIntArithOp (Shl nuw nsw) = text "shl" <+> L.ppSignBits nuw nsw
ppIntArithOp (Lshr e)      = text "lshr" <+> L.ppExact e
ppIntArithOp (Ashr e)      = text "ashr" <+> L.ppExact e
ppIntArithOp And           = text "and"
ppIntArithOp Or            = text "or"
ppIntArithOp Xor           = text "xor"

-- | Condition used to indicate this operation should be applied to
-- a vector (and the number of elements the vector(s) should contains).
type OptVectorLength = Maybe Int

data TypedExpr v
    -- | @IntArith op mn w x y@ performs the operation @op@ on @x@ and @y@.
    -- If @mn@ is @Nothing@, then @x@ and @y@ are integers with length @w@.  Otherwise
    -- @x@ and @y@ are vectors with integer elements of length @w@, and @mn@ contains the
    -- number of elements.
  = IntArith IntArithOp OptVectorLength BitWidth v v
    -- | @PtrAdd p i@ increments the value of the pointer @p@ by @i@ bytes.  @p@ must
    -- be a pointer, and @i@ must be an integer with the same width as a pointer.
    -- Addition uses standard two's complement rules.
  | PtrAdd v v
    -- | @UAddWithOverflow w x y@ adds @x@ and @y@ and returns a struct whose first element
    -- contains a @w@-bit sum of @x@ and @y@ and second element contains the single overflow bit. 
  | UAddWithOverflow BitWidth v v
    -- | @IntCmp op mn w x y@ performs the operation @op@ on @x@ and @y@.
    -- If @mn@ is @Nothing@, then @x@ and @y@ are integers with length @w@.  Otherwise
    -- @x@ and @y@ are vectors with integer elements of length @w@, and @mn@ contains the
    -- number of elements.
  | IntCmp L.ICmpOp OptVectorLength BitWidth v v
    -- | @Trunc mn iw t rw@ assumes that @rw < iw@, and truncates an integer @t@
    -- with @iw@ bits to an integer with @rw@ bits.
  | Trunc OptVectorLength BitWidth v BitWidth
    -- | @ZExt mn iw t rw@ assumes that @iw < rw@, and zero extends an
    -- integer @t@ with @iw@ bits to an integer with @rw@ bits.
  | ZExt OptVectorLength BitWidth v BitWidth
    -- | @SExt mn iw t rw@ assumes that @iw < rw@, and sign extends an
    -- integer @t@ with @iw@ bits to an integer with @rw@ bits.
  | SExt OptVectorLength BitWidth v BitWidth
    -- | @PtrToInt tp t rw@ converts a pointer @t@ with type @tp@ to an
    -- integer with width @rw@.  The value of the pointer is truncated or zero
    -- extended as necessary to have the correct length.
  | PtrToInt OptVectorLength SymType v BitWidth
    -- | @IntToPtr iw t tp@ converts an integer @t@ with width @iw@ to
    -- a pointer.  The value of the integer is truncated or zero
    -- extended as necessary to have the correct length.
  | IntToPtr OptVectorLength BitWidth v SymType
    -- | @Select mn c tp x y@ selects the arguments in @x@ if @c@ evaluated to @1@ and
    -- @y@ if @c@ evaluates to @0@.   @c@ must have type @i1@ and @x@ and @y@ have type
    -- @tp@.  The function is extended pairwise to vectors if @mn@ holds an integer. 
  | Select OptVectorLength v MemType v v
    -- | GEP instruction
  | GEP Bool v [GEPOffset v] MemType
    -- | Return a field out of a struct 
  | GetStructField StructInfo v Int
    -- | Return a specific element of an array.
    -- Arguments are: number of elements, element type, array, and index.
  | GetConstArrayElt Int MemType v Int

    -- | Integer width and value.
  | SValInteger BitWidth Integer
  | SValFloat  Float
  | SValDouble Double
    -- | Null pointer value with the type of element that it points to.
  | SValNull SymType
    -- | Array of values (strings are mapped to 8-bit integer arrays).
  | SValArray MemType (Vector v)
    -- | Vector element with given values.
  | SValVector MemType (Vector v)
    -- | Create a struct with the given field values.
  | SValStruct StructInfo (Vector v)
 deriving (Functor, Foldable, Traversable)

-- | Pretty print a typed expression.
ppTypedExpr :: -- | Pretty printer for conversions
               (String -> Doc -> e -> Doc -> Doc)
               -- | Pretty printer for values
            -> (e -> Doc)
            -> TypedExpr e -> Doc
ppTypedExpr ppConv ppValue tpExpr =
    case tpExpr of
      IntArith op mn w x y ->
        ppIntArithOp op <+> tp <+> ppValue x <> comma <+> ppValue y
       where tp  = maybe ppIntType ppIntVector mn w
      PtrAdd p o -> text "ptrAdd" <+> ppValue p <> comma <+> ppValue o
      UAddWithOverflow w x y -> text ("@llvm.uadd.with.overflow.i" ++ show w)
        <> parens (ppValue x <> comma <+> ppValue y)
      IntCmp op mn w x y ->
         text "icmp" <+> L.ppICmpOp op <+> tp <+> ppValue x <> comma <+> ppValue y
       where tp  = maybe ppIntType ppIntVector mn w

      Trunc mn iw v rw    -> ppConv "trunc"    (ppMIntType mn iw) v (ppMIntType mn rw)
      ZExt  mn iw v rw    -> ppConv "zext"     (ppMIntType mn iw) v (ppMIntType mn rw)
      SExt  mn iw v rw    -> ppConv "sext"     (ppMIntType mn iw) v (ppMIntType mn rw)
      PtrToInt mn tp v rw -> ppConv "ptrtoint" (ppMSymType mn tp) v (ppMIntType mn rw)
      IntToPtr mn iw v tp -> ppConv "inttoptr" (ppMIntType mn iw) v (ppMSymType mn tp)
      Select mn c tp t f -> text "select" <+> ppMIntType mn 1 <+> ppValue  c
                                 <> comma <+> ppMMemType mn tp <+> ppValue t
                                 <> comma <+> ppValue f
      GEP ib ptr idxl _ -> text "getelementptr" <+> L.opt ib (text "inbounds")
          <+> commas (ppValue ptr : (ppGEPOffset ppValue <$> idxl))
      GetStructField _ v i -> text "extractfield" <+> ppValue v <+> text (show i)
      GetConstArrayElt _ _ v i -> text "arrayelt" <+> ppValue v <+> text (show i)
      SValInteger _ i -> integer i
      SValDouble i -> double i 
      SValFloat i -> float i 
      SValNull{} -> text "null"
      SValArray _ es -> brackets $ commas $ V.toList $ ppValue <$> es
      SValStruct si values -> L.structBraces (commas fl)
        where fn tp v = ppMemType tp <+> ppValue v
              fl = V.toList $ V.zipWith fn (siFieldTypes si) values
      SValVector _ es -> brackets $ commas $ V.toList $ ppValue <$> es
  where ppMIntType mn w = maybe id ppVectorType mn (ppIntType w)
        ppMMemType mn tp = maybe id ppVectorType mn (ppMemType tp)
        ppMSymType mn tp = maybe id ppVectorType mn (ppSymType tp)

-- | Represents a value in the symbolic simulator.
data TypedSymValue
    -- | Register identifier.
  = SValIdent L.Ident 
     -- | Symbol 
  | SValSymbol L.Symbol
  | SValExpr (TypedExpr TypedSymValue)

sValString :: String -> TypedSymValue
sValString s = SValExpr $ SValArray tp (toChar <$> V.fromList s)
 where tp = IntType 8
       toChar c = SValExpr $ SValInteger 8 (toInteger (fromEnum c))

ppTypedSymValue :: TypedSymValue -> Doc
ppTypedSymValue = go
  where ppConv nm itp v rtp = text nm <+> parens (itp <+> go v <+> text "to" <+> rtp) 
        go (SValIdent i) = L.ppIdent i
        go (SValSymbol s) = L.ppSymbol s
        go (SValExpr te) = ppTypedExpr ppConv go te

-- | Expression in Symbolic instruction set.
-- | TODO: Make this data-type strict.
data SymExpr
  -- | Statement for type-checked operations.
  -- = TypedExpr (TypedExpr TypedSymValue)
  = Val TypedSymValue
  -- | @Alloca tp sz align@  allocates a new pointer to @sz@ elements of type
  -- @tp@ with alignment @align@.
  | Alloca MemType (Maybe (BitWidth, TypedSymValue)) (Maybe Int)
    -- @Load ptr tp malign@ tp is type to load.
  | Load TypedSymValue MemType (Maybe L.Align)

-- | Pretty print symbolic expression.
ppSymExpr :: SymExpr -> Doc
ppSymExpr (Val v) = ppTypedSymValue v
ppSymExpr (Alloca ty mbLen mbAlign) = text "alloca" <+> ppMemType ty <> len <> align
  where len   = maybe empty (\(w,l) -> comma <+> ppIntType w <+> ppTypedSymValue l) mbLen
        align = maybe empty (\a -> comma <+> text "align" <+> int a) mbAlign
ppSymExpr (Load ptr tp malign) =
  text "load" <+> ppPtrType (ppMemType tp) <+> ppTypedSymValue ptr <> L.ppAlign malign

-- | Predicates in symbolic simulator context.
data SymCond
  -- | @HasConstValue v w i@ holds if @v@ corresponds to the constant @i@.
  = HasConstValue TypedSymValue BitWidth Integer

-- | Pretty print symbolic condition.
ppSymCond :: SymCond -> Doc
ppSymCond (HasConstValue v _ i) = ppTypedSymValue v <+> text "==" <+> integer i

-- | A merge location is a block or Nothing if the merge happens at a return.
type MergeLocation = Maybe SymBlockID

-- | Instruction in symbolic level.
data SymStmt
  -- | @PushCallFrame fn args res retTarget@ pushes a invoke frame to the merge frame stack
  -- that will call @fn@ with @args@, and store the result in @res@ if the function
  -- returns normally.  The calling function will resume execution at retTarget.
  = PushCallFrame TypedSymValue [TypedSymValue] (Maybe (MemType, L.Ident)) SymBlockID
  -- | @Return@ pops top call frame from path, merges (current path return value)
  -- with call frame, and clears current path.
  | Return (Maybe TypedSymValue)
  -- | @PushPendingExecution tgt c rest@ make the current state a pending execution in the
  -- top-most merge frame with the additional path constraint c, and current block @tgt@.
  -- The final arguments contains the statements to execute with the other path (which 
  -- may assume the negation of the path condition @c@. 
  | PushPendingExecution SymBlockID SymCond MergeLocation [SymStmt]
  -- | Sets the block to the given location.
  | SetCurrentBlock SymBlockID
  -- | Assign result of instruction to register.
  | Assign L.Ident MemType SymExpr
  -- | @Store v addr@ stores value @v@ in @addr@.
  | Store MemType TypedSymValue TypedSymValue (Maybe L.Align)
  -- | Print out an error message if we reach an unreachable.
  | Unreachable
  -- | An LLVM statement that could not be translated.
  | BadSymStmt L.Stmt

ppSymStmt :: SymStmt -> Doc
ppSymStmt (PushCallFrame fn args res retTgt)
  = text "pushCallFrame" <+> ppTypedSymValue fn
  <> parens (commas (map ppTypedSymValue args))
  <+> maybe (text "void") (\(tp,v) -> ppMemType tp <+> L.ppIdent v) res
  <+> text "returns to" <+> ppSymBlockID retTgt
ppSymStmt (Return mv) = text "return" <+> maybe empty ppTypedSymValue mv
ppSymStmt (PushPendingExecution b c ml rest) =
    text "pushPendingExecution" <+> ppSymBlockID b <+> ppSymCond c <+> text "merge" <+> loc
      $+$ vcat (fmap ppSymStmt rest)
  where loc = maybe (text "return") ppSymBlockID ml
ppSymStmt (SetCurrentBlock b) = text "setCurrentBlock" <+> ppSymBlockID b
ppSymStmt (Assign v _ e) = L.ppIdent v <+> char '=' <+> ppSymExpr e
ppSymStmt (Store tp v addr malign) =
  text "store" <+> ppMemType tp <+> ppTypedSymValue v <> comma
               <+> ppTypedSymValue addr <> L.ppAlign malign
ppSymStmt Unreachable = text "unreachable"
ppSymStmt (BadSymStmt s) = L.ppStmt s

data SymBlock = SymBlock {
         sbId :: SymBlockID -- ^ Identifier for block (unique within definition).
       , sbStmts :: [SymStmt]
       }

ppSymBlock :: SymBlock -> Doc
ppSymBlock sb = ppSymBlockID (sbId sb) $+$ nest 2 (vcat (map ppSymStmt (sbStmts sb)))

-- | Symbolically lifted version of a LLVM definition.
data SymDefine = SymDefine {
         sdName :: L.Symbol
       , sdArgs :: [(L.Ident, MemType)]
       , sdRetType :: Maybe MemType
       , sdBody :: Map SymBlockID SymBlock
       }

lookupSymBlock :: SymDefine -> SymBlockID -> SymBlock
lookupSymBlock sd sid =
  case Map.lookup sid (sdBody sd) of
    Nothing  -> error $ "Failed to locate symblock " ++ show (ppSymBlockID sid)
    Just blk -> blk

ppSymDefine :: SymDefine -> Doc
ppSymDefine d = text "define"
              <+> ppRetType (sdRetType d)
              <+> L.ppSymbol (sdName d)
              <> parens (commas ((\(i,tp) -> ppMemType tp <+> L.ppIdent i) <$> sdArgs d))
              <+> char '{'
              $+$ vcat (map ppSymBlock (Map.elems (sdBody d)))
              $+$ char '}'