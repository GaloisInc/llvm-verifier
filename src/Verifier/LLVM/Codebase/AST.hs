{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
-- | This module defines the main data types for the AST used directly by the symbolic
-- simulator.  This AST data type is the interface between the symbolic execution and
-- the LLVM lifting operating.
--
-- The Symbolic IR is similar to the LLVM IR, but includes several differences:
--
-- * The Symbolic IR includes explicit instructions for pushing and popping frames from
--   the merge frame stack.
module Verifier.LLVM.Codebase.AST
  ( FuncID
  , SymBlockID
  , ExprEvalFn(..)
  , L.Symbol(..)
  , ppSymbol
  , L.BlockLabel
  , L.ICmpOp(..)
  , SymValue(..)
  , ppSymValue
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
  , MergeLocation
  , SymStmt(..)
  , SymBlock(..)
  , SymDefine(..)
  , entrySymbol
  , entryRetNormalID
  , lookupSymBlock
  , ppSymBlock
  , ppSymBlockID
  , ppSymDefine
  , ppStmt
  , symBlockID
  , symBlockLabel
  , commas
  , module Verifier.LLVM.Codebase.DataLayout
  ) where

import Control.Applicative (Applicative, (<$>))
import Control.Lens hiding (op)
import Control.Monad.IO.Class
import Data.Foldable
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Text.LLVM.AST as L
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Verifier.LLVM.Codebase.DataLayout
import Verifier.LLVM.Utils.PrettyPrint

ppSymbol :: L.Symbol -> Doc
ppSymbol = text . show . L.ppSymbol

-- | A fake entry label to represent the function that calls a user function.
entrySymbol :: L.Symbol
entrySymbol = L.Symbol "__galois_entry"

-- | A fake sentinel SymBlockID to represent a fictitious target block for after
-- a normal return from a toplevel function invocation.
entryRetNormalID :: SymBlockID
entryRetNormalID = NamedBlock (L.Named $ L.Ident "__galois_entry_ret_normal") (-1)

-- | Intersperse commas into document.
commas :: [Doc] -> Doc
commas = hsep . punctuate (char ',')

-- | Identifier for a function.
type FuncID = L.Symbol

-- | Identifier for a basic block.
data SymBlockID
  -- | Identifier for blocks derived from LLVM blocks.
  = NamedBlock !(L.BlockLabel) !Int
  deriving (Eq, Ord, Show)

-- | Create new block id for block with given name and unique integer.
-- The first block is for the entry point to the LLVM block.
symBlockID :: L.BlockLabel -> Int -> SymBlockID
symBlockID i = NamedBlock i

symBlockLabel :: SymBlockID -> Maybe L.BlockLabel
symBlockLabel (NamedBlock i _) = Just i

-- | Pretty print SymBlockID
ppSymBlockID :: SymBlockID -> Doc
ppSymBlockID (NamedBlock b n) = text (show (L.ppLabel b)) <> char '.' <> int n

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

ppExact :: ExactFlag -> Doc
ppExact = text . show . L.ppExact

ppSignBits :: NUWFlag -> NSWFlag -> Doc
ppSignBits nuw nsw = text $ show $ L.ppSignBits nuw nsw

ppIntArithOp :: IntArithOp -> Doc
ppIntArithOp (Add nuw nsw) = text "add" <+> ppSignBits nuw nsw
ppIntArithOp (Sub nuw nsw) = text "sub" <+> ppSignBits nuw nsw
ppIntArithOp (Mul nuw nsw) = text "mul" <+> ppSignBits nuw nsw
ppIntArithOp (UDiv e)      = text "udiv" <+> ppExact e
ppIntArithOp (SDiv e)      = text "sdiv" <+> ppExact e
ppIntArithOp URem          = text "urem"
ppIntArithOp SRem          = text "srem"
ppIntArithOp (Shl nuw nsw) = text "shl" <+> ppSignBits nuw nsw
ppIntArithOp (Lshr e)      = text "lshr" <+> ppExact e
ppIntArithOp (Ashr e)      = text "ashr" <+> ppExact e
ppIntArithOp And           = text "and"
ppIntArithOp Or            = text "or"
ppIntArithOp Xor           = text "xor"

-- | Condition used to indicate this operation should be applied to
-- a vector (and the number of elements the vector(s) should contains).
type OptVectorLength = Maybe Int

data TypedExpr v
    -- | Integer width and value.
  = SValInteger BitWidth Integer
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
    -- | @IntArith op mn w x y@ performs the operation @op@ on @x@ and @y@.
    -- If @mn@ is @Nothing@, then @x@ and @y@ are integers with length @w@.  Otherwise
    -- @x@ and @y@ are vectors with integer elements of length @w@, and @mn@ contains the
    -- number of elements.
  | IntArith IntArithOp OptVectorLength BitWidth v v
    -- | @PtrAdd p i@ increments the value of the pointer @p@ by @i@ bytes.  @p@ must
    -- be a pointer, and @i@ must be an integer with the same width as a pointer.
    -- Addition uses standard two's complement rules.
  | PtrAdd v v
    -- | @UAddWithOverflow w x y@ adds @x@ and @y@ and returns a struct whose first element
    -- contains a @w@-bit sum of @x@ and @y@ and second element contains the single overflow bit. 
  | UAddWithOverflow BitWidth v v
    -- | @ICmp op mn tp x y@ performs the operation @op@ on @x@ and @y@.  If @mn@ is @Nothing@,
    -- then @x@ and @y@ are scalars, otherwise they are vectors of scalars, and @mn@ contains the
    -- number of elements. If @tp@ contains a bitwidth, then the scalars are integers with that
    -- bitwidth, otherwise the scalars are pointers, and @tp@ contains the type of pointer.
  | ICmp L.ICmpOp OptVectorLength (Either BitWidth SymType) v v
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
    -- | Return a field out of a struct 
  | GetStructField StructInfo v Int
    -- | Return a specific element of an array.
    -- Arguments are: number of elements, element type, array, and index.
  | GetConstArrayElt Int MemType v Int

 deriving (Functor, Foldable, Traversable)

-- | Pretty print a typed expression.
ppTypedExpr :: (String -> Doc -> e -> Doc -> Doc) -- ^ Pretty printer for conversions
            -> (e -> Doc) -- ^ Pretty printer for values
            -> TypedExpr e
            -> Doc
ppTypedExpr ppConv ppValue tpExpr =
    case tpExpr of
      IntArith op mn w x y ->
        ppIntArithOp op <+> tp <+> ppValue x <> comma <+> ppValue y
       where tp  = maybe ppIntType ppIntVector mn w
      PtrAdd p o -> text "ptrAdd" <+> ppValue p <> comma <+> ppValue o
      UAddWithOverflow w x y -> text ("@llvm.uadd.with.overflow.i" ++ show w)
        <> parens (ppValue x <> comma <+> ppValue y)
      ICmp op mn etp x y ->
         text "icmp" <+> text (show (L.ppICmpOp op)) <+> tp <+> ppValue x <> comma <+> ppValue y
       where tp  = maybe id ppVectorType mn scalarTp
             scalarTp = either ppIntType (ppPtrType . ppSymType) etp
      Trunc mn iw v rw    -> ppConv "trunc"    (ppMIntType mn iw) v (ppMIntType mn rw)
      ZExt  mn iw v rw    -> ppConv "zext"     (ppMIntType mn iw) v (ppMIntType mn rw)
      SExt  mn iw v rw    -> ppConv "sext"     (ppMIntType mn iw) v (ppMIntType mn rw)
      PtrToInt mn tp v rw -> ppConv "ptrtoint" (ppMSymType mn tp) v (ppMIntType mn rw)
      IntToPtr mn iw v tp -> ppConv "inttoptr" (ppMIntType mn iw) v (ppMSymType mn tp)
      Select mn c tp t f -> text "select" <+> ppMIntType mn 1 <+> ppValue  c
                                 <> comma <+> ppMMemType mn tp <+> ppValue t
                                 <> comma <+> ppValue f
      GetStructField _ v i -> text "extractfield" <+> ppValue v <+> text (show i)
      GetConstArrayElt _ _ v i -> text "arrayelt" <+> ppValue v <+> text (show i)
      SValInteger _ i -> integer i
      SValDouble i -> double i 
      SValFloat i -> float i 
      SValNull{} -> text "null"
      SValArray _ es -> brackets $ commas $ V.toList $ ppValue <$> es
      SValStruct si values -> structBraces (commas fl)
        where fn tp v = ppMemType tp <+> ppValue v
              fl = V.toList $ V.zipWith fn (siFieldTypes si) values
      SValVector _ es -> brackets $ commas $ V.toList $ ppValue <$> es
  where ppMIntType mn w = maybe id ppVectorType mn (ppIntType w)
        ppMMemType mn tp = maybe id ppVectorType mn (ppMemType tp)
        ppMSymType mn tp = maybe id ppVectorType mn (ppSymType tp)

-- | Represents a function for evaluating expressions.
newtype ExprEvalFn v t
  = ExprEvalFn (forall m . (Applicative m, MonadIO m) => (v -> m t) -> m t)


-- | Represents a value in the symbolic simulator.
data SymValue t
    -- | Register identifier.
  = SValIdent L.Ident 
     -- | Symbol 
  | SValSymbol L.Symbol
  | SValExpr (TypedExpr (SymValue t)) (ExprEvalFn (SymValue t) t)


ppSymValue :: SymValue t -> Doc
ppSymValue = go
  where ppConv nm itp v rtp = text nm <+> parens (itp <+> go v <+> text "to" <+> rtp) 
        go (SValIdent i) = ppIdent i
        go (SValSymbol s) = ppSymbol s
        go (SValExpr te _) = ppTypedExpr ppConv go te

ppAlign :: Alignment -> Doc
ppAlign a = text (", align " ++ show a)

-- | A merge location is a block or Nothing if the merge happens at a return.
type MergeLocation = Maybe SymBlockID

-- | Instruction in symbolic level.
data SymStmt t
  -- | @Call fn args res@ pushes a invoke frame to the merge frame stack
  -- that will call @fn@ with @args@, and store the result in @res@ if the function
  -- returns normally.
  = Call (SymValue t) [(MemType,SymValue t)] (Maybe (MemType, L.Ident))
  -- | @Ret mval@ returns from procedure with value.
  | Ret (Maybe (SymValue t))

    -- | Jump to target location.
  | Jump SymBlockID
    -- | @Br c t f ml@ branch to @t@ if @c@ if true and @f@ if @c@ is false.
    -- The immediate post dominator is @ml@. 
  | Br (SymValue t) SymBlockID SymBlockID MergeLocation
    -- | @Switch w v def cases ml@ switches to a matching case base on @v@.
  | Switch BitWidth (SymValue t) SymBlockID (Map Integer SymBlockID) MergeLocation

    -- | Assign expression values in current context to registers.
  | Assign L.Ident MemType (SymValue t)
    -- | @Alloca r tp sz align@ allocates a new pointer to @sz@ elements of type
    -- @tp@ with alignment @align@, and stores result in @r@.
  | Alloca L.Ident MemType (Maybe (BitWidth, SymValue t)) Alignment
    -- | @Load r ptr tp align@ loads the value at @ptr@ as a value with type @tp@, and
    -- stores the result in @r@.
  | Load L.Ident (SymValue t) MemType Alignment
  -- | @Store v addr@ stores value @v@ in @addr@.
  | Store MemType (SymValue t) (SymValue t) Alignment
  -- | Print out an error message if we reach an unreachable.
  | Unreachable
  -- | An LLVM statement that could not be translated.
  | BadSymStmt L.Stmt

ppStmt :: SymStmt t -> Doc
ppStmt (Call fn args res)
  = text "call" <+> ppSymValue fn
  <> parens (commas (ppSymValue . snd <$> args))
  <+> maybe (text "void") (\(tp,v) -> ppMemType tp <+> ppIdent v) res
ppStmt (Ret mv) = text "ret" <+> maybe empty ppSymValue mv
ppStmt (Jump b) = text "jump" <+> ppSymBlockID b
ppStmt (Br c t f ml) =
    text "br" <+> ppSymValue c <+> ppSymBlockID t <+> ppSymBlockID f
                  <+> text "merge" <+> loc
  where loc = maybe (text "return") ppSymBlockID ml
ppStmt (Switch _ c def choices ml) =
    text "switch" <+> ppSymValue c <+> ppSymBlockID def
         <+> list (ppChoice <$> Map.toList choices)
         <+> loc
  where loc = maybe (text "return") ppSymBlockID ml
        ppChoice (i,b) = integer i <+> text "label" <+> ppSymBlockID b
ppStmt (Assign r _ e) = ppIdent r <+> char '=' <+> ppSymValue e
ppStmt (Alloca r ty mbLen a) = 
    ppIdent r <+> text "= alloca" <+> ppMemType ty <> len <> ppAlign a
  where len   = maybe empty (\(w,l) -> comma <+> ppIntType w <+> ppSymValue l) mbLen
ppStmt (Load r ptr tp a) =
  ppIdent r <+> text "= load" <+> ppPtrType (ppMemType tp) <+> ppSymValue ptr <> ppAlign a
ppStmt (Store tp v addr a) =
  text "store" <+> ppMemType tp <+> ppSymValue v <> comma
               <+> ppSymValue addr <> ppAlign a
ppStmt Unreachable = text "unreachable"
ppStmt (BadSymStmt s) = text (show (L.ppStmt s))

data SymBlock t = SymBlock {
         sbId :: SymBlockID -- ^ Identifier for block (unique within definition).
       , sbStmts :: Vector (SymStmt t)
       }

ppSymBlock :: SymBlock t -> Doc
ppSymBlock sb = ppSymBlockID (sbId sb) <$$> indent 2 (vcat (V.toList (ppStmt <$> sbStmts sb)))

-- | Symbolically lifted version of a LLVM definition.
data SymDefine t = SymDefine {
         sdName :: L.Symbol
       , sdArgs :: [(L.Ident, MemType)]
       , sdRetType :: Maybe MemType
       , sdEntry :: SymBlockID
       , sdBody :: Map SymBlockID (SymBlock t)
       }

lookupSymBlock :: SymDefine t -> SymBlockID -> SymBlock t
lookupSymBlock sd sid =
  case Map.lookup sid (sdBody sd) of
    Nothing  -> error $ "Failed to locate symblock " ++ show (ppSymBlockID sid)
    Just blk -> blk

ppSymDefine :: SymDefine t -> Doc
ppSymDefine d = text "define"
              <+> ppRetType (sdRetType d)
              <+> ppSymbol (sdName d)
              <> parens (commas ((\(i,tp) -> ppMemType tp <+> ppIdent i) <$> sdArgs d))
              <+> char '{'
              <$$> vcat (ppSymBlock <$> Map.elems (sdBody d))
              <$$> char '}'
