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
  , Reg
  , SymValue
  , sValString
  , TypedSymValue(..)
  , ppTypedSymValue
  , BitWidth
  , NUWFlag
  , NSWFlag
  , ExactFlag
  , IntArithOp(..)
  , OptVectorLength
  , TypedExpr(..)
  , typedExprType
  , StructInfo(..)
  , Offset
  , Size
  , mkStructInfo
  , structFieldOffset
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
  ) where

import Control.Applicative ((<$>))
import Control.Exception (assert)
import Data.Foldable
import Data.Traversable
import Data.Int
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import qualified Text.LLVM.AST as LLVM
import qualified Text.LLVM.AST as L
import Text.PrettyPrint.HughesPJ

import Verifier.LLVM.LLVMContext

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

type SymValue = L.Value

type BitWidth = Int 

type Size = Word64
type Offset = Word64


ppIntType :: BitWidth -> Doc
ppIntType i = char 'i' <> integer (toInteger i)

ppPtrType :: L.Type -> Doc
ppPtrType tp = L.ppType tp <> char '*'

ppVector :: Int -> Doc -> Doc
ppVector n e = L.angles (int n <+> char 'x' <+> e)

ppIntVector :: Int -> BitWidth -> Doc
ppIntVector n w = ppVector n (ppIntType w)

ppTypeVector :: Int -> L.Type -> Doc
ppTypeVector n w = ppVector n (L.ppType w)

intLType :: BitWidth -> L.Type
intLType w = L.PrimType (L.Integer (fromIntegral w))

arrayLType :: Int -> L.Type -> L.Type
arrayLType n tp = L.Array (fromIntegral n) tp

-- | Information about structs.  Offsets and size is in bytes.
data StructInfo = StructInfo { structPacked :: !Bool
                             , structSize :: !Size
                               
                             , structFields :: !(Vector (L.Type,Offset))
                             }

structInfoType :: StructInfo -> L.Type
structInfoType si
    | structPacked si = L.PackedStruct flds
    | otherwise = L.Struct flds
  where flds = V.toList $ fst <$> structFields si

mkStructInfo :: (?lc :: LLVMContext) => Bool -> [L.Type] -> StructInfo
mkStructInfo packed tpl = StructInfo { structPacked = packed
                                     , structSize = fromInteger (ssiBytes ssi)
                                     , structFields = V.fromList flds
                                     }
  where ssi = llvmStructInfo' ?lc packed tpl
        flds = zip tpl (fromInteger <$> ssiOffsets ssi)

structFieldOffset :: StructInfo -> Int -> Offset
structFieldOffset si i = assert (i < V.length f) o
  where f = structFields si
        (_,o) = f V.! i

data GEPOffset v
     -- | Returns the address of a field in a struct.
   = StructField StructInfo Int
     -- | @ArrayElement sz w i@ denotes the ith value in an array with
     -- elements @sz@ wide.  The value @i@ has @w@ bits.
   | ArrayElement Integer BitWidth v
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
  | PtrToInt OptVectorLength L.Type v BitWidth
    -- | @IntToPtr iw t tp@ converts an integer @t@ with width @iw@ to
    -- a pointer.  The value of the integer is truncated or zero
    -- extended as necessary to have the correct length.
  | IntToPtr OptVectorLength BitWidth v L.Type
    -- | @Select mn c tp x y@ selects the arguments in @x@ if @c@ evaluated to @1@ and
    -- @y@ if @c@ evaluates to @0@.   @c@ must have type @i1@ and @x@ and @y@ have type
    -- @tp@.  The function is extended pairwise to vectors if @mn@ holds an integer. 
  | Select OptVectorLength v L.Type v v
    -- | @Bitcast itp t rtp@ converts @t@ from type @itp@ to type @rtp@.
    -- The size of types @itp@ and @rtp@ is assumed to be equal.
  | Bitcast L.Type v L.Type
    -- | GEP instruction
  | GEP Bool v [GEPOffset v] L.Type
    -- | Return a field out of a struct 
  | GetStructField StructInfo v Int
    -- | Return a specific elemnt of an array.
  | GetConstArrayElt L.Type v Int32
    -- | Integer width and value.
  | SValInteger BitWidth Integer
  | SValFloat  Float
  | SValDouble Double
    -- | Null pointer value with the width of the pointer and the type of the element
    -- that it points to.
  | SValNull BitWidth L.Type
    -- | Array of values (strings are mapped to 8-bit integer arrays).
  | SValArray L.Type (Vector v)
  | SValStruct StructInfo (Vector v)
    -- | Vector element with given values.
  | SValVector L.Type (Vector v)
 deriving (Functor, Foldable, Traversable)

typedExprType :: TypedExpr v -> L.Type
typedExprType tpe =
  case tpe of
    IntArith _ mn w _ _ -> maybe id arrayLType mn (intLType w)
    IntCmp _ mn _ _ _   -> maybe id arrayLType mn (intLType 1)
    Trunc    mn _ _ w -> maybe id arrayLType mn (intLType w)
    ZExt     mn _ _ w -> maybe id arrayLType mn (intLType w)
    SExt     mn _ _ w -> maybe id arrayLType mn (intLType w)
    PtrToInt mn _ _ w -> maybe id arrayLType mn (intLType w)
    IntToPtr mn _ _ p -> maybe id arrayLType mn (L.PtrTo p)
    Select mn _ tpv _ _ -> maybe id arrayLType mn tpv
    Bitcast _ _ tp    -> tp
    GEP _ _ _ tp      -> tp
    GetStructField si _ i -> fst (structFields si V.! i)
    GetConstArrayElt tp _ _ -> tp
    SValInteger w _ -> intLType w
    SValDouble{} -> L.PrimType (L.FloatType L.Double)
    SValFloat{} -> L.PrimType (L.FloatType L.Float)
    SValNull _ tp -> L.PtrTo tp
    SValArray tp l -> L.Array (fromIntegral (V.length l)) tp
    SValStruct si _ -> structInfoType si
    SValVector tp l -> L.Vector (fromIntegral (V.length l)) tp

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
      IntCmp op mn w x y ->
         text "icmp" <+> L.ppICmpOp op <+> tp <+> ppValue x <> comma <+> ppValue y
       where tp  = maybe ppIntType ppIntVector mn w

      Trunc mn iw v rw    -> ppConv "trunc"    (ppMIntType mn iw) v (ppMIntType mn rw)
      ZExt  mn iw v rw    -> ppConv "zext"     (ppMIntType mn iw) v (ppMIntType mn rw)
      SExt  mn iw v rw    -> ppConv "sext"     (ppMIntType mn iw) v (ppMIntType mn rw)
      PtrToInt mn tp v rw -> ppConv "ptrtoint" (ppMType mn tp)    v (ppMIntType mn rw)
      IntToPtr mn iw v tp -> ppConv "inttoptr" (ppMIntType mn iw) v (ppMType mn tp)
      Select mn c tp t f -> text "select" <+> ppMIntType mn 1 <+> ppValue  c
                              <> comma <+> ppMType mn tp <+> ppValue t
                              <> comma <+> ppValue f
      Bitcast itp v rtp   -> ppConv "bitcast"  (L.ppType itp)     v (L.ppType rtp)
      GEP ib ptr idxl _ -> text "getelementptr" <+> L.opt ib (text "inbounds")
          <+> commas (ppValue ptr : (ppGEPOffset ppValue <$> idxl))
      GetStructField _ v i -> text "extractfield" <+> ppValue v <+> text (show i)
      GetConstArrayElt _ v i -> text "arrayelt" <+> ppValue v <+> text (show i)
      SValInteger _ i -> integer i
      SValDouble i -> double i 
      SValFloat i -> float i 
      SValNull{} -> text "null"
      SValArray _ es -> brackets $ commas $ V.toList $ ppValue <$> es
      SValStruct si values -> L.structBraces (commas fl)
        where fn (tp,_) v = L.ppType tp <+> ppValue v
              fl = V.toList $ V.zipWith fn (structFields si) values
      SValVector _ es -> brackets $ commas $ V.toList $ ppValue <$> es
  where ppMIntType Nothing w = ppIntType w
        ppMIntType (Just n) w = ppIntVector n w
        ppMType Nothing tp = L.ppType tp
        ppMType (Just n) tp = ppTypeVector n tp

-- | Represents a value in the symbolic simulator.
data TypedSymValue
    -- | Register identifier.
  = SValIdent L.Ident 
     -- | Symbol 
  | SValSymbol L.Symbol
  | SValExpr (TypedExpr TypedSymValue)

sValString :: String -> TypedSymValue
sValString s = SValExpr $ SValArray tp (toChar <$> V.fromList s)
 where tp = L.PrimType (L.Integer 8)
       toChar c = SValExpr $ SValInteger 8 (toInteger (fromEnum c))

ppTypedSymValue :: TypedSymValue -> Doc
ppTypedSymValue = go
  where ppConv nm itp v rtp = text nm <+> parens (itp <+> go v <+> text "to" <+> rtp) 
        go (SValIdent i) = L.ppIdent i
        go (SValSymbol s) = L.ppSymbol s
        go (SValExpr te) = ppTypedExpr ppConv go te
--        go (SValStruct fs) = L.structBraces (commas (L.ppTyped go <$> fs))
--        go (SValUndef _) = text "undef"
--        go (SValZeroInit _) = text "zeroinitializer"

-- | Expression in Symbolic instruction set.
-- | TODO: Make this data-type strict.
data SymExpr
  -- | Statement for type-checked operations.
  -- = TypedExpr (TypedExpr TypedSymValue)
  = Val TypedSymValue
  -- | @Alloca tp sz align@  allocates a new pointer to @sz@ elements of type
  -- @tp@ with alignment @align@.
  | Alloca LLVM.Type (Maybe (BitWidth, TypedSymValue)) (Maybe Int)
    -- @Load ptr tp malign@ tp is type to load.
  | Load TypedSymValue L.Type (Maybe LLVM.Align)

-- | Pretty print symbolic expression.
ppSymExpr :: SymExpr -> Doc
ppSymExpr (Val v) = ppTypedSymValue v
ppSymExpr (Alloca ty mbLen mbAlign) = text "alloca" <+> L.ppType ty <> len <> align
  where len   = maybe empty (\(w,l) -> comma <+> ppIntType w <+> ppTypedSymValue l) mbLen
        align = maybe empty (\a -> comma <+> text "align" <+> int a) mbAlign
ppSymExpr (Load ptr tp malign) =
  text "load" <+> ppPtrType tp <+> ppTypedSymValue ptr <> L.ppAlign malign

-- | Predicates in symbolic simulator context.
data SymCond
  -- | @HasConstValue v w i@ holds if @v@ corresponds to the constant @i@.
  = HasConstValue TypedSymValue BitWidth Integer
  -- | @TrueSymCond@ always holds.
  | TrueSymCond

-- | Pretty print symbolic condition.
ppSymCond :: SymCond -> Doc
ppSymCond (HasConstValue v _ i) = ppTypedSymValue v <+> text "==" <+> integer i
ppSymCond TrueSymCond = text "true"

-- | A merge location is a block or Nothing if the merge happens at a return.
type MergeLocation = Maybe SymBlockID

-- | Instruction in symbolic level.
data SymStmt
  -- | @PushCallFrame fn args res retTarget@ pushes a invoke frame to the merge frame stack
  -- that will call @fn@ with @args@, and store the result in @res@ if the function
  -- returns normally.  The calling function will resume execution at retTarget.
  = PushCallFrame TypedSymValue [TypedSymValue] (Maybe (Typed Reg)) SymBlockID
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
  | Assign (L.Typed Reg) SymExpr
  -- | @Store v addr@ stores value @v@ in @addr@.
  | Store (L.Typed TypedSymValue) TypedSymValue (Maybe LLVM.Align)
  -- | Print out an error message if we reach an unreachable.
  | Unreachable
  -- | An LLVM statement that could not be translated.
  | BadSymStmt L.Stmt
  -- TODO: Support all exception handling.

ppSymStmt :: SymStmt -> Doc
ppSymStmt (PushCallFrame fn args res retTgt)
  = text "pushCallFrame" <+> ppTypedSymValue fn
  <> parens (commas (map ppTypedSymValue args))
  <+> maybe (text "void") (LLVM.ppTyped ppReg) res
  <+> text "returns to" <+> ppSymBlockID retTgt
ppSymStmt (Return mv) = text "return" <+> maybe empty ppTypedSymValue mv
ppSymStmt (PushPendingExecution b c ml rest) =
    text "pushPendingExecution" <+> ppSymBlockID b <+> ppSymCond c <+> text "merge" <+> loc
      $+$ vcat (fmap ppSymStmt rest)
  where loc = maybe (text "return") ppSymBlockID ml
ppSymStmt (SetCurrentBlock b) = text "setCurrentBlock" <+> ppSymBlockID b
ppSymStmt (Assign (L.Typed _ v) e) = ppReg v <+> char '=' <+> ppSymExpr e
ppSymStmt (Store v addr malign) =
  text "store" <+> L.ppTyped ppTypedSymValue v <> comma
               <+> ppTypedSymValue addr <> LLVM.ppAlign malign
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
