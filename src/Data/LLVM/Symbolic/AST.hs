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
module Data.LLVM.Symbolic.AST
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
  , OptVectorLength
  , IntArithOp(..)
  , TypedExpr(..)
  , typedExprType
  , StructInfo(..)
  , mkStructInfo
  , structFieldOffset
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

import Data.LLVM.TargetData

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

 
ppSymValue :: SymValue -> Doc
ppSymValue = LLVM.ppValue

ppTypedValue :: Typed SymValue -> Doc
ppTypedValue = LLVM.ppTyped ppSymValue  -- TODO: See if type information is needed.

type BitWidth = Int 

type Size = Word64
type Offset = Word64

ppIntType :: BitWidth -> Doc
ppIntType i = char 'i' <> integer (toInteger i)

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

data StructInfo = StructInfo { structSize :: Size
                             , structFields :: Vector (L.Type,Offset)
                             }

mkStructInfo :: (?lc :: LLVMContext) => Bool -> [L.Type] -> StructInfo
mkStructInfo packed tpl = StructInfo { structSize = fromInteger (ssiBytes ssi)
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

type NUWFlag = Bool
type NSWFlag = Bool
type ExactFlag = Bool
-- | Condition used to indicate this operation should be applied to
-- a vector (and the number of elements the vector(s) should contains).
type OptVectorLength = Maybe Int

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
    -- | @Trunc iw t rw@ assumes that @rw < iw@, and truncates an integer @t@
    -- with @iw@ bits to an integer with @rw@ bits.
  | Trunc BitWidth v BitWidth
    -- | @TruncV n iw t rw@ assumes that @rw < iw@, and truncates a vector of
    -- integers @t@ with @iw@ bits to a vector of integers with @rw@ bits.
  | TruncV Int BitWidth v BitWidth
    -- | @ZExt iw t rw@ assumes that @iw < rw@, and zero extends an
    -- integer @t@ with @iw@ bits to an integer with @rw@ bits.
  | ZExt BitWidth v BitWidth
    -- | @ZExtV n iw v rw@ assumes that @iw < rw@, and zero extends a
    -- vector of integers @v@ each with @iw@ bits to a vector of integers with
    -- @rw@ bits.
  | ZExtV Int BitWidth v BitWidth
    -- | @SExt iw t rw@ assumes that @iw < rw@, and sign extends an
    -- integer @t@ with @iw@ bits to an integer with @rw@ bits.
  | SExt BitWidth v BitWidth
    -- | @SExtV n iw v rw@ assumes that @iw < rw@, and sign extends a
    -- vector of integers @v@ each with @iw@ bits to a vector of integers with
    -- @rw@ bits.
  | SExtV Int BitWidth v BitWidth
    -- | @PtrToInt tp t rw@ converts a pointer @t@ with type @tp@ to an
    -- integer with width @rw@.  The value of the pointer is truncated or zero
    -- extended as necessary to have the correct length.
  | PtrToInt L.Type v BitWidth
    -- | @PtrToIntV n tp v rw@ converts a vector of pointers @v@ with type
    --  @tp@ to an integer with width @rw@.  The value of each pointer is
    -- truncated or zero extended as necessary to have the correct length.
  | PtrToIntV Int L.Type v BitWidth
    -- | @IntToPtr iw t tp@ converts an integer @t@ with width @iw@ to
    -- a pointer.  The value of the integer is truncated or zero
    -- extended as necessary to have the correct length.
  | IntToPtr BitWidth v L.Type
    -- | @IntToPtrV n iw t tp@ converts a vector of integers @t@ with width
    -- @iw@ to a vector of pointers.  The value of each integer is truncated
    -- or zero extended as necessary to have the correct length.
  | IntToPtrV Int BitWidth v L.Type
    -- | @Bitcast itp t rtp@ converts @t@ from type @itp@ to type @rtp@.
    -- The size of types @itp@ and @rtp@ is assumed to be equal.
  | Bitcast L.Type v L.Type
    -- | GEP instruction
  | GEP Bool v [GEPOffset v] L.Type
  deriving (Functor, Foldable, Traversable)

typedExprType :: TypedExpr v -> L.Type
typedExprType tpe =
  case tpe of
    IntArith _ mn w _ _ -> maybe id arrayLType mn (intLType w)
    IntCmp _ mn _ _ _   -> maybe id arrayLType mn (intLType 1)
    Trunc       _ _ w -> intLType w
    TruncV    n _ _ w -> arrayLType n (intLType w)
    ZExt        _ _ w -> intLType w
    ZExtV     n _ _ w -> arrayLType n (intLType w)
    SExt        _ _ w -> intLType w
    SExtV     n _ _ w -> arrayLType n (intLType w)
    PtrToInt    _ _ w -> intLType w
    PtrToIntV n _ _ w -> arrayLType n (intLType w)
    IntToPtr    _ _ p -> L.PtrTo p
    IntToPtrV n _ _ p -> arrayLType n (L.PtrTo p)
    Bitcast _ _ tp    -> tp
    GEP _ _ _ tp      -> tp

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
      Trunc    iw v rw    -> ppConv "trunc"    (ppIntType iw)      v (ppIntType rw)
      TruncV n iw v rw    -> ppConv "trunc"    (ppIntVector n iw)  v (ppIntVector n rw)
      ZExt     iw v rw    -> ppConv "zext"     (ppIntType iw)      v (ppIntType rw)
      ZExtV  n iw v rw    -> ppConv "zext"     (ppIntVector n iw)  v (ppIntVector n rw)
      SExt     iw v rw    -> ppConv "sext"     (ppIntType iw)      v (ppIntType rw)
      SExtV  n iw v rw    -> ppConv "sext"     (ppIntVector n iw)  v (ppIntVector n rw)
      PtrToInt    tp v rw -> ppConv "ptrtoint" (L.ppType tp)       v (ppIntType rw)
      PtrToIntV n tp v rw -> ppConv "ptrtoint" (ppTypeVector n tp) v (ppIntVector n rw)
      IntToPtr    iw v tp -> ppConv "inttoptr" (ppIntType iw)      v (L.ppType tp)
      IntToPtrV n iw v tp -> ppConv "inttoptr" (ppIntVector n iw)  v (ppTypeVector n tp)
      Bitcast itp v rtp   -> ppConv "bitcast"  (L.ppType itp)      v (L.ppType rtp)
      GEP ib ptr idxl _ -> text "getelementptr" <+> L.opt ib (text "inbounds")
          <+> commas (ppValue ptr : (ppGEPOffset ppValue <$> idxl))

-- | Represents a value in the symbolic simulator.
data TypedSymValue
    -- | Integer width and value.
  = SValInteger Int Integer
  -- | SValBool Bool
  -- Gap
  | SValDouble Double
  -- Gap
  | SValIdent L.Ident 
  -- | Symbol and arguments types if it is a function.
  | SValSymbol L.Symbol (Maybe [L.Type])
  -- | Null pointer value with the type of the element it points to.
  | SValNull L.Type
    -- | Array of values (strings are mapped to 8-bit integer arrays).
  | SValArray L.Type [TypedSymValue]
  | SValStruct [L.Typed TypedSymValue]
  | SValExpr (TypedExpr TypedSymValue)
  | SValUndef L.Type
  | SValZeroInit L.Type

sValString :: String -> TypedSymValue
sValString s = SValArray tp (toChar <$> s)
 where tp = L.PrimType (L.Integer 8)
       toChar c = SValInteger 8 (toInteger (fromEnum c))

ppTypedSymValue :: TypedSymValue -> Doc
ppTypedSymValue = go
  where ppConv nm itp v rtp = text nm <+> parens (itp <+> go v <+> text "to" <+> rtp) 
        go (SValInteger _ i) = integer i
        --go (SValBool b) = L.ppBool b
        go (SValDouble i) = double i 
        go (SValIdent i) = L.ppIdent i
        go (SValSymbol s _) = L.ppSymbol s
        go SValNull{} = text "null"
        go (SValArray ty es) = brackets $ commas ((L.ppTyped go . L.Typed ty) <$> es)
        go (SValStruct fs) = L.structBraces (commas (L.ppTyped go <$> fs))
        go (SValExpr te) = ppTypedExpr ppConv go te
        go (SValUndef _) = text "undef"
        go (SValZeroInit _) = text "zeroinitializer"

-- | Expression in Symbolic instruction set.
-- | TODO: Make this data-type strict.
data SymExpr
  -- | Statement for type-checked operations.
  = TypedExpr L.Type (TypedExpr TypedSymValue)
  -- | @Alloca tp sz align@  allocates a new pointer to @sz@ elements of type
  -- @tp@ with alignment @align@.
  | Alloca LLVM.Type (Maybe (Typed SymValue)) (Maybe Int)
  | Load (Typed SymValue) (Maybe LLVM.Align)
  -- | A copy of a value.
  | Val (Typed SymValue)
  | Select (Typed SymValue) (Typed SymValue) SymValue
  | ExtractValue (Typed SymValue) [Int32]
  | InsertValue (Typed SymValue) (Typed SymValue) [Int32]

-- | Pretty print symbolic expression.
ppSymExpr :: SymExpr -> Doc
ppSymExpr (TypedExpr _ te) = ppTypedExpr ppConv ppTypedSymValue te
  where ppConv nm itp v rtp = text nm <+> itp <+> ppTypedSymValue v <> comma <+> rtp
ppSymExpr (Alloca ty len align) = LLVM.ppAlloca ty len align
ppSymExpr (Load ptr malign) = text "load" <+> ppTypedValue ptr <> LLVM.ppAlign malign
ppSymExpr (Val v) = ppTypedValue v
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

-- | A merge location is a block or Nothing if the merge happens at a return.
type MergeLocation = Maybe SymBlockID

-- | Instruction in symbolic level.
data SymStmt
  -- | @PushCallFrame fn args res retTarget@ pushes a invoke frame to the merge frame stack
  -- that will call @fn@ with @args@, and store the result in @res@ if the function
  -- returns normally.  The calling function will resume execution at retTarget.
  = PushCallFrame TypedSymValue [Typed SymValue] (Maybe (Typed Reg)) SymBlockID
  -- | @Return@ pops top call frame from path, merges (current path return value)
  -- with call frame, and clears current path.
  | Return (Maybe (Typed SymValue))
  -- | @PushPendingExecution tgt c rest@ make the current state a pending execution in the
  -- top-most merge frame with the additional path constraint c, and current block @tgt@.
  -- The final arguments contains the statements to execute with the other path (which 
  -- may assume the negation of the path condition @c@. 
  | PushPendingExecution SymBlockID SymCond MergeLocation [SymStmt]
  -- | Sets the block to the given location.
  | SetCurrentBlock SymBlockID
  -- | Assign result of instruction to register.
  | Assign Reg SymExpr
  -- | @Store v addr@ stores value @v@ in @addr@.
  | Store (L.Typed TypedSymValue) TypedSymValue (Maybe LLVM.Align)
  -- | Print out an error message if we reach an unreachable.
  | Unreachable
  -- | Unwind exception path.
  | Unwind
  -- | An LLVM statement that could not be translated.
  | BadSymStmt L.Stmt
  -- TODO: Support all exception handling.

ppSymStmt :: SymStmt -> Doc
ppSymStmt (PushCallFrame fn args res retTgt)
  = text "pushCallFrame" <+> ppTypedSymValue fn
  <> parens (commas (map ppTypedValue args))
  <+> maybe (text "void") (LLVM.ppTyped ppReg) res
  <+> text "returns to" <+> ppSymBlockID retTgt
--ppSymStmt (PushPostDominatorFrame b) = text "pushPostDominatorFrame" <+> ppSymBlockID b
--ppSymStmt (MergePostDominator b) = text "mergePostDominator" <+> ppSymBlockID b 
ppSymStmt (Return mv) = text "return" <+> maybe empty ppTypedValue mv
ppSymStmt (PushPendingExecution b c ml rest) =
    text "pushPendingExecution" <+> ppSymBlockID b <+> ppSymCond c <+> text "merge" <+> loc
      $+$ vcat (fmap ppSymStmt rest)
  where loc = maybe (text "return") ppSymBlockID ml
ppSymStmt (SetCurrentBlock b) = text "setCurrentBlock" <+> ppSymBlockID b
ppSymStmt (Assign v e) = ppReg v <+> char '=' <+> ppSymExpr e
ppSymStmt (Store v addr malign) =
  text "store" <+> L.ppTyped ppTypedSymValue v <> comma
               <+> ppTypedSymValue addr <> LLVM.ppAlign malign
ppSymStmt Unreachable = text "unreachable"
ppSymStmt Unwind = text "unwind"
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
