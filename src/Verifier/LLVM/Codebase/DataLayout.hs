{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Verifier.LLVM.Codebase.DataLayout
  ( Size
  , Offset
  , Alignment
    -- * Utilities
  , structBraces
    -- * Data layout declarations.
  , DataLayout
  , EndianForm(..)
  , intLayout
  , maxAlignment
  , ptrSize
  , ptrAlign
  , ptrBitwidth
  , defaultDataLayout
  , parseDataLayout
  , memTypeAlign
  , memTypeSize

    -- * Type information.
  , SymType(..)
  , ppSymType
    -- ** MemType
  , MemType(..)
  , ppMemType
  , i1, i8, i16, i32, i64
  , i8p, i16p, i32p, i64p
    -- ** Function type information.
  , FunDecl(..)
  , RetType
  , voidFunDecl
  , funDecl
  , varArgsFunDecl
  , ppFunDecl
  , ppRetType

    -- ** Struct type information.
  , StructInfo
  , siIsPacked
  , mkStructInfo
  , siFieldCount
  , FieldInfo
  , fiOffset
  , fiType
  , fiPadding
  , siFieldInfo
  , siFieldTypes
  , siFieldOffset
  , siFields
  , siIndexOfOffset
  , siDropLastField
    -- * Re-exports
  , L.Ident(..)
  , ppIdent
  ) where

import Control.Applicative ((<$>))
import Control.Lens
import Control.Monad.State.Strict
import qualified Data.FingerTree as FT
import Data.Maybe
import Data.Monoid (Monoid(..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word32, Word64)
import qualified Text.LLVM as L
import Text.PrettyPrint.Leijen hiding ((<$>))
import Verifier.LLVM.Utils

ppIdent :: L.Ident -> Doc
ppIdent = text . show . L.ppIdent

-- | Performs a binary search on a range of ints.
binarySearch :: (Int -> Ordering)
             -> Int -- ^ Lower bound (included in range)
             -> Int -- ^ Upper bound (excluded from range)
             -> Maybe Int
binarySearch f = go
  where go l h | l == h = Nothing
               | otherwise = case f i of
                               -- Index is less than one f is searching for
                               LT -> go (i+1) h
                               EQ -> Just i
                               -- Index is greater than one f is searching for.
                               GT -> go l i
          where i = l + (h - l) `div` 2

-- | Size is in bytes unless bits is explicitly stated.
type Size = Word64

type Offset = Word64

-- | Alignment's must be a power of two, so we just store the exponent.
-- e.g., alignment value of 3 indicates the pointer must align on 2^3-byte boundaries.
type Alignment = Word32


newtype BW = BW Word64
  deriving (Eq, Ord, Num)

instance Monoid BW where
  mempty                = BW 0
  mappend (BW a) (BW b) = BW (max a b)

data AlignSpec =
  AlignSpec { -- Size in bits
              asBitWidth :: Int
              -- Alignment in bytes (this is the ABI value).
            , asAlign    :: Alignment
            }

instance FT.Measured BW AlignSpec where
  measure = fromIntegral . asBitWidth

newtype AlignTree = AT (FT.FingerTree BW AlignSpec)

-- | Make alignment tree from sorted list.
emptyAlignTree :: AlignTree
emptyAlignTree = AT FT.empty

-- | Return alignment exactly at point if any.
findExact :: Int -> AlignTree -> Maybe Alignment
findExact w (AT t) =
    case FT.viewl mge of
     e FT.:< _ | asBitWidth e == w -> Just (asAlign e)
     _ -> Nothing
  where mge = snd $ FT.split (>= fromIntegral w) t

-- | Find match for alignment using LLVM's rules for integer types.
findIntMatch :: Int -> AlignTree -> Maybe Alignment
findIntMatch w (AT t) = 
    case FT.viewl mge of
     e FT.:< _ -> Just (asAlign e)
     FT.EmptyL ->
       case FT.viewr lt of
         _ FT.:> e -> Just (asAlign e)
         FT.EmptyR -> Nothing
  where (lt, mge) = FT.split (>= fromIntegral w) t

-- | Return maximum alignment constriant stored in tree.
maxAlignmentInTree :: AlignTree -> Alignment
maxAlignmentInTree (AT t) = foldrOf folded (max . asAlign) 0 t

-- | Update alignment tree
updateAlign :: Int
            -> AlignTree
            -> Maybe Alignment
            -> AlignTree
updateAlign w (AT t) ma = AT $ 
    case FT.split (> fromIntegral w) t of
      (FT.viewr -> lt FT.:> ml, gt) | asBitWidth ml == w -> merge lt gt
      (mle, gt) -> merge mle gt
  where merge l r = (FT.><) l (maybe r (\a -> AlignSpec w a FT.<| r) ma)

type instance Index AlignTree = Int
type instance IxValue AlignTree = Alignment
 
instance At AlignTree where
  at k f m = updateAlign k m <$> indexed f k (findExact k m)

-- | Flags byte orientation of target machine.
data EndianForm = BigEndian | LittleEndian

-- | Parsed data layout
data DataLayout
   = DL { _intLayout :: EndianForm
        , _stackAlignment :: !Alignment
          -- | Size of pointers in bytes.
        , _ptrSize     :: !Size
           -- | ABI pointer alignment in bytes.
        , _ptrAlign    :: !Alignment
        , _integerInfo :: !AlignTree
        , _vectorInfo  :: !AlignTree
        , _floatInfo   :: !AlignTree
           -- | Information about aggregate size.
        , _aggInfo     :: !AlignTree
           -- | Layout constraints on a stack object with the given size.
        , _stackInfo   :: !AlignTree
          -- | Layout specs that could not be parsed.
        , _layoutWarnings :: [L.LayoutSpec]
        }

makeLenses ''DataLayout

ptrBitwidth :: DataLayout -> BitWidth
ptrBitwidth dl = 8 * fromIntegral (dl^.ptrSize)

-- | Reduce the bit level alignment to a byte value, and error if it is not
-- a multiple of 8.
fromBits :: Int -> Either String Alignment
fromBits a | w <= 0 = Left $ "Alignment must be a positive number."
           | r /= 0 = Left $ "Alignment specification must occupy a byte boundary."
           | not (isPow2 w) = Left $ "Alignment must be a power of two."
           | otherwise = Right $ fromIntegral (lg w)
  where (w,r) = toInteger a `divMod` 8

-- | Insert alignment into spec.
setAt :: Simple Lens DataLayout AlignTree -> Int -> Alignment -> State DataLayout ()
setAt f sz a = f . at sz ?= a

-- | Get default data layout if no spec is defined.
defaultDataLayout :: DataLayout
defaultDataLayout = execState defaults dl
  where dl = DL { _intLayout = BigEndian
                , _stackAlignment = 1
                , _ptrSize  = 8 -- 64 bit pointers
                , _ptrAlign = 8 -- 64 bit alignment
                , _integerInfo = emptyAlignTree
                , _floatInfo   = emptyAlignTree
                , _vectorInfo  = emptyAlignTree
                , _aggInfo     = emptyAlignTree
                , _stackInfo   = emptyAlignTree
                , _layoutWarnings = []
                }
        defaults = do
          -- Default integer alignments
          setAt integerInfo  1 0 -- 1-bit values aligned on byte addresses.
          setAt integerInfo  8 0 -- 8-bit values aligned on byte addresses.
          setAt integerInfo 16 1 -- 16-bit values aligned on 2 byte addresses.
          setAt integerInfo 32 2 -- 32-bit values aligned on 4 byte addresses.
          setAt integerInfo 64 2 -- 64-bit balues aligned on 4 byte addresses.
          -- Default float alignments
          setAt floatInfo  16 1 -- Half is aligned on 2 byte addresses.
          setAt floatInfo  32 2 -- Float is aligned on 4 byte addresses.
          setAt floatInfo  64 3 -- Double is aligned on 8 byte addresses.
          setAt floatInfo 128 4 -- Quad is aligned on 16 byte addresses.
          -- Default vector alignments.
          setAt vectorInfo  64 3 -- 64-bit vector is 8 byte aligned.
          setAt vectorInfo 128 4  -- 128-bit vector is 16 byte aligned.
          -- Default aggregate alignments.
          setAt aggInfo  0 3  -- Aggregates are 8 byte aligned.

-- | Maximum aligment for any type (used by malloc).
maxAlignment :: DataLayout -> Alignment
maxAlignment dl =
  maximum [ dl^.stackAlignment
          , dl^.ptrAlign
          , maxAlignmentInTree (dl^.integerInfo)
          , maxAlignmentInTree (dl^.vectorInfo)
          , maxAlignmentInTree (dl^.floatInfo)
          , maxAlignmentInTree (dl^.aggInfo)
          , maxAlignmentInTree (dl^.stackInfo)
          ]

-- | Insert alignment into spec.
setAtBits :: Simple Lens DataLayout AlignTree -> L.LayoutSpec -> Int -> Int -> State DataLayout ()
setAtBits f spec sz a = 
  case fromBits a of
    Left{} -> layoutWarnings %= (spec:)
    Right w -> f . at sz .= Just w

-- | Insert alignment into spec.
setBits :: Simple Lens DataLayout Alignment -> L.LayoutSpec -> Int -> State DataLayout ()
setBits f spec a = 
  case fromBits a of
    Left{} -> layoutWarnings %= (spec:)
    Right w -> f .= w

-- | Add information from layout spec into parsed data layout.
addLayoutSpec :: L.LayoutSpec -> State DataLayout ()
addLayoutSpec ls =
  -- TODO: Check that sizes and alignment is using bits versus bytes consistently.
    case ls of
      L.BigEndian    -> intLayout .= BigEndian
      L.LittleEndian -> intLayout .= LittleEndian
      L.PointerSize     sz a _ ->
         case fromBits a of
           Right a' | r == 0 -> do ptrSize .= w
                                   ptrAlign .= a'
           _ -> layoutWarnings %= (ls:)
       where (w,r) = fromIntegral sz `divMod` 8
      L.IntegerSize    sz a _ -> setAtBits integerInfo ls sz a
      L.VectorSize     sz a _ -> setAtBits vectorInfo  ls sz a
      L.FloatSize      sz a _ -> setAtBits floatInfo   ls sz a
      L.AggregateSize  sz a _ -> setAtBits aggInfo     ls sz a
      L.StackObjSize   sz a _ -> setAtBits stackInfo   ls sz a
      L.NativeIntSize _ -> return ()
      L.StackAlign a    -> setBits stackAlignment ls a

-- | Create parsed data layout from layout spec AST.
parseDataLayout :: L.DataLayout -> DataLayout
parseDataLayout dl =
  execState (mapM_ addLayoutSpec dl) defaultDataLayout

-- | Type supported by symbolic simulator.
data SymType
  = MemType MemType
  | Alias L.Ident
  | FunType FunDecl
  | VoidType
    -- | A type that LLVM does not know the structure of such as
    -- a struct that is declared, but not defined.
  | OpaqueType
    -- | A type not supported by the symbolic simulator.
  | UnsupportedType L.Type

ppSymType :: SymType -> Doc
ppSymType (MemType tp) = ppMemType tp
ppSymType (Alias i) = ppIdent i
ppSymType (FunType d) = ppFunDecl d
ppSymType VoidType = text "void"
ppSymType OpaqueType = text "opaque"
ppSymType (UnsupportedType tp) = text (show (L.ppType tp)) 

-- | LLVM Types supported by simulator with a defined size and alignment.
data MemType
  = IntType BitWidth
  | PtrType SymType
  | FloatType
  | DoubleType
  | ArrayType Int MemType
  | VecType Int MemType
  | StructType StructInfo

ppMemType :: MemType -> Doc
ppMemType mtp = 
  case mtp of
    IntType w -> ppIntType w
    FloatType -> text "float"
    DoubleType -> text "double"
    PtrType tp -> ppPtrType (ppSymType tp)
    ArrayType n tp -> ppArrayType n (ppMemType tp)
    VecType n tp  -> ppVectorType n (ppMemType tp)
    StructType si -> ppStructInfo si

i1, i8, i16, i32, i64 :: MemType
i1     = IntType 1
i8     = IntType 8
i16    = IntType 16
i32    = IntType 32
i64    = IntType 64

i8p, i16p, i32p, i64p :: MemType
i8p    = PtrType (MemType i8)
i16p   = PtrType (MemType i16)
i32p   = PtrType (MemType i32)
i64p   = PtrType (MemType i64)

-- | Alignment restriction in bytes.
data FunDecl = FunDecl { fdRetType  :: !RetType
                       , fdArgTypes :: ![MemType]
                       , fdVarArgs  :: !Bool
                       }
-- | Return type if any.
type RetType = Maybe MemType

-- | Declare function that returns void
voidFunDecl :: [MemType] -> FunDecl
voidFunDecl tps = FunDecl { fdRetType = Nothing
                          , fdArgTypes = tps
                          , fdVarArgs = False
                          }

-- | Declare function that returns a value.
funDecl :: MemType -> [MemType] -> FunDecl
funDecl rtp tps = FunDecl { fdRetType = Just rtp
                          , fdArgTypes = tps
                          , fdVarArgs = False
                          }

-- | Declare function that returns a value.
varArgsFunDecl :: MemType -> [MemType] -> FunDecl
varArgsFunDecl rtp tps = FunDecl { fdRetType = Just rtp
                                 , fdArgTypes = tps
                                 , fdVarArgs = True
                                 }

ppFunDecl :: FunDecl -> Doc
ppFunDecl (FunDecl rtp args va) = rdoc <> parens (commas (fmap ppMemType args ++ vad))
  where rdoc = maybe (text "void") ppMemType rtp
        vad = if va then [text "..."] else []

-- | Pretty print return type.
ppRetType :: RetType -> Doc
ppRetType = maybe (text "void") ppMemType

intWidthSize ::  BitWidth -> Size
intWidthSize w = (fromIntegral w + 7) `div` 8 -- Convert bits to bytes.

-- | Returns size of MemType in bytes.
memTypeSize :: DataLayout -> MemType -> Size
memTypeSize dl mtp =
  case mtp of
    IntType w -> intWidthSize w
    FloatType -> 4
    DoubleType -> 8
    PtrType{} -> dl ^. ptrSize
    ArrayType n tp -> fromIntegral n * memTypeSize dl tp
    VecType n tp -> fromIntegral n * memTypeSize dl tp
    StructType si -> structSize si

memTypeSizeInBits :: DataLayout -> MemType -> BitWidth
memTypeSizeInBits dl tp = fromIntegral $ 8 * memTypeSize dl tp

-- | Returns ABI byte alignment constraint in bytes.
memTypeAlign :: DataLayout -> MemType -> Alignment
memTypeAlign dl mtp =
  case mtp of
    IntType w -> a
      where Just a = findIntMatch (fromIntegral w) (dl ^. integerInfo)
    FloatType -> a
      where Just a = findExact 32 (dl ^. floatInfo)
    DoubleType -> a
      where Just a = findExact 64 (dl ^. floatInfo)
    PtrType{} -> dl ^. ptrAlign
    ArrayType _ tp -> memTypeAlign dl tp
    VecType n tp   -> 
      case findExact (memTypeSizeInBits dl tp) (dl^.vectorInfo) of
        Just a -> a
        Nothing -> fromIntegral (lgCeil n) + memTypeAlign dl tp
    StructType si  -> structAlign si

-- | Information about structs.  Offsets and size is in bytes.
data StructInfo = StructInfo { siDataLayout :: !DataLayout
                             , siIsPacked   :: !Bool
                             , structSize   :: !Size
                             , structAlign  :: !Alignment
                             , siFields     :: !(V.Vector FieldInfo)
                             }

data FieldInfo = FieldInfo { fiOffset    :: !Offset
                           , fiType      :: !MemType
                             -- | Number of bytes of padding at end of field.
                           , fiPadding   :: !Size
                           }

-- | Constructs a function for obtaining target-specific size/alignment
-- information about structs.  The function produced corresponds to the
-- StructLayout object constructor in TargetData.cpp.
mkStructInfo :: DataLayout -> Bool -> [MemType] -> StructInfo
mkStructInfo dl packed tps0 = go [] 0 (max a0 (nextAlign a0 tps0)) tps0 
  where a0 | packed = 0
           | otherwise = fromMaybe 1 (findExact 0 (dl^.aggInfo))
        -- Aligment of next type if any.
        nextAlign :: Alignment -> [MemType] -> Alignment
        nextAlign _ _ | packed = 0
        nextAlign a [] = a
        nextAlign _ (tp:_) = memTypeAlign dl tp
        -- Process fields
        go :: [FieldInfo] -- ^ Fields so far in reverse order.
           -> Size        -- ^ Total size so far (aligned to next element)
           -> Alignment   -- ^ Maximum alignment
           -> [MemType]   -- ^ Fields to process
           -> StructInfo
        go flds sz maxAlign [] =
            StructInfo { siDataLayout = dl
                       , siIsPacked = packed
                       , structSize = sz
                       , structAlign = maxAlign
                       , siFields = V.fromList (reverse flds)
                       }
        go flds sz a (tp:tpl) = go (fi:flds) sz' (max a a') tpl 
          where fi = FieldInfo { fiOffset = sz
                               , fiType = tp
                               , fiPadding = sz' - e
                               }
                -- End of field for tp
                e = sz + memTypeSize dl tp
                -- Alignment of next field
                a' = nextAlign a tpl
                -- Size of field at alignment for next thing.
                sz' = nextPow2Multiple e (fromIntegral a')

siFieldTypes :: StructInfo -> Vector MemType
siFieldTypes si = fiType <$> siFields si

siFieldCount :: StructInfo -> Int
siFieldCount = V.length . siFields

-- | Returns inforation at given field if int is a valid index.
siFieldInfo :: StructInfo -> Int -> Maybe FieldInfo
siFieldInfo si i = siFields si V.!? i

-- | Returns offset of field if it is defined.
siFieldOffset :: StructInfo -> Int -> Maybe Offset
siFieldOffset si i = fiOffset <$> siFieldInfo si i

-- | Returns index of field at the given byte offset (if any).
siIndexOfOffset :: StructInfo -> Offset -> Maybe Int
siIndexOfOffset si o = binarySearch f 0 (V.length flds)
  where flds = siFields si
        f i | e <= o = LT -- Index too low if field ends before offset.
            | o < s  = GT -- Index too high if field starts after offset.
            | otherwise = EQ
         where s = fiOffset (flds V.! i)
               e | i+1 == V.length flds = structSize si
                 | otherwise = fiOffset (flds V.! i)

commas :: [Doc] -> Doc
commas = hsep . punctuate (char ',')

structBraces :: Doc -> Doc
structBraces b = char '{' <+> b <+> char '}'

-- | Pretty print struct info.
ppStructInfo :: StructInfo -> Doc
ppStructInfo si = structBraces $ commas (V.toList fields)
  where fields = ppMemType <$> siFieldTypes si

-- | Removes the last field from a struct if at least one field exists.
siDropLastField :: StructInfo -> Maybe (StructInfo, FieldInfo)
siDropLastField si
  | V.null (siFields si) = Nothing
  | otherwise = Just (si', V.last (siFields si))
 where si' = mkStructInfo (siDataLayout si) (siIsPacked si) flds'
       flds' = V.toList $ V.init $ siFieldTypes si