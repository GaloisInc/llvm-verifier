{- |
Module           : $Header$
Description      : Provides information about types in an LLVM Module.
Stability        : provisional
Point-of-contact : jhendrix
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
module Verifier.LLVM.LLVMContext
  ( Size
  , Offset
  , ParsedDataLayout(..)
  , pdlPtrBitwidth
  , SymType(..)
  , ppSymType
  , MemType(..)
  , ppMemType
  , RetType
  , ppRetType
  , StructInfo
  , FieldInfo(..)
  , mkStructInfo
  , siFields
  , siFieldInfo
  , siFieldTypes
  , siFieldType
  , siFieldOffset
  , FunDecl(..)
  , voidFunDecl
  , funDecl
  , varArgsFunDecl
  , ppFunDecl
  , memTypeSize
  , LLVMContext
  , llvmDataLayout
  , llvmPtrSize
  , llvmPtrAlign
  , llvmTypeAliasMap
  , llvmAddrWidthBits
  , llvmLookupAlias
  , llvmLookupAlias'
  , buildLLVMContext
  , liftMemType
  , liftRetType
  , asMemType
  , asRetType
  , Addr
  , MemGeom(..)
  , defaultMemGeom
  ) where

import Control.Applicative
import Control.Exception (assert)
import Control.Lens
import Control.Monad.State (State, runState, MonadState(..), modify)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import qualified Data.FingerTree   as FT
import           Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Text.LLVM         as L
import Text.PrettyPrint

import Verifier.LLVM.Utils


data EndianForm = BigEndian | LittleEndian

-- | Size is in bytes unless bits is explicitly stated.
type Size = Word64
type Offset = Word64

-- | Alignment restriction in bytes.
type Alignment = Word64

newtype BW = BW Size
  deriving (Eq, Ord, Num)

instance Monoid BW where
  mempty                = BW 0
  mappend (BW a) (BW b) = BW (max a b)

data AlignSpec =
  AlignSpec { -- Size in bits
              asBitWidth :: BW
              -- Alignment in bytes (this is the ABI value not the "preferred")
            , asAlign    :: Alignment
            }

instance FT.Measured BW AlignSpec where
  measure = asBitWidth

type AlignTree = FT.FingerTree BW AlignSpec

-- | Make alignment tree from sorted list.
alignTree :: [AlignSpec] -> AlignTree
alignTree = FT.fromList

-- | Insert spec into align tree.
insertAlign :: AlignSpec -> AlignTree -> AlignTree
insertAlign s t =
    case FT.viewr mle of
      lt FT.:> ml | asBitWidth ml == sw -> (lt FT.|> s) FT.>< gt
      _ -> (mle FT.|> s) FT.>< gt
  where sw = asBitWidth s
        (mle, gt) = FT.split (> sw) t


findExact :: BW -> AlignTree -> Maybe Alignment
findExact w t =
    case FT.viewl mge of
     e FT.:< _ | asBitWidth e == w -> Just (asAlign e)
     _ -> Nothing
  where mge = snd $ FT.split (>= w) t


findIntMatch :: BW -> AlignTree -> Maybe Alignment
findIntMatch w t = 
    case FT.viewl mge of
     e FT.:< _ -> Just (asAlign e)
     FT.EmptyL ->
       case FT.viewr lt of
         _ FT.:> e -> Just (asAlign e)
         FT.EmptyR -> Nothing
  where (lt, mge) = FT.split (>= w) t

-- | Parsed data layout
data ParsedDataLayout
   = PDL { pdlEndian :: EndianForm
         , pdlStackAlignment :: !Alignment
           -- | Size of pointers in bytes.
         , pdlPtrSize     :: !Size
           -- | ABI pointer alignment in bytes.
         , pdlPtrAlign    :: !Alignment
         , pdlIntegerInfo :: !AlignTree
         , pdlVectorInfo  :: !AlignTree
         , pdlFloatInfo   :: !AlignTree
           -- | Information abour aggregate size.
         , pdlAggInfo     :: !AlignTree
           -- | Layout constraints on a stack object with the given size.
         , pdlStackInfo   :: !AlignTree
         }

pdlPtrBitwidth :: ParsedDataLayout -> BitWidth
pdlPtrBitwidth pdl = 8 * fromIntegral (pdlPtrSize pdl)

-- | Add information from layout spec into parsed data layout.
addLayoutSpec :: L.LayoutSpec -> ParsedDataLayout -> ParsedDataLayout
addLayoutSpec ls pdl =
  -- TODO: Check that sizes and alignment is using bits versus bytes consistently.
    case ls of
      L.BigEndian    -> pdl { pdlEndian = BigEndian }
      L.LittleEndian -> pdl { pdlEndian = LittleEndian }
      L.PointerSize     sz a _ 
         | r  /= 0 -> error $ "Pointers must occupy an even number of bytes."
         | otherwise -> pdl { pdlPtrSize  = w
                            , pdlPtrAlign = alignFromBits "Pointer" a
                            }
       where (w,r) = fromIntegral sz `divMod` 8
      L.IntegerSize    sz a _ -> pdl { pdlIntegerInfo = ins sz a pdlIntegerInfo }
      L.VectorSize     sz a _ -> pdl { pdlVectorInfo  = ins sz a pdlVectorInfo  }
      L.FloatSize      sz a _ -> pdl { pdlFloatInfo   = ins sz a pdlFloatInfo   }
      L.AggregateSize  sz a _ -> pdl { pdlAggInfo     = ins sz a pdlAggInfo     }
      L.StackObjSize   sz a _ -> pdl { pdlStackInfo   = ins sz a pdlStackInfo   }
      L.NativeIntSize _ -> pdl
      L.StackAlign a    -> pdl { pdlStackAlignment = fromIntegral a }
  where -- | Reduce the bit level alignment to a byte value, and error if it is not
        -- a multiple of 8.
        alignFromBits nm a | r == 0 = w
                           | otherwise = error $ nm ++ " must align on a byte boundary."
          where (w,r) = fromIntegral a `divMod` 8
        ins sz a f = insertAlign as (f pdl)
          where as = AlignSpec (fromIntegral sz) (alignFromBits "Alignment specification" a)

-- | Create parsed data layout from layout spec AST.
parseDataLayout :: L.DataLayout -> ParsedDataLayout
parseDataLayout = foldl (flip addLayoutSpec) pdl0
  where pdl0 = PDL { pdlEndian = BigEndian
                   , pdlStackAlignment = 1
                   , pdlPtrSize  = 8 -- 64 bit pointers
                   , pdlPtrAlign = 8 --  64 bit alignment
                   , pdlIntegerInfo = alignTree
                       [ AlignSpec  1 1 -- 1-bit values aligned on byte addresses.
                       , AlignSpec  8 1 -- 8-bit values aligned on byte addresses.
                       , AlignSpec 16 2 -- 16-bit values aligned on 2 byte addresses.
                       , AlignSpec 32 4 -- 32-bit values aligned on 4 byte addresses.
                       , AlignSpec 64 4 -- 64-bit balues aligned on 4 byte addresses.
                       ]
                   , pdlFloatInfo = alignTree
                       [ AlignSpec  16  2
                       , AlignSpec  32  4
                       , AlignSpec  64  8
                       , AlignSpec 128 16
                       ]
                   , pdlVectorInfo = alignTree
                       [ AlignSpec  64  8
                       , AlignSpec 128 16
                       ]
                   , pdlAggInfo = alignTree [ AlignSpec 0 1 ]
                   , pdlStackInfo = alignTree []
                   }
        

data FunDecl = FunDecl { fdRetType  :: !(Maybe MemType)
                       , fdArgTypes :: ![MemType]
                       , fdVarArgs  :: !Bool
                       }

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

instance (?lc :: LLVMContext) => Eq FunDecl where
  FunDecl xr xa xv == FunDecl yr ya yv = (xr,xa,xv) == (yr,ya,yv)

-- | Type supported by symbolic simulator.
data SymType
  = MemType MemType
  | Alias L.Ident
  | FunType FunDecl
    -- | A type not supported by the symbolic simulator.
  | UnsupportedType L.Type
  | VoidType

instance (?lc :: LLVMContext) => Eq SymType where
  MemType x == MemType y = x == y
  Alias i == y = maybe False (== y) (lookupAlias i)
  x == Alias i = maybe False (x ==) (lookupAlias i)
  FunType x == FunType y = x == y
  UnsupportedType{} == UnsupportedType{} = True
  VoidType == VoidType = True
  _ == _ = False

ppFunDecl :: FunDecl -> Doc
ppFunDecl (FunDecl rtp args va) = rdoc <> parens (L.commas (fmap ppMemType args ++ vad))
  where rdoc = maybe (text "void") ppMemType rtp
        vad = if va then [text "..."] else []

ppSymType :: SymType -> Doc
ppSymType (MemType tp) = ppMemType tp
ppSymType (Alias i) = L.ppIdent i
ppSymType (FunType d) = ppFunDecl d
ppSymType (UnsupportedType tp) = L.ppType tp    
ppSymType VoidType = text "void"

-- | LLVM Types supported by simulator with a defined size and alignment.
data MemType
  = IntType BitWidth
  | FloatType
  | DoubleType
  | PtrType SymType
  | ArrayType Int MemType
  | VecType Int MemType
  | StructType StructInfo

instance (?lc :: LLVMContext) => Eq MemType where
  IntType x == IntType y = x == y
  FloatType == FloatType = True
  DoubleType == DoubleType = True
  PtrType x == PtrType y = x == y
  ArrayType xn xt == ArrayType yn yt = (xn,xt) == (yn,yt)
  VecType   xn xt == VecType   yn yt = (xn,xt) == (yn,yt)
  StructType x == StructType y = x == y
  _ == _ = False

data FieldInfo = FieldInfo { fiOffset :: !Offset
                           , fiType :: !MemType
                             -- | Amount of padding in bytes at end of field.
                           , fiPadding :: !Size
                           }

-- | Information about structs.  Offsets and size is in bytes.
data StructInfo = StructInfo { structPacked :: !Bool
                             , structSize :: !Size
                             , structAlign :: !Alignment
                             , siFields :: !(Vector FieldInfo)
                             }

-- | Parser state contains 
data StructParserState =
  SPS { spsSize :: Size
      , spsMaxAlign :: Size
      }

type StructParser = State StructParserState

-- | Constructs a function for obtaining target-specific size/alignment
-- information about structs.  The function produced corresponds to the
-- StructLayout object constructor in TargetData.cpp.
mkStructInfo :: ParsedDataLayout -> Bool -> [MemType] -> StructInfo
mkStructInfo pdl packed tps =
    StructInfo { structPacked = packed
               , structSize = sz
               , structAlign = spsMaxAlign sps1
               , siFields = V.fromList $
                   zipWith3 mkFieldInfo offsets tps (drop 1 (fst <$> offsets) ++ [sz])
               }
  where sps0 = SPS { spsSize = 0
                   , spsMaxAlign = 1
                   }
        (offsets,sps1) = runState (mapM go tps) sps0
        sz = nextMultiple (spsMaxAlign sps1) (spsSize sps1)
        mkFieldInfo (o,e) tp o' =
          assert (o' >= e)
            FieldInfo { fiOffset = o
                      , fiType = tp
                      , fiPadding = o' - e
                      }
        go :: MemType -> StructParser (Size,Size)
        go etp = do      
          sps <- get
          let a = memTypeAlign pdl etp
          let off | packed = spsSize sps
                  | otherwise = nextMultiple a (spsSize sps)
          let end = off + memTypeSize pdl etp
          put SPS { spsSize = end
                  , spsMaxAlign = max a (spsMaxAlign sps)
                  }
          return (off,end)

siFieldTypes :: StructInfo -> Vector MemType
siFieldTypes si = fiType <$> siFields si

siFieldInfo :: StructInfo -> Int -> Maybe FieldInfo
siFieldInfo si i
   | 0 <= i && i < V.length flds = Just $ flds V.! i
   | otherwise = Nothing
 where flds = siFields si


siFieldType :: StructInfo -> Int -> Maybe MemType
siFieldType si i = fiType <$> siFieldInfo si i

-- | Returns offset of field if it is defined.
siFieldOffset :: StructInfo -> Int -> Maybe Offset
siFieldOffset si i = fiOffset <$> siFieldInfo si i

instance (?lc :: LLVMContext) => Eq StructInfo where
  x == y = (structPacked x, siFieldTypes x)
        == (structPacked y, siFieldTypes y)

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

-- | Returns size of sym type in bytes.
memTypeSize :: ParsedDataLayout -> MemType -> Size
memTypeSize pdl mtp =
  case mtp of
    IntType w -> (fromIntegral w + 7) `div` 8 -- Convert bits to bytes.
    FloatType -> 4
    DoubleType -> 8
    PtrType{} -> pdlPtrSize pdl
    ArrayType n tp -> fromIntegral n * memTypeSize pdl tp
    VecType n tp -> fromIntegral n * memTypeSize pdl tp
    StructType si -> structSize si

-- | Returns ABI byte alignment constraint in bytes.
memTypeAlign :: ParsedDataLayout -> MemType -> Alignment
memTypeAlign pdl mtp =
  case mtp of
    IntType w -> a
      where Just a = findIntMatch (fromIntegral w) (pdlIntegerInfo pdl)
    FloatType -> a
      where Just a = findExact 32 (pdlFloatInfo pdl)
    DoubleType -> a
      where Just a = findExact 64 (pdlFloatInfo pdl)
    PtrType{} -> pdlPtrAlign pdl
    ArrayType _ tp -> memTypeAlign pdl tp
    VecType _ tp -> memTypeAlign pdl tp
    StructType si -> structAlign si





ppStructInfo :: StructInfo -> Doc
ppStructInfo si = L.structBraces $ L.commas (V.toList fields)
  where fields = ppMemType <$> siFieldTypes si

data IdentStatus
  = Resolved SymType
  | Active
  | Pending L.Type

data TCState = TCS { tcsDataLayout :: ParsedDataLayout
                   , tcsMap :: Map L.Ident IdentStatus 
                     -- | Set of types encountered that are not supported by
                     -- the 
                   , tcsUnsupported :: Set L.Type
                   , tcsUnresolvable :: Set L.Ident
                   }

mkTCState :: ParsedDataLayout
          -> Map L.Ident IdentStatus
          -> TCState
mkTCState pdl m = TCS { tcsDataLayout = pdl
                      , tcsMap = m
                      , tcsUnsupported = Set.empty
                      , tcsUnresolvable = Set.empty
                      }

tcsErrors :: TCState -> [Doc]
tcsErrors tcs = (ppUnsupported <$> Set.toList (tcsUnsupported tcs))
             ++ (ppUnresolvable <$> Set.toList (tcsUnresolvable tcs))
  where ppUnsupported tp = text "Unsupported type:" <+> L.ppType tp
        ppUnresolvable i = text "Could not resolve identifier:" <+> L.ppIdent i
 
-- | Type lifter contains types that could not be parsed.
type TC = State TCState

recordUnsupported :: L.Type -> TC ()
recordUnsupported tp = modify fn
  where fn tcs = tcs { tcsUnsupported = Set.insert tp (tcsUnsupported tcs) }

-- | Returns the type bound to an identifier.
tcIdent :: L.Ident -> TC SymType
tcIdent i = do
  im <- tcsMap <$> get
  let retUnsupported = tp <$ modify fn
        where tp = UnsupportedType (L.Alias i)
              fn tcs = tcs { tcsUnresolvable = Set.insert i (tcsUnresolvable tcs) }
  case Map.lookup i im of
    Nothing -> retUnsupported
    Just (Resolved tp) -> return tp
    Just Active -> retUnsupported
    Just (Pending tp) -> do
        modify (ins Active)
        stp <- tcType tp
        stp <$ modify (ins (Resolved stp))
      where ins v tcs = tcs { tcsMap = Map.insert i v (tcsMap tcs) }

resolveMemType :: SymType -> TC (Maybe MemType)
resolveMemType = resolve
  where resolve (MemType mt) = return (Just mt)
        resolve (Alias i) = resolve =<< tcIdent i
        resolve FunType{} = return Nothing
        resolve UnsupportedType{} = return Nothing
        resolve VoidType = return Nothing

type RetType = Maybe MemType

ppRetType :: RetType -> Doc
ppRetType = maybe (text "void") ppMemType

resolveRetType :: SymType -> TC (Maybe RetType)
resolveRetType = resolve
  where resolve (MemType mt) = return (Just (Just mt))
        resolve (Alias i) = resolve =<< tcIdent i
        resolve VoidType = return (Just Nothing)
        resolve _ = return Nothing

tcMemType :: L.Type -> TC (Maybe MemType)
tcMemType tp = resolveMemType =<< tcType tp

tcType :: L.Type -> TC SymType
tcType tp0 = do
  let badType = UnsupportedType tp0 <$ recordUnsupported tp0
  let maybeApp :: (a -> MemType) -> TC (Maybe a) -> TC SymType
      maybeApp f mmr = maybe badType (return . MemType . f) =<< mmr
  case tp0 of
    L.PrimType pt ->
      case pt of
        L.Void -> return VoidType
        L.Integer w -> return $ MemType $ IntType (fromIntegral w)
        L.FloatType ft -> do
          case ft of
            L.Float -> return $ MemType FloatType
            L.Double -> return $ MemType DoubleType
            _ -> badType
        _ -> badType
    L.Alias i -> return (Alias i)
    L.Array n etp -> maybeApp (ArrayType (fromIntegral n)) $ tcMemType etp
    L.FunTy res args va -> do
      mrt <- resolveRetType =<< tcType res
      margs <- mapM tcMemType args
      maybe badType (return . FunType) $
        FunDecl <$> mrt <*> sequence margs <*> pure va
    L.PtrTo tp ->  (MemType . PtrType) <$> tcType tp
    L.Struct tpl       -> maybeApp StructType $ tcStruct False tpl
    L.PackedStruct tpl -> maybeApp StructType $ tcStruct True  tpl
    L.Vector n etp -> maybeApp (VecType (fromIntegral n)) $ tcMemType etp
    L.Opaque -> badType

-- | Constructs a function for obtaining target-specific size/alignment
-- information about structs.  The function produced corresponds to the
-- StructLayout object constructor in TargetData.cpp.
tcStruct :: Bool -> [L.Type] -> TC (Maybe StructInfo)
tcStruct packed fldTys = do
  pdl <- tcsDataLayout <$> get
  fmap (mkStructInfo pdl packed) . sequence <$> 
    mapM tcMemType fldTys

type TypeAliasMap = Map L.Ident SymType

data LLVMContext = LLVMContext
  { llvmDataLayout :: ParsedDataLayout
  , llvmTypeAliasMap  :: TypeAliasMap
  }

instance Show LLVMContext where
  show lc =
    "LC: PtrSize = " ++ show (llvmPtrSize lc)
       ++ ", Ptralign = " ++ show (llvmPtrAlign lc)

-- | Size of pointer in bytes.
llvmPtrSize :: LLVMContext -> Size
llvmPtrSize = pdlPtrSize . llvmDataLayout

-- | Pointer alignment, in bytes
llvmPtrAlign :: LLVMContext -> Alignment
llvmPtrAlign = pdlPtrAlign . llvmDataLayout

llvmAddrWidthBits :: LLVMContext -> BitWidth
llvmAddrWidthBits lc = 8 * fromIntegral (llvmPtrSize lc)

llvmLookupAlias' :: LLVMContext -> L.Ident -> Maybe SymType
llvmLookupAlias' lc i = Map.lookup i (llvmTypeAliasMap lc) 

llvmLookupAlias :: LLVMContext -> L.Ident -> SymType
llvmLookupAlias lc i = fromMaybe (error msg) $ llvmLookupAlias' lc i
  where msg = "Failed to locate type alias named "
                ++ show (L.ppIdent i) ++ " in code base"

lookupAlias :: (?lc :: LLVMContext) => L.Ident -> Maybe SymType
lookupAlias i = llvmLookupAlias' ?lc i

asMemType :: (?lc :: LLVMContext) => SymType -> Maybe MemType
asMemType (MemType mt) = Just mt
asMemType (Alias i) = asMemType =<< lookupAlias i
asMemType _ = Nothing

asRetType :: (?lc :: LLVMContext) => SymType -> Maybe RetType
asRetType (MemType mt) = Just (Just mt)
asRetType VoidType = Just Nothing
asRetType (Alias i) = asRetType =<< lookupAlias i
asRetType _ = Nothing

liftType :: (?lc :: LLVMContext) => L.Type -> Maybe SymType
liftType tp | null (tcsErrors tcs) = Just stp
            | otherwise = Nothing
  where m0 = Resolved <$> llvmTypeAliasMap ?lc
        tcs0 = mkTCState (llvmDataLayout ?lc) m0
        (stp, tcs) = runState (tcType tp) tcs0

liftMemType :: (?lc :: LLVMContext) => L.Type -> Maybe MemType
liftMemType tp = asMemType =<< liftType tp

liftRetType :: (?lc :: LLVMContext) => L.Type -> Maybe RetType
liftRetType tp = asRetType =<< liftType tp

-- | Returns an LLVM context and types that are not supported by symbolic simulator.
buildLLVMContext :: [L.TypeDecl] -> L.DataLayout -> ([Doc], LLVMContext)
buildLLVMContext decls dl = (tcsErrors tcs, lc)
  where
    pdl = parseDataLayout dl
    tps = [ (L.typeName d, L.typeValue d) | d <- decls ]
    go (i,tp) = fmap (i,) $ tcType tp
    tcs0 = mkTCState pdl (Pending <$> Map.fromList tps)
    (tam,tcs) = runState (traverse go tps) tcs0
    lc = LLVMContext
         { llvmDataLayout = pdl
         , llvmTypeAliasMap = Map.fromList tam
         }

-- Memery Geometry

type Addr = Integer

data MemGeom = MemGeom {
        mgStack :: (Addr, Addr)
      , mgCode :: (Addr, Addr)
      , mgData :: (Addr, Addr)
      , mgHeap :: (Addr, Addr)
      }

-- We make a keep it simple concession and divide up the address space as
-- follows:
--
-- Top  1/4: Stack
-- Next 1/8: Code
-- Next 1/8: Data
-- Last 1/2: Heap
--
-- One advantage of this is that it's easy to tell the segment to which a
-- pointer belongs simply by inspecting its address.
--
-- TODO: Allow user overrides of memory geom
defaultMemGeom :: LLVMContext -> MemGeom
defaultMemGeom lc
    | w < 16 =  error "Pointers must be at least 16bits to get sufficient memory size."
    | otherwise = 
        MemGeom (stackStart, stackEnd)
                (codeStart,  codeEnd)
                (dataStart,  dataEnd)
                (heapStart,  heapEnd)
  where
    w           = 8*llvmPtrSize lc
    stackStart  = 4096 -- Start at first page rather than null
    codeStart   = 2 ^ w `div` 4
    dataStart   = codeStart + 2 ^ w `div` 8
    heapStart   = dataStart + 2 ^ w `div` 8

    stackEnd    = codeStart - 1
    codeEnd     = dataStart - 1
    dataEnd     = heapStart - 1
    heapEnd     = 2 ^ w - 1

--------------------------------------------------------------------------------
-- Testing

__nowarn_unused :: a
__nowarn_unused = undefined testLC

testLC :: LLVMContext
testLC = snd $
  buildLLVMContext
    (error "type alias lookup not defined")
    [ L.LittleEndian
    , L.PointerSize 64 64 (Just 64)
    , L.IntegerSize 1 8 (Just 8)
    , L.IntegerSize 8 8 (Just 8)
    , L.IntegerSize 16 16 (Just 16)
    , L.IntegerSize 32 32 (Just 32)
    , L.IntegerSize 64 64 (Just 64)
    , L.FloatSize 32 32 (Just 32)
    , L.FloatSize 64 64 (Just 64)
    , L.FloatSize 80 128 (Just 128)
    , L.VectorSize 64 64 (Just 64)
    , L.VectorSize 128 128 (Just 128)
    , L.AggregateSize 0 0 (Just 64)
    , L.StackObjSize 0 64 (Just 64)
    , L.NativeIntSize [8, 16, 32, 64]
    ]
