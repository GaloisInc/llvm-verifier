{- |
Module           : $Header$
Description      : Info about LLVM memory
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.LLVM.TargetData
  ( LLVMContext( llvmAddrWidthBits
               , llvmPtrAlign
               , llvmLookupAlias
               )
  , buildLLVMContext
  , llvmAllocSizeOf
  , llvmMinBitSizeOf
  , llvmStoreSizeOf
  )
where

import           Data.Bits
import           Data.FingerTree
import           Data.Maybe
import           Data.Monoid
import           LSS.LLVMUtils
import qualified Control.Exception as CE
import qualified Data.FingerTree   as FT
import qualified Data.Foldable     as DF
import qualified Text.LLVM         as L

data LLVMContext = LLVMContext
  { llvmAddrWidthBits :: Int
  , llvmPtrAlign      :: Integer -- pointer alignment, in bytes
  , llvmAlignmentOf   :: AlignQuery
  , llvmStructInfo    :: StructSizeQuery
  , llvmLookupAlias   :: L.Ident -> L.Type
  , llvmDataLayout    :: L.DataLayout
  }
instance Show LLVMContext where
  show (LLVMContext w a _ _ _ _) =
    "LC: Addrwidth = " ++ show w ++ ", Ptralign = " ++ show a

type AlignQuery = L.Type -> AlignType -> AlignInfo

data AlignType
  = IntegerAlign
  | VectorAlign
  | FloatAlign
  | AggregateAlign
  | StackAlign
  deriving (Ord, Eq, Show)

data AlignInfo =
  AlignInfo
  { aiType     :: AlignType
  , aiABI      :: Integer
  , aiBitwidth :: Integer
  }
  deriving (Show, Eq)

newtype BW          = BW Integer
type AlignTree      = FingerTree BW AlignInfo
data AlignTreeMatch = Exact AlignInfo | Succ AlignInfo | Missing
instance Monoid BW where
  mempty                = BW 0
  mappend (BW a) (BW b) = BW (max a b)
instance Measured BW AlignInfo where
  measure (AlignInfo _ _ bw) = BW bw

buildLLVMContext :: (L.Ident -> L.Type) -> L.DataLayout -> LLVMContext
buildLLVMContext lkupAlias dl = lc
  where
    lc = LLVMContext
         { llvmAddrWidthBits = dlPtrSize dl
         , llvmPtrAlign      = (dlPtrAlign dl + 7) `shiftR` 3
         , llvmAlignmentOf   = mkAlignQuery lc
         , llvmStructInfo    = mkStructSizeQuery lc
         , llvmLookupAlias   = lkupAlias
         , llvmDataLayout    = dl
         }

-- | Obtain the alignment, in bytes, of the given type.  Note that we do not
--  currently support "preferred" alignments.  Corresponds to
--  TargetData::getAlignment().
llvmTypeABIAlignOf :: LLVMContext -> L.Type -> Integer
llvmTypeABIAlignOf lc ty = case ty of
  L.Alias a                -> llvmTypeABIAlignOf lc (llvmLookupAlias lc a)
  L.PtrTo{}                -> llvmPtrAlign lc
  L.Array _ ety            -> llvmTypeABIAlignOf lc ety
  L.PackedStruct{}         -> 1
  L.Struct{}               -> max (aiABI $ llvmAlignmentOf lc ty AggregateAlign)
                                  (ssiAlign $ llvmStructInfo lc ty)
  L.PrimType L.Integer{}   -> align IntegerAlign
  L.PrimType L.Void        -> align IntegerAlign
  L.PrimType L.FloatType{} -> align FloatAlign
  L.Vector{}               -> align VectorAlign
  L.PrimType L.X86mmx      -> align VectorAlign

  _ ->
    error $ "internal: " ++ "unsupported llvmTypeABIAlignOf type: "
            ++ show (L.ppType ty)
  where
    align = aiABI . llvmAlignmentOf lc ty

-- | Returns the number of bytes that alloca must reserve for the given type;
-- includes any target-specific alignment padding that must occur.  Corresponds
-- to TargetData::getTypeAllocSize().
llvmAllocSizeOf :: LLVMContext -> L.Type -> Integer
llvmAllocSizeOf lc ty = CE.assert (isPow2 align) $
  nextMultiple align (llvmStoreSizeOf lc ty)
  where
    align = llvmTypeABIAlignOf lc ty

-- | Returns the minumum number of bytes required to represent a value of the
-- given type (e.g., i7 requires 1 byte); this is the maximum number of bytes
-- that may be overwritten when storing the specified type.  Corresponds to
-- TargetData::getTypeStoreSize().
llvmStoreSizeOf :: LLVMContext -> L.Type -> Integer
llvmStoreSizeOf lc ty = (llvmMinBitSizeOf lc ty + 7) `shiftR` 3

-- | Returns the minimum number of bits required to represent a value of the
-- given type (e.g. i7 requires exactly 7 bits).  Corresponds to
-- TargetData::getTypeSizeInBits().
llvmMinBitSizeOf :: LLVMContext -> L.Type -> Integer
llvmMinBitSizeOf lc ty = case ty of
  L.PrimType pty                 -> prim pty
  L.Alias a                      -> llvmMinBitSizeOf lc (llvmLookupAlias lc a)
  L.Array (toInteger -> len) ety -> len * llvmMinBitSizeOf lc ety
  L.PtrTo{}                      -> toInteger (llvmAddrWidthBits lc)
  L.Vector (toInteger -> len) et -> toInteger len * llvmMinBitSizeOf lc et
  L.PackedStruct{}               -> (ssiBytes $ llvmStructInfo lc ty) `shiftL` 3
  L.Struct{}                     -> (ssiBytes $ llvmStructInfo lc ty) `shiftL` 3
  L.FunTy{}                      -> error "internal: Cannot get size of function type."
  L.Opaque{}                     -> error "internal: Cannot get size of opaque type."
  where
    prim (L.Integer w)             = toInteger w
    prim (L.FloatType L.Float)     = 32
    prim (L.FloatType L.Double)    = 64
    prim (L.FloatType L.Fp128)     = 128
    prim (L.FloatType L.X86_fp80)  = 80
    prim (L.FloatType L.PPC_fp128) = 128
    prim L.Label                   = error "internal: Cannot get size of label."
    prim L.Void                    = error "internal: Cannot get size of void."
    prim L.X86mmx                  = error "internal: X86MMX memory size is undefined."
    prim L.Metadata                = error "internal: Cannnot get size of metadata."

--------------------------------------------------------------------------------
-- Data layout query functions -- see Data Layout section at
-- http://llvm.org/docs/LangRef.html for details.  These functions yield the
-- specified default values in the absence of overriding target data layout
-- information.  NB: Currently, we do not use any of the "preferred" alignments.

-- | Constructs a function for obtaining target-specific alignment info.  The
-- function produced corresponds to TargetData::getAlignmentInfo().
mkAlignQuery :: LLVMContext -> AlignQuery
mkAlignQuery lc = \ty alignTy ->
  let w = bitwidth ty in
  case alignTy of
    IntegerAlign -> case lkupAlign i w of
      Exact ai -> ai
      Succ ai  -> ai                 -- Use smallest size > w
      Missing  -> DF.maximumBy cmp i -- Fall back on largest integer
    VectorAlign -> case lkupAlign v w of
      Exact ai -> ai
      _        ->
        -- Fall back on "natural alignment for vector types"
        case ty of
          L.Vector (toInteger -> len) ety ->
            let ea        = llvmAllocSizeOf lc ety * len
                elemAlign = if isPow2 ea then ea else nextPow2 ea
            in
              AlignInfo VectorAlign elemAlign w
          _ -> error $ "internal: attempted to determine vector alignment "
                       ++ "for non-vector type"
    FloatAlign     -> requireExact "float"     (lkupAlign f w)
    AggregateAlign -> requireExact "aggregate" (lkupAlign ag w)
    StackAlign     -> requireExact "stack"     (lkupAlign st w)
  where
    bitwidth L.Struct{} = 0
    bitwidth ty         = llvmMinBitSizeOf lc ty
    dl                  = llvmDataLayout lc
    splitBitwidth w     = FT.split $ \(BW y) -> y > w
    cmp x y             = aiBitwidth x `compare` aiBitwidth y

    requireExact :: String -> AlignTreeMatch -> AlignInfo
    requireExact _ (Exact ai) = ai
    requireExact msg _        = error $ "internal: failed to find exact "
                                        ++ "match for " ++ msg
                                        ++ " alignment type."

    -- Look for the AlignInfo with the given bitwidth in the giventree; if it
    -- does not match exactly, provide the successor bitwidth if it exists.
    lkupAlign alignTree w = case viewr ftl of
      (_ :> a)
        | aiBitwidth a == w -> Exact a
        | otherwise         -> case viewl ftr of
                                 (a' :< _) -> Succ a'
                                 _        -> Missing
      EmptyR -> Missing
      where
        (ftl, ftr) = splitBitwidth w alignTree

    -- AlignTrees by alignment type
    [i, f, v, ag, st] =
      map (\aty ->
             let dflts    = filterAligns aty dfltAligns
                 dfltTree = ins FT.empty dflts
                 ins      = foldr insAlign
             in
               ins dfltTree              -- ^ ...and override defaults as needed
               $ filterAligns aty        -- ^ ...for this alignment type...
               $ mapMaybe mkAlignInfo dl -- ^ pick up target data alignment info...
          )
          [IntegerAlign, FloatAlign, VectorAlign, AggregateAlign, StackAlign]

    insAlign :: AlignInfo -> AlignTree -> AlignTree
    insAlign ai ft = case viewr ftl of
      (ftl' :> ai') -> ftl' >< mid ai' >< ftr
      EmptyR        -> case viewl ftr of
                         (ai' :< ftr') -> ftl >< mid ai' >< ftr'
                         EmptyL        -> s ai
      where
        s          = FT.singleton
        (ftl, ftr) = splitBitwidth (aiBitwidth ai) ft
        mid x      = case ai `cmp` x of
                       LT -> s ai >< s x
                       GT -> s x  >< s ai
                       EQ -> s ai -- replace old value

    mkAlignInfo :: L.LayoutSpec -> Maybe AlignInfo
    mkAlignInfo ls = case ls of
      L.IntegerSize w abi _   -> ai IntegerAlign abi w
      L.VectorSize w abi _    -> ai VectorAlign abi w
      L.FloatSize w abi _     -> ai FloatAlign abi w
      L.AggregateSize w abi _ -> ai AggregateAlign abi w
      L.StackObjSize w abi _  -> ai StackAlign abi w
      _                       -> Nothing
      where
        ai at abi w = Just $ AlignInfo at (toInteger abi `shiftR` 3) (toInteger w)

    filterAligns :: AlignType -> [AlignInfo] -> [AlignInfo]
    filterAligns at = filter ((==at) . aiType)

    -- Default alignments for integer types
    dfltAligns :: [AlignInfo]
    dfltAligns =
      [ AlignInfo IntegerAlign     1   1 -- i1
      , AlignInfo IntegerAlign     1   8 -- i8
      , AlignInfo IntegerAlign     2  16 -- i16
      , AlignInfo IntegerAlign     4  32 -- i32
      , AlignInfo IntegerAlign     4  64 -- i64
      , AlignInfo FloatAlign       4  32 -- float
      , AlignInfo FloatAlign       8  64 -- double
      , AlignInfo VectorAlign      8  64 -- v2i32, v1i64, ...
      , AlignInfo VectorAlign     16 128 -- v16i8, v8i16, v4i32, ...
      , AlignInfo AggregateAlign   0   0 -- struct
      ]

type StructSizeQuery = L.Type -> StructSizeInfo
data StructSizeInfo   = SSI
  { ssiBytes   :: Integer    -- struct size in bytes
  , ssiAlign   :: Integer    -- struct alignment in bytes
  , _ssiOffsets :: [Integer] -- /i/th element contains the 0-based byte offset to
                             -- struct member /i/
  }
  deriving (Show)

-- | Constructs a function for obtaining target-specific size/alignment
-- information about structs.  The function produced corresponds to the
-- StructLayout object constructor in TargetData.cpp.
mkStructSizeQuery :: LLVMContext -> StructSizeQuery
mkStructSizeQuery lc = \ty ->
  let SSI sz align offs = foldl (impl packed) (SSI 0 0 []) fldTys
      (fldTys, packed)  = case ty of
                            L.Struct tys       -> (tys, False)
                            L.PackedStruct tys -> (tys, True)
                            _                  -> err
      align'            = if align == 0 then 1 else align
  in
    SSI (nextMultiple align' sz) (align') offs
  where
    err = error "internal: struct info query given non-struct type"
    impl packed (SSI sz align offsets) elemTy =
      SSI
        (sz' + llvmAllocSizeOf lc elemTy)
        (max elemAlign align)
        (offsets ++ [sz'])
      where
        sz'       = nextMultiple elemAlign sz
        elemAlign = if packed then 1 else llvmTypeABIAlignOf lc elemTy

-- | Extract target pointer size from data layout, or fall back on default.
dlPtrSize :: L.DataLayout -> Int
dlPtrSize []                      = 64
dlPtrSize (L.PointerSize w _ _:_) = w
dlPtrSize (_:dls)                 = dlPtrSize dls

-- | Extract target pointer alignment from data layout, or fall back on default.
dlPtrAlign :: L.DataLayout -> Integer
dlPtrAlign []                      = 64
dlPtrAlign (L.PointerSize _ a _:_) = toInteger a
dlPtrAlign (_:dls)                 = dlPtrAlign dls

--------------------------------------------------------------------------------
-- Testing

__nowarn_unused :: a
__nowarn_unused = undefined testLC

testLC :: LLVMContext
testLC =
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
