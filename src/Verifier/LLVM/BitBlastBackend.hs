{- |
Module           : $Header$
Description      : A symbolic backend that bitblasts
Stability        : provisional
Point-of-contact : atomb, jhendrix
-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}
module Verifier.LLVM.BitBlastBackend
  ( module Verifier.LLVM.Backend
  , BitBlastSBE
  , BitTerm
--  , BitTermClosed(..)
  , sbeBitBlast
  , liftSBEBitBlast
    -- Memmodel code
  , MemModel(..)
  , BitBlastMemModel
  , BitMemory
  , buddyMemModel
  , buddyInitMemory
  , createBuddyMemModel
  , createBuddyAll
  , DagMemory
  , createDagMemModel
  , createDagAll
  -- for testing only
  , BitIO
  , bmDataAddr
  ) where

import           Control.Applicative       ((<$>), (<$))
import qualified Control.Arrow as Arrow
import           Control.Exception         (assert)
import           Control.Lens hiding (ix, op)
import           Control.Monad (ap, unless, when)
import           Control.Monad.IO.Class
import           Data.Binary.IEEE754
import           Data.Bits
import           Data.Foldable
import           Data.IORef
import           Data.Map                  (Map)
import           Data.Set                  (Set)
import           Verifier.LLVM.Backend
import           Numeric                   (showHex)
import Text.PrettyPrint.Leijen hiding ((<$>), align)

import           Verinf.Symbolic.Lit
import           Verinf.Symbolic.Lit.Functional
import qualified Data.Map                  as Map
import qualified Data.Set                  as Set
import qualified Data.Vector               as V
import qualified Data.Vector.Storable      as LV
import qualified Text.LLVM.AST             as L
import System.IO.Unsafe (unsafePerformIO)

import           Verifier.LLVM.AST
import           Verifier.LLVM.Simulator.SimUtils
import           Verifier.LLVM.Utils

-- Utility functions and declarations {{{1

c2 :: (r -> s) -> (a -> b -> r) -> a -> b -> s
g `c2` f = \x y -> g (f x y)

c3 :: (r -> s) -> (a -> b -> c -> r) -> a -> b -> c -> s
g `c3` f = \x y z -> g (f x y z)

c4 :: (r -> s) -> (a -> b -> c -> d -> r) -> a -> b -> c -> d -> s
g `c4` f = \w x y z -> g (f w x y z)

c5 :: (r -> s) -> (a -> b -> c -> d -> e -> r) -> a -> b -> c -> d -> e -> s
g `c5` f = \v w x y z -> g (f v w x y z)

c6 :: (r -> s) -> (a -> b -> c -> d -> e -> f -> r)
               -> (a -> b -> c -> d -> e -> f -> s)
g `c6` f = \u v w x y z -> g (f u v w x y z)

lfp :: Ord a => (a -> [a]) -> Set a -> Set a
lfp fn initSet = impl initSet (Set.toList initSet)
  where impl s [] = s
        impl s (h:r) = impl (foldl' (flip Set.insert) s new) (new ++ r)
          where new = filter (flip Set.notMember s) (fn h)

-- | Memoizes a function using a map.
memo :: Ord s => (s -> t) -> (s -> t)
memo fn = unsafePerformIO $ do
  ref <- newIORef Map.empty
  return $ \key -> unsafePerformIO $ do
    m <- readIORef ref
    case Map.lookup key m of
      Just v -> return v
      Nothing -> v <$ modifyIORef ref (Map.insert key v)
        where v = fn key

-- | Returns number of bytes.
byteSize :: LV.Storable l => LV.Vector l -> Int
byteSize v = LV.length v `shiftR` 3

-- | Slice a vector into a list of vectors, one for each byte.
sliceIntoBytes :: LV.Storable l => LV.Vector l -> V.Vector (LV.Vector l)
sliceIntoBytes v = V.generate (byteSize v) $ \i -> LV.slice (i `shiftL` 3) 8 v

-- | Slice a single vector into a vector of lit vectors with n elements.
sliceN :: LV.Storable l => Int -> LV.Vector l -> V.Vector (LV.Vector l)
sliceN n v = assert (n > 0 && r == 0) $
    V.generate n $ \i -> LV.slice (l*i) l v  
  where (l,r) = LV.length v `divMod` n

-- | Slice a single vector into a vector of lit vectors each with the given number of elements.
joinN :: LV.Storable l => V.Vector (LV.Vector l) -> LV.Vector l
joinN = LV.concat . V.toList

-- | @alignUp addr i@ returns the smallest multiple of @2^i@ that it
-- at least @addr@.
alignUp :: Addr -> Alignment -> Addr
alignUp addr i = (addr + mask) .&. complement mask
 where mask = setBit 0 (fromIntegral i) - 1

-- | @alignDn addr i@ returns the largest multiple of @2^i@ that it
-- at most @addr@.
alignDn :: Addr -> Alignment -> Addr
alignDn addr i = addr .&. complement mask
 where mask = setBit 0 (fromIntegral i) - 1

lvAnd :: (?be :: BitEngine l, LV.Storable l)
      => LV.Vector l -> LV.Vector l -> LV.Vector l
lvAnd = LV.zipWith lAnd

lvSetBits :: (?be :: BitEngine l, LV.Storable l) => Int -> (Int -> Bool) -> LV.Vector l
lvSetBits n pr = LV.generate n (lFromBool . pr)

-- | @lAlignUp addr i@ returns pair @(c,v)@ where @v@ is the smallest multiple of @2^i@
-- not smaller than @addr@, and @c@ is set if computation overflowed.
lAlignUp :: (?be :: BitEngine l, LV.Storable l) => LV.Vector l -> Alignment -> (l,LV.Vector l)
lAlignUp addr i = (c, s `lvAnd` lvSetBits n (>= fromIntegral i))
  where n = LV.length addr
        (c,s) = addr `lFullAdd` lvSetBits n (< fromIntegral i)

-- | @lAlignDown addr i@ returns pair @(c,v)@ where @v@ is the largest multiple of @2^i@
-- not larger than @addr@, and @c@ is set if computation overflowed.
lAlignDn :: (?be :: BitEngine l, LV.Storable l) => LV.Vector l -> Alignment -> LV.Vector l
lAlignDn addr i = addr `lvAnd` lvSetBits (LV.length addr) (>= fromIntegral i)

bmError :: String -> a
bmError = error

mgSanityCheck :: MemGeom -> Maybe String
mgSanityCheck mg
  | decreasing (mgCode mg) = Just "Code segment start and end are in wrong order."
  | decreasing (mgData mg) = Just "Data segment start and end are in wrong order."
  | decreasing (mgHeap mg) = Just "Heap segment start and end are in wrong order."
  | norm (mgStack mg) `overlap` mgCode mg = Just "Stack and code segments overlap."
  | norm (mgStack mg) `overlap` mgData mg = Just "Stack and data segments overlap."
  | norm (mgStack mg) `overlap` mgHeap mg = Just "Stack and heap segments overlap."
  | mgCode mg `overlap` mgData mg = Just "Code and data segments overlap."
  | mgCode mg `overlap` mgHeap mg = Just "Code and heap segments overlap."
  | mgData mg `overlap` mgHeap mg = Just "Data and code segments overlap."
  | otherwise = Nothing

-- Range {{{1

type Range t = (t,t)

norm :: Range Addr -> Range Addr
norm (x,y) = (min x y, max x y)

overlap :: Range Addr -> Range Addr -> Bool
overlap (s1,e1) (s2,e2) =
  if s1 <= s2 then s2 < e2 && s2 < e1 else s1 < e1 && s1 < e2

start :: Range i -> i
start = fst

end :: Range i -> i
end = snd

-- | Returns true if range is decreasing.
decreasing :: Ord i => Range i -> Bool
decreasing (x,y) = x > y

-- | @inRangeAny a rs@ returns true if @a@ is in any of the ranges given in
-- | @rs@.  Ranges are open on the high end.
inRangeAny :: Addr -> [Range Addr] -> Bool
inRangeAny a = not . null . filter (\(lo,hi) -> lo <= a && a < hi)

-- BitEngine primitives {{{1
-- | Dont care bit in bit engine.
-- TODO: Figure out if this is a useful primitive to add to bit engine (e.g., does abc support ti).
beDontCare :: BitEngine l -> l
beDontCare be = beFalse be

mergeCondVector :: (Eq l, LV.Storable l)
                => BitEngine l
                -> l
                -> IO (l, LV.Vector l)
                -> IO (l, LV.Vector l)
                -> IO (l, LV.Vector l)
mergeCondVector be =
  beLazyMux be $ \c (tc,tv) (fc,fv) ->
    return (,) `ap` beIte be c tc fc `ap` LV.zipWithM (beIte be c) tv fv

beDontCareByte :: LV.Storable l => BitEngine l -> LV.Vector l
beDontCareByte = LV.replicate 8 . beDontCare

-- | @lInRange be p (s,e)@ returns predicate that holds in @s <= p & p < e@
-- when treated as unsigned values.
lInRange :: (?be :: BitEngine l, LV.Storable l) => LV.Vector l -> (LV.Vector l, LV.Vector l) -> l
lInRange p (s,e) = (s `lUnsignedLeq` p) `lAnd` (p `lUnsignedLt` e)

-- | @lRangeCovered be subFn r1 r2@ returns true if @subFn@ returns true for
-- all ranges in @r1 - r2@.  N.B. Only calls subFn on an empty range if @r1@ is
-- empty.
lRangeCovered :: (?be :: BitEngine l, LV.Storable l)
              => (Range (LV.Vector l) -> l)
              -> Range (LV.Vector l)
              -> Range (LV.Vector l)
              -> l
lRangeCovered subFn (s1,e1) (s2,e2) =
 lIte ((s2 `lt` e1) &&& (s1 `lt` e2))
      (((s2 `le` s1) ||| subFn (s1,s2)) &&& ((e1 `le` e2) ||| subFn (e2,e1)))
      (subFn (s1,e1))
 where le = lUnsignedLeq
       lt = lUnsignedLt
       (&&&) = lAnd
       (|||) = lOr

-- | Returns true if vector is non-zero.
lIsNonZero :: (?be :: BitEngine l, LV.Storable l) => LV.Vector l -> l
lIsNonZero = LV.foldl' lOr lFalse

-- | Returns true if vector is zero.
lIsZero :: (?be :: BitEngine l, LV.Storable l) => LV.Vector l -> l
lIsZero = lNeg . lIsNonZero

-- BitTerm {{{1

data BitTerm l
    = IntTerm (LV.Vector l)
    | FloatTerm Float
    | DoubleTerm Double
    | PtrTerm (LV.Vector l)
    | ArrayTerm (V.Vector (BitTerm l))
    | VecTerm (V.Vector (BitTerm l))
      -- | Could be packed or unpacked struct.
    | StructTerm (V.Vector (BitTerm l))
  deriving (Eq, Ord)

lPrettyLit :: (?be :: BitEngine l) => l -> Doc
lPrettyLit x | x `lEqLit` lFalse = text "False"
             | x `lEqLit` lTrue = text "True"
             | otherwise = text "?:[1]"

lPrettyLV :: (?be :: BitEngine l, LV.Storable l) => LV.Vector l -> Doc
lPrettyLV bv
  | 1 == LV.length bv = lPrettyLit (bv LV.! 0)
  | otherwise = text str <> colon <>  brackets (text $ show $ LV.length bv)
                  <+> maybe empty cvt (lGetSigned bv)
  where
    cvt x = parens (integer x)
            <+> if x >= 0
                then hex x
                else case lGetUnsigned bv of
                       Nothing -> empty
                       Just u  -> hex u
    hex x = parens $ text "0x" <> text (showHex x "")
    str      = LV.foldr (\lit acc -> acc ++ [toChar lit]) "" bv
    toChar x = if x `lEqLit` lFalse then '0' else if x `lEqLit` lTrue then '1' else '?'

beAsBool :: (Eq l, ?be :: BitEngine l) => l -> Maybe Bool
beAsBool x | x `lEqLit` lFalse = Just False
           | x `lEqLit` lTrue = Just True
           | otherwise = Nothing

-- | Attempts to convert a bitvector to a term.
-- May fail if some values are symbolic, but floating point.
bytesToTerm :: (?be :: BitEngine l, LV.Storable l)
            => DataLayout
            -> MemType
            -> LV.Vector l
            -> Maybe (BitTerm l)
bytesToTerm dl tp0 v = 
  case tp0 of
    IntType w
      | w <= LV.length v -> return $ IntTerm (LV.take w v)
      | otherwise -> badVec $ "integer.\nExpected at least " ++ show w
                              ++ "; Found " ++ show (LV.length v)
    FloatType
      | LV.length v == 32 ->
          FloatTerm  . wordToFloat  . fromIntegral <$> lGetUnsigned v
      | otherwise -> badVec "float."
    DoubleType
      | LV.length v == 64 ->
          DoubleTerm . wordToDouble . fromIntegral <$> lGetUnsigned v
      | otherwise -> badVec "double."
    PtrType{} | ptrBitwidth dl == fromIntegral (LV.length v) -> return $ PtrTerm v
    ArrayType n etp -> ArrayTerm <$> traverse (bytesToTerm dl etp) (sliceN n v)
    VecType n etp   -> VecTerm <$> traverse (bytesToTerm dl etp) (sliceN n v)
    StructType si   -> StructTerm <$> traverse go (siFields si)
      where go fi = bytesToTerm dl (fiType fi) (LV.slice o sz v)
              where o = 8 * fromIntegral (fiOffset fi)
                    sz = 8 * fromIntegral (memTypeSize dl (fiType fi))
    _ -> error "internalError: bytes to term given mismatched arguments"
 where badVec nm = error $
        "internalError: bytesToTerm given incorrect number of bits for " ++ nm


lVectorFromFloat :: (?be :: BitEngine l, LV.Storable l) => Float -> LV.Vector l
lVectorFromFloat = lVectorFromInt 32 . toInteger . floatToWord

lVectorFromDouble :: (?be :: BitEngine l, LV.Storable l)
                  => Double -> LV.Vector l
lVectorFromDouble = lVectorFromInt 64 . toInteger . doubleToWord

-- | Convert term into a list of bytes suitable for storing in memory.
termToBytes :: (?be :: BitEngine l, LV.Storable l)
            => DataLayout
            -> MemType
            -> BitTerm l
            -> LV.Vector l
termToBytes dl tp0 t0 = do
  case (tp0, t0) of
    (IntType w, IntTerm v) -> v LV.++ ext
      where newBits = (8 - (w .&. 0x7)) .&. 0x7
            ext = LV.replicate (fromIntegral newBits) (beDontCare ?be)
    (FloatType,      FloatTerm v)  -> lVectorFromFloat v
    (DoubleType,     DoubleTerm v) -> lVectorFromDouble v
    (PtrType{},      PtrTerm v)
      | ptrBitwidth dl == LV.length v -> v
    (ArrayType _ tp, ArrayTerm v)  -> joinN (termToBytes dl tp <$> v)
    (VecType _ tp,   VecTerm v)    -> joinN (termToBytes dl tp <$> v)
    (StructType si,  StructTerm v) -> joinN $ V.zipWith (termToBytes dl) (siFieldTypes si) v
    _ -> error $ show $ text "internalError: termToBytes given mismatched arguments:" <$$>
           nest 2 (text "Type:" <+> ppMemType tp0 <$$>
                   text "Term:" <+> ppBitTerm t0)
           
{-
termToBytes lc be tp (BitTerm val) =
  case resolveType lc tp of
    -- Extend integer types to full width.
    L.PrimType (L.Integer w) -> val LV.++ ext
      where newBits = (8 - (w .&. 0x7)) .&. 0x7
            ext = LV.replicate (fromIntegral newBits) (beDontCare be)
    -- Treat other types as same.
    _ -> val
-}

-- MemModel {{{1

data MemModel sbe bytes = MemModel {
    mmDump :: Bool -> SBEMemory sbe -> Maybe [Range Addr] -> IO ()
  , mmLoad :: SBEMemory sbe
           -> SBETerm sbe
           -> Size
           -> Alignment
           -> IO (SBEPred sbe, bytes)
    -- | @mmStore mem value addr@
  , mmStore :: SBEMemory sbe
            -> SBETerm sbe
            -> bytes
            -> Alignment
            -> IO (SBEPred sbe, SBEMemory sbe)
  , mmInitGlobal :: SBEMemory sbe
                 -> bytes
                 -> IO (Maybe (SBETerm sbe, SBEMemory sbe))
  , mmAddDefine :: SBEMemory sbe
                -> L.Symbol
                -> [L.BlockLabel]
                -> IO (Maybe (SBETerm sbe, [SBETerm sbe], SBEMemory sbe))
  , mmLookupSymbol :: SBEMemory sbe -> SBETerm sbe -> LookupSymbolResult
    -- | Alloc structure on stack
  , mmStackAlloc :: SBEMemory sbe -- ^ Memory
                 -> Size          -- ^ Size of each element
                 -> SBETerm sbe   -- ^ Number of elements
                 -> Alignment     -- ^ Alignment constraint in bytes.
                 -> IO (AllocResult sbe)
  , mmStackPush :: SBEMemory sbe -> IO (SBEPred sbe, SBEMemory sbe)
  , mmStackPop  :: SBEMemory sbe -> IO (SBEMemory sbe)
  , mmHeapAlloc :: SBEMemory sbe
                -> Size
                -> SBETerm sbe
                -> Alignment
                -> IO (AllocResult sbe)
  , mmMemCopy :: SBEMemory sbe
              -> SBETerm sbe            -- ^ Destination pointer
              -> SBETerm sbe            -- ^ Source pointer
              -> BitWidth               -- ^ Width of length value.
              -> SBETerm sbe            -- ^ Length value 
              -> SBETerm sbe            -- ^ Alignment in bytes (32-bit value)
              -> IO (SBEPred sbe, SBEMemory sbe) -- ^ Condition and new value.
    -- | Push a merge frame.
  , mmRecordBranch :: SBEMemory sbe -> IO (SBEMemory sbe)
    -- | Pop a merge frame without merging.
  , mmBranchAbort :: SBEMemory sbe -> IO (SBEMemory sbe)
    -- | @mmMux c t f@ returns a memory equivalent to @t@ when @c@ holds,
    -- and @f@ otherwise.  The number of merge frames 
  , mmMux :: SBEPred sbe -> SBEMemory sbe -> SBEMemory sbe -> IO (SBEMemory sbe)
  }

-- | A memory model over terms that are bits.
type BitBlastMemModel m l =
  MemModel (BitIO m l) (LV.Vector l)

-- | Load memory using
loadTerm :: (?be :: BitEngine l, Eq l, LV.Storable l)
         => DataLayout
         -> MemModel (BitIO m l) (LV.Vector l)
         -> m
         -> MemType -- ^ Type to read
         -> BitTerm l -- ^ Pointer to load
         -> Alignment
         -> IO (l, BitTerm l)
loadTerm dl mm bm tp ptr a = do
  (c,v) <- mmLoad mm bm ptr (memTypeSize dl tp) a
  case bytesToTerm dl tp v of
    Just t -> return (c,t)
    Nothing -> fail "Backend asked to read symblic floating point number."

-- | Store term in memory model.
storeTerm :: (?be :: BitEngine l, Eq l, LV.Storable l)
          => DataLayout
          -> MemModel (BitIO m l) (LV.Vector l)
          -> SBEMemory (BitIO m l)
          -> BitTerm l
          -> MemType
          -> BitTerm l
          -> Alignment
          -> IO (SBEPred (BitIO m l), SBEMemory (BitIO m l))
storeTerm dl mm m ptr tp v a
  | LV.length bytes == 0 = return (lTrue, m)
  | otherwise = mmStore mm m ptr bytes a
  where bytes = termToBytes dl tp v

-- BitMemory {{{1
-- Storage {{{2

-- | Tree-based data structure for representing value of bytes in memory.
-- The height of the storage tree is equal to the address width of
-- pointers in memory.  Leaves in the tree correspond to either:
-- * Single bytes
-- * The address of a LLVM definition.
-- * The address of a LLVM basic block
-- * An uninitalized, but allocated byte.
-- * An unallocated block of @2^n@ bytes.
-- Interior nodes are branches @SBranch f t@ that correspond to the value
-- of bits beneath that tree.  The top-most branch corresponds to the
-- branch for the most-significant bits, while lower branches correspond
-- to the less significant bits.
data Storage l
  = SBranch (Storage l) (Storage l) -- ^ Branch falseBranch trueBranch
  | SValue l l (LV.Vector l) -- ^ SValue allocatedBit initializedBit value
  | SDefine L.Symbol -- ^ Memory value for function definition.
  | SBlock L.Symbol L.BlockLabel -- ^ Memory value for block within function.
  | SUnallocated -- ^ A memory section that has not been allocated to the program.

-- A derived(Show)-like pretty printer for the Storage type
ppStorageShow :: (Eq l, LV.Storable l) => BitEngine l -> Storage l -> Doc
ppStorageShow be (SBranch f t) = text "SBranch" <+> parens (ppStorageShow be f) <+> parens (ppStorageShow be t)
ppStorageShow be (SValue a i v)
  = text "SValue" <+> pl a <+> pl i <+> parens (let ?be = be in lPrettyLV v)
  where pl = let ?be = be in lPrettyLit
ppStorageShow _ (SDefine d) = text ("SDefine " ++ show d)
ppStorageShow _ (SBlock d b) = text ("SBlock " ++ show d ++ " " ++ show b)
ppStorageShow _ SUnallocated = text "SUnallocated"

-- A "sparse" pretty printer for the Storage type; skips unallocated regions and
-- shows addresses explicitly.

ppStorage :: (Eq l, LV.Storable l) => Maybe [Range Addr] -> BitEngine l -> Storage l -> Doc
ppStorage mranges be = impl 0 Nothing
  where
    impl _ Nothing SUnallocated      = text "empty memory"
    impl a Nothing s                 = impl a (Just empty) s
    impl a mdoc (SBranch f t)        = let la = a `shiftL` 1
                                           ra = la `setBit` 0
                                       in impl ra (Just $ impl la mdoc f) t
    impl a (Just doc) (SValue al il v)
      | il == beTrue be = whenInRange a $ item doc a (let ?be = be in lPrettyLV v)
      | il == beFalse be = whenInRange a $ item doc a (text "uninitialized")
      | otherwise = whenInRange a
                  $ item doc a
                  $ (let ?be = be in lPrettyLV v) 
                     <+> parens (text "allocated:" <+> pl al <> comma
                                 <+> text "initialized:" <+> pl il)
    impl a (Just doc) (SDefine sym)  =
      whenInRange a $ item doc a $ ppSymbol sym
    impl a (Just doc) (SBlock s l)
      = whenInRange a $ item doc a 
      $ ppSymbol s <> char '/' <> text (show (L.ppLabel l))
    impl _ (Just doc) SUnallocated   = doc
    item doc addr desc               = doc <$$> text (showHex addr "") <> colon <+> desc
    pl = let ?be = be in lPrettyLit
    whenInRange a doc = case mranges of
      Nothing     -> doc
      Just ranges -> if inRangeAny a ranges then doc else empty

mergeStorage :: (Eq l, LV.Storable l)
             => BitEngine l -> l -> Storage l -> Storage l -> IO (Storage l)
mergeStorage be c x y = impl x y
  where impl (SBranch fx tx) (SBranch fy ty) = do
          f <- impl fx fy
          t <- impl tx ty
          return (SBranch f t)
        impl (SValue ax ix vx) (SValue ay iy vy) =
          return SValue `ap` beIte be c ax ay
                        `ap` beIte be c ix iy
                        `ap` beIteVector be c (return vx) (return vy)
        impl (SDefine dx) (SDefine dy)
          | dx == dy = return (SDefine dx)
          | otherwise = bmError "Attempt to merge memories with incompatible definitions."
        impl (SBlock dx bx) (SBlock dy by)
          | dx == dy && bx == by = return (SBlock dx bx)
          | otherwise = bmError "Attempt to merge memories with incompatible block values."
        impl SUnallocated SUnallocated = return SUnallocated
        impl (SValue ax ix vx) SUnallocated =
          (\az -> SValue az ix vx) <$> beAnd be c ax
        impl SUnallocated (SValue ay iy vy) =
          (\az -> SValue az iy vy) <$> beAnd be (beNeg be c) ay
        impl b@SBranch{} SUnallocated =
          impl b (SBranch SUnallocated SUnallocated)
        impl SUnallocated b@SBranch{} =
          impl (SBranch SUnallocated SUnallocated) b
        impl a b = do
          dbugM $ "mergeStorage failure case: a = " ++ show (ppStorageShow be a)
          dbugM $ "mergeStorage failure case: b = " ++ show (ppStorageShow be b)
          bmError "Attempt to merge incompatible valid addresses."

-- | @loadBytes be mem ptr size@ returns term representing all the bits with
-- given size.
loadBytes :: (?be :: BitEngine l, Eq l, LV.Storable l)
          => (LV.Vector l -> IO (l, LV.Vector l))
          -> BitTerm l
          -> Size
          -> Alignment
          -> IO (l, LV.Vector l)
loadBytes byteLoader (PtrTerm ptr) sz _ =
    impl [] lTrue (toInteger sz)
  where impl l c 0 = return (c, LV.concat l)
        impl l c i = do
          (bc, bv) <- byteLoader =<< beAddIntConstant ?be ptr (i-1)
          c' <- beAnd ?be c bc
          impl (bv:l) c' (i-1)
loadBytes _ _ _ _ = illegalArgs "loadBytes"

-- | Returns condition under which store is permitted.
storeByteCond :: (Eq l, LV.Storable l) => BitEngine l -> Storage l -> LV.Vector l -> IO l
storeByteCond be mem ptr = impl mem (LV.length ptr)
  where impl (SBranch f t) i =
          beLazyMux be (beIte be) (ptr LV.! (i-1)) (impl t (i-1)) (impl f (i-1))
        impl (SValue ax _ _) i = assert (i == 0) $ return ax
        impl _ _ = return (beFalse be)

-- | Return storage with individual byte changed.
storeByte :: (Eq l, LV.Storable l)
          => BitEngine l -- ^ Bit engine for bits
          -> Storage l -- ^ Memory to update
          -> LV.Vector l -- ^ Value to write
          -> LV.Vector l -- ^ Address to write to
          -> IO (Storage l)
storeByte be mem new ptr = impl mem (LV.length ptr) (beTrue be)
  where impl (SBranch f t) i c
          | lo == beFalse be = (\fr -> SBranch fr t) <$> impl f (i-1) c
          | lo == beTrue be  = (\tr -> SBranch f tr) <$> impl t (i-1) c
          | otherwise = do
            fr <- impl f (i-1) =<< beAnd be c (beNeg be lo)
            tr <- impl t (i-1) =<< beAnd be c lo
            return (SBranch fr tr)
          where lo = ptr LV.! (i-1)
        impl (SValue ax ix vx) i c = assert (i == 0) $
          return (SValue ax) `ap` beOr be c ix
                             `ap` beIteVector be c (return new) (return vx)
        impl m _ _ = return m

storeBytes :: (Eq l, LV.Storable l)
           => BitEngine l
           -> Storage l      -- ^ Base storage
           -> LV.Vector l    -- ^ Address to store value in
           -> LV.Vector l    -- ^ Value to store
           -> Alignment
           -> IO (l, Storage l) -- ^ Condition for address to be valid, and updated storage.
storeBytes be mem ptr value _ = impl 0 (beTrue be) mem
  where bv = sliceIntoBytes value
        impl i c m
          | i == byteSize value = return (c,m)
          | otherwise = do
            p <- beAddIntConstant be ptr (toInteger i)
            c' <- beAnd be c =<< storeByteCond be m p
            m' <- storeByte be m (bv V.! i) p
            impl (i+1) c' m'

loadDef :: Storage l -> Int -> Addr -> Maybe L.Symbol
loadDef s w a = impl s (w-1)
  where impl (SBranch f t) i = impl (if testBit a i then t else f) (i-1)
        impl (SDefine d) _ = Just d
        impl _ _ = Nothing

-- @setBytes w low high val mem@ sets all bytes in [low .. high) to @val@.
-- The address with is w.
setBytes :: Int -> Addr -> Addr -> (Addr -> Storage l) -> Storage l -> Storage l
setBytes w low high fn mem
   | low == high = mem
   | otherwise = impl mem (w-1) 0
  where impl _ (-1) v = fn v
        impl (SBranch f t) i v =
           SBranch (if low < mid  then impl f (i-1) v   else f)
                   (if mid < high then impl t (i-1) mid else t)
          where mid = v + 2 ^ i
        impl SUnallocated i v =
           SBranch (if low < mid  then impl f (i-1) v   else f)
                   (if mid < high then impl t (i-1) mid else t)
          where mid = v + 2 ^ i
                f = SUnallocated
                t = SUnallocated
        impl _ _ _ = bmError "internal: Malformed storage"

-- Allocator and allocation {{{2
-- @uninitRegion w low high@ marks all bytes in [low..high) as uninitialized.
-- N.B. @w@ is the width of pointers in bits.
uninitRegion :: LV.Storable l => BitEngine l -> Int -> Addr -> Addr -> Storage l -> Storage l
uninitRegion be ptrWidth low high = setBytes ptrWidth low high (const v)
  where v = SValue (beTrue be) (beFalse be) (beDontCareByte be)

-- FreeList {{{2

-- | Free list maps each index i to the address of blocks with 2^i
-- eleements that are free.
type FreeList = V.Vector [Addr]

-- | @initFreeList low high@ returns free list containing bytes from @[low..high)@.
initFreeList :: Addr -> Addr -> FreeList
initFreeList low high = V.unfoldr fn (low,high,0)
  where fn (l,h,i) | l >= h = Nothing
                   | otherwise =  Just $
                       let hc = h `clearBit` i
                           l' = if l `testBit` i then l + 1 `shiftL` i else l
                           h' = if h `testBit` i then hc else h
                           elts = case (l `testBit` i, h `testBit` i) of
                                    (True,  True)  -> [l,hc]
                                    (False, True)  -> [hc]
                                    (True, False)  -> [l]
                                    (False, False) -> []
                        in (elts, (l', h', i+1))

-- | @allocBlock l n@ attempt to allocate a block with @2^n@
allocBlock :: FreeList -> Int -> Maybe (FreeList, Addr)
allocBlock fl n = impl n
  where -- @impl i@
        impl i | V.length fl <= i = Nothing
               | otherwise =
                   case fl V.! i of
                     [] -> impl (i+1)
                     a:r ->
                       -- Generation function for new free list removes
                       -- free block found and adds new free blocks for
                       -- space that was unallocated by previous request.
                       let genFn j | j < n = fl V.! j
                                   | j < i = (a + 2 ^ j) : (fl V.! j)
                                   | j == i = r
                                   | otherwise = fl V.! j
                        in Just (V.generate (V.length fl) genFn, a)

-- BitMemory {{{2

data BitMemory l = BitMemory {
    -- | Stores state of memory.
    bmStorage :: Storage l
    -- | Current address of stack
  , bmStackAddr :: Addr
    -- | Maximum address for stack.
  , bmStackEnd :: Addr
    -- | Address for initial code pointers.
  , bmCodeAddr :: Addr
    -- | Maximum address for code pointers.
  , bmCodeEnd :: Addr
    -- | Address for initial global data pointers (and constants)
  , bmDataAddr :: Addr
    -- | Maximum address for data pointers.
  , bmDataEnd :: Addr
    -- | Starting address for heap pointers
  , bmHeapAddr :: Addr
    -- | Maximum address for heap pointers
  , bmHeapEnd  :: Addr
    -- | The heap freelist
  , bmFreeList :: V.Vector [Addr]
    -- | Frames on stack.
  , bmStackFrames :: [Integer]
  }

-- | Returns true if stack addresses increase as more elements are pushed on
-- stack.
bmStackGrowsUp :: BitMemory l -> Bool
bmStackGrowsUp bm = bmStackAddr bm <= bmStackEnd bm

bmDump :: (Eq l, LV.Storable l)
  => BitEngine l -> Bool -> BitMemory l -> Maybe [Range Addr] -> IO ()
bmDump be sparse bm mranges = do
  banners $ show $
    text "Memory Model Dump" <$$>
    text "Stack Range:" <+> text (h $ bmStackAddr bm) <> comma <+> text (h $ bmStackEnd bm) <$$>
    text "Code Range:"  <+> text (h $ bmCodeAddr bm) <> comma <+> text (h $ bmCodeEnd bm) <$$>
    text "Data Range:"  <+> text (h $ bmDataAddr bm) <> comma <+> text (h $ bmDataEnd bm) <$$>
    text "Heap Range:"  <+> text (h $ bmHeapAddr bm) <> comma <+> text (h $ bmHeapEnd bm) <$$>
    text "Frame pointers:" <+> hcat (punctuate comma (map text $ map hx $ bmStackFrames bm)) <$$>
    text "Storage:" <$$> 
    (if sparse then ppStorage mranges else ppStorageShow) be (bmStorage bm)
  where
    h s  = showHex s ""
    hx s = "0x" ++ h s
--     fl i as = text "Size = 2^" <> int i <> colon <+>
--               sep (punctuate comma (map (text . h) as))

-- | @loadBytes be mem ptr size@ returns term representing all the bits with given size.
bmLoadByte :: (Eq l, LV.Storable l)
         => BitEngine l
         -> BitMemory l
         -> LV.Vector l
         -> IO (l, LV.Vector l)
bmLoadByte be bm vi =
  let load (SBranch f t) i =
        mergeCondVector be (vi LV.! i) (load t (i-1)) (load f (i-1))
      load (SValue _ i v) _ = return (i, v)
      load _ _ = return (beFalse be, beDontCareByte be)
   in load (bmStorage bm) (LV.length vi - 1)

bmMux :: (Eq l, LV.Storable l)
      => BitEngine l
      -> l -> BitMemory l -> BitMemory l -> IO (BitMemory l)
bmMux be c m m' = do
  unless (bmStackAddr m == bmStackAddr m') $
    fail "Attempt to merge memories at different stack addresses is unsupported."
  unless (bmStackEnd m == bmStackEnd m') $
    fail "internal: Attempt to merge memories with different stack maximum addresses."
  unless (bmStackFrames m == bmStackFrames m') $
    fail "Attempt to merge memories with different stack frames."
  unless (bmCodeAddr m == bmCodeAddr m') $
    fail "internal: Attempt to merge memories with different code addresses."
  unless (bmCodeEnd m == bmCodeEnd m') $
    fail "internal: Attempt to merge memories with different code endpoints."
  unless (bmDataAddr m == bmDataAddr m') $
    fail "Attempt to merge memories with different data segment addresses."
  unless (bmDataEnd m == bmDataEnd m') $
    fail "internal: Attempt to merge memories with different data segment endpoints."
  newStorage <- mergeStorage be c (bmStorage m) (bmStorage m')
  -- Free lists should be implicitly equivalent if storages are compatible.
  return m { bmStorage = newStorage }

bmInitGlobalBytes :: (Eq l, LV.Storable l)
                  => BitEngine l
                  -> Int         -- ^ Width of pointers in bits.
                  -> BitMemory l
                  -> LV.Vector l
                  -> IO (Maybe (BitTerm l, BitMemory l))
bmInitGlobalBytes be ptrWidth m bytes
  | newDataAddr > bmDataEnd m = return Nothing
  | otherwise = do
      let ptrv = beVectorFromInt be ptrWidth dataAddr
          mem  = uninitRegion be ptrWidth dataAddr newDataAddr (bmStorage m)
      (c,newStorage) <- storeBytes be mem ptrv bytes 0
      assert (c == beTrue be) $
        return $ Just ( PtrTerm ptrv
                      , m { bmStorage = newStorage, bmDataAddr = newDataAddr })
  where
    dataAddr    = bmDataAddr m
    newDataAddr = dataAddr + toInteger (byteSize bytes)

bmAddDefine :: (Eq l, LV.Storable l)
            => BitEngine l -- ^ Bit engine for literals.
            -> Int -- ^ Width of pointers
            -> BitMemory l -- ^ Memory
            -> L.Symbol -- ^ Definition
            -> [L.BlockLabel] -- ^ Labels for blocks
            -> Maybe (BitTerm l, [BitTerm l], BitMemory l)
bmAddDefine be ptrWidth m def (V.fromList -> blocks)
    | newCodeAddr > bmCodeEnd m
    = Nothing
    | otherwise
    = Just ( PtrTerm (beVectorFromInt be ptrWidth codeAddr)
           , blockAddrs
           , m { bmStorage = newStorage
               , bmCodeAddr = newCodeAddr
               }
           )
  where blockCount = toInteger $ V.length blocks
        newSpaceReq = 1 + blockCount
        codeAddr = bmCodeAddr m
        newCodeAddr = codeAddr + newSpaceReq
        updateAddr a | a == codeAddr = SDefine def
                     | otherwise     = SBlock def (blocks V.! fromInteger (a - codeAddr - 1))
        newStorage = setBytes ptrWidth codeAddr newCodeAddr updateAddr (bmStorage m)
        blockAddrs = PtrTerm . beVectorFromInt be ptrWidth . (codeAddr +) <$> [1..blockCount]

-- | Return symbol as given address in memory.
bmLookupSymbol :: (Eq l, LV.Storable l)
               => BitEngine l
               -> BitMemory l
               -> BitTerm l -- Pointer to symbol
               -> LookupSymbolResult
bmLookupSymbol be m (PtrTerm a) = do
  case beVectorToMaybeInt be a of
    Nothing -> Indeterminate
    Just (w, v) ->
      case loadDef (bmStorage m) w v of
        Nothing -> Invalid
        Just d -> LookupResult d
bmLookupSymbol _ _ _ = illegalArgs "bmLookupSymbol"

bmStackAlloc :: (Eq l, LV.Storable l)
             => BitEngine l
             -> Int       -- ^ Width of pointer in bits.
             -> BitMemory l
             -> Size      -- ^ Element size
             -> BitTerm l -- ^ Number of elements
             -> Alignment -- ^ Alignment constraint
             -> AllocResult (BitIO (BitMemory l) l)
bmStackAlloc be ptrWidth bm eltSize (IntTerm cntVector) a =
  case beVectorToMaybeInt be cntVector of
    Nothing -> ASymbolicCountUnsupported
    Just (_,cnt) -> r
      where mkRes c res endAddr newAddr = AResult (beLitFromBool be c) ptr bm'
              where newStorage = uninitRegion be ptrWidth res endAddr (bmStorage bm)
                    ptr = PtrTerm (beVectorFromInt be ptrWidth res)
                    bm' = bm { bmStorage = newStorage, bmStackAddr = newAddr }
            -- Get new bit memory.
            r | bmStackGrowsUp bm = 
                  let a' = alignUp (bmStackAddr bm) a
                      sa = a' + toInteger eltSize * cnt
                   in mkRes (sa <= bmStackEnd bm) a' sa sa
              | otherwise =
                  let sz = toInteger eltSize * cnt
                      a' = alignDn (bmStackAddr bm - sz) a
                   in mkRes (a' >= bmStackEnd bm) a' (a' + sz) a'
bmStackAlloc _ _ _ _ _ _ = illegalArgs "bmStackAlloc"

-- | Push stack frame to memory.
bmStackPush :: BitMemory l -> BitMemory l
bmStackPush bm = bm { bmStackFrames = bmStackAddr bm : bmStackFrames bm }

-- | Pop stack frame in memory and invalidate old addresses.
bmStackPop :: Int -- ^ Width of pointer in bits.
           -> BitMemory l
           -> BitMemory l
bmStackPop _ BitMemory { bmStackFrames = [] } =
  bmError "internal: Attempted to pop stack frame from memory when no stack frames have been pushed."
bmStackPop ptrWidth bm@BitMemory { bmStackFrames = f : fl } =
  bm { bmStorage =
         let (l,h) | bmStackGrowsUp bm = (f, bmStackAddr bm)
                   | otherwise = (bmStackAddr bm, f)
          in setBytes ptrWidth l h (\_ -> SUnallocated) (bmStorage bm)
     , bmStackAddr = f
     , bmStackFrames = fl
     }

blockPower :: Integer -> Int
blockPower v = go 0 1
  where go p n | n >= v = p
               | otherwise = go (p + 1) (n `shiftL` 1)

bmHeapAlloc :: (Eq l, LV.Storable l)
            => BitEngine l
            -> Int       -- ^ Width of pointer in bits.
            -> BitMemory l
            -> Size      -- ^ Element size
            -> BitTerm l -- ^ Number of elements
            -> Alignment -- ^ Alignment constraint
            -> AllocResult (BitIO (BitMemory l) l)
bmHeapAlloc be ptrWidth bm eltSize (IntTerm cntVector) a =
  case beVectorToMaybeInt be cntVector of
    Nothing -> ASymbolicCountUnsupported
    Just (_, cnt) ->
        case allocBlock (bmFreeList bm) pwr of
          Just (freeList, addr) ->
               AResult (beTrue be)
                       addrTerm
                       bm { bmFreeList = freeList
                          , bmStorage = newStorage
                          }
            where endAddr = addr + size
                  size = 2 ^ pwr
                  addrTerm = PtrTerm $ beVectorFromInt be ptrWidth addr
                  newStorage = uninitRegion be ptrWidth addr endAddr (bmStorage bm)
          Nothing -> AResult (beFalse be) zeroTerm bm
            where zeroTerm = PtrTerm (beVectorFromInt be ptrWidth 0)
      where -- Pad up to the end of the aligned region; we do this because any
            -- global data that gets copied into this space will be padded to this
            -- size by L.
            sz = toInteger eltSize * cnt
            padBytes = nextMultiple (2 ^ a :: Integer) sz - sz
            pwr = blockPower (sz + padBytes)
bmHeapAlloc _ _ _ _ _ _ = illegalArgs "bmHeapAlloc"

bmMemCopy :: (Eq l, LV.Storable l)
          => BitEngine l -- ^ Bit engine for literals
          -> BitMemory l
          -> BitTerm l   -- ^ Destination pointer
          -> BitTerm l   -- ^ Source pointer
          -> BitWidth    -- ^ Width of length value.
          -> BitTerm l   -- ^ Length value
          -> BitTerm l   -- ^ Alignment in bytes
          -> IO (l, BitMemory l)
bmMemCopy be m (PtrTerm dst) src _ (IntTerm len0) (IntTerm _align0) = do
  -- TODO: Alignment and overlap checks?
  let ?be = be
  (cr, bytes) <- loadBytes (bmLoadByte be m) src len 0
  (cw, newStorage) <- storeBytes be (bmStorage m) dst bytes 0
  c <- beAnd be cr cw
  return (c, m { bmStorage = newStorage })
  where
    len = case beVectorToMaybeInt be len0 of
            Nothing    -> bmError $ "Symbolic memcpy len not supported"
            Just (_,x) -> fromInteger x

bmMemCopy _ _ _ _ _ _ _ = illegalArgs "bmMemCopy"

-- | Memory model for explicit buddy allocation scheme.
buddyMemModel :: (Eq l, LV.Storable l)
              => DataLayout
              -> BitEngine l
              -> BitBlastMemModel (BitMemory l) l
buddyMemModel dl be = mm
 where ptrWidth = ptrBitwidth dl
       mm = MemModel {
                mmDump = bmDump be
              , mmLoad = \m ->
                 let ?be = be in loadBytes (bmLoadByte be m)
              , mmStore = \m (PtrTerm ptr) bytes a -> do
                 Arrow.second (\s -> m { bmStorage = s }) <$>
                   storeBytes be (bmStorage m) ptr bytes a
              , mmInitGlobal = bmInitGlobalBytes be ptrWidth
              , mmAddDefine = return `c3` bmAddDefine be ptrWidth
              , mmLookupSymbol = bmLookupSymbol be
              , mmStackAlloc = return `c4` bmStackAlloc be ptrWidth
              , mmStackPush = \mem -> return (beTrue be, bmStackPush mem)
              , mmStackPop = return . bmStackPop ptrWidth
              , mmHeapAlloc = return `c4` bmHeapAlloc be ptrWidth
              , mmMemCopy = bmMemCopy be
              , mmRecordBranch = return -- do nothing
              , mmBranchAbort = return -- do nothing
              , mmMux = bmMux be
              }

illegalArgs :: String -> a
illegalArgs nm = error $ "Illegal arguments to " ++ nm

buddyInitMemory :: MemGeom -> BitMemory l
buddyInitMemory mg =
  case mgSanityCheck mg of
    Just msg -> bmError ("internal: " ++ msg)
    Nothing ->
      BitMemory { bmStorage = SUnallocated
                , bmStackAddr = start (mgStack mg)
                , bmStackEnd = end (mgStack mg)
                , bmStackFrames = []
                , bmCodeAddr = start (mgCode mg)
                , bmCodeEnd = end (mgCode mg)
                , bmDataAddr = start (mgData mg)
                , bmDataEnd = end (mgData mg)
                , bmHeapAddr = start (mgHeap mg)
                , bmHeapEnd = end (mgHeap mg)
                , bmFreeList = initFreeList (start (mgHeap mg)) (end (mgHeap mg))
                }

createBuddyAll :: (Ord l, LV.Storable l)
               => BitEngine l
               -> DataLayout
               -> MemGeom
               -> SBEPair
createBuddyAll be dl mg = SBEPair sbe mem0
  where sbe = let ?be = be in sbeBitBlast dl (buddyMemModel dl be)
        mem0 = buddyInitMemory mg

createBuddyMemModel :: (Eq l, LV.Storable l)
                    => DataLayout
                    -> BitEngine l
                    -> MemGeom
                    -> IO ( BitBlastMemModel (BitMemory l) l
                          , BitMemory l)
createBuddyMemModel dl be mg =
  return (buddyMemModel dl be, buddyInitMemory mg)

-- DagMemory {{{1

type RefIdx = IORef Int

data DagMemoryState l =
   DMS { dmsStack :: LV.Vector l
       , dmsData :: Addr
       , dmsHeap :: LV.Vector l
       }

data DagMemory l = DagMemory {
    dmNodeIdx :: !Int
  , dmNodeApp :: !(DMApp l)
  , dmState :: DagMemoryState l
    -- | Frames on stack.
  , dmStackFrames :: [LV.Vector l]
    -- | Address for next value in code segment.
  , dmCode :: Addr
     -- | Maps concrete addresses to associated symbol.
  , dmDefineMap :: Map Addr L.Symbol
    -- Returns literal indicating if range is allocated.
  , dmIsAllocated :: Range (LV.Vector l) -> l
    -- Returns literal indicating if range is initialized.
  , dmIsInitialized :: Range (LV.Vector l) -> l
    -- Returns byte associated with given address (only valid when dmIsInitialized returns true for range covering byte).
  , dmLoadByte :: LV.Vector l -> LV.Vector l
  , dmMergeDepth :: !Int
  }

instance Eq (DagMemory l) where
  x == y = dmNodeIdx x == dmNodeIdx y

instance Ord (DagMemory l) where
  x `compare` y = dmNodeIdx x `compare` dmNodeIdx y

-- Query operations:
--  Load
--  Allocat


data DMMod t
     -- | @DMStore addr end valueBytes@.  The range [addr end) is
     -- non-empty.
   = DMStore t t (V.Vector t)
     -- | @DMMemCopy dest destEnd src@.  The range [dest destEnd)
     -- may be empty.
   | DMMemCopy t t t

dmModStart :: DMMod t -> t
dmModStart (DMStore s _ _) = s
dmModStart (DMMemCopy s _ _) = s

dmModEnd :: DMMod t -> t
dmModEnd (DMStore _ e _) = e
dmModEnd (DMMemCopy _ e _) = e

type Byte l = LV.Vector l

type SymAddr l = LV.Vector l

data DMApp l
   = DMInitial
     -- | @DMAddDefine s bl p@ denotes memory obtained from adding definition to
     -- memory.
   | DMAddDefine L.Symbol (V.Vector L.BlockLabel) (DagMemory l)
     -- | @DMAlloc base end prev@ denotes the memory obtained by
     -- allocating bytes in @[base,end)@ to @prev@.
   | DMAlloc (SymAddr l) (SymAddr l) (DagMemory l)
   | DMStackPush (DagMemory l)
     -- | @DMStackPop base end prev@ denotes the memory obtained by?b
     -- deallocating bytes in @[base,end)@ to @prev@.  The range may
     -- be empty.
   | DMStackPop (SymAddr l) (SymAddr l) (DagMemory l)
   | DMMod (DMMod (LV.Vector l)) (DagMemory l)
   | DMMergeFramePush (DagMemory l) 
   | DMMergeFramePop (DagMemory l)
   | DMMux l (DagMemory l) (DagMemory l)

prettyMemIdx :: DagMemory l -> Doc
prettyMemIdx m = char '$' <> int (dmNodeIdx m)

lPrettyRange :: (?be :: BitEngine l, LV.Storable l) => Range (LV.Vector l) -> Doc
lPrettyRange (s,e) = char '[' <> lPrettyLV s <> comma <+> lPrettyLV e <> char ')'

dmPrintApp :: (?be :: BitEngine l, LV.Storable l) => DMApp l -> Doc
dmPrintApp app =
  case app of
    DMInitial -> text "initial"
    DMAddDefine d bl mem  -> text "define"    <+> text (show d) <+> pblocks bl <+> pm mem
    DMAlloc s e mem       -> text "alloc"     <+> pr (s,e) <+> pm mem
    DMStackPush mem       -> text "stackPush" <+> pm mem
    DMStackPop s e mem    -> text "stackPop"  <+> pr (s,e) <+> pm mem
    DMMod (DMStore s e bytes) mem -> text "store"     <+> pr (s,e) <+> text ":=" <+> pbytes bytes <+> pm mem
    DMMod (DMMemCopy s e src) mem -> text "memCopy"   <+> pr (s,e) <+> text ":= *" <> paddr src <+> pm mem
    DMMergeFramePush mem  -> text "mergePush" <+> pm mem
    DMMergeFramePop  mem  -> text "mergePop" <+> pm mem 
    DMMux c t f           -> text "mux"     <+> pl c <+> char '?' <+> pm t <+> char ':' <> pm f
 where pr = lPrettyRange
       pl = lPrettyLit
       pm m = prettyMemIdx m
       pv = brackets . hsep . punctuate comma . V.toList
       pblocks = pv . V.map (text . show)
       pbytes  = pv . V.map lPrettyLV
       paddr   = lPrettyLV

dmMemArgs :: DMApp l -> [DagMemory l]
dmMemArgs DMInitial = []
dmMemArgs (DMAddDefine _ _ m) = [m]
dmMemArgs (DMAlloc _ _ m) = [m]
dmMemArgs (DMStackPush m) = [m]
dmMemArgs (DMStackPop _ _ m) = [m]
dmMemArgs (DMMod _ m) = [m]
dmMemArgs (DMMergeFramePush m) = [m]
dmMemArgs (DMMergeFramePop m) = [m]
dmMemArgs (DMMux _ t f) = [t, f]

dmDump :: (?be :: BitEngine l, LV.Storable l)
       => Bool -> DagMemory l -> Maybe [Range Addr] -> IO ()
dmDump _ mem _ = do
  -- Steps: Build list of memory addresses to print out.
  let allNodes = lfp (dmMemArgs . dmNodeApp) (Set.singleton mem)
  forM_ (Set.toList allNodes) $ \m -> do
    putStrLn $ show $ prettyMemIdx m <> colon <+> dmPrintApp (dmNodeApp m)

-- | @loadBytes be mem ptr size@ returns term representing all the bits with given size.
dmLoadBytes :: (?be :: BitEngine l, Ord l, LV.Storable l)
            => DagMemory l
            -> BitTerm l
            -> Size
            -> Alignment
            -> IO (l, LV.Vector l)
dmLoadBytes _ (PtrTerm _) 0 _ = return (lTrue, LV.empty)
dmLoadBytes mem (PtrTerm ptr) sz _ = do
  let ptrOffset i = snd $ ptr `lFullAdd` lVectorFromInt (LV.length ptr) (toInteger i)
  return ( dmIsInitialized mem (ptr,ptrOffset sz)
         , LV.concat [ dmLoadByte mem (ptrOffset (i-1)) | i <- [1..sz] ]
         )
dmLoadBytes _ _ _ _ = illegalArgs "dmLoadBytes"

-- | Returns node with given app, creating it if necessary.  The function passed
-- in gives the opportunity to modify the node before it is cached.
dmGetMem :: LV.Storable l => RefIdx -> DMApp l -> DagMemory l -> IO (DagMemory l)
dmGetMem ref app newMem = do
  c <- readIORef ref
  let r = newMem { dmNodeIdx = c, dmNodeApp = app }
  writeIORef ref $! c + 1
  return r

dmLoadByteFromStore :: (?be :: BitEngine l, Ord l, LV.Storable l)
                    => Range (LV.Vector l)
                    -> V.Vector (LV.Vector l)
                    -> DagMemory l
                    -> (LV.Vector l -> LV.Vector l)
dmLoadByteFromStore (s,e) bytes mem p =
  lIteVector (p `lInRange` (s, e))
             (lMuxInteger lIteVector
                          (toInteger (V.length bytes - 1))
                          (snd (p `lFullSub` s))
                          (\i -> bytes V.! fromInteger i))
             (dmLoadByte mem p)

dmRawStore :: (?be :: BitEngine l, Ord l, LV.Storable l)
           => RefIdx
           -> LV.Vector l -> LV.Vector l -> V.Vector (Byte l) -> DagMemory l -> IO (DagMemory l)
dmRawStore ref b e bytes mem = do
  dmGetMem ref (DMMod (DMStore b e bytes) mem) $
    mem { dmIsInitialized = memo $ \range ->
            lRangeCovered (dmIsInitialized mem) range (b,e)
        , dmLoadByte = memo $ dmLoadByteFromStore (b,e) bytes mem
        }

dmStore :: (?be :: BitEngine l, Ord l, LV.Storable l)
           => RefIdx -> LV.Vector l -> LV.Vector l -> V.Vector (Byte l) -> DagMemory l -> IO (DagMemory l)
dmStore ref = simp
  where simp nb ne nvals (dmNodeApp -> DMMod m@(DMStore ob oe ovals) mem)
          | b `lEqLit` lTrue = dmMod ref m =<< simp nb ne nvals mem
          | b `lEqLit` lFalse
          , Just off <- lGetUnsigned diff
          , off <= toInteger (V.length nvals) =
            simp nb oe (nvals V.++ V.drop (fromInteger off) ovals) mem
          | (bo, lGetUnsigned -> Just off) <- oe `lFullSub` nb
          , bo `lEqLit` lFalse
          , off <= toInteger (V.length nvals) =
            simp ob ne (dropEnd (fromInteger off) ovals V.++ nvals) mem
         where (b, diff) = ne `lFullSub` ob
        simp nb ne nvals mem = dmRawStore ref nb ne nvals mem

dropEnd :: Int -> V.Vector a -> V.Vector a
dropEnd i v = V.take (V.length v - i) v

dmMod :: (?be :: BitEngine l, Ord l, LV.Storable l)
      => RefIdx -> DMMod (LV.Vector l) -> DagMemory l -> IO (DagMemory l)
dmMod ref (DMStore s e b) m = dmStore ref s e b m
dmMod ref (DMMemCopy s e src) m = dmMemCopyImpl ref s e src m

-- | Store bytes in memory
dmStoreBytes :: (?be :: BitEngine l, Ord l, LV.Storable l)
             => RefIdx
             -> DagMemory l
             -> BitTerm l
             -> LV.Vector l
             -> Alignment
             -> IO (l, DagMemory l)
dmStoreBytes ref mem (PtrTerm ptr) flatBytes _
  | byteCount == 0 = return (lTrue, mem)
  | otherwise = do
    --TODO: Figure out how to handle possibility that ptrEnd addition overflows.
    let (_of, ptrEnd) = ptr `lFullAdd` lVectorFromInt (LV.length ptr) (toInteger byteCount)
    m <- dmStore ref ptr ptrEnd newBytes mem
    return (dmIsAllocated mem (ptr,ptrEnd), m)
 where newBytes = sliceIntoBytes flatBytes
       byteCount = V.length newBytes
dmStoreBytes _ _ _ _ _ = illegalArgs "dmStoreBytes"

-- | Initialize global data memory.
dmInitGlobal :: (?be :: BitEngine l, Ord l, LV.Storable l)
             => Int  -- ^ Width of pointer
             -> Addr -- ^ End of data region
             -> RefIdx
             -> DagMemory l -> LV.Vector l -> IO (Maybe (BitTerm l, DagMemory l))
dmInitGlobal ptrWidth dataEnd ref mem flatBytes
  | byteCount == 0 = return $ Just (PtrTerm ptr, mem)
  | dataEnd - dmsData (dmState mem) < byteCount = return Nothing
  | otherwise = do
      mem1 <- dmAllocSpace ref (ptr,ptrEnd) ((dmState mem) { dmsData = nextData }) mem
      mem2 <- dmStore ref ptr ptrEnd bytes mem1
      -- Return result
      return $ Just (PtrTerm ptr, mem2)
  where bytes = sliceIntoBytes flatBytes
        byteCount = toInteger (V.length bytes)
        nextData = dmsData (dmState mem) + byteCount
        ptr = lVectorFromInt ptrWidth (dmsData (dmState mem))
        ptrEnd = lVectorFromInt ptrWidth nextData

dmAddDefine :: (?be :: BitEngine l, Ord l, LV.Storable l)
            => Int -- ^ width of pointers
            -> Addr -- ^ code end
            -> RefIdx
            -> DagMemory l -- ^ Memory
            -> L.Symbol -- ^ Definition
            -> [L.BlockLabel] -- ^ Labels for blocks
            -> IO (Maybe (BitTerm l, [BitTerm l], DagMemory l))
dmAddDefine ptrWidth codeEnd ref mem def blocks
   -- TODO: Alignment and overlap checks?
  | remaining >= bytesReq = do
      -- Get new memory
      m <- dmGetMem ref (DMAddDefine def (V.fromList blocks) mem) $
        mem { dmCode = ptr + bytesReq
            , dmDefineMap = Map.insert ptr def (dmDefineMap mem)
            }
      -- Return result
      return $ Just ( PtrTerm (lVectorFromInt ptrWidth ptr)
                    , blockAddrs
                    , m)
  | otherwise = return Nothing
  where blockCount = length blocks
        ptr = dmCode mem
        remaining = codeEnd - ptr
        bytesReq = 1 + toInteger blockCount
        blockAddrs = PtrTerm . lVectorFromInt ptrWidth . (ptr +) <$> [1..toInteger blockCount]


dmLookupSymbol :: (?be :: BitEngine l, Eq l, LV.Storable l)
               => DagMemory l -- ^ Memory
               -> BitTerm l   -- ^ Pointer to symbol
               -> LookupSymbolResult
dmLookupSymbol mem (PtrTerm a) = do
  case lGetUnsigned a of
    Nothing -> Indeterminate
    Just v ->
      case Map.lookup v (dmDefineMap mem) of
        Nothing -> Invalid
        Just d -> LookupResult d
dmLookupSymbol _ _ = illegalArgs "dmLookupSymbol"

dmAllocSpace :: (?be :: BitEngine l, Ord l, LV.Storable l)
             => RefIdx 
             -> Range (LV.Vector l)
             -> DagMemoryState l
             -> DagMemory l
             -> IO (DagMemory l)
dmAllocSpace ref (s, e) dms (dmNodeApp -> (DMAlloc ps pe mem))
  | pe `lEqVector` s == lTrue = dmAllocSpace ref (ps,e) dms mem
dmAllocSpace ref (s, e) dms (dmNodeApp -> (DMAlloc ps pe mem))
  | e `lEqVector` ps == lTrue = dmAllocSpace ref (s,pe) dms mem
dmAllocSpace ref (s, e) dms (dmNodeApp -> (DMMod m mem)) = do
  dmMod ref m =<< dmAllocSpace ref (s,e) dms mem
dmAllocSpace ref (s, e) dms mem = do
  dmGetMem ref (DMAlloc s e mem) $
    mem { dmState = dms
        , dmIsAllocated = memo $ \range ->
            lRangeCovered (dmIsAllocated mem) range (s, e)
        }

dmStackAlloc :: (?be :: BitEngine l, Ord l, LV.Storable l)
              => Int -- ^ Width of pointer in bits
              -> Bool -- ^ Flag indicates stack grows up
              -> LV.Vector l -- ^ End of stack
              -> RefIdx
              -> DagMemory l
              -> Size
              -> BitTerm l
              -> Alignment
              -> IO (AllocResult (BitIO (DagMemory l) l))
dmStackAlloc ptrWidth stackGrowsUp stackEnd ref mem eltSize (IntTerm eltCount) align = do
  let stack = dmsStack (dmState mem)
  -- Declare functions for extending and truncating vectors.
  let eltCountSize = LV.length eltCount
      extVector = (LV.++ LV.replicate eltCountSize lFalse)
      truncVector = LV.take ptrWidth
  let newSizeExt = lVectorFromInt ptrWidth (toInteger eltSize) `lFullMul` eltCount
  let stackEndExt = extVector stackEnd
  let (&&&) = lAnd
  let (.<=) = lUnsignedLeq
  let (c, truncVector -> newStack)
        | stackGrowsUp =
           let (ac, aStack) = lAlignUp stack align
               (ao, newStackExt) = extVector aStack `lFullAdd` newSizeExt
            in ( lNeg ac &&& lNeg ao &&& (newStackExt .<= stackEndExt)
               , newStackExt)
        | otherwise =
           let aStack = lAlignDn stack align
               (ab, newStackExt) = extVector aStack `lFullSub` newSizeExt
            in ( lNeg ab &&& (stackEndExt .<= newStackExt)
               , newStackExt)
  case () of
   _ | c == lFalse -> do
        return $ AResult c (PtrTerm stack) mem
     | stackGrowsUp -> do
        let a = (dmState mem) { dmsStack = newStack }
        m <- dmAllocSpace ref (stack, newStack) a mem
        return $ AResult c (PtrTerm stack) m
     | otherwise -> do
        let a = (dmState mem) { dmsStack = newStack }
        m <- dmAllocSpace ref (newStack, stack) a mem
        return $ AResult c (PtrTerm newStack) m
dmStackAlloc _ _ _ _ _ _ _ _ = illegalArgs "dmStackAlloc"

-- | Push stack frame to memory.
-- N.B. To avoid empty deallocations in stack pop, we always add a byte to
-- the stack when pushing.
dmStackPushFrame :: (?be :: BitEngine l, Ord l, LV.Storable l)
                 => RefIdx
                 -> DagMemory l -> IO (l, DagMemory l)
dmStackPushFrame ref mem = do
  r <- dmGetMem ref (DMStackPush mem) $
         mem { dmStackFrames = dmsStack (dmState mem) : dmStackFrames mem }
  return (lTrue, r)

-- | Pop stack frame in memory and invalidate old addresses.
dmStackPopFrame :: (?be :: BitEngine l, Ord l, LV.Storable l)
                => Bool -- ^ Flag indicating if stack should grow up in memory.
                -> RefIdx
                -> DagMemory l
                -> IO (DagMemory l)
dmStackPopFrame stackGrowsUp ref mem =
  case dmStackFrames mem of
   [] -> bmError "internal: Attempted to pop stack frame from memory when no stack frames have been pushed."
   f : fl -> do
     let (ptr,ptrEnd) | stackGrowsUp = (f, dmsStack (dmState mem))
                      | otherwise    = (dmsStack (dmState mem), f)
         eq = lEqVector
         leq = lUnsignedLeq
         x `isLeq` y = (x `lUnsignedLeq` y) `lEqLit` lTrue
         notInRange (s,e) = (e `leq` ptr) `lOr` (ptrEnd `leq` s) `lOr` (ptr `eq` ptrEnd)
     let simp (dmNodeApp -> (DMAlloc l e smem))
           | (ptr `isLeq` l) && (e `isLeq` ptrEnd)
           = simp smem
         simp (dmNodeApp -> (DMMod (DMStore l e _) smem))
           | (ptr `isLeq` l) && (e `isLeq` ptrEnd)
           = simp smem
         simp (dmNodeApp -> (DMMod m smem))
           | ptrEnd `isLeq` dmModStart m = dmMod ref m =<< simp smem
         simp (dmNodeApp -> (DMMod m smem))
           | dmModEnd m `isLeq` ptr = dmMod ref m =<< simp smem
         -- TODO: Add mark to stop simplification.
         simp (dmNodeApp -> DMStackPush smem) = return smem
         simp smem = do
           dmGetMem ref (DMStackPop ptr ptrEnd smem) $
             smem { dmState = (dmState smem) { dmsStack = f }
                  , dmStackFrames = fl
                  , dmIsAllocated = memo $ \range ->
                      notInRange range `lAnd` dmIsAllocated smem range
                  , dmIsInitialized = memo $ \range ->
                      notInRange range `lAnd` dmIsInitialized smem range
                  }
      in simp mem

dmHeapAlloc :: (?be :: BitEngine l, Ord l, LV.Storable l)
            => Int -- ^ Width of pointer in bits
            -> LV.Vector l -- ^ End of heap
            -> RefIdx
            -> DagMemory l -- ^ Memory
            -> Size        -- ^ Size of elements
            -> BitTerm l   -- ^ Number of elements
            -> Alignment         -- ^ Alignment
            -> IO (AllocResult (BitIO (DagMemory l) l))
dmHeapAlloc ptrWidth heapEnd ref mem eltSize (IntTerm eltCount) _align =
    AResult c (PtrTerm heap) <$> mmem
  where heap = dmsHeap (dmState mem)
        eltCountSize = LV.length eltCount
        newSizeExt = lVectorFromInt ptrWidth (toInteger eltSize) `lFullMul` eltCount
        extVector = (LV.++ LV.replicate eltCountSize lFalse)
        truncVector = LV.take ptrWidth
        (ao, newHeapExt) = extVector heap `lFullAdd` newSizeExt
        c = lNeg ao `lAnd` (newHeapExt `lUnsignedLeq` extVector heapEnd)
        mmem | c == lFalse = return mem
             | otherwise = dmAllocSpace ref (heap, newHeap) a mem
          where a = (dmState mem) { dmsHeap = newHeap }
                newHeap = truncVector newHeapExt
dmHeapAlloc _ _ _ _ _ _ _ = illegalArgs "dmHeapAlloc"

dmMemCopyImpl :: (?be :: BitEngine l, Ord l, LV.Storable l)
              => RefIdx 
              -> LV.Vector l -- ^ Destination start
              -> LV.Vector l -- ^ Destination end
              -> LV.Vector l -- ^ Source start
              -> DagMemory l -- ^ Memory to start with
              -> IO (DagMemory l)
dmMemCopyImpl ref dest destEnd src mem =
  dmGetMem ref (DMMod (DMMemCopy dest destEnd src) mem) $
     mem { dmIsInitialized = memo $ \range ->
             lRangeCovered (dmIsInitialized mem) range (dest, destEnd)
         , dmLoadByte = memo $ \p ->
             let (b,offset) = p `lFullSub` dest
                 inRange = lNeg b `lAnd` (p `lUnsignedLt` destEnd)
              in dmLoadByte mem (lIteVector inRange (snd (src `lFullAdd` offset)) p)
         }
  

-- | Store bytes in memory
dmMemCopy :: (?be :: BitEngine l, Ord l, LV.Storable l)
          => Int -- ^ Pointer width
          -> RefIdx
          -> DagMemory l
          -> BitTerm l   -- ^ Destination pointer
          -> BitTerm l   -- ^ Source pointer
          -> BitWidth    -- ^ Width of length value. 
          -> BitTerm l   -- ^ Length value
          -> BitTerm l   -- ^ Alignment in bytes
          -> IO (l, DagMemory l)
dmMemCopy ptrWidth ref mem (PtrTerm dest) (PtrTerm src) _ (IntTerm l) _
 | LV.length src /= ptrWidth = bmError "internal: src pointer size does not match pointer width."
 | LV.length dest /= ptrWidth = bmError "internal: dest pointer size does not match pointer width"
 | otherwise = do
    let lWidth = LV.length l
    let lext | lWidth == ptrWidth = l
             | lWidth >= ptrWidth = LV.drop (lWidth - ptrWidth) l
             | otherwise = LV.replicate (ptrWidth - lWidth) lFalse LV.++ l
    let ( srcOverflow,  srcEnd) =  src `lFullAdd` lext
    let (destOverflow, destEnd) = dest `lFullAdd` lext
    let lenOverflow | lWidth >= ptrWidth = lIsNonZero (LV.take (lWidth - ptrWidth) l)
                    | otherwise = lFalse
    let addrOverflow = lenOverflow `lOr` (srcOverflow `lOr` destOverflow)
                             -- Check src is readable and dest is writable
    let memValid = lAnd (dmIsInitialized mem (src, srcEnd))
                        (dmIsAllocated mem (dest, destEnd))
    let c = lIsZero l `lOr` (lNeg addrOverflow `lAnd` memValid)
    (c,) <$> dmMemCopyImpl ref dest destEnd src mem
dmMemCopy _ _ _ _ _ _ _ _ = illegalArgs "dmMemCopy"

dmRecordBranch :: (?be :: BitEngine l, LV.Storable l) => RefIdx -> DagMemory l -> IO (DagMemory l)
-- We can essentially undo merge frame changes if no merges happened since pop.
--dmRecordBranch _ (dmNodeApp -> DMMergeFramePop mem) = do
--  return mem
dmRecordBranch ref mem =
  dmGetMem ref (DMMergeFramePush mem) mem { dmMergeDepth = dmMergeDepth mem + 1 }

dmBranchAbort :: LV.Storable l => RefIdx -> DagMemory l -> IO (DagMemory l)
dmBranchAbort ref mem 
  | d <= 0 = error "internal: dmBranchAbort called on negative merge depth"
  | otherwise = do
      dmGetMem ref (DMMergeFramePop mem) mem { dmMergeDepth = d - 1 }
 where d = dmMergeDepth mem

dmMux :: (?be :: BitEngine l, Ord l, LV.Storable l)
      => RefIdx
      -> l -> DagMemory l -> DagMemory l -> IO (DagMemory l)
dmMux ref c t f = do
  unless (length (dmStackFrames t) == length (dmStackFrames f)) $
    fail "internal: Attempt to merge memories with different numbers of stacks pushed."
  unless (dmCode t == dmCode f) $
    fail "internal: Attempt to merge memories with different code addresses."
  unless (dmMergeDepth t == dmMergeDepth f) $
    fail "internal: Attempt to merge memories with different merge depths."
  when (dmMergeDepth t == 0) $
    fail "Merging before branch"
  let ta = dmState t
  let fa = dmState f
  unless (dmsData ta == dmsData fa) $
    fail "Attempt to merge memories with different data segment addresses."
  let mux = lIte c
  dmGetMem ref (DMMux c t f) $
    t { dmState = DMS { dmsStack = LV.zipWith mux (dmsStack ta) (dmsStack fa)
                      , dmsData = dmsData ta
                      , dmsHeap = LV.zipWith mux (dmsHeap ta) (dmsHeap fa) }
      , dmStackFrames   = zipWith (LV.zipWith mux) (dmStackFrames t) (dmStackFrames f)
      , dmMergeDepth = dmMergeDepth t - 1
      , dmIsAllocated   = memo $ \r -> mux (dmIsAllocated   t r) (dmIsAllocated   f r)
      , dmIsInitialized = memo $ \r -> mux (dmIsInitialized t r) (dmIsInitialized f r)
      , dmLoadByte      = memo $ \p -> LV.zipWith mux (dmLoadByte t p) (dmLoadByte f p)
      }


createDagMemModel :: (Ord l, LV.Storable l)
                  => DataLayout
                  -> BitEngine l
                  -> MemGeom
                  -> IO (BitBlastMemModel (DagMemory l) l, DagMemory l)
createDagMemModel dl be mg = do
  let ?be = be
  let ptrWidth = ptrBitwidth dl
  let stackGrowsUp = not (decreasing (mgStack mg))
  ref <- newIORef 1
  let ptrStart range = lVectorFromInt ptrWidth (start range)
  let ptrEnd range = lVectorFromInt ptrWidth (end range)
  let mm = MemModel {
               mmLoad = dmLoadBytes
             , mmStore = dmStoreBytes ref
             , mmInitGlobal = dmInitGlobal ptrWidth (end (mgData mg)) ref
             , mmDump = dmDump
             , mmAddDefine = dmAddDefine ptrWidth (end (mgCode mg)) ref
             , mmLookupSymbol = dmLookupSymbol
             , mmStackAlloc = dmStackAlloc ptrWidth stackGrowsUp (ptrEnd (mgStack mg)) ref
             , mmStackPush = dmStackPushFrame ref
             , mmStackPop = dmStackPopFrame stackGrowsUp ref
             , mmHeapAlloc = dmHeapAlloc ptrWidth (ptrEnd (mgHeap mg)) ref
             , mmMemCopy = dmMemCopy ptrWidth ref
             , mmRecordBranch = dmRecordBranch ref
             , mmBranchAbort = dmBranchAbort ref
             , mmMux = dmMux ref
             }
  let mem = DagMemory { dmNodeIdx = 0
                      , dmNodeApp = DMInitial
                      , dmState = DMS { dmsStack = ptrStart (mgStack mg)
                                      , dmsData  = start (mgData mg)
                                      , dmsHeap  = ptrStart (mgHeap mg)
                                      }
                      , dmStackFrames = []
                      , dmCode = start (mgCode mg)
                      , dmMergeDepth = 0
                      , dmDefineMap = Map.empty
                      , dmIsAllocated = uncurry lEqVector
                      , dmIsInitialized = uncurry lEqVector
                      , dmLoadByte = const (beDontCareByte be)
                      }
  return (mm, mem)

createDagAll :: (Ord l, LV.Storable l)
             => BitEngine l
             -> DataLayout
             -> MemGeom
             -> IO SBEPair
createDagAll be dl mg = do
    uncurry SBEPair . Arrow.first (sbeBitBlast dl) <$> createDagMemModel dl be mg
  where ?be = be

-- Aiger operations {{{1

evalAigerImpl :: (?be :: BitEngine l, Eq l, LV.Storable l)
              => DataLayout -> [Bool] -> MemType -> BitTerm l -> IO (BitTerm l)
evalAigerImpl dl inps tp t = do
  unflattenTerm dl tp <$> beEvalAigV ?be (LV.fromList inps) (flattenTerm t)

--  SBE Definition {{{1

newtype BitIO m l v = BitIO { liftSBEBitBlast :: IO v }
  deriving (Monad, MonadIO, Functor)

type instance SBETerm (BitIO m l)       = BitTerm l
type instance SBEPred (BitIO m l)       = l
type instance SBEMemory (BitIO m l)     = m

type BitBlastSBE m l = SBE (BitIO m l)

beZeroIntCoerce :: (Eq l, LV.Storable l) => BitEngine l -> Int -> LV.Vector l -> LV.Vector l
beZeroIntCoerce be r t
    | r > l = beZext be r t
    | r < l = beTrunc be r t
    | otherwise = t
  where l = LV.length t

applyIntArithOp :: (Eq l, LV.Storable l) 
                => IntArithOp
                -> OptVectorLength
                -> BitEngine l
                -> BitTerm l
                -> BitTerm l
                -> IO (BitTerm l)
applyIntArithOp op = vf
  where vf (Just n) be (VecTerm x) (VecTerm y)
          | V.length x == n && V.length y == n = VecTerm <$> V.zipWithM (ef be) x y
        vf Just{} _ _ _ = badArgs
        vf Nothing be x y = ef be x y
        ef be (IntTerm x) (IntTerm y) = IntTerm <$> f be x y
        ef _ _ _ = badArgs
        badArgs = error $ show $ text "applyIntArithOp" <+> ppIntArithOp op
                             <+> text "given invalid arguments."
        f = case op of
              Add _ _ -> beAddInt
              Sub _ _ -> beSubInt
              Mul _ _ -> beMulInt
              UDiv _  -> beQuotUnsigned
              SDiv _  -> beQuot
              URem    -> beRemUnsigned
              SRem    -> beRem
              Shl _ _ -> beShl
              Lshr _  -> beUnsignedShr
              Ashr _  -> beSignedShr
              And -> beAndInt
              Or  -> beOrInt
              Xor -> beXorInt


applyExpr :: forall l . (Eq l, LV.Storable l, ?be :: BitEngine l)
          => DataLayout
          -> TypedExpr (BitTerm l)
          -> IO (BitTerm l)
applyExpr dl texpr = do
  let ptrWidth = ptrBitwidth dl

  let applyICmp f Nothing x y = liftBinIntRel (f ?be) x y
      applyICmp f Just{} (VecTerm x) (VecTerm y) =
        VecTerm <$> V.zipWithM (liftBinIntRel (f ?be)) x y
      applyICmp _ _ _ _ = illegalArgs "applyICmp"

      retMV :: Maybe Int
            -> (BitTerm l -> BitTerm l)
            -> BitTerm l -> IO (BitTerm l)
      retMV Nothing f t = return (f t)
      retMV Just{} f (VecTerm t) = return (VecTerm (f <$> t))
      retMV _ _ _ = illegalArgs "retMV"

      retIntMV :: LV.Storable l
               => Maybe Int
               -> (LV.Vector l -> LV.Vector l)
               -> BitTerm l -> IO (BitTerm l)
      retIntMV mn f t = retMV mn (IntTerm . f . asIntTerm) t

      expectPtrArg :: (LV.Vector l -> a) -> BitTerm l -> a
      expectPtrArg f (PtrTerm x) = f x
      expectPtrArg _ _ = error "expectPtrArg given illegal argument"

  case texpr of
    IntArith op mn _ x y -> applyIntArithOp op mn ?be x y
    PtrAdd (PtrTerm x) (IntTerm y)
      | LV.length x == LV.length y ->
        PtrTerm <$> beAddInt ?be x y
    PtrAdd{} -> illegalArgs "PtrAdd"
    UAddWithOverflow _ (IntTerm x) (IntTerm y) ->
       (\(c,u) -> StructTerm (V.fromList [IntTerm (LV.singleton c), IntTerm u]))
         <$> beFullAddInt ?be x y
    UAddWithOverflow{} -> illegalArgs "UAddWithOverflow"
    ICmp op mn _ x y -> applyICmp opFn mn x y
      where neg fn bend u v = beNeg bend <$> fn bend u v
            opFn = case op of
                     L.Ieq  -> beEqVector
                     L.Ine  -> neg beEqVector
                     L.Iugt -> neg beUnsignedLeq
                     L.Iuge -> neg beUnsignedLt
                     L.Iult -> beUnsignedLt
                     L.Iule -> beUnsignedLeq
                     L.Isgt -> neg beSignedLeq
                     L.Isge -> neg beSignedLt
                     L.Islt -> beSignedLt
                     L.Isle -> beSignedLeq
    Trunc mn _ t rw -> retIntMV mn (beTrunc ?be rw) t
    ZExt  mn _ t rw   -> retIntMV mn (beZext  ?be rw) t
    SExt   mn _ t rw  -> retIntMV mn (beSext  ?be rw) t
    PtrToInt mn _ t w -> retMV mn (IntTerm . expectPtrArg f) t
      where f = beZeroIntCoerce ?be w
    IntToPtr mn _ t _ -> retMV mn (PtrTerm . f . asIntTerm) t
      where f = beZeroIntCoerce ?be ptrWidth
    Select Nothing c _ t f -> (fail Arrow.||| return) (muxTerm (asInt1 c) t f)
    Select (Just n) (VecTerm cv) _ (VecTerm tv) (VecTerm fv)
      | V.length cv == n -> (fail Arrow.||| (return .VecTerm)) rv
          where rv = sequenceOf traverse $ V.zipWith3 muxTerm (asInt1 <$> cv) tv fv
    Select{} -> illegalArgs "Select"                                       
    GetStructField _ (StructTerm t) i -> return $ t V.! i
    GetStructField{} -> illegalArgs "GetStructField"
    GetConstArrayElt _ _ (ArrayTerm t) i -> return $ t V.! i
    GetConstArrayElt{} -> illegalArgs "GetConstArrayElt"
    SValInteger w v -> return $ IntTerm $ lVectorFromInt w v
    SValFloat v  -> return $ FloatTerm v
    SValDouble v -> return $ DoubleTerm v
    SValNull _ -> return $ PtrTerm $ beVectorFromInt ?be ptrWidth 0
    SValArray  _ valTerms -> return (ArrayTerm valTerms)
    SValVector _ valTerms -> return (VecTerm valTerms)
    SValStruct _ valTerms -> return (StructTerm valTerms)

sbeBitBlast :: forall m l .
               (Eq l, LV.Storable l, ?be :: BitEngine l)
            => DataLayout
            -> BitBlastMemModel m l
            -> SBE (BitIO m l)
sbeBitBlast dl mm = sbe
  where
    be = ?be
    sbe = SBE
          { sbeTruePred      = beLitFromBool be True
          , applyIEq         = \_ (IntTerm x) (IntTerm y) -> do
              BitIO $ beEqVector be x y
          , applyAnd         = BitIO `c2` beAnd be
          , applyBNot        = return . beNeg be
          , applyPredIte     = BitIO `c3` beIte be
          , applyIte         = \_ c x y -> return $ muxTerm c x y
          , freshInt         = \w -> BitIO $
              IntTerm <$> LV.replicateM w (beMakeInputLit be)

          , typedExprEval    = \expr ->
             return $ ExprEvalFn $ \eval -> liftIO . applyExpr dl =<< traverse eval expr
          , applyTypedExpr   = BitIO . applyExpr dl
          , prettyPredD      = lPrettyLV . LV.singleton
          , prettyTermD      = ppBitTerm
          , asBool           = beAsBool
          , evalPred = \inps p -> BitIO $ do
              (LV.! 0) <$> beEvalAigV be (LV.fromList inps) (LV.singleton p)
          , asUnsignedInteger = \_ -> lGetUnsigned . asIntTerm
          , asConcretePtr     = lGetUnsigned . asPtrTerm
          , memDump          = BitIO `c2` mmDump mm True
          , memLoad          = BitIO `c4` loadTerm dl mm
          , memStore         = BitIO `c5` storeTerm dl mm
          , memBranch        = BitIO . mmRecordBranch mm
          , memBranchAbort   = BitIO . mmBranchAbort mm
          , memMerge         = BitIO `c3` mmMux mm
          , memAddDefine     = \mem d vl -> BitIO $ mmAddDefine mm mem d vl
          , memInitGlobal    = \m ty gd ->
                                 BitIO $ mmInitGlobal mm m (termToBytes dl ty gd)
          , codeLookupSymbol = return `c2` mmLookupSymbol mm
          , stackAlloc = \m eltTp _ cnt a -> BitIO $
              mmStackAlloc mm m (memTypeSize dl eltTp) cnt a
          , stackPushFrame   = BitIO . mmStackPush mm
          , stackPopFrame    = BitIO . mmStackPop mm
          , heapAlloc        = \m eltTp _ cnt a ->
              BitIO $ mmHeapAlloc mm m (memTypeSize dl eltTp) cnt a
          , memCopy          = BitIO `c6` mmMemCopy mm

          , termSAT          =
              case beCheckSat be of
                Nothing  -> BitIO . return . const Unknown
                Just sat -> BitIO . sat
          , writeAiger       = \f ts -> BitIO $ do
              inputs <- beInputLits be
              let outputs = LV.concat (flattenTerm . snd <$> ts)
              beWriteAigerV be f inputs outputs
          , evalAiger        = BitIO `c3` evalAigerImpl dl
          , writeCnf         = \f _ t -> BitIO $ do
              let ?be = be
              V.toList <$> beWriteCNF be f mempty (lIsZero (flattenTerm t))
          , createSMTLIB1Script = Nothing
          , createSMTLIB2Script = Nothing
          , sbeRunIO = liftSBEBitBlast 
          }

ppBitTerm :: (?be :: BitEngine l, LV.Storable l) => BitTerm l -> Doc
ppBitTerm (IntTerm t) = text "i" <> lPrettyLV t
ppBitTerm (FloatTerm v) = text (show v)
ppBitTerm (DoubleTerm v) = text (show v)
ppBitTerm (PtrTerm t) = text "p" <> lPrettyLV t
ppBitTerm (ArrayTerm v) = brackets $ commas $ V.toList $ ppBitTerm <$> v
ppBitTerm (VecTerm v) = brackets $ commas $ V.toList $ ppBitTerm <$> v
ppBitTerm (StructTerm v) = structBraces $ commas $ V.toList $ ppBitTerm <$> v

liftBinIntRel :: (LV.Storable l)
              => (LV.Vector l -> LV.Vector l -> IO l)
              -> BitTerm l -> BitTerm l -> IO (BitTerm l)
liftBinIntRel f (IntTerm x) (IntTerm y) = IntTerm . LV.singleton <$> f x y
liftBinIntRel f (PtrTerm x) (PtrTerm y) = IntTerm . LV.singleton <$> f x y
liftBinIntRel _ _ _ = error "Illegal arguments to liftBinIntRel"

asInt1 :: (LV.Storable l) => BitTerm l -> l
asInt1 (IntTerm x) = assert (LV.length x == 1) $ x LV.! 0
asInt1 _ = error "Illegal arguments to asInt1"

asIntTerm :: BitTerm l -> LV.Vector l
asIntTerm (IntTerm x) = x
asIntTerm _ = illegalArgs "asIntTerm"

asPtrTerm :: BitTerm l -> LV.Vector l
asPtrTerm (PtrTerm x) = x
asPtrTerm _ = illegalArgs "asIntTerm"

-- | Return if then else of terms.
muxTerm :: (Eq l, LV.Storable l, ?be :: BitEngine l)
        => l -> BitTerm l -> BitTerm l -> Either String (BitTerm l)
muxTerm c x0 y0 
  | c == lTrue  = return x0
  | c == lFalse = return y0
  | otherwise = 
     case (x0, y0) of
       (IntTerm x, IntTerm y) -> return $ IntTerm $ lIteVector c x y
       (FloatTerm x, FloatTerm y)
         | x == y -> return $ FloatTerm x
         | otherwise -> Left "Backend does not support merging symbolic floating point values."
       (DoubleTerm x, DoubleTerm y)
         | x == y -> return $ DoubleTerm x
         | otherwise -> Left "Backend does not support merging symbolic floating point values."
       (PtrTerm x, PtrTerm y) -> return $ PtrTerm $ lIteVector c x y
       (ArrayTerm x, ArrayTerm y) ->
         ArrayTerm <$> V.zipWithM (muxTerm c) x y
       (VecTerm x, VecTerm y) ->
         VecTerm <$> V.zipWithM (muxTerm c) x y
       (StructTerm x, StructTerm y) ->
         StructTerm <$> V.zipWithM (muxTerm c) x y
       _ -> Left "Internal error: Backend given incompatible terms" 

-- | Converts a bit term into a single lit vector.
flattenTerm :: (?be :: BitEngine l, LV.Storable l) => BitTerm l -> LV.Vector l
flattenTerm t0 =
  case t0 of
    IntTerm v -> v
    FloatTerm v -> lVectorFromFloat v
    DoubleTerm v -> lVectorFromDouble v
    PtrTerm v -> v
    ArrayTerm v  -> joinN (flattenTerm <$> v)
    VecTerm v    -> joinN (flattenTerm <$> v)
    StructTerm v -> joinN (flattenTerm <$> v)

-- | Returns minimum number of bits to encode type.
memTypeBitsize :: DataLayout -> MemType -> Int
memTypeBitsize dl tp0 =
  case tp0 of
    IntType w  -> w
    FloatType  -> 32
    DoubleType -> 64
    PtrType{}  -> ptrBitwidth dl
    ArrayType n etp -> n * memTypeBitsize dl etp
    VecType n etp   -> n * memTypeBitsize dl etp
    StructType si   -> V.sum $ memTypeBitsize dl <$> siFieldTypes si


vecToBits :: (Bits a, Num a) => LV.Vector Bool -> a
vecToBits v = impl 0 0 
  where impl i r 
          | i == LV.length v = r
          | v LV.! i  = impl (i+1) (r `setBit` i)
          | otherwise = impl (i+1) r

-- | Converts from flat lit vector to bitterm based on type.
unflattenTerm :: (?be ::BitEngine l, LV.Storable l)
              => DataLayout -> MemType -> LV.Vector Bool -> BitTerm l
unflattenTerm dl tp0 v =
  case tp0 of
    IntType w
      | LV.length v == w ->
          IntTerm (LV.map lFromBool v)
      | otherwise -> badVec $ "integer"
    FloatType
      | LV.length v == 32 ->
          FloatTerm  $ wordToFloat $ vecToBits v
      | otherwise -> badVec "float"
    DoubleType
      | LV.length v == 64 ->
          DoubleTerm $ wordToDouble $ vecToBits v
      | otherwise -> badVec "double"
    PtrType{}
      | ptrBitwidth dl == fromIntegral (LV.length v) ->
        PtrTerm (LV.map lFromBool v)
      | otherwise -> badVec "ptr"
    ArrayType n etp -> ArrayTerm  $ unflattenTerm dl etp <$> sliceN n v
    VecType n etp   -> VecTerm    $ unflattenTerm dl etp <$> sliceN n v
    StructType si   -> StructTerm $ V.zipWith (unflattenTerm dl) flv vv
      where flv = siFieldTypes si
            szv = memTypeBitsize dl <$> flv
            ofv = V.prescanl (+) 0 szv
            vv = V.zipWith (\i o-> LV.slice i o v) ofv szv
 where badVec nm = error $
        "internalError: unflattern given incorrect number of bits for "
        ++ nm ++ "."
