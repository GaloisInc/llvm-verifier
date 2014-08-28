{- |
Module           : $Header$
Description      : A symbolic backend that bitblasts
Stability        : provisional
Point-of-contact : atomb, jhendrix
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE PatternGuards              #-}

module Verifier.LLVM.Backend.BitBlastNew
  ( -- * Re-exports to create and interact with backend.
    module Verifier.LLVM.Backend
  , module Verifier.LLVM.MemModel.Geometry
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

import           Control.Applicative       ((<$>), (<*>), pure)
import qualified Control.Arrow as Arrow
import           Control.Exception         (assert)
import           Control.Lens hiding (ix, op)
import           Control.Monad (ap, unless, when, join, (<=<), foldM, zipWithM)
import           Control.Monad.IO.Class
import           Data.Binary.IEEE754
import           Data.Bits
import           Data.Foldable
import           Data.IORef
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import qualified Data.Vector               as V
-- import qualified Data.Vector.Storable      as LV
import           Numeric                   (showHex)
import qualified Text.LLVM.AST             as L
import           Text.PrettyPrint.Leijen hiding ((<$>), align)

import qualified Data.AIG as AIG
import           Data.AIG ( IsAIG, BV )
import qualified Data.AIG.Operations as BV


import Verifier.LLVM.Backend
import Verifier.LLVM.Codebase.AST
import Verifier.LLVM.MemModel.Geometry
import Verifier.LLVM.Simulator.SimUtils
import Verifier.LLVM.Utils.Arithmetic

-- -- Utility functions and declarations {{{1

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

-- -- | Memoizes a function using a map.
memo :: Ord s => (s -> IO t) -> IO (s -> IO t)
memo fn = do
   ref <- newIORef Map.empty
   return $ \key -> do
     m <- readIORef ref
     case Map.lookup key m of
       Just v -> return v
       Nothing -> fn key >>= \v -> modifyIORef ref (Map.insert key v) >> return v

-- | Returns number of bytes.
byteSize :: BV l -> Int
byteSize v = BV.length v `shiftR` 3

-- | Slice a vector into a list of vectors, one for each byte.
sliceIntoBytes :: BV l -> V.Vector (BV l)
sliceIntoBytes v = V.reverse $ V.generate (byteSize v) $ \i -> BV.slice v (i `shiftL` 3) 8

-- | Slice a single vector into a vector of lit vectors with n elements.
sliceN :: Int -> BV l -> V.Vector (BV l)
sliceN n v = assert (n > 0 && r == 0) $
    V.generate n $ \i -> BV.slice v (l*i) l
  where (l,r) = BV.length v `divMod` n

-- | Slice a single vector into a vector of lit vectors each with the given number of elements.
joinN :: V.Vector (BV l) -> BV l
joinN = BV.concat . V.toList

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

bvSetBits :: (IsAIG l g) => g s -> Int -> (Int -> Bool) -> BV (l s)
bvSetBits g n pr = BV.generate_lsb0 n (AIG.constant g . pr)

-- | @lAlignUp addr i@ returns pair @(c,v)@ where @v@ is the smallest multiple of @2^i@
-- not smaller than @addr@, and @c@ is set if computation overflowed.
bvAlignUp :: (IsAIG l g) => g s -> BV (l s) -> Alignment -> IO (l s, BV (l s))
bvAlignUp g addr i = do 
   let n = BV.length addr
   (s,c) <- BV.addC g addr (bvSetBits g n (< fromIntegral i))
   s' <- BV.zipWithM (AIG.and g) s (bvSetBits g n (>= fromIntegral i))
   return (c, s')
        
-- | @lAlignDown addr i@ returns pair @(c,v)@ where @v@ is the largest multiple of @2^i@
-- not larger than @addr@, and @c@ is set if computation overflowed.
bvAlignDn :: (IsAIG l g) => g s -> BV (l s) -> Alignment -> IO (BV (l s))
bvAlignDn g addr i = BV.zipWithM (AIG.and g) addr (bvSetBits g (BV.length addr) (>= fromIntegral i))

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

-- -- BitEngine primitives {{{1
-- -- | Dont care bit in bit engine.

-- TODO: Figure out if this is a useful primitive to add to bit engine (e.g., does abc support ti).
dontCareLit :: IsAIG l g => g s -> l s
dontCareLit g = AIG.falseLit g

dontCareByte :: IsAIG l g => g s -> BV (l s)
dontCareByte = BV.replicate 8 . dontCareLit

mergeCondVector :: (IsAIG l g, Eq (l s))
                => g s
                -> l s
                -> IO (l s, BV (l s))
                -> IO (l s, BV (l s))
                -> IO (l s, BV (l s))
mergeCondVector g c x y
  | c AIG.=== AIG.trueLit g  = x
  | c AIG.=== AIG.falseLit g = y
  | otherwise = do
        (x1,x2) <- x
        (y1,y2) <- y
        z1 <- AIG.mux g c x1 y1
        z2 <- BV.zipWithM (AIG.mux g c) x2 y2
        return (z1,z2)

-- | @lInRange be p (s,e)@ returns predicate that holds in @s <= p & p < e@
-- when treated as unsigned values.
bvInRange :: (IsAIG l g) => g s -> BV (l s) -> (BV (l s), BV (l s)) -> IO (l s)
bvInRange g p (s,e) = join $ pure (AIG.and g) <*> (BV.ule g s p) <*> (BV.ult g p e)

-- | @bvRangeCovered g subFn r1 r2@ returns true if @subFn@ returns true for
-- all ranges in @r1 - r2@.  N.B. Only calls subFn on an empty range if @r1@ is
-- empty.
bvRangeCovered :: (IsAIG l g)
              => g s
              -> (Range (BV (l s)) -> IO (l s))
              -> Range (BV (l s))
              -> Range (BV (l s))
              -> IO (l s)
bvRangeCovered g subFn (s1,e1) (s2,e2) =
    ite
      ((s2 `lt` e1) &&& (s1 `lt` e2))
      (((s2 `le` s1) ||| subFn (s1,s2)) &&& ((e1 `le` e2) ||| subFn (e2,e1)))
      (subFn (s1,e1))

  where lt = BV.ult g
        le = BV.ule g
        ite x y z = join $ pure (AIG.mux g) <*> x <*> y <*> z
        x &&& y = join $ pure (AIG.and g) <*> x <*> y
        x ||| y = join $ pure (AIG.or g) <*> x <*> y

-- BitTerm {{{1

data BitTerm l
    = IntTerm (BV l)
    | FloatTerm Float
    | DoubleTerm Double
    | PtrTerm (BV l)
    | ArrayTerm (V.Vector (BitTerm l))
    | VecTerm (V.Vector (BitTerm l))
      -- | Could be packed or unpacked struct.
    | StructTerm (V.Vector (BitTerm l))
  deriving (Eq, Ord)

lPrettyLit :: IsAIG l g => g s -> l s -> Doc
lPrettyLit g x | x AIG.=== AIG.falseLit g = text "False"
               | x AIG.=== AIG.trueLit g  = text "True"
               | otherwise = text "?:[1]"

lPrettyLV :: IsAIG l g => g s -> BV (l s) -> Doc
lPrettyLV g bv
  | 1 == BV.length bv = lPrettyLit g (bv BV.! 0)
  | otherwise = text str <> colon <>  brackets (text $ show $ BV.length bv)
                  <+> maybe empty cvt (BV.asSigned g bv)
  where
    cvt x = parens (integer x)
            <+> if x >= 0
                then hex x
                else case BV.asUnsigned g bv of
                       Nothing -> empty
                       Just u  -> hex u
    hex x = parens $ text "0x" <> text (showHex x "")
    str      = map toChar $ BV.bvToList bv
    toChar x
        | x AIG.=== AIG.falseLit g = '0'
        | x AIG.=== AIG.trueLit g  = '1'
        | otherwise = 'x'

litAsBool :: (IsAIG l g) => g s -> l s -> Maybe Bool
litAsBool g x
    | x AIG.=== AIG.falseLit g = Just False
    | x AIG.=== AIG.trueLit g  = Just True
    | otherwise = Nothing

-- | Attempts to convert a bitvector to a term.
-- May fail if some values are symbolic, but floating point.
bytesToTerm :: (IsAIG l g)
            => g s
            -> DataLayout
            -> MemType
            -> BV (l s)
            -> Maybe (BitTerm (l s))
bytesToTerm g dl tp0 v = 
  case tp0 of
    IntType w
      | w <= BV.length v -> return $ IntTerm (BV.trunc w v)
      | otherwise -> badVec $ "integer.\nExpected at least " ++ show w
                              ++ "; Found " ++ show (BV.length v)
    FloatType
      | BV.length v == 32 ->
          FloatTerm  . wordToFloat  . fromIntegral <$> BV.asUnsigned g v
      | otherwise -> badVec "float."
    DoubleType
      | BV.length v == 64 ->
          DoubleTerm . wordToDouble . fromIntegral <$> BV.asUnsigned g v
      | otherwise -> badVec "double."
    PtrType{} | ptrBitwidth dl == fromIntegral (BV.length v) -> return $ PtrTerm v
    ArrayType n etp -> ArrayTerm <$> traverse (bytesToTerm g dl etp) (sliceN n v)
    VecType n etp   -> VecTerm <$> traverse (bytesToTerm g dl etp) (sliceN n v)
    StructType si   -> StructTerm <$> traverse go (siFields si)
      where go fi = bytesToTerm g dl (fiType fi) (BV.slice v o sz)
              where o = 8 * fromIntegral (fiOffset fi)
                    sz = 8 * fromIntegral (memTypeSize dl (fiType fi))
    _ -> error $ unwords ["internalError: bytes to term given mismatched arguments",show (ppMemType tp0),BV.bvShow g v]
 where badVec nm = error $
        "internalError: bytesToTerm given incorrect number of bits for " ++ nm


lVectorFromFloat :: (IsAIG l g) => g s -> Float -> BV (l s)
lVectorFromFloat g = BV.bvFromInteger g 32 . toInteger . floatToWord

lVectorFromDouble :: (IsAIG l g) => g s -> Double -> BV (l s)
lVectorFromDouble g = BV.bvFromInteger g 64 . toInteger . doubleToWord

-- | Convert term into a list of bytes suitable for storing in memory.
termToBytes :: (IsAIG l g)
            => g s
            -> DataLayout
            -> MemType
            -> BitTerm (l s)
            -> BV (l s)
termToBytes g dl tp0 t0 = do
  case (tp0, t0) of
    (IntType w, IntTerm v) -> v BV.++ ext
      where newBits = (8 - (w .&. 0x7)) .&. 0x7
            ext = BV.replicate (fromIntegral newBits) (dontCareLit g)
    (FloatType,      FloatTerm v)  -> lVectorFromFloat g v
    (DoubleType,     DoubleTerm v) -> lVectorFromDouble g v
    (PtrType{},      PtrTerm v)
      | ptrBitwidth dl == BV.length v -> v
    (ArrayType _ tp, ArrayTerm v)  -> joinN (termToBytes g dl tp <$> v)
    (VecType _ tp,   VecTerm v)    -> joinN (termToBytes g dl tp <$> v)
    (StructType si,  StructTerm v) -> joinN $ V.zipWith (termToBytes g dl) (siFieldTypes si) v
    _ -> error $ show $ text "internalError: termToBytes given mismatched arguments:" <$$>
           nest 2 (text "Type:" <+> ppMemType tp0 <$$>
                   text "Term:" <+> ppBitTerm g t0)
           
-- {-
-- termToBytes lc be tp (BitTerm val) =
--   case resolveType lc tp of
--     -- Extend integer types to full width.
--     L.PrimType (L.Integer w) -> val LV.++ ext
--       where newBits = (8 - (w .&. 0x7)) .&. 0x7
--             ext = LV.replicate (fromIntegral newBits) (beDontCare be)
--     -- Treat other types as same.
--     _ -> val
-- -}

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
  , mmStackAlloc :: SBEMemory sbe -- Memory
                 -> Size          -- Size of each element
                 -> SBETerm sbe   -- Number of elements
                 -> Alignment     -- Alignment constraint in bytes.
                 -> IO (AllocResult sbe)
  , mmStackPush :: SBEMemory sbe -> IO (SBEPred sbe, SBEMemory sbe)
  , mmStackPop  :: SBEMemory sbe -> IO (SBEMemory sbe)
  , mmHeapAlloc :: SBEMemory sbe
                -> Size
                -> SBETerm sbe
                -> Alignment
                -> IO (AllocResult sbe)
  , mmMemCopy :: SBEMemory sbe
              -> SBETerm sbe            -- Destination pointer
              -> SBETerm sbe            -- Source pointer
              -> BitWidth               -- Width of length value.
              -> SBETerm sbe            -- Length value 
              -> SBETerm sbe            -- Alignment in bytes (32-bit value)
              -> IO (SBEPred sbe, SBEMemory sbe) -- Condition and new value.
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
  MemModel (BitIO m l) (BV l)

-- | Load memory using
loadTerm :: (IsAIG l g, Eq (l s))
         => g s
         -> DataLayout
         -> MemModel (BitIO m (l s)) (BV (l s))
         -> m
         -> MemType -- ^ Type to read
         -> BitTerm (l s) -- ^ Pointer to load
         -> Alignment
         -> IO (l s, BitTerm (l s))
loadTerm g dl mm bm tp ptr a = do
  (c,v) <- mmLoad mm bm ptr (memTypeSize dl tp) a
  case bytesToTerm g dl tp v of
    Just t -> return (c,t)
    Nothing -> fail "Backend asked to read symblic floating point number."

-- | Store term in memory model.
storeTerm :: (IsAIG l g, Eq (l s))
          => g s
          -> DataLayout
          -> MemModel (BitIO m (l s)) (BV (l s))
          -> SBEMemory (BitIO m (l s))
          -> BitTerm (l s)
          -> MemType
          -> BitTerm (l s)
          -> Alignment
          -> IO (SBEPred (BitIO m (l s)), SBEMemory (BitIO m (l s)))
storeTerm g dl mm m ptr tp v a
  | BV.length bytes == 0 = return (AIG.trueLit g, m)
  | otherwise = mmStore mm m ptr bytes a
  where bytes = termToBytes g dl tp v

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
  | SValue l l (BV l) -- ^ SValue allocatedBit initializedBit value
  | SDefine L.Symbol -- ^ Memory value for function definition.
  | SBlock L.Symbol L.BlockLabel -- ^ Memory value for block within function.
  | SUnallocated -- ^ A memory section that has not been allocated to the program.

-- A derived(Show)-like pretty printer for the Storage type
ppStorageShow :: (IsAIG l g, Eq (l s)) => g s -> Storage (l s) -> Doc
ppStorageShow g (SBranch f t) = text "SBranch" <+> parens (ppStorageShow g f) <+> parens (ppStorageShow g t)
ppStorageShow g (SValue a i v)
  = text "SValue" <+> pl a <+> pl i <+> parens (lPrettyLV g v)
  where pl = lPrettyLit g
ppStorageShow _ (SDefine d) = text ("SDefine " ++ show d)
ppStorageShow _ (SBlock d b) = text ("SBlock " ++ show d ++ " " ++ show b)
ppStorageShow _ SUnallocated = text "SUnallocated"

-- A "sparse" pretty printer for the Storage type; skips unallocated regions and
-- shows addresses explicitly.

ppStorage :: (IsAIG l g, Eq (l s))
          => Maybe [Range Addr]
          -> g s
          -> Storage (l s)
          -> Doc
ppStorage mranges g = impl 0 Nothing
  where
    impl _ Nothing SUnallocated      = text "empty memory"
    impl a Nothing s                 = impl a (Just empty) s
    impl a mdoc (SBranch f t)        = let la = a `shiftL` 1
                                           ra = la `setBit` 0
                                       in impl ra (Just $ impl la mdoc f) t
    impl a (Just doc) (SValue al il v)
      | il AIG.=== AIG.trueLit g  = whenInRange a $ item doc a (lPrettyLV g v)
      | il AIG.=== AIG.falseLit g = whenInRange a $ item doc a (text "uninitialized")
      | otherwise = whenInRange a
                  $ item doc a
                  $ (lPrettyLV g v) 
                     <+> parens (text "allocated:" <+> pl al <> comma
                                 <+> text "initialized:" <+> pl il)
    impl a (Just doc) (SDefine sym)  =
      whenInRange a $ item doc a $ ppSymbol sym
    impl a (Just doc) (SBlock s l)
      = whenInRange a $ item doc a 
      $ ppSymbol s <> char '/' <> text (show (L.ppLabel l))
    impl _ (Just doc) SUnallocated   = doc
    item doc addr desc               = doc <$$> text (showHex addr "") <> colon <+> desc
    pl = lPrettyLit g
    whenInRange a doc = case mranges of
      Nothing     -> doc
      Just ranges -> if inRangeAny a ranges then doc else empty

mergeStorage :: (IsAIG l g, Eq (l s))
             => g s
             -> l s
             -> Storage (l s)
             -> Storage (l s) 
             -> IO (Storage (l s))
mergeStorage g c x y = impl x y
  where impl (SBranch fx tx) (SBranch fy ty) = do
          f <- impl fx fy
          t <- impl tx ty
          return (SBranch f t)
        impl (SValue ax ix vx) (SValue ay iy vy) =
          return SValue `ap` AIG.mux g c ax ay
                        `ap` AIG.mux g c ix iy
                        `ap` BV.iteM g c (return vx) (return vy)
        impl (SDefine dx) (SDefine dy)
          | dx == dy = return (SDefine dx)
          | otherwise = bmError "Attempt to merge memories with incompatible definitions."
        impl (SBlock dx bx) (SBlock dy by)
          | dx == dy && bx == by = return (SBlock dx bx)
          | otherwise = bmError "Attempt to merge memories with incompatible block values."
        impl SUnallocated SUnallocated = return SUnallocated
        impl (SValue ax ix vx) SUnallocated =
          (\az -> SValue az ix vx) <$> AIG.and g c ax
        impl SUnallocated (SValue ay iy vy) =
          (\az -> SValue az iy vy) <$> AIG.and g (AIG.not c) ay
        impl b@SBranch{} SUnallocated =
          impl b (SBranch SUnallocated SUnallocated)
        impl SUnallocated b@SBranch{} =
          impl (SBranch SUnallocated SUnallocated) b
        impl a b = do
          dbugM $ "mergeStorage failure case: a = " ++ show (ppStorageShow g a)
          dbugM $ "mergeStorage failure case: b = " ++ show (ppStorageShow g b)
          bmError "Attempt to merge incompatible valid addresses."

-- | @loadBytes be mem ptr size@ returns term representing all the bits with
-- given size.
loadBytes :: (IsAIG l g, Eq (l s))
          => (BV (l s) -> IO (l s, BV (l s)))
          -> g s
          -> BitTerm (l s)
          -> Size
          -> Alignment
          -> IO (l s, BV (l s))
loadBytes byteLoader g (PtrTerm ptr) sz _ =
    impl [] (AIG.trueLit g) (toInteger sz)
  where impl l c 0 = return (c, BV.concat (reverse l))
        impl l c i = do
          (bc, bv) <- byteLoader =<< BV.addConst g ptr (i-1)
          c' <- AIG.and g c bc
          impl (bv:l) c' (i-1)
loadBytes _ _ _ _ _ = illegalArgs "loadBytes"

-- | Returns condition under which store is permitted.
storeByteCond :: (IsAIG l g, Eq (l s)) => g s -> Storage (l s) -> BV (l s) -> IO (l s)
storeByteCond g mem ptr = impl mem (BV.length ptr)
  where impl (SBranch f t) i
          | ptr BV.! (i-1) AIG.=== AIG.trueLit g  = impl t (i-1)
          | ptr BV.! (i-1) AIG.=== AIG.falseLit g = impl f (i-1)
          | otherwise = join $ return (AIG.mux g (ptr BV.! (i-1))) <*> (impl t (i-1)) <*> (impl f (i-1))
        impl (SValue ax _ _) i = assert (i == 0) $ return ax
        impl _ _ = return (AIG.falseLit g)

-- | Return storage with individual byte changed.
storeByte :: (IsAIG l g, Eq (l s))
          => g s -- ^ Bit engine for bits
          -> Storage (l s) -- ^ Memory to update
          -> BV (l s) -- ^ Value to write
          -> BV (l s) -- ^ Address to write to
          -> IO (Storage (l s))
storeByte g mem new ptr = impl mem (BV.length ptr) (AIG.trueLit g)
  where impl (SBranch f t) i c
          | lo AIG.=== AIG.falseLit g = (\fr -> SBranch fr t) <$> impl f (i-1) c
          | lo AIG.=== AIG.trueLit g  = (\tr -> SBranch f tr) <$> impl t (i-1) c
          | otherwise = do
            fr <- impl f (i-1) =<< AIG.and g c (AIG.not lo)
            tr <- impl t (i-1) =<< AIG.and g c lo
            return (SBranch fr tr)
          where lo = ptr BV.! (i-1)
        impl (SValue ax ix vx) i c = assert (i == 0) $
          return (SValue ax) `ap` AIG.or g c ix
                             `ap` BV.iteM g c (return new) (return vx)
        impl m _ _ = return m

storeBytes :: (IsAIG l g, Eq (l s))
           => g s
           -> Storage (l s)      -- ^ Base storage
           -> BV (l s)    -- ^ Address to store value in
           -> BV (l s)    -- ^ Value to store
           -> Alignment
           -> IO (l s, Storage (l s)) -- ^ Condition for address to be valid, and updated storage.
storeBytes g mem ptr value _ = impl 0 (AIG.trueLit g) mem
  where bv = sliceIntoBytes value
        impl i c m
          | i == byteSize value = return (c,m)
          | otherwise = do
            p <- BV.addConst g ptr (toInteger i)
            c' <- AIG.and g c =<< storeByteCond g m p
            m' <- storeByte g m (bv V.! i) p
            impl (i+1) c' m'

loadDef :: Storage a -> Int -> Addr -> Maybe L.Symbol
loadDef s w a = impl s (w-1)
  where impl (SBranch f t) i = impl (if testBit a i then t else f) (i-1)
        impl (SDefine d) _ = Just d
        impl _ _ = Nothing

-- @setBytes w low high val mem@ sets all bytes in [low .. high) to @val@.
-- The address with is w.
setBytes :: Int -> Addr -> Addr -> (Addr -> Storage a) -> Storage a -> Storage a
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
uninitRegion :: IsAIG l g => g s -> Int -> Addr -> Addr -> Storage (l s) -> Storage (l s)
uninitRegion g ptrWidth low high = setBytes ptrWidth low high (const v)
  where v = SValue (AIG.trueLit g) (AIG.falseLit g) (dontCareByte g)

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

bmDump :: (IsAIG l g, Eq (l s))
       => g s
       -> Bool
       -> BitMemory (l s)
       -> Maybe [Range Addr]
       -> IO ()
bmDump g sparse bm mranges = do
  banners $ show $
    text "Memory Model Dump" <$$>
    text "Stack Range:" <+> text (h $ bmStackAddr bm) <> comma <+> text (h $ bmStackEnd bm) <$$>
    text "Code Range:"  <+> text (h $ bmCodeAddr bm) <> comma <+> text (h $ bmCodeEnd bm) <$$>
    text "Data Range:"  <+> text (h $ bmDataAddr bm) <> comma <+> text (h $ bmDataEnd bm) <$$>
    text "Heap Range:"  <+> text (h $ bmHeapAddr bm) <> comma <+> text (h $ bmHeapEnd bm) <$$>
    text "Frame pointers:" <+> hcat (punctuate comma (map text $ map hx $ bmStackFrames bm)) <$$>
    text "Storage:" <$$> 
    (if sparse then ppStorage mranges else ppStorageShow) g (bmStorage bm)
  where
    h s  = showHex s ""
    hx s = "0x" ++ h s
--     fl i as = text "Size = 2^" <> int i <> colon <+>
--               sep (punctuate comma (map (text . h) as))

-- | @loadBytes be mem ptr size@ returns term representing all the bits with given size.
bmLoadByte :: (IsAIG l g, Eq (l s))
         => g s
         -> BitMemory (l s)
         -> BV (l s)
         -> IO (l s, BV (l s))
bmLoadByte g bm vi = load (bmStorage bm) (BV.length vi - 1)
  where load (SBranch f t) i =
          mergeCondVector g (vi BV.! i) (load t (i-1)) (load f (i-1))
        load (SValue _ i v) _ = return (i, v)
        load _ _ = return (AIG.falseLit g, dontCareByte g)

bmMux :: (IsAIG l g, Eq (l s))
      => g s
      -> l s -> BitMemory (l s) -> BitMemory (l s) -> IO (BitMemory (l s))
bmMux g c m m' = do
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
  newStorage <- mergeStorage g c (bmStorage m) (bmStorage m')
  -- Free lists should be implicitly equivalent if storages are compatible.
  return m { bmStorage = newStorage }

bmInitGlobalBytes :: (IsAIG l g, Eq (l s))
                  => g s
                  -> Int         -- ^ Width of pointers in bits.
                  -> BitMemory (l s)
                  -> BV (l s)
                  -> IO (Maybe (BitTerm (l s), BitMemory (l s)))
bmInitGlobalBytes g ptrWidth m bytes
  | newDataAddr > bmDataEnd m = return Nothing
  | otherwise = do
      let ptrv = BV.bvFromInteger g ptrWidth dataAddr
          mem  = uninitRegion g ptrWidth dataAddr newDataAddr (bmStorage m)
      (c,newStorage) <- storeBytes g mem ptrv bytes 0
      assert (c AIG.=== AIG.trueLit g) $
        return $ Just ( PtrTerm ptrv
                      , m { bmStorage = newStorage, bmDataAddr = newDataAddr })
  where
    dataAddr    = bmDataAddr m
    newDataAddr = dataAddr + toInteger (byteSize bytes)

bmAddDefine :: (IsAIG l g, Eq (l s))
            => g s
            -> Int -- ^ Width of pointers
            -> BitMemory (l s) -- ^ Memory
            -> L.Symbol -- ^ Definition
            -> [L.BlockLabel] -- ^ Labels for blocks
            -> Maybe (BitTerm (l s), [BitTerm (l s)], BitMemory (l s))
bmAddDefine g ptrWidth m def (V.fromList -> blocks)
    | newCodeAddr > bmCodeEnd m
    = Nothing
    | otherwise
    = Just ( PtrTerm (BV.bvFromInteger g ptrWidth codeAddr)
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
        blockAddrs = PtrTerm . BV.bvFromInteger g ptrWidth . (codeAddr +) <$> [1..blockCount]

bvToMaybeInt :: IsAIG l g => g s -> BV (l s) -> Maybe (Int, Integer)
bvToMaybeInt g v = fmap (\x -> (BV.length v, x)) $ BV.asUnsigned g v

-- | Return symbol as given address in memory.
bmLookupSymbol :: (IsAIG l g, Eq (l s))
               => g s
               -> BitMemory (l s)
               -> BitTerm (l s) -- Pointer to symbol
               -> LookupSymbolResult
bmLookupSymbol g m (PtrTerm a) = do
  case bvToMaybeInt g a of
    Nothing -> Left Indeterminate
    Just (w, v) ->
      case loadDef (bmStorage m) w v of
        Nothing -> Left Invalid
        Just d -> Right d
bmLookupSymbol _ _ _ = illegalArgs "bmLookupSymbol"

bmStackAlloc :: (IsAIG l g, Eq (l s))
             => g s
             -> Int       -- ^ Width of pointer in bits.
             -> BitMemory (l s)
             -> Size      -- ^ Element size
             -> BitTerm (l s) -- ^ Number of elements
             -> Alignment -- ^ Alignment constraint
             -> AllocResult (BitIO (BitMemory (l s)) (l s))
bmStackAlloc g ptrWidth bm eltSize (IntTerm cntVector) a =
  case bvToMaybeInt g cntVector of
    Nothing -> AError msg
     where msg = "Stack allocation with symbolic size requested; "
              ++ "This is not supported by the bitblast backend."
    Just (_,cnt) -> r
      where mkRes c res endAddr newAddr = AResult (AIG.constant g c) ptr bm'
              where newStorage = uninitRegion g ptrWidth res endAddr (bmStorage bm)
                    ptr = PtrTerm (BV.bvFromInteger g ptrWidth res)
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
bmStackPush :: BitMemory a -> BitMemory a
bmStackPush bm = bm { bmStackFrames = bmStackAddr bm : bmStackFrames bm }

-- | Pop stack frame in memory and invalidate old addresses.
bmStackPop :: Int -- ^ Width of pointer in bits.
           -> BitMemory a
           -> BitMemory a
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

bmHeapAlloc :: (IsAIG l g, Eq (l s))
            => g s
            -> Int       -- ^ Width of pointer in bits.
            -> BitMemory (l s)
            -> Size      -- ^ Element size
            -> BitTerm (l s) -- ^ Number of elements
            -> Alignment -- ^ Alignment constraint
            -> AllocResult (BitIO (BitMemory (l s)) (l s))
bmHeapAlloc g ptrWidth bm eltSize (IntTerm cntVector) a =
  case bvToMaybeInt g cntVector of
    Nothing -> AError msg
     where msg = "Heap allocation with symbolic size requested; "
              ++ "This is not supported by the bitblast backend."
    Just (_, cnt) ->
        case allocBlock (bmFreeList bm) pwr of
          Just (freeList, addr) ->
               AResult (AIG.trueLit g)
                       addrTerm
                       bm { bmFreeList = freeList
                          , bmStorage = newStorage
                          }
            where endAddr = addr + size
                  size = 2 ^ pwr
                  addrTerm = PtrTerm $ BV.bvFromInteger g ptrWidth addr
                  newStorage = uninitRegion g ptrWidth addr endAddr (bmStorage bm)
          Nothing -> AResult (AIG.falseLit g) zeroTerm bm
            where zeroTerm = PtrTerm (BV.bvFromInteger g ptrWidth 0)
      where -- Pad up to the end of the aligned region; we do this because any
            -- global data that gets copied into this space will be padded to this
            -- size by L.
            sz = toInteger eltSize * cnt
            padBytes = nextMultiple (2 ^ a :: Integer) sz - sz
            pwr = blockPower (sz + padBytes)
bmHeapAlloc _ _ _ _ _ _ = illegalArgs "bmHeapAlloc"

bmMemCopy :: (IsAIG l g, Eq (l s))
          => g s
          -> BitMemory (l s)
          -> BitTerm (l s)   -- ^ Destination pointer
          -> BitTerm (l s)   -- ^ Source pointer
          -> BitWidth    -- ^ Width of length value.
          -> BitTerm (l s)   -- ^ Length value
          -> BitTerm (l s)   -- ^ Alignment in bytes
          -> IO (l s, BitMemory (l s))
bmMemCopy g m (PtrTerm dst) src _ (IntTerm len0) (IntTerm _align0) = do
  -- TODO: Alignment and overlap checks?
  (cr, bytes) <- loadBytes (bmLoadByte g m) g src len 0
  (cw, newStorage) <- storeBytes g (bmStorage m) dst bytes 0
  c <- AIG.and g cr cw
  return (c, m { bmStorage = newStorage })
  where
    len = case bvToMaybeInt g len0 of
            Nothing    -> bmError $ "Symbolic memcpy len not supported"
            Just (_,x) -> fromInteger x

bmMemCopy _ _ _ _ _ _ _ = illegalArgs "bmMemCopy"


-- | Memory model for explicit buddy allocation scheme.
buddyMemModel :: (IsAIG l g, Eq (l s))
              => DataLayout
              -> g s
              -> BitBlastMemModel (BitMemory (l s)) (l s)
buddyMemModel dl g = mm
  where ptrWidth = ptrBitwidth dl
        mm = MemModel
              { mmDump = bmDump g
              , mmLoad = \m -> loadBytes (bmLoadByte g m) g
              , mmStore = \m (PtrTerm ptr) bytes a -> do
                  Arrow.second (\s -> m { bmStorage = s }) <$>
                   storeBytes g (bmStorage m) ptr bytes a
              , mmInitGlobal = bmInitGlobalBytes g ptrWidth
              , mmAddDefine = return `c3` bmAddDefine g ptrWidth
              , mmLookupSymbol = bmLookupSymbol g
              , mmStackAlloc = return `c4` bmStackAlloc g ptrWidth
              , mmStackPush = \mem -> return (AIG.trueLit g, bmStackPush mem)
              , mmStackPop = return . bmStackPop ptrWidth
              , mmHeapAlloc = return `c4` bmHeapAlloc g ptrWidth
              , mmMemCopy = bmMemCopy g
              , mmRecordBranch = return -- do nothing
              , mmBranchAbort = return -- do nothing
              , mmMux = bmMux g
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

createBuddyAll :: (IsAIG l g, Ord (l s))
               => g s
               -> (FilePath -> l s -> IO [Maybe Int])
               -> DataLayout
               -> MemGeom
               -> SBEPair
createBuddyAll g cnfFunc dl mg = SBEPair sbe mem0
  where sbe = sbeBitBlast g cnfFunc dl (buddyMemModel dl g)
        mem0 = buddyInitMemory mg

createBuddyMemModel :: (IsAIG l g, Eq (l s))
                    => DataLayout
                    -> g s
                    -> MemGeom
                    -> IO ( BitBlastMemModel (BitMemory (l s)) (l s)
                          , BitMemory (l s)
                          )
createBuddyMemModel dl g mg =
  return (buddyMemModel dl g, buddyInitMemory mg)

-- DagMemory {{{1

type RefIdx = IORef Int

data DagMemoryState l =
   DMS { dmsStack :: BV l
       , dmsData :: Addr
       , dmsHeap :: BV l
       }

data DagMemory l = DagMemory {
    dmNodeIdx :: !Int
  , dmNodeApp :: !(DMApp l)
  , dmState :: DagMemoryState l
    -- | Frames on stack.
  , dmStackFrames :: [BV l]
    -- | Address for next value in code segment.
  , dmCode :: Addr
     -- | Maps concrete addresses to associated symbol.
  , dmDefineMap :: Map Addr L.Symbol
    -- Returns literal indicating if range is allocated.
  , dmIsAllocated :: Range (BV l) -> IO l
    -- Returns literal indicating if range is initialized.
  , dmIsInitialized :: Range (BV l) -> IO l
    -- Returns byte associated with given address (only valid when dmIsInitialized returns true for range covering byte).
  , dmLoadByte :: BV l -> IO (BV l)
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

type Byte l = BV l

type SymAddr l = BV l

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
   | DMMod (DMMod (BV l)) (DagMemory l)
   | DMMergeFramePush (DagMemory l) 
   | DMMergeFramePop (DagMemory l)
   | DMMux l (DagMemory l) (DagMemory l)

prettyMemIdx :: DagMemory l -> Doc
prettyMemIdx m = char '$' <> int (dmNodeIdx m)

lPrettyRange :: (IsAIG l g) => g s -> Range (BV (l s)) -> Doc
lPrettyRange g (s,e) = char '[' <> lPrettyLV g s <> comma <+> lPrettyLV g e <> char ')'

dmPrintApp :: (IsAIG l g) => g s -> DMApp (l s) -> Doc
dmPrintApp g app =
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
 where pr = lPrettyRange g
       pl = lPrettyLit g
       pm m = prettyMemIdx m
       pv = brackets . hsep . punctuate comma . V.toList
       pblocks = pv . V.map (text . show)
       pbytes  = pv . V.map (lPrettyLV g)
       paddr   = lPrettyLV g

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

dmDump :: (IsAIG l g)
       => g s -> Bool -> DagMemory (l s) -> Maybe [Range Addr] -> IO ()
dmDump g _ mem _ = do
  -- Steps: Build list of memory addresses to print out.
  let allNodes = lfp (dmMemArgs . dmNodeApp) (Set.singleton mem)
  forM_ (Set.toList allNodes) $ \m -> do
    putStrLn $ show $ prettyMemIdx m <> colon <+> dmPrintApp g (dmNodeApp m)

-- | @loadBytes be mem ptr size@ returns term representing all the bits with given size.
dmLoadBytes :: (IsAIG l g, Ord (l s))
            => g s
            -> DagMemory (l s)
            -> BitTerm (l s)
            -> Size
            -> Alignment
            -> IO (l s, BV (l s))
dmLoadBytes g _ (PtrTerm _) 0 _ = return (AIG.trueLit g, BV.empty)
dmLoadBytes g mem (PtrTerm ptr) sz _ = do
  let ptrOffset i = BV.addConst g ptr (toInteger i)
  szl <- ptrOffset sz
  bs  <- mapM (dmLoadByte mem <=< ptrOffset) $ reverse [0 .. sz-1]
  isInit <- dmIsInitialized mem (ptr,szl)
  return ( isInit, BV.concat bs )
dmLoadBytes _ _ _ _ _ = illegalArgs "dmLoadBytes"

-- | Returns node with given app, creating it if necessary.  The function passed
-- in gives the opportunity to modify the node before it is cached.
dmGetMem :: RefIdx -> DMApp a -> DagMemory a -> IO (DagMemory a)
dmGetMem ref app newMem = do
  c <- readIORef ref
  let r = newMem { dmNodeIdx = c, dmNodeApp = app }
  writeIORef ref $! c + 1
  return r

dmLoadByteFromStore :: (IsAIG l g, Ord (l s))
                    => g s
                    -> Range (BV (l s))
                    -> V.Vector (BV (l s))
                    -> DagMemory (l s)
                    -> (BV (l s) -> IO (BV (l s)))
dmLoadByteFromStore g (s,e) bytes mem p = do
  c <- bvInRange g p (s, e)
  diff <- BV.sub g p s
  BV.iteM g c 
            (BV.muxInteger (BV.iteM g)
                          (toInteger (V.length bytes - 1))
                          diff
                          (\i -> return $ bytes V.! fromInteger i))
            (dmLoadByte mem p)

dmRawStore :: (IsAIG l g, Ord (l s))
           => g s 
           -> RefIdx
           -> BV (l s)
           -> BV (l s)
           -> V.Vector (Byte (l s))
           -> DagMemory (l s)
           -> IO (DagMemory (l s))
dmRawStore g ref b e bytes mem = do
  initFn <- memo $ \range ->
          bvRangeCovered g (dmIsInitialized mem) range (b,e)
  loadFn <- memo $ dmLoadByteFromStore g (b,e) bytes mem
  dmGetMem ref (DMMod (DMStore b e bytes) mem) $
    mem { dmIsInitialized = initFn
        , dmLoadByte = loadFn
        }

dmStore :: (IsAIG l g, Ord (l s))
           => g s
           -> RefIdx
           -> BV (l s) 
           -> BV (l s)
           -> V.Vector (Byte (l s)) 
           -> DagMemory (l s) 
           -> IO (DagMemory (l s))
dmStore g ref = simp
  where simp nb ne nvals mem0@DagMemory{ dmNodeApp = DMMod m@(DMStore ob oe ovals) mem } = do
 -- (dmNodeApp -> DMMod m@(DMStore ob oe ovals) mem) = do
           (diff, b) <- BV.subC g ne ob
           (diffo, bo) <- BV.subC g oe nb
           case () of
             _ | b AIG.=== AIG.trueLit g
                  -> dmMod g ref m =<< simp nb ne nvals mem

               | b AIG.=== AIG.falseLit g
               , Just off <- BV.asUnsigned g diff
               , off <= toInteger (V.length nvals)
                  -> simp nb oe (nvals V.++ V.drop (fromInteger off) ovals) mem

               | bo AIG.=== AIG.falseLit g
               , Just off <- BV.asUnsigned g diffo
               , off <= toInteger (V.length nvals) 
                  -> simp ob ne (dropEnd (fromInteger off) ovals V.++ nvals) mem

               | otherwise -> dmRawStore g ref nb ne nvals mem0

        simp nb ne nvals mem0 = dmRawStore g ref nb ne nvals mem0

dropEnd :: Int -> V.Vector a -> V.Vector a
dropEnd i v = V.take (V.length v - i) v

dmMod :: (IsAIG l g, Ord (l s))
      => g s -> RefIdx -> DMMod (BV (l s)) -> DagMemory (l s) -> IO (DagMemory (l s))
dmMod g ref (DMStore s e b) m = dmStore g ref s e b m
dmMod g ref (DMMemCopy s e src) m = dmMemCopyImpl g ref s e src m

-- | Store bytes in memory
dmStoreBytes :: (IsAIG l g, Ord (l s))
             => g s
             -> RefIdx
             -> DagMemory (l s)
             -> BitTerm (l s)
             -> BV (l s)
             -> Alignment
             -> IO (l s, DagMemory (l s))
dmStoreBytes g ref mem (PtrTerm ptr) flatBytes _
  | byteCount == 0 = return (AIG.trueLit g, mem)
  | otherwise = do

    --TODO: Figure out how to handle possibility that ptrEnd addition overflows.
    (ptrEnd,_of) <- BV.addC g ptr (BV.bvFromInteger g (BV.length ptr) (toInteger byteCount))
    m <- dmStore g ref ptr ptrEnd newBytes mem
    isalloc <- dmIsAllocated mem (ptr,ptrEnd)
    return (isalloc, m)
 where newBytes = sliceIntoBytes flatBytes
       byteCount = V.length newBytes
dmStoreBytes _ _ _ _ _ _ = illegalArgs "dmStoreBytes"

-- | Initialize global data memory.
dmInitGlobal :: (IsAIG l g, Ord (l s))
             => g s
             -> Int  -- ^ Width of pointer
             -> Addr -- ^ End of data region
             -> RefIdx
             -> DagMemory (l s) -> BV (l s) -> IO (Maybe (BitTerm (l s), DagMemory (l s)))
dmInitGlobal g ptrWidth dataEnd ref mem flatBytes
  | byteCount == 0 = return $ Just (PtrTerm ptr, mem)
  | dataEnd - dmsData (dmState mem) < byteCount = return Nothing
  | otherwise = do
      mem1 <- dmAllocSpace g ref (ptr,ptrEnd) ((dmState mem) { dmsData = nextData }) mem
      mem2 <- dmStore g ref ptr ptrEnd bytes mem1
      -- Return result
      return $ Just (PtrTerm ptr, mem2)
  where bytes = sliceIntoBytes flatBytes
        byteCount = toInteger (V.length bytes)
        nextData = dmsData (dmState mem) + byteCount
        ptr = BV.bvFromInteger g ptrWidth (dmsData (dmState mem))
        ptrEnd = BV.bvFromInteger g ptrWidth nextData

dmAddDefine :: (IsAIG l g, Ord (l s))
            => g s
            -> Int -- ^ width of pointers
            -> Addr -- ^ code end
            -> RefIdx
            -> DagMemory (l s) -- ^ Memory
            -> L.Symbol -- ^ Definition
            -> [L.BlockLabel] -- ^ Labels for blocks
            -> IO (Maybe (BitTerm (l s), [BitTerm (l s)], DagMemory (l s)))
dmAddDefine g ptrWidth codeEnd ref mem def blocks
   -- TODO: Alignment and overlap checks?
  | remaining >= bytesReq = do
      -- Get new memory
      m <- dmGetMem ref (DMAddDefine def (V.fromList blocks) mem) $
        mem { dmCode = ptr + bytesReq
            , dmDefineMap = Map.insert ptr def (dmDefineMap mem)
            }
      -- Return result
      return $ Just ( PtrTerm (BV.bvFromInteger g ptrWidth ptr)
                    , blockAddrs
                    , m)
  | otherwise = return Nothing
  where blockCount = length blocks
        ptr = dmCode mem
        remaining = codeEnd - ptr
        bytesReq = 1 + toInteger blockCount
        blockAddrs = PtrTerm . (BV.bvFromInteger g ptrWidth) . (ptr +) <$> [1..toInteger blockCount]


dmLookupSymbol :: (IsAIG l g, Eq (l s))
               => g s
               -> DagMemory (l s) -- ^ Memory
               -> BitTerm (l s)   -- ^ Pointer to symbol
               -> LookupSymbolResult
dmLookupSymbol g mem (PtrTerm a) = do
  case BV.asUnsigned g a of
    Nothing -> Left Indeterminate
    Just v ->
      case Map.lookup v (dmDefineMap mem) of
        Nothing -> Left Invalid
        Just d -> Right d
dmLookupSymbol _ _ _ = illegalArgs "dmLookupSymbol"

dmAllocSpace :: (IsAIG l g, Ord (l s))
             => g s
             -> RefIdx
             -> Range (BV (l s))
             -> DagMemoryState (l s)
             -> DagMemory (l s)
             -> IO (DagMemory (l s))
dmAllocSpace g ref (s, e) dms (dmNodeApp -> (DMAlloc ps pe mem))
  | BV.bvSame pe s = dmAllocSpace g ref (ps,e) dms mem
dmAllocSpace g ref (s, e) dms (dmNodeApp -> (DMAlloc ps pe mem))
  | BV.bvSame e ps = dmAllocSpace g ref (s,pe) dms mem
dmAllocSpace g ref (s, e) dms (dmNodeApp -> (DMMod m mem)) = do
  dmMod g ref m =<< dmAllocSpace g ref (s,e) dms mem
dmAllocSpace g ref (s, e) dms mem = do
  fn <- memo $ \range ->
          bvRangeCovered g (dmIsAllocated mem) range (s, e)
  dmGetMem ref (DMAlloc s e mem) $
    mem { dmState = dms
        , dmIsAllocated = fn
        }

dmStackAlloc :: (IsAIG l g, Ord (l s))
              => g s
              -> Int -- ^ Width of pointer in bits
              -> Bool -- ^ Flag indicates stack grows up
              -> BV (l s) -- ^ End of stack
              -> RefIdx
              -> DagMemory (l s)
              -> Size
              -> BitTerm (l s)
              -> Alignment
              -> IO (AllocResult (BitIO (DagMemory (l s)) (l s)))
dmStackAlloc g ptrWidth stackGrowsUp stackEnd ref mem eltSize (IntTerm eltCount) align = do
  let stack = dmsStack (dmState mem)
  newSizeExt <- BV.mulFull g (BV.bvFromInteger g ptrWidth (toInteger eltSize)) eltCount
  let extSize = BV.length newSizeExt
  let extVector v   = BV.zext g v extSize
  let truncVector v = BV.trunc ptrWidth v
  let stackEndExt = extVector stackEnd
  let x &&& y = join $ pure (AIG.and g) <*> x <*> y
  let x .<= y = BV.ule g x y
  let lneg x = return (AIG.not x)
  (c, newStack) <-
       if stackGrowsUp
          then do (ac, aStack) <- bvAlignUp g stack align
                  (newStackExt, ao) <- BV.addC g (extVector aStack) newSizeExt
                  c <- lneg ac &&& lneg ao &&& (newStackExt .<= stackEndExt)
                  return (c, truncVector newStackExt)
          else do aStack <- bvAlignDn g stack align
                  (newStackExt, ab) <- BV.subC g (extVector aStack) newSizeExt
                  c <- lneg ab &&& (stackEndExt .<= newStackExt)
                  return (c, truncVector newStackExt)
  case () of
   _ | c == AIG.falseLit g -> do
        return $ AResult c (PtrTerm stack) mem
     | stackGrowsUp -> do
        let a = (dmState mem) { dmsStack = newStack }
        m <- dmAllocSpace g ref (stack, newStack) a mem
        return $ AResult c (PtrTerm stack) m
     | otherwise -> do
        let a = (dmState mem) { dmsStack = newStack }
        m <- dmAllocSpace g ref (newStack, stack) a mem
        return $ AResult c (PtrTerm newStack) m
dmStackAlloc _ _ _ _ _ _ _ _ _ = illegalArgs "dmStackAlloc"

-- | Push stack frame to memory.
-- N.B. To avoid empty deallocations in stack pop, we always add a byte to
-- the stack when pushing.
dmStackPushFrame :: (IsAIG l g, Ord (l s))
                 => g s
                 -> RefIdx
                 -> DagMemory (l s)
                 -> IO ((l s), DagMemory (l s))
dmStackPushFrame g ref mem = do
  r <- dmGetMem ref (DMStackPush mem) $
         mem { dmStackFrames = dmsStack (dmState mem) : dmStackFrames mem }
  return (AIG.trueLit g, r)

-- | Pop stack frame in memory and invalidate old addresses.
dmStackPopFrame :: (IsAIG l g, Ord (l s))
                => g s
                -> Bool -- ^ Flag indicating if stack should grow up in memory.
                -> RefIdx
                -> DagMemory (l s)
                -> IO (DagMemory (l s))
dmStackPopFrame g stackGrowsUp ref mem =
  case dmStackFrames mem of
   [] -> bmError "internal: Attempted to pop stack frame from memory when no stack frames have been pushed."
   f : fl -> do
     let (ptr,ptrEnd) | stackGrowsUp = (f, dmsStack (dmState mem))
                      | otherwise    = (dmsStack (dmState mem), f)

         leq = BV.ule g
         eq = BV.bvEq g
         x &&& y = join $ pure (AIG.and g) <*> x <*> y
         x ||| y = join $ pure (AIG.or g) <*> x <*> y
         x `isLeq` y = (Just (<=) <*> BV.asUnsigned g x <*> BV.asUnsigned g y) == Just True

         notInRange (s,e) = (e `leq` ptr) ||| (ptrEnd `leq` s) ||| (ptr `eq` ptrEnd)

     let simp (dmNodeApp -> (DMAlloc l e smem))
           | (ptr `isLeq` l) && (e `isLeq` ptrEnd)
           = simp smem
         simp (dmNodeApp -> (DMMod (DMStore l e _) smem))
           | (ptr `isLeq` l) && (e `isLeq` ptrEnd)
           = simp smem
         simp (dmNodeApp -> (DMMod m smem))
           | ptrEnd `isLeq` dmModStart m = dmMod g ref m =<< simp smem
         simp (dmNodeApp -> (DMMod m smem))
           | dmModEnd m `isLeq` ptr = dmMod g ref m =<< simp smem
         -- TODO: Add mark to stop simplification.
         simp (dmNodeApp -> DMStackPush smem) = return smem
         simp smem = do
           allocFn <- memo $ \range -> (notInRange range) &&& (dmIsAllocated smem range)
           initFn <- memo $ \range -> (notInRange range) &&& (dmIsInitialized smem range)
           dmGetMem ref (DMStackPop ptr ptrEnd smem) $
             smem { dmState = (dmState smem) { dmsStack = f }
                  , dmStackFrames = fl
                  , dmIsAllocated = allocFn
                  , dmIsInitialized = initFn
                  }
      in simp mem

dmHeapAlloc :: (IsAIG l g, Ord (l s))
            => g s
            -> Int -- ^ Width of pointer in bits
            -> BV (l s) -- ^ End of heap
            -> RefIdx
            -> DagMemory (l s) -- ^ Memory
            -> Size        -- ^ Size of elements
            -> BitTerm (l s)   -- ^ Number of elements
            -> Alignment         -- ^ Alignment
            -> IO (AllocResult (BitIO (DagMemory (l s)) (l s)))
dmHeapAlloc g ptrWidth heapEnd ref mem eltSize (IntTerm eltCount) _align = do
        let heap = dmsHeap (dmState mem)
        newSizeExt <- BV.mul g (BV.bvFromInteger g ptrWidth (toInteger eltSize)) eltCount
        let extSize = BV.length newSizeExt
        let extVector v = BV.zext g v extSize
        let truncVector v = BV.trunc ptrWidth v
        (newHeapExt, ao) <- BV.addC g (extVector heap) newSizeExt
        c <- AIG.and g (AIG.not ao) =<< BV.ule g newHeapExt (extVector heapEnd)
        let mmem = if c AIG.=== AIG.falseLit g
                      then return mem
                      else let a = (dmState mem) { dmsHeap = newHeap }
                               newHeap = truncVector newHeapExt
                            in dmAllocSpace g ref (heap, newHeap) a mem
        AResult c (PtrTerm heap) <$> mmem

dmHeapAlloc _ _ _ _ _ _ _ _ = illegalArgs "dmHeapAlloc"

dmMemCopyImpl :: (IsAIG l g, Ord (l s))
              => g s
              -> RefIdx 
              -> BV (l s) -- ^ Destination start
              -> BV (l s) -- ^ Destination end
              -> BV (l s) -- ^ Source start
              -> DagMemory (l s) -- ^ Memory to start with
              -> IO (DagMemory (l s))
dmMemCopyImpl g ref dest destEnd src mem = do
  initFn <- memo $ \range ->
             bvRangeCovered g (dmIsInitialized mem) range (dest, destEnd)
  loadFn <- memo $ \p -> do
               (offset,b) <- BV.subC g p dest
               inRange <- join $ pure (AIG.and g (AIG.not b)) <*> (BV.ult g p destEnd)
               ptr <- BV.iteM g inRange (BV.add g src offset) (return p)
               dmLoadByte mem ptr
  dmGetMem ref (DMMod (DMMemCopy dest destEnd src) mem) $
     mem { dmIsInitialized = initFn
         , dmLoadByte = loadFn
         }
  

-- | Store bytes in memory
dmMemCopy :: (IsAIG l g, Ord (l s))
          => g s
          -> Int -- ^ Pointer width
          -> RefIdx
          -> DagMemory (l s)
          -> BitTerm (l s)   -- ^ Destination pointer
          -> BitTerm (l s)   -- ^ Source pointer
          -> BitWidth    -- ^ Width of length value. 
          -> BitTerm (l s)   -- ^ Length value
          -> BitTerm (l s)  -- ^ Alignment in bytes
          -> IO (l s, DagMemory (l s))
dmMemCopy g ptrWidth ref mem (PtrTerm dest) (PtrTerm src) _ (IntTerm l) _
 | BV.length src /= ptrWidth = bmError "internal: src pointer size does not match pointer width."
 | BV.length dest /= ptrWidth = bmError "internal: dest pointer size does not match pointer width"
 | otherwise = do
    let lWidth = BV.length l
    let lext = BV.zeroIntCoerce g ptrWidth l
    (srcEnd,  srcOverflow)  <- BV.addC g src  lext
    (destEnd, destOverflow) <- BV.addC g dest lext
    lenOverflow <- if lWidth >= ptrWidth
                      then BV.nonZero g (BV.trunc (lWidth - ptrWidth) l)
                      else return (AIG.falseLit g)
    addrOverflow <- foldM (AIG.or g) (AIG.falseLit g) [lenOverflow, srcOverflow, destOverflow]
                             -- Check src is readable and dest is writable
    memValid <- join $ pure (AIG.and g) <*> (dmIsInitialized mem (src, srcEnd))
                                        <*> (dmIsAllocated mem (dest, destEnd))
    c <- join $ pure (AIG.or g) <*> (BV.isZero g l) <*> (AIG.and g (AIG.not addrOverflow) memValid)
    mem' <- dmMemCopyImpl g ref dest destEnd src mem
    return (c,mem')
dmMemCopy _ _ _ _ _ _ _ _ _ = illegalArgs "dmMemCopy"

dmRecordBranch :: RefIdx -> DagMemory l -> IO (DagMemory l)
-- We can essentially undo merge frame changes if no merges happened since pop.
--dmRecordBranch _ (dmNodeApp -> DMMergeFramePop mem) = do
--  return mem
dmRecordBranch ref mem =
  dmGetMem ref (DMMergeFramePush mem) mem { dmMergeDepth = dmMergeDepth mem + 1 }

dmBranchAbort :: RefIdx -> DagMemory l -> IO (DagMemory l)
dmBranchAbort ref mem 
  | d <= 0 = error "internal: dmBranchAbort called on negative merge depth"
  | otherwise = do
      dmGetMem ref (DMMergeFramePop mem) mem { dmMergeDepth = d - 1 }
 where d = dmMergeDepth mem

dmMux :: (IsAIG l g, Ord (l s))
      => g s 
      ->RefIdx
      -> l s 
      -> DagMemory (l s)
      -> DagMemory (l s)
      -> IO (DagMemory (l s))
dmMux g ref c t f = do
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

  let mux
       | c AIG.=== AIG.falseLit g = \x _y -> return x
       | c AIG.=== AIG.trueLit g  = \_x y -> return y
       | otherwise = AIG.mux g c

  -- FIXME? should we lift the lazy mux up a level instead if zipping 'mux' into everything?
  dms <- BV.zipWithM mux (dmsStack ta) (dmsStack fa)
  dmh <- BV.zipWithM mux (dmsHeap ta)  (dmsHeap fa)
  dmsf <- zipWithM (BV.zipWithM mux) (dmStackFrames t) (dmStackFrames f)
  allocFn <- memo $ \r -> join $ pure mux <*> (dmIsAllocated   t r) <*> (dmIsAllocated   f r)
  initFn  <- memo $ \r -> join $ pure mux <*> (dmIsInitialized t r) <*> (dmIsInitialized f r)
  loadFn  <- memo $ \p -> BV.iteM g c (dmLoadByte t p) (dmLoadByte f p)

  dmGetMem ref (DMMux c t f) $
    t { dmState = DMS { dmsStack = dms
                      , dmsData = dmsData ta
                      , dmsHeap = dmh
                      }
      , dmStackFrames   = dmsf
      , dmMergeDepth = dmMergeDepth t - 1
      , dmIsAllocated   = allocFn
      , dmIsInitialized = initFn 
      , dmLoadByte      = loadFn
      }


createDagMemModel :: (IsAIG l g, Ord (l s))
                  => DataLayout
                  -> g s
                  -> MemGeom
                  -> IO (BitBlastMemModel (DagMemory (l s)) (l s), DagMemory (l s))
createDagMemModel dl g mg = do
  let ptrWidth = ptrBitwidth dl
  let stackGrowsUp = not (decreasing (mgStack mg))
  ref <- newIORef 1
  let ptrStart range = BV.bvFromInteger g ptrWidth (start range)
  let ptrEnd range = BV.bvFromInteger g ptrWidth (end range)
  let mm = MemModel
              { mmLoad = dmLoadBytes g
              , mmStore = dmStoreBytes g ref
              , mmInitGlobal = dmInitGlobal g ptrWidth (end (mgData mg)) ref
              , mmDump = dmDump g
              , mmAddDefine = dmAddDefine g ptrWidth (end (mgCode mg)) ref
              , mmLookupSymbol = dmLookupSymbol g
              , mmStackAlloc = dmStackAlloc g ptrWidth stackGrowsUp (ptrEnd (mgStack mg)) ref
              , mmStackPush = dmStackPushFrame g ref
              , mmStackPop = dmStackPopFrame g stackGrowsUp ref
              , mmHeapAlloc = dmHeapAlloc g ptrWidth (ptrEnd (mgHeap mg)) ref
              , mmMemCopy = dmMemCopy g ptrWidth ref
              , mmRecordBranch = dmRecordBranch ref
              , mmBranchAbort = dmBranchAbort ref
              , mmMux = dmMux g ref
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
                      , dmIsAllocated = uncurry (BV.bvEq g)
                      , dmIsInitialized = uncurry (BV.bvEq g)
                      , dmLoadByte = return . const (dontCareByte g)
                      }
  return (mm, mem)

createDagAll :: (IsAIG l g, Ord (l s))
             => g s
             -> (FilePath -> l s -> IO [Maybe Int])
             -> DataLayout
             -> MemGeom
             -> IO SBEPair
createDagAll g cnfFunc dl mg = do
    uncurry SBEPair . Arrow.first (sbeBitBlast g cnfFunc dl) <$> createDagMemModel dl g mg

-- -- Aiger operations {{{1

evalAigerImpl :: (IsAIG l g, Eq (l s))
              => g s -> DataLayout -> [Bool] -> MemType -> BitTerm (l s) -> IO (BitTerm (l s))
evalAigerImpl g dl inps tp t = do
      eval <- AIG.evaluator g inps
      return $ unflattenTerm g dl tp $ fmap eval $ flattenTerm g t

--  SBE Definition {{{1

newtype BitIO m l v = BitIO { liftSBEBitBlast :: IO v }
   deriving (Monad, MonadIO, Functor)

type instance SBETerm (BitIO m l)       = BitTerm l
type instance SBEPred (BitIO m l)       = l
type instance SBEMemory (BitIO m l)     = m

type BitBlastSBE m l = SBE (BitIO m l)


applyIntArithOp :: (IsAIG l g, Eq (l s))
                => g s
                -> IntArithOp
                -> OptVectorLength
                -> BitTerm (l s)
                -> BitTerm (l s)
                -> IO (BitTerm (l s))
applyIntArithOp g op = vf
  where vf (Just n) (VecTerm x) (VecTerm y)
          | V.length x == n && V.length y == n = VecTerm <$> V.zipWithM ef x y
        vf Just{} _ _ = badArgs
        vf Nothing x y = ef x y
        ef (IntTerm x) (IntTerm y) = IntTerm <$> f g x y
        ef _ _ = badArgs
        badArgs = error $ show $ text "applyIntArithOp" <+> ppIntArithOp op
                             <+> text "given invalid arguments."
        f = case op of
              Add _ _ -> BV.add
              Sub _ _ -> BV.sub
              Mul _ _ -> BV.mul
              UDiv _  -> BV.uquot
              SDiv _  -> BV.squot
              URem    -> BV.urem
              SRem    -> BV.srem
              Shl _ _ -> BV.shl
              Lshr _  -> BV.ushr
              Ashr _  -> BV.sshr
              And -> BV.zipWithM . AIG.and
              Or  -> BV.zipWithM . AIG.or
              Xor -> BV.zipWithM . AIG.xor


applyExpr :: forall l g s
          .  (IsAIG l g, Eq (l s))
          => g s 
          -> DataLayout
          -> TypedExpr (BitTerm (l s))
          -> IO (BitTerm (l s))
applyExpr g dl texpr = do
  let ptrWidth = ptrBitwidth dl

  let applyICmp f Nothing x y = liftBinIntRel f x y
      applyICmp f Just{} (VecTerm x) (VecTerm y) =
        VecTerm <$> V.zipWithM (liftBinIntRel f) x y
      applyICmp _ _ _ _ = illegalArgs "applyICmp"

      retMV :: Maybe Int
            -> (BitTerm l -> BitTerm l)
            -> BitTerm l -> IO (BitTerm l)
      retMV Nothing f t = return (f t)
      retMV Just{} f (VecTerm t) = return (VecTerm (f <$> t))
      retMV _ _ _ = illegalArgs "retMV"

      retIntMV :: Maybe Int
               -> (BV l -> BV l)
               -> BitTerm l -> IO (BitTerm l)
      retIntMV mn f t = retMV mn (IntTerm . f . asIntTerm) t

      expectPtrArg :: (BV l -> a) -> BitTerm l -> a
      expectPtrArg f (PtrTerm x) = f x
      expectPtrArg _ _ = error "expectPtrArg given illegal argument"

  case texpr of
    IntArith op mn _ x y -> applyIntArithOp g op mn x y
    PtrAdd (PtrTerm x) (IntTerm y)
      | BV.length x == BV.length y ->
        PtrTerm <$> BV.add g x y
    PtrAdd{} -> illegalArgs "PtrAdd"
    UAddWithOverflow _ (IntTerm x) (IntTerm y) ->
       (\(u,c) -> StructTerm (V.fromList [IntTerm u, IntTerm (BV.singleton c)]))
         <$> BV.addC g x y
    UAddWithOverflow{} -> illegalArgs "UAddWithOverflow"
    ICmp op mn _ x y -> applyICmp opFn mn x y
      where neg fn u v = AIG.not <$> fn u v
            opFn = case op of
                     L.Ieq  -> BV.bvEq g
                     L.Ine  -> neg $ BV.bvEq g
                     L.Iugt -> neg $ BV.ule g
                     L.Iuge -> neg $ BV.ult g
                     L.Iult -> BV.ult g
                     L.Iule -> BV.ule g
                     L.Isgt -> neg $ BV.sle g
                     L.Isge -> neg $ BV.slt g
                     L.Islt -> BV.slt g
                     L.Isle -> BV.sle g
    Trunc mn _ t rw -> retIntMV mn (\v -> assert (BV.length v >= rw) $ BV.trunc rw v) t
    ZExt  mn _ t rw   -> retIntMV mn (\x -> BV.zext g x rw) t
    SExt   mn _ t rw  -> retIntMV mn (\x -> BV.sext g x rw) t
    PtrToInt mn _ t w -> retMV mn (IntTerm . expectPtrArg f) t
      where f = BV.zeroIntCoerce g w
    IntToPtr mn _ t _ -> retMV mn (PtrTerm . f . asIntTerm) t
      where f = BV.zeroIntCoerce g ptrWidth
    Select Nothing c _ t f -> either fail return =<< muxTerm g (asInt1 c) t f
    Select (Just n) (VecTerm cv) _ (VecTerm tv) (VecTerm fv)
      | V.length cv == n -> either fail (return . VecTerm) =<< (fmap V.sequence rv)
          where rv = sequenceOf traverse $ V.zipWith3 (muxTerm g) (asInt1 <$> cv) tv fv
    Select{} -> illegalArgs "Select"                                       
    GetStructField _ (StructTerm t) i -> return $ t V.! i
    GetStructField{} -> illegalArgs "GetStructField"
    GetConstArrayElt _ _ (ArrayTerm t) i -> return $ t V.! i
    GetConstArrayElt{} -> illegalArgs "GetConstArrayElt"
    SValInteger w v -> return $ IntTerm $ BV.bvFromInteger g w v
    SValFloat v  -> return $ FloatTerm v
    SValDouble v -> return $ DoubleTerm v
    SValNull _ -> return $ PtrTerm $ BV.bvFromInteger g ptrWidth 0
    SValArray  _ valTerms -> return (ArrayTerm valTerms)
    SValVector _ valTerms -> return (VecTerm valTerms)
    SValStruct _ valTerms -> return (StructTerm valTerms)


sbeBitBlast :: forall m g l s
             . (IsAIG l g, Eq (l s))
            => g s
            -> (FilePath -> l s -> IO [Maybe Int])
            -> DataLayout
            -> BitBlastMemModel m (l s)
            -> SBE (BitIO m (l s))
sbeBitBlast g cnfFunc dl mm =
           SBE
           { sbeTruePred      = AIG.trueLit g
           , applyIEq         = \_ (IntTerm x) (IntTerm y) -> do
                  BitIO $ BV.bvEq g x y
           , applyAnd         = BitIO `c2` AIG.and g
           , applyBNot        = return . AIG.not
           , applyPredIte     = BitIO `c3` AIG.lazyMux g
           , applyIte         = \_ c x y -> BitIO $ muxTerm g c x y
           , freshInt         = \w -> BitIO $
                 IntTerm <$> BV.replicateM w (AIG.newInput g)
           , typedExprEval    = \expr ->
                 return $ ExprEvalFn $ \eval -> liftIO . applyExpr g dl =<< traverse eval expr
           , applyTypedExpr   = BitIO . applyExpr g dl
           , prettyPredD      = lPrettyLV g . BV.singleton
           , prettyTermD      = ppBitTerm g
           , asBool           = litAsBool g
           , evalPred = \inps p -> BitIO $ fmap ($ p) $ AIG.evaluator g inps
           , asUnsignedInteger = \_ -> BV.asUnsigned g . asIntTerm
           , asSignedInteger   = \_ -> BV.asSigned g . asIntTerm
           , asConcretePtr     = BV.asUnsigned g . asPtrTerm
           , memDump          = BitIO `c2` mmDump mm True
           , memLoad          = BitIO `c4` loadTerm g dl mm
           , memStore         = BitIO `c5` storeTerm g dl mm
           , memBranch        = BitIO . mmRecordBranch mm
           , memBranchAbort   = BitIO . mmBranchAbort mm
           , memMerge         = BitIO `c3` mmMux mm
           , memAddDefine     = \mem d vl -> BitIO $ mmAddDefine mm mem d vl
           , memInitGlobal    = \m ty gd ->
                                  BitIO $ mmInitGlobal mm m (termToBytes g dl ty gd)
           , codeLookupSymbol = return `c2` mmLookupSymbol mm
           , stackAlloc = \m eltTp _ cnt a -> BitIO $
               mmStackAlloc mm m (memTypeSize dl eltTp) cnt a
           , stackPushFrame   = BitIO . mmStackPush mm
           , stackPopFrame    = BitIO . mmStackPop mm
           , heapAlloc        = \m eltTp _ cnt a ->
               BitIO $ mmHeapAlloc mm m (memTypeSize dl eltTp) cnt a
           , memCopy          = BitIO `c6` mmMemCopy mm

           , termSAT          = BitIO . AIG.checkSat g
           , writeAiger       = \f ts -> BitIO $ do
               let outputs = BV.concat (flattenTerm g . snd <$> ts)
               AIG.writeAiger f (AIG.Network g (BV.bvToList outputs))

           , evalAiger        = BitIO `c3` evalAigerImpl g dl
           , writeCnf         = \f _ t -> BitIO $ BV.isZero g (flattenTerm g t) >>= cnfFunc f
           , writeSAWCore = Nothing
           , createSMTLIB1Script = Nothing
           , createSMTLIB2Script = Nothing
           , sbeRunIO = liftSBEBitBlast 
           }

ppBitTerm :: (IsAIG l g) => g s -> BitTerm (l s) -> Doc
ppBitTerm g (IntTerm t) = text "i" <> lPrettyLV g t
ppBitTerm _ (FloatTerm v) = text (show v)
ppBitTerm _ (DoubleTerm v) = text (show v)
ppBitTerm g (PtrTerm t) = text "p" <> lPrettyLV g t
ppBitTerm g (ArrayTerm v) = brackets $ commas $ V.toList $ ppBitTerm g <$> v
ppBitTerm g (VecTerm v) = brackets $ commas $ V.toList $ ppBitTerm g <$> v
ppBitTerm g (StructTerm v) = structBraces $ commas $ V.toList $ ppBitTerm g <$> v

liftBinIntRel :: (BV l -> BV l -> IO l)
              -> BitTerm l -> BitTerm l -> IO (BitTerm l)
liftBinIntRel f (IntTerm x) (IntTerm y) = IntTerm . BV.singleton <$> f x y
liftBinIntRel f (PtrTerm x) (PtrTerm y) = IntTerm . BV.singleton <$> f x y
liftBinIntRel _ _ _ = error "Illegal arguments to liftBinIntRel"

asInt1 :: BitTerm l -> l
asInt1 (IntTerm x) = assert (BV.length x == 1) $ x BV.! 0
asInt1 _ = error "Illegal arguments to asInt1"

asIntTerm :: BitTerm l -> BV l
asIntTerm (IntTerm x) = x
asIntTerm _ = illegalArgs "asIntTerm"

asPtrTerm :: BitTerm l -> BV l
asPtrTerm (PtrTerm x) = x
asPtrTerm _ = illegalArgs "asIntTerm"

-- | Return if then else of terms.
muxTerm :: (Eq (l s), IsAIG l g) 
        => g s -> l s -> BitTerm (l s) -> BitTerm (l s) -> IO (Either String (BitTerm (l s)))
muxTerm g c x0 y0 
  | c AIG.=== AIG.trueLit g  = return $ Right x0
  | c AIG.=== AIG.falseLit g = return $ Right y0
  | otherwise = 
     case (x0, y0) of
       (IntTerm x, IntTerm y) -> fmap (Right . IntTerm) $ BV.ite g c x y
       (FloatTerm x, FloatTerm y)
         | x == y -> return $ Right $ FloatTerm x
         | otherwise -> return $ Left "Backend does not support merging symbolic floating point values."
       (DoubleTerm x, DoubleTerm y)
         | x == y -> return $ Right $ DoubleTerm x
         | otherwise -> return $ Left "Backend does not support merging symbolic floating point values."
       (PtrTerm x, PtrTerm y) -> fmap (Right. PtrTerm) $ BV.ite g c x y
       (ArrayTerm x, ArrayTerm y) -> fmap (fmap ArrayTerm . V.sequence) $ V.zipWithM (muxTerm g c) x y
       (VecTerm x, VecTerm y) -> fmap (fmap VecTerm . V.sequence) $ V.zipWithM (muxTerm g c) x y
       (StructTerm x, StructTerm y) -> fmap (fmap VecTerm . V.sequence) $ V.zipWithM (muxTerm g c) x y
       _ -> return $ Left "Internal error: Backend given incompatible terms" 

-- | Converts a bit term into a single lit vector.
flattenTerm :: (IsAIG l g) => g s -> BitTerm (l s) -> BV (l s)
flattenTerm g t0 =
  case t0 of
    IntTerm v -> v
    FloatTerm v -> lVectorFromFloat g v
    DoubleTerm v -> lVectorFromDouble g v
    PtrTerm v -> v
    ArrayTerm v  -> joinN (flattenTerm g <$> v)
    VecTerm v    -> joinN (flattenTerm g <$> v)
    StructTerm v -> joinN (flattenTerm g <$> v)

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


vecToBits :: (Bits a, Num a) => BV Bool -> a
vecToBits v = impl 0 0 
  where impl i r 
          | i == BV.length v = r
          | v BV.! i  = impl (i+1) (r `setBit` i)
          | otherwise = impl (i+1) r

-- | Converts from flat lit vector to bitterm based on type.
unflattenTerm :: (IsAIG l g) => g s -> DataLayout -> MemType -> BV Bool -> BitTerm (l s)
unflattenTerm g dl tp0 v =
  case tp0 of
    IntType w
      | BV.length v == w ->
          IntTerm (fmap (AIG.constant g) v)
      | otherwise -> badVec $ "integer"
    FloatType
      | BV.length v == 32 ->
          FloatTerm  $ wordToFloat $ vecToBits v
      | otherwise -> badVec "float"
    DoubleType
      | BV.length v == 64 ->
          DoubleTerm $ wordToDouble $ vecToBits v
      | otherwise -> badVec "double"
    PtrType{}
      | ptrBitwidth dl == fromIntegral (BV.length v) ->
        PtrTerm (fmap (AIG.constant g) v)
      | otherwise -> badVec "ptr"
    ArrayType n etp -> ArrayTerm  $ unflattenTerm g dl etp <$> sliceN n v
    VecType n etp   -> VecTerm    $ unflattenTerm g dl etp <$> sliceN n v
    StructType si   -> StructTerm $ V.zipWith (unflattenTerm g dl) flv vv
      where flv = siFieldTypes si
            szv = memTypeBitsize dl <$> flv
            ofv = V.prescanl (+) 0 szv
            vv = V.zipWith (\i o-> BV.slice v i o) ofv szv
 where badVec nm = error $
        "internalError: unflattern given incorrect number of bits for "
        ++ nm ++ "."
