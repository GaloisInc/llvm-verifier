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
{-# LANGUAGE ViewPatterns               #-}

module LSS.SBEBitBlast
  ( module LSS.SBEInterface
  , module Data.LLVM.Memory
  , BitBlastSBE
  , BitTerm
  , BitTermClosed(..)
  , sbeBitBlast
  , liftSBEBitBlast
    -- Memmodel code
  , MemModel(..)
  , BitBlastMemModel
  , BitMemory
  , buddyMemModel
  , buddyInitMemory
  , createBuddyMemModel
  , DagMemory
  , createDagMemModel
  -- for testing only
  , BitIO
  , bmDataAddr
  ) where

import           Control.Applicative       ((<$>))
import           Control.Exception         (assert)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bits
import           Data.Int
import           Data.IORef
import           Data.LLVM.Memory
import           Data.LLVM.TargetData
import           Data.List                 (foldl', unfoldr)
import           Data.Map                  (Map)
import           Data.Set                  (Set)
import           Debug.Trace
import           LSS.Execution.Utils
import           LSS.SBEInterface
import           Numeric                   (showHex)
import           Text.PrettyPrint.HughesPJ
import           Verinf.Symbolic.Common    (ConstantProjection(..),
                                            createBitEngine,
                                            CValue(..))
import           Verinf.Symbolic.Lit
import           Verinf.Symbolic.Lit.Functional
import qualified Data.Map                  as Map
import qualified Data.Set                  as Set
import qualified Data.Vector               as V
import qualified Data.Vector.Storable      as LV
import qualified Text.LLVM.AST             as LLVM
import qualified Verinf.Symbolic           as S

-- Utility functions and declarations {{{1

c2 :: (r -> s) -> (a -> b -> r) -> a -> b -> s
g `c2` f = \x y -> g (f x y)

c3 :: (r -> s) -> (a -> b -> c -> r) -> a -> b -> c -> s
g `c3` f = \x y z -> g (f x y z)

c4 :: (r -> s) -> (a -> b -> c -> d -> r) -> a -> b -> c -> d -> s
g `c4` f = \w x y z -> g (f w x y z)

c5 :: (r -> s) -> (a -> b -> c -> d -> e -> r) -> a -> b -> c -> d -> e -> s
g `c5` f = \v w x y z -> g (f v w x y z)

-- | Returns number of bytes.
byteSize :: LV.Storable l => LV.Vector l -> Int
byteSize v = LV.length v `shiftR` 3

-- | Slice a vector into a list of vectors, one for each byte.
sliceIntoBytes :: LV.Storable l => LV.Vector l -> V.Vector (LV.Vector l)
sliceIntoBytes v = V.generate (byteSize v) $ \i -> LV.slice (i `shiftL` 3) 8 v

-- | @alignUp addr i@ returns the smallest multiple of @2^i@ that it
-- at least @addr@.
alignUp :: Addr -> Int -> Addr
alignUp addr i = (addr + mask) .&. complement mask
 where mask = (setBit 0 i) - 1

-- | @alignDn addr i@ returns the largest multiple of @2^i@ that it
-- at most @addr@.
alignDn :: Addr -> Int -> Addr
alignDn addr i = addr .&. complement mask
 where mask = (setBit 0 i) - 1

lvAnd :: (?be :: BitEngine l, LV.Storable l) 
      => LV.Vector l -> LV.Vector l -> LV.Vector l
lvAnd = LV.zipWith lAnd

lvSetBits :: (?be :: BitEngine l, LV.Storable l) => Int -> (Int -> Bool) -> LV.Vector l
lvSetBits n pr = LV.generate n (lFromBool . pr)

-- | @lAlignUp addr i@ returns pair @(c,v)@ where @v@ is the smallest multiple of @2^i@
-- not smaller than @addr@, and @c@ is set if computation overflowed.
lAlignUp :: (?be :: BitEngine l, LV.Storable l) => LV.Vector l -> Int -> (l,LV.Vector l)
lAlignUp addr i = (c, s `lvAnd` lvSetBits n (>= i))
  where n = LV.length addr
        (c,s) = addr `lFullAdd` lvSetBits n (< i)

-- | @lAlignDown addr i@ returns pair @(c,v)@ where @v@ is the largest multiple of @2^i@
-- not larger than @addr@, and @c@ is set if computation overflowed.
lAlignDn :: (?be :: BitEngine l, LV.Storable l) => LV.Vector l -> Int -> LV.Vector l
lAlignDn addr i = addr `lvAnd` lvSetBits (LV.length addr) (>= i)

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

beIteM :: Eq l => BitEngine l -> l -> IO l -> IO l -> IO l
beIteM be c tm fm
  | c == beTrue be = tm
  | c == beFalse be = fm
  | otherwise = tm >>= \t -> fm >>= \f -> beIte be c t f

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

-- | @beInRange be p (s,e)@ returns predicate that holds in @s <= p & p < e@
-- when treated as unsigned values.
beInRange :: (Eq l, LV.Storable l) => BitEngine l -> LV.Vector l -> (LV.Vector l, LV.Vector l) -> IO l
beInRange be p (s,e) = beAndM be (beUnsignedLeq be s p) (beUnsignedLt be p e)

-- | @beRangeCovered be subFn r1 r2@ returns true if @subFn@ returns true for
-- all ranges in @r1 - r2@.  N.B. Only calls subFn on an empty range if @r1@ is
-- empty.
beRangeCovered :: (Eq l, LV.Storable l)
               => BitEngine l
               -> (Range (LV.Vector l) -> IO l)
               -> Range (LV.Vector l)
               -> Range (LV.Vector l)
               -> IO l
beRangeCovered be subFn (s1,e1) (s2,e2) =
 ite rangesOverlap
     (((s2 `le` s1) ||| subFn (s1,s2)) &&& ((e1 `le` e2) ||| subFn (e2,e1)))
     (subFn (s1,e1))
 where le = beUnsignedLeq be
       lt = beUnsignedLt be
       ite cm tm fm = cm >>= \c -> beIteM be c tm fm
       (&&&) = beAndM be
       (|||) = beOrM be
       rangesOverlap = (s2 `lt` e1) &&& (s1 `lt` e2)

-- | Returns true if vector is non-zero.
beIsNonZero :: LV.Storable l => BitEngine l -> LV.Vector l -> IO l
beIsNonZero be = LV.foldM (beOr be) (beFalse be)

-- | Returns true if vector is zero.
beIsZero :: LV.Storable l => BitEngine l -> LV.Vector l -> IO l
beIsZero be v = beNeg be <$> beIsNonZero be v

-- BitTerm {{{1

newtype BitTerm l = BitTerm { btVector :: LV.Vector l }
  deriving (Eq)
newtype BitTermClosed l = BitTermClosed (BitEngine l, BitTerm l)

termFromLit :: LV.Storable l => l -> BitTerm l
termFromLit = BitTerm . LV.singleton

termToSVal :: (Eq l, LV.Storable l) => BitEngine l -> LV.Vector l -> Maybe Integer
termToSVal be lv =
  case beVectorToMaybeInt be lv of
    Nothing     -> Nothing
    Just (w, v) -> Just $ case w of
      8             -> fromIntegral (i8 v)
      16            -> fromIntegral (fromIntegral v :: Int16)
      32            -> fromIntegral (fromIntegral v :: Int32)
      64            -> fromIntegral (fromIntegral v :: Int64)
      n | n < 8     -> fromIntegral (i8 v .&. (2 ^ n - 1))
        | otherwise -> v -- TODO: Truncation to length n
            -- error $ "BitTermClosed/getSVal: unsupported integer width " ++ show w
      where
        i8 :: Integer -> Int8
        i8 x = fromIntegral x :: Int8

bePrettyLit :: Eq l => BitEngine l -> l -> Doc
bePrettyLit be x | x == beFalse be = text "False"
                 | x == beTrue be = text "True"
                 | otherwise = text "?:[1]"

bePrettyLV :: (Eq l, LV.Storable l) => BitEngine l -> LV.Vector l -> Doc
bePrettyLV be bv
  | 1 == LV.length bv = bePrettyLit be (bv LV.! 0)
  | otherwise = text str <> colon <>  brackets (text $ show $ LV.length bv)
                  <+> maybe empty cvt (termToSVal be bv)
  where
    cvt x = parens (integer x)
            <+> if x >= 0
                then hex x
                else case beVectorToMaybeInt be bv of
                       Nothing -> empty
                       Just (_,u)  -> hex u
    hex x = parens $ text "0x" <> text (showHex x "")
    str      = LV.foldr (\lit acc -> acc ++ [toChar lit]) "" bv
    toChar x = if x == beFalse be then '0' else if x == beTrue be then '1' else '?'

instance (Eq l, LV.Storable l) => S.PrettyTerm (BitTermClosed l) where
  prettyTermWithD _ppconf (BitTermClosed (be, BitTerm bv)) = bePrettyLV be bv

instance (LV.Storable l, Eq l) => ConstantProjection (BitTermClosed l) where
  getSVal (BitTermClosed (be, t)) = termToSVal be (btVector t)
  getUVal (BitTermClosed (be, t)) = snd <$> beVectorToMaybeInt be (btVector t)

  getBool (BitTermClosed (be, t)) =
    case beVectorToMaybeInt be (btVector t) of
      Nothing     -> Nothing
      Just (1, v) -> Just (toEnum (fromIntegral v) :: Bool)
      Just (_, _) -> error "BitTermClosed/getBool: term bit width not 1"

  -- TODO: this isn't a complete implementation
  termConst (BitTermClosed (be, t)) =
    case beVectorToMaybeInt be (btVector t) of
      Nothing     -> Nothing
      Just (w, v) -> Just (CInt (fromIntegral w) v)

bytesToTerm :: LV.Storable l => LLVMContext -> LLVM.Type -> LV.Vector l -> BitTerm l
bytesToTerm _ (LLVM.PrimType (LLVM.Integer w)) bits =
  BitTerm (LV.take (fromIntegral w) bits)
bytesToTerm _ _ bits = BitTerm bits

termToBytes :: LV.Storable l
            => LLVMContext -> BitEngine l -> LLVM.Type -> BitTerm l -> LV.Vector l
termToBytes lc be tp (BitTerm val) =
  case resolveType lc tp of
    -- Extend integer types to full width.
    LLVM.PrimType (LLVM.Integer w) ->
      let newBits = (8 - (w .&. 0x7)) .&. 0x7
       in val LV.++ LV.replicate (fromIntegral newBits) (beDontCare be)
    -- Treat other types as same.
    _ -> val


-- MemModel {{{1

data MemModel mem ptr int bytes cond = MemModel {
    mmDump :: Bool -> mem -> Maybe [Range Addr] -> IO ()
  , mmLoad :: mem -> ptr -> Integer -> IO (cond, bytes)
    -- | @mmStore mem value addr@
  , mmStore :: mem -> bytes -> ptr -> IO (cond, mem)
  , mmMux :: cond -> mem -> mem -> IO mem
  , mmInitGlobal :: mem -> bytes -> IO (Maybe (ptr, mem))
  , mmAddDefine :: mem -> LLVM.Symbol -> V.Vector LLVM.BlockLabel -> IO (Maybe (ptr, mem))
  , mmBlockAddress :: mem -> LLVM.Symbol -> LLVM.BlockLabel -> ptr
  , mmLookupDefine :: mem -> ptr -> LookupDefineResult
    -- | Alloc structure on stack
  , mmStackAlloca :: mem -- ^ Memory
                  -> Integer -- ^ Size of each element
                  -> int -- ^ Number of elements
                  -> Int -- ^ Log-base 2 of alignment
                  -> IO (StackAllocaResult ptr mem)
  , mmStackPush :: mem -> IO (cond, mem)
  , mmStackPop :: mem -> IO mem
  , mmHeapAlloc :: mem -> Integer -> int -> Int -> IO (HeapAllocResult ptr mem)
  , mmMemCopy :: mem
              -> ptr            -- ^ Destination pointer
              -> ptr            -- ^ Source pointer
              -> int            -- ^ Length value
              -> int            -- ^ Alignment in bytes
              -> IO (cond, mem) -- ^ Condition and new value.
  }

type BitBlastMemModel m l = MemModel m (BitTerm l) (BitTerm l) (LV.Vector l) (BitTerm l)

-- | Load memory using
loadTerm :: (Eq l, LV.Storable l)
         => LLVMContext
         -> MemModel m ptr (BitTerm l) (LV.Vector l) (BitTerm l)
         -> m
         -> LLVM.Typed ptr
         -> IO (BitTerm l, BitTerm l)
loadTerm lc mm bm ptr
  | LLVM.PtrTo tp <- resolveType lc (LLVM.typedType ptr) = do
      (c, bits) <- mmLoad mm bm (LLVM.typedValue ptr) (llvmStoreSizeOf lc tp)
      return (c, bytesToTerm lc tp bits)
  | otherwise = bmError "internal: Illegal type given to load"

-- | Store term in memory model.
storeTerm :: (Eq l, LV.Storable l)
          => LLVMContext
          -> BitEngine l
          -> MemModel mem ptr (BitTerm l) (LV.Vector l) (BitTerm l)
          -> mem
          -> LLVM.Typed (BitTerm l)
          -> ptr
          -> IO (BitTerm l, mem)
storeTerm lc be mm m v ptr
  | LV.length bytes == 0 = return (termFromLit (beTrue be), m)
  | otherwise = mmStore mm m bytes ptr
  where bytes = termToBytes lc be (LLVM.typedType v) (LLVM.typedValue v)

-- BasicBlockMap {{{1

type BasicBlockMap l = Map (LLVM.Symbol,LLVM.BlockLabel) (BitTerm l)

blockAddress :: BasicBlockMap l -> LLVM.Symbol -> LLVM.BlockLabel -> BitTerm l
blockAddress bbm d b =
  let errMsg = "internal: Failed to find block " ++ show b ++ " in " ++ show d ++ "."
   in maybe (bmError errMsg) id $ Map.lookup (d,b) bbm

addBlockLabels :: LV.Storable l
               => Int -- ^ Pointer width
               -> BitEngine l
               -> LLVM.Symbol -- ^ Definition
               -> V.Vector LLVM.BlockLabel -- ^ Block labels.
               -> Addr -- ^ Base address
               -> BasicBlockMap l -- ^ Initial map
               -> BasicBlockMap l
addBlockLabels ptrWidth be def blocks base initMap =
  V.foldl insertAddr initMap (V.enumFromN 0 (V.length blocks))
 where insertAddr m i =
         let v = beVectorFromInt be ptrWidth (base + i)
          in Map.insert (def,blocks V.! fromInteger i) (BitTerm v) m

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
  | SDefine LLVM.Symbol -- ^ Memory value for function definition.
  | SBlock LLVM.Symbol LLVM.BlockLabel -- ^ Memory value for block within function.
  | SUnallocated -- ^ A memory section that has not been allocated to the program.

-- A derived(Show)-like pretty printer for the Storage type
ppStorageShow :: (Eq l, LV.Storable l) => BitEngine l -> Storage l -> Doc
ppStorageShow be (SBranch f t) = text "SBranch" <+> parens (ppStorageShow be f) <+> parens (ppStorageShow be t)
ppStorageShow be (SValue a i v)
  = text "SValue" <+> pl a <+> pl i <+> parens (bePrettyLV be v)
  where pl = bePrettyLit be
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
      | il == beTrue be = whenInRange a $ item doc a (bePrettyLV be v)
      | il == beFalse be = whenInRange a $ item doc a (text "uninitialized")
      | otherwise = whenInRange a
                  $ item doc a
                  $ bePrettyLV be v <+> parens (text "allocated:" <+> pl al <> comma
                                            <+> text "initialized:" <+> pl il)
    impl a (Just doc) (SDefine sym)  = whenInRange a $ item doc a $ LLVM.ppSymbol sym
    impl a (Just doc) (SBlock s l)   = whenInRange a
                                       $ item doc a
                                         $ LLVM.ppSymbol s
                                           <> char '/'
                                           <> LLVM.ppLabel l
    impl _ (Just doc) SUnallocated   = doc
    item doc addr desc               = doc $+$ text (showHex addr "") <> colon <+> desc
    pl = bePrettyLit be
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
        impl (SValue ax ix vx) SUnallocated = (\az -> SValue az ix vx) <$> beAnd be c ax
        impl SUnallocated (SValue ay iy vy) = (\az -> SValue az iy vy) <$> beAnd be (beNeg be c) ay
        impl b@SBranch{} SUnallocated = impl b (SBranch SUnallocated SUnallocated)
        impl SUnallocated b@SBranch{} = impl (SBranch SUnallocated SUnallocated) b
        impl a b = do
          dbugM $ "mergeStorage failure case: a = " ++ show (ppStorageShow be a)
          dbugM $ "mergeStorage failure case: b = " ++ show (ppStorageShow be b)
          bmError "Attempt to merge incompatible valid addresses."

-- | @loadBytes be mem ptr size@ returns term representing all the bits with given size.
loadBytes :: (Eq l, LV.Storable l)
          => BitEngine l
          -> (LV.Vector l -> IO (l, LV.Vector l))
          -> BitTerm l
          -> Integer
          -> IO (l, LV.Vector l)
loadBytes be byteLoader (BitTerm ptr) sz = impl [] (beTrue be) sz
  where impl l c 0 = return (c, LV.concat l)
        impl l c i = do
          (bc, bv) <- byteLoader =<< beAddIntConstant be ptr (i-1)
          c' <- beAnd be c bc
          impl (bv:l) c' (i-1)

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
           -> LV.Vector l    -- ^ Value to store
           -> LV.Vector l    -- ^ Address to store value in
           -> IO (l, Storage l) -- ^ Storage with value written and condition under which it is true.
storeBytes be mem value ptr = impl 0 (beTrue be) mem
  where bv = sliceIntoBytes value
        impl i c m
          | i == byteSize value = return (c,m)
          | otherwise = do
            p <- beAddIntConstant be ptr (toInteger i)
            c' <- beAnd be c =<< storeByteCond be m p
            m' <- storeByte be m (bv V.! i) p
            impl (i+1) c' m'

loadDef :: Storage l -> Int -> Addr -> Maybe LLVM.Symbol
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
  , bmFreeList :: V.Vector [Addr]
    -- | Frames on stack.
  , bmStackFrames :: [Integer]
    -- | Maps (def,block) pairs to associated address.
  , bmBasicBlockMap :: BasicBlockMap l
  }

-- | Returns true if stack addresses increase as more elements are pushed on
-- stack.
bmStackGrowsUp :: BitMemory l -> Bool
bmStackGrowsUp bm = bmStackAddr bm <= bmStackEnd bm

bmDump :: (Eq l, LV.Storable l)
  => BitEngine l -> Bool -> BitMemory l -> Maybe [Range Addr] -> IO ()
bmDump be sparse bm mranges = do
  banners $ render $
    text "Memory Model Dump"
    $+$ text "Stack Range:" <+> text (h $ bmStackAddr bm) <> comma <+> text (h $ bmStackEnd bm)
    $+$ text "Code Range:"  <+> text (h $ bmCodeAddr bm) <> comma <+> text (h $ bmCodeEnd bm)
    $+$ text "Data Range:"  <+> text (h $ bmDataAddr bm) <> comma <+> text (h $ bmDataEnd bm)
    $+$ text "Frame pointers:" <+> text (show (bmStackFrames bm))
    $+$ text "Storage:"
    $+$ (if sparse then ppStorage mranges else ppStorageShow) be (bmStorage bm)
  where
    h s = showHex s ""

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

bmMerge :: (Eq l, LV.Storable l)
        => BitEngine l
        -> BitTerm l -> BitMemory l -> BitMemory l -> IO (BitMemory l)
bmMerge be (BitTerm c) m m' = assert (LV.length c == 1) $ do
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
  unless (bmBasicBlockMap m == bmBasicBlockMap m') $
    fail "internal: Attempt to merge memories with different block addresses."
  unless (bmDataAddr m == bmDataAddr m') $
    fail "Attempt to merge memories with different data segment addresses."
  unless (bmDataEnd m == bmDataEnd m') $
    fail "internal: Attempt to merge memories with different data segment endpoints."
  newStorage <- mergeStorage be (c LV.! 0) (bmStorage m) (bmStorage m')
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
      (c,newStorage) <- storeBytes be mem bytes ptrv
      assert (c == beTrue be) $
        return $ Just ( BitTerm ptrv
                      , m { bmStorage = newStorage, bmDataAddr = newDataAddr })
  where
    dataAddr    = bmDataAddr m
    newDataAddr = dataAddr + toInteger (byteSize bytes)

bmAddDefine :: (Eq l, LV.Storable l)
            => BitEngine l -- ^ Bit engine for literals.
            -> Int -- ^ Width of pointers
            -> BitMemory l -- ^ Memory
            -> LLVM.Symbol -- ^ Definition
            -> V.Vector LLVM.BlockLabel -- ^ Labels for blocks
            -> Maybe (BitTerm l, BitMemory l)
bmAddDefine be ptrWidth m def blocks
    | newCodeAddr > bmCodeEnd m
    = Nothing
    | otherwise
    = Just ( BitTerm (beVectorFromInt be ptrWidth codeAddr)
           , m { bmStorage = newStorage
               , bmCodeAddr = newCodeAddr
               , bmBasicBlockMap = newBBMap
               }
           )
  where newSpaceReq = 1 + toInteger (V.length blocks)
        codeAddr = bmCodeAddr m
        newCodeAddr = codeAddr + newSpaceReq
        updateAddr a | a == codeAddr = SDefine def
                     | otherwise     = SBlock def (blocks V.! fromInteger (a - codeAddr - 1))
        newStorage = setBytes ptrWidth codeAddr newCodeAddr updateAddr (bmStorage m)
        newBBMap = addBlockLabels ptrWidth be def blocks (codeAddr + 1) (bmBasicBlockMap m)

-- | Return symbol as given address in memory.
bmLookupDefine :: (Eq l, LV.Storable l)
               => BitEngine l
               -> BitMemory l
               -> BitTerm l
               -> LookupDefineResult
bmLookupDefine be m (BitTerm a) = do
  case beVectorToMaybeInt be a of
    Nothing -> Indeterminate
    Just (w, v) ->
      case loadDef (bmStorage m) w v of
        Nothing -> Invalid
        Just d -> Result d

bmStackAlloca :: (Eq l, LV.Storable l)
              => BitEngine l
              -> Int       -- ^ Width of pointer in bits.
              -> BitMemory l
              -> Integer   -- ^ Element size
              -> BitTerm l -- ^ Number of elements
              -> Int       -- ^ Alignment constraint
              -> StackAllocaResult (BitTerm l) (BitMemory l)
bmStackAlloca be ptrWidth bm eltSize (BitTerm cntVector) a =
  case beVectorToMaybeInt be cntVector of
    Nothing -> SASymbolicCountUnsupported
    Just (_,cnt) ->
      let mkRes c res endAddr newAddr =
            let newStorage = uninitRegion be ptrWidth res endAddr (bmStorage bm)
             in SAResult (termFromLit (beLitFromBool be c))
                         (BitTerm (beVectorFromInt be ptrWidth res))
                         bm { bmStorage = newStorage, bmStackAddr = newAddr }
          -- Get new bit memory.
          r | bmStackGrowsUp bm =
                let alignedAddr  = alignUp (bmStackAddr bm) a
                    newStackAddr = alignedAddr + eltSize * cnt
                 in mkRes (newStackAddr <= bmStackEnd bm)
                          alignedAddr
                          newStackAddr
                          newStackAddr
            | otherwise =
                let sz = eltSize * cnt
                    alignedAddr = alignDn (bmStackAddr bm - sz) a
                 in mkRes (alignedAddr >= bmStackEnd bm)
                          alignedAddr
                          (alignedAddr + sz)
                          alignedAddr
       in r

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
            -> Integer   -- ^ Element size
            -> BitTerm l -- ^ Number of elements
            -> Int       -- ^ Alignment constraint
            -> HeapAllocResult (BitTerm l) (BitMemory l)
bmHeapAlloc be ptrWidth bm eltSize (BitTerm cntVector) a =
  case beVectorToMaybeInt be cntVector of
    Nothing -> HASymbolicCountUnsupported
    Just (_, cnt) ->
      let -- Get number requested.
          -- @nm x y@ computes the next multiple m of y s.t. m >= y
          nm x y = ((y + x - 1) `div` x) * x
          -- Pad up to the end of the aligned region; we do this because any
          -- global data that gets copied into this space will be padded to this
          -- size by LLVM.
          sz = eltSize * cnt
          padBytes = nm (2 ^ a :: Integer) sz - sz
          pwr = blockPower (sz + padBytes)
          size = 2 ^ pwr
          mres = allocBlock (bmFreeList bm) pwr
          false = BitTerm $ beVectorFromInt be 1 0
          true = BitTerm $ beVectorFromInt be 1 1
          zeroTerm = BitTerm (beVectorFromInt be (LV.length cntVector) 0) in
      case mres of
        Just (freeList, addr) ->
          let endAddr = addr + size
              addrTerm = BitTerm $ beVectorFromInt be ptrWidth addr
              newStorage = uninitRegion be ptrWidth addr endAddr (bmStorage bm) in
          HAResult true
                   addrTerm
                   bm { bmFreeList = freeList
                      , bmStorage = newStorage
                      }
        Nothing -> HAResult false zeroTerm bm

bmMemCopy :: (Eq l, LV.Storable l)
          => BitEngine l -- ^ Bit engine for literals
          -> BitMemory l
          -> BitTerm l   -- ^ Destination pointer
          -> BitTerm l   -- ^ Source pointer
          -> BitTerm l   -- ^ Length value
          -> BitTerm l   -- ^ Alignment in bytes
          -> IO (BitTerm l, BitMemory l)
bmMemCopy be m (BitTerm dst) src (BitTerm len0) (BitTerm _align) = do
  -- TODO: Alignment and overlap checks?
  (cr, bytes) <- loadBytes be (bmLoadByte be m) src len
  (cw, newStorage) <- storeBytes be (bmStorage m) bytes dst
  c <- beAnd be cr cw
  return (termFromLit c, m { bmStorage = newStorage })
  where
    len = case beVectorToMaybeInt be len0 of
            Nothing    -> bmError $ "Symbolic memcpy len not supported"
            Just (_,x) -> x

-- | Memory model for explicit buddy allocation scheme.
buddyMemModel :: (Eq l, LV.Storable l)
              => LLVMContext
              -> BitEngine l
              -> BitBlastMemModel (BitMemory l) l
buddyMemModel lc be = mm
 where ptrWidth = llvmAddrWidthBits lc
       mm = MemModel {
                mmDump = bmDump be
              , mmLoad = \m ptr sz -> do
                 (c,v) <- loadBytes be (bmLoadByte be m) ptr sz
                 return (termFromLit c,v)
              , mmStore = \m bytes (BitTerm ptr) -> do
                 (c,newStorage) <- storeBytes be (bmStorage m) bytes ptr
                 return (termFromLit c, m { bmStorage = newStorage })
              , mmMux = bmMerge be
              , mmInitGlobal = bmInitGlobalBytes be ptrWidth
              , mmAddDefine = return `c3` bmAddDefine be ptrWidth
              , mmBlockAddress = blockAddress . bmBasicBlockMap
              , mmLookupDefine = bmLookupDefine be
              , mmStackAlloca = return `c4` bmStackAlloca be ptrWidth
              , mmStackPush = \mem -> return (termFromLit (beTrue be), bmStackPush mem)
              , mmStackPop = return . bmStackPop (llvmAddrWidthBits lc)
              , mmHeapAlloc = return `c4` bmHeapAlloc be ptrWidth
              , mmMemCopy = bmMemCopy be
              }

buddyInitMemory :: MemGeom -> BitMemory l
buddyInitMemory mg =
  case mgSanityCheck mg of
    Just msg -> bmError ("internal: " ++ msg)
    Nothing ->
      BitMemory { bmStorage = SUnallocated
                , bmBasicBlockMap = Map.empty
                , bmStackAddr = start (mgStack mg)
                , bmStackEnd = end (mgStack mg)
                , bmStackFrames = []
                , bmCodeAddr = start (mgCode mg)
                , bmCodeEnd = end (mgCode mg)
                , bmDataAddr = start (mgData mg)
                , bmDataEnd = end (mgData mg)
                , bmFreeList = initFreeList (start (mgHeap mg)) (end (mgHeap mg))
                }

createBuddyMemModel :: (Eq l, LV.Storable l)
                    => LLVMContext
                    -> BitEngine l
                    -> MemGeom
                    -> IO ( BitBlastMemModel (BitMemory l) l
                          , BitMemory l)
createBuddyMemModel lc be mg =
  return (buddyMemModel lc be, buddyInitMemory mg)

-- DagMemory {{{1

data DMDag l = DMDag {
    dmAppNodeMap :: !(Map (DMApp l) (DagMemory l))
  , dmNodeCount :: !Int
  }

data DagMemory l = DagMemory {
    dmNodeIdx :: Int
  , dmNodeApp :: DMApp l
    -- | Current address for stack.
  , dmStack :: LV.Vector l
    -- | Frames on stack.
  , dmStackFrames :: [LV.Vector l]
    -- | Address for next value in code segment.
  , dmCode :: Addr
     -- | Maps concrete addresses to associated symbol.
  , dmDefineMap :: Map Addr LLVM.Symbol
     -- | Maps basic blocks to associated address.
  , dmBasicBlockMap :: BasicBlockMap l
    -- | Address for next value in data segment.
  , dmData :: Addr
    -- | Address for next value in heap.
  , dmHeap :: LV.Vector l
    -- Cache mapping ranges to literal indicating if range is allocated.
  , dmAllocCache :: IORef (Map (Range (LV.Vector l)) l)
    -- Cache mapping ranges to literal indicating if range is initialized.
  , dmInitCache :: IORef (Map (Range (LV.Vector l)) l)
    -- | Maps byte addresses to (allocated,initialized,value) tuple.
  , dmLoadCache :: IORef (Map (LV.Vector l) (LV.Vector l))
  }

instance Eq (DagMemory l) where
  x == y = dmNodeIdx x == dmNodeIdx y

instance Ord (DagMemory l) where
  x `compare` y = dmNodeIdx x `compare` dmNodeIdx y

-- Query operations:
--  Load
--  Allocat

type Byte l = LV.Vector l

type SymAddr l = LV.Vector l

data DMApp l
   = DMInitial
     -- | @DMAddDefine s bl p@ denotes memory obtained from adding definition to
     -- memory.
   | DMAddDefine LLVM.Symbol (V.Vector LLVM.BlockLabel) (DagMemory l)
     -- | @DMAlloc base end prev@ denotes the memory obtained by
     -- allocating bytes in @[base,end)@ to @prev@.
   | DMAlloc (SymAddr l) (SymAddr l) (DagMemory l)
   | DMStackPush (DagMemory l)
     -- | @DMStackPop base end prev@ denotes the memory obtained by?b
     -- deallocating bytes in @[base,end)@ to @prev@.  The range may
     -- be empty.
   | DMStackPop (SymAddr l) (SymAddr l) (DagMemory l)
     -- | @DMStore addr end valueBytes prev@.  The range [addr end) is
     -- non-empty.
   | DMStore (SymAddr l) (SymAddr l) (V.Vector (Byte l)) (DagMemory l)
     -- | @DMMemCopy dest destEnd src prev@.  The range [dest destEnd)
     -- may be empty.
   | DMMemCopy (SymAddr l) (SymAddr l) (SymAddr l) (DagMemory l)
   | DMMerge l (DagMemory l) (DagMemory l)
  deriving (Eq, Ord)

prettyMemIdx :: DagMemory l -> Doc
prettyMemIdx m = char '$' <> int (dmNodeIdx m)

bePrettyRange :: (Eq l, LV.Storable l) => BitEngine l -> Range (LV.Vector l) -> Doc
bePrettyRange be (s,e) = char '[' <> bePrettyLV be s <> comma <+> bePrettyLV be e <> char ')'

dmPrintApp :: (Eq l, LV.Storable l) => BitEngine l -> DMApp l -> Doc
dmPrintApp be app =
  case app of
    DMInitial -> text "initial"
    DMAddDefine d bl mem  -> text "define"    <+> text (show d) <+> pblocks bl <+> pm mem
    DMAlloc s e mem       -> text "alloc"     <+> pr (s,e) <+> pm mem
    DMStackPush mem       -> text "stackPush" <+> pm mem
    DMStackPop s e mem    -> text "stackPop"  <+> pr (s,e) <+> pm mem
    DMStore s e bytes mem -> text "store"     <+> pr (s,e) <+> text ":=" <+> pbytes bytes <+> pm mem
    DMMemCopy s e src mem -> text "memCopy"   <+> pr (s,e) <+> text ":= *" <> paddr src <+> pm mem
    DMMerge c t f         -> text "merge"     <+> pl c <+> char '?' <+> pm t <+> char ':' <> pm f
 where pr = bePrettyRange be
       pl = bePrettyLit be
       pm m = prettyMemIdx m
       pblocks = brackets . hsep . punctuate comma . V.toList . V.map (text . show)
       pbytes  = brackets . hsep . punctuate comma . V.toList . V.map (bePrettyLV be)
       paddr   = bePrettyLV be

dmMemArgs :: DMApp l -> [DagMemory l]
dmMemArgs DMInitial = []
dmMemArgs (DMAddDefine _ _ m) = [m]
dmMemArgs (DMAlloc _ _ m) = [m]
dmMemArgs (DMStackPush m) = [m]
dmMemArgs (DMStackPop _ _ m) = [m]
dmMemArgs (DMStore _ _ _ m) = [m]
dmMemArgs (DMMemCopy _ _ _ m) = [m]
dmMemArgs (DMMerge _ t f) = [t, f]

lfp :: Ord a => (a -> [a]) -> Set a -> Set a
lfp fn initSet = impl initSet (Set.toList initSet)
  where impl s [] = s
        impl s (h:r) = impl (foldl' (flip Set.insert) s new) (new ++ r)
          where new = filter (flip Set.notMember s) (fn h)

dmDump :: (Eq l, LV.Storable l)
       => BitEngine l -> Bool -> DagMemory l -> Maybe [Range Addr] -> IO ()
dmDump be _ mem _ = do
  -- Steps: Build list of memory addresses to print out.
  let allNodes = lfp (dmMemArgs . dmNodeApp) (Set.singleton mem)
  forM_ (Set.toList allNodes) $ \m -> do
    putStrLn $ render $ prettyMemIdx m <> colon <+> dmPrintApp be (dmNodeApp m)

-- | Returns predicate indicating if memory can be writen to.
dmIsAllocated :: (LV.Storable l, Ord l)
             => BitEngine l
             -> DagMemory l
             -> Range (LV.Vector l) -- ^ Bytes to check if writable (assumed to be non-empty)
             -> IO l
dmIsAllocated be = parseMem
  where eq = beEqVector be
        (|||) = beOrM be
        leq = beUnsignedLeq be
        parseMem mem range = do
          let ref = dmAllocCache mem
          let cache mp = mp >>= \p -> modifyIORef ref (Map.insert range p) >> return p
          allocCache <- readIORef ref
          case Map.lookup range allocCache of
            Just p -> return p
            Nothing -> do
              case dmNodeApp mem of
                DMInitial           -> cache $ beEqVector be (start range) (end range)
                DMAlloc s e pm      -> cache $ beRangeCovered be (parseMem pm) range (s,e)
                DMStackPop s e pm   -> cache $
                  beAndM be
                         ((end range `leq` s) ||| (e `leq` start range) ||| (s `eq` e))
                         (parseMem pm range)
                DMMerge dmc tm fm   -> cache $ beIteM be dmc (parseMem tm range) (parseMem fm range)
                DMStackPush pm      -> parseMem pm range
                DMAddDefine _ _ pm  -> parseMem pm range
                DMStore _ _ _ pm    -> parseMem pm range
                DMMemCopy _ _ _ pm  -> parseMem pm range

dmIsInitialized :: (Ord l, LV.Storable l)
                => BitEngine l
                -> DagMemory l
                -> Range (LV.Vector l)
                -> IO l
dmIsInitialized be = parseMem
  where eq = beEqVector be
        (|||) = beOrM be
        leq = beUnsignedLeq be
        parseMem mem range = do
          let ref = dmInitCache mem
          let cache mp = mp >>= \p -> modifyIORef ref (Map.insert range p) >> return p
          initCache <- readIORef ref
          case Map.lookup range initCache of
            Just p -> return p
            Nothing -> do
              case dmNodeApp mem of
                DMInitial -> return (beFalse be)
                DMStackPop s e pm -> cache $
                  beAndM be
                         ((end range `leq` s) ||| (e `leq` start range) ||| (s `eq` e))
                         (parseMem pm range)
                DMStore s e _ pm   -> cache $ beRangeCovered be (parseMem pm) range (s,e)
                DMMemCopy d e _ pm -> cache $ beRangeCovered be (parseMem pm) range (d,e)
                DMMerge dmc t f    -> cache $ beIteM be dmc (parseMem t range) (parseMem f range)
                DMStackPush pm     -> parseMem pm range
                DMAddDefine _ _ pm -> parseMem pm range
                DMAlloc _ _ pm     -> parseMem pm range

dmLoadByte :: (Ord l, LV.Storable l)
           => BitEngine l
           -> DagMemory l
           -> LV.Vector l
           -> IO (LV.Vector l) -- ^ Verification condition and value.
dmLoadByte be = parseMem
  where invalidResult = beDontCareByte be
        -- Perform mux of (Lit,LitVector) pairs
        parseMem n ptr = do
          let ref = dmLoadCache n
          let cache mp = mp >>= \p -> modifyIORef ref (Map.insert ptr p) >> return p
          loadCache <- readIORef ref
          case Map.lookup ptr loadCache of
            Just res -> return res
            Nothing -> do
              case dmNodeApp n of
                DMInitial -> return invalidResult
                DMStore s e bytes pm -> cache $ do
                  inRange <- beInRange be ptr (s, e)
                  beIteVector be
                              inRange
                              (do vidx <- beSubInt be ptr s
                                  beMuxGeneral (beIteVector be)
                                               (toInteger (V.length bytes - 1))
                                               vidx
                                               (return . (bytes V.!) . fromInteger))
                              (parseMem pm ptr)
                DMMemCopy d e src pm -> cache $ do
                  inRange <- beInRange be ptr (d,e)
                  let prevPtr = beAddInt be src =<< beSubInt be ptr d
                  parseMem pm =<< beIteVector be inRange prevPtr (return ptr)
                DMMerge dmc t f    -> cache $ beIteVector be dmc (parseMem t ptr) (parseMem f ptr)
                DMAddDefine _ _ pm -> parseMem pm ptr
                DMAlloc _ _ pm     -> parseMem pm ptr
                DMStackPush pm     -> parseMem pm ptr
                DMStackPop _ _ pm  -> parseMem pm ptr

-- | @loadBytes be mem ptr size@ returns term representing all the bits with given size.
dmLoadBytes :: (Ord l, LV.Storable l)
            => BitEngine l
            -> DagMemory l
            -> BitTerm l
            -> Integer
            -> IO (BitTerm l, LV.Vector l)
dmLoadBytes be _ (BitTerm _) 0 = return (termFromLit (beTrue be), LV.empty)
dmLoadBytes be mem (BitTerm ptr) sz = do
  ptrEnd <- beAddIntConstant be ptr sz
  c <- dmIsInitialized be mem (ptr,ptrEnd)
  r <- forM [1..sz] $ \i -> do
         dmLoadByte be mem =<< beAddIntConstant be ptr (i-1)
  return (termFromLit c, LV.concat r)

-- | Returns node with given app, creating it if necessary.  The function passed
-- in gives the opportunity to modify the node before it is cached.
dmGetMem :: (Ord l, LV.Storable l)
         => IORef (DMDag l)
         -> DagMemory l
         -> DMApp l
         -> (DagMemory l -> IO (DagMemory l))
         -> IO (DagMemory l)
dmGetMem ref base app nodeFn = do
  dg <- readIORef ref
  case Map.lookup app (dmAppNodeMap dg) of
    Just r -> return r
    Nothing -> do
      let c = dmNodeCount dg
      r <- nodeFn base { dmNodeIdx = c, dmNodeApp = app }
      let dg' = DMDag { dmAppNodeMap = Map.insert app r (dmAppNodeMap dg)
                      , dmNodeCount = 1 + c }
      dg' `seq` writeIORef ref dg'
      return r

-- | Store bytes in memory
dmStoreBytes :: (Ord l, LV.Storable l)
             => BitEngine l -> IORef (DMDag l)
             -> DagMemory l -> LV.Vector l -> BitTerm l -> IO (BitTerm l, DagMemory l)
dmStoreBytes be ref mem flatBytes (BitTerm ptr)
  | V.length bytes == 0 = return (termFromLit (beTrue be), mem)
  | otherwise = do
    ptrEnd <- beAddIntConstant be ptr (toInteger (V.length bytes))
    c <- dmIsAllocated be mem (ptr,ptrEnd)
    m <- dmGetMem ref mem (DMStore ptr ptrEnd bytes mem) $ \m -> do
           initRef <- newIORef Map.empty
           loadRef <- newIORef Map.empty
           return m { dmInitCache = initRef
                    , dmLoadCache = loadRef
                    }
    return (termFromLit c, m)
 where bytes = sliceIntoBytes flatBytes

dmMux :: (Ord l, LV.Storable l)
      => BitEngine l -> IORef (DMDag l)
      -> BitTerm l -> DagMemory l -> DagMemory l -> IO (DagMemory l)
dmMux be ref (BitTerm c) t f = assert (LV.length c == 1) $ do
  unless (dmBasicBlockMap t == dmBasicBlockMap f) $
    fail "internal: Attempt to merge memories with different block addresses."
  unless (length (dmStackFrames t) == length (dmStackFrames f)) $
    fail "internal: Attempt to merge memories with different numbers of stacks pushed."
  unless (dmCode t == dmCode f) $
    fail "internal: Attempt to merge memories with different code addresses."
  unless (dmData t == dmData f) $
    fail "Attempt to merge memories with different data segment addresses."
  let mux = LV.zipWithM (beIte be (c LV.! 0))
  dmGetMem ref t (DMMerge (c LV.! 0) t f) $ \m -> do
    newStack <- mux (dmStack t) (dmStack f)
    newStackFrames <- zipWithM mux (dmStackFrames t) (dmStackFrames f)
    newHeap <- mux (dmHeap t) (dmHeap f)
    allocRef <- newIORef Map.empty
    initRef <- newIORef Map.empty
    loadRef <- newIORef Map.empty
    return m { dmStack = newStack
             , dmStackFrames = newStackFrames
             , dmHeap = newHeap
             , dmAllocCache = allocRef
             , dmInitCache = initRef
             , dmLoadCache = loadRef
             }

-- | Initialize global data memory.
dmInitGlobal :: (Ord l, LV.Storable l)
             => BitEngine l
             -> Int  -- ^ Width of pointer
             -> Addr -- ^ End of data region
             -> IORef (DMDag l)
             -> DagMemory l -> LV.Vector l -> IO (Maybe (BitTerm l, DagMemory l))
dmInitGlobal be ptrWidth dataEnd ref mem flatBytes
  | byteCount == 0 = return $ Just (BitTerm ptr, mem)
  | dataEnd - dmData mem < byteCount = return Nothing
  | otherwise = do
      -- Allocate space in data segment
      mem1 <- dmGetMem ref mem (DMAlloc ptr ptrEnd mem) $ \m -> do
        allocRef <- newIORef Map.empty
        return m { dmData = nextData
                 , dmAllocCache = allocRef }
      -- Store bytes
      mem2 <- dmGetMem ref mem1 (DMStore ptr ptrEnd bytes mem) $ \m -> do
        initRef <- newIORef Map.empty
        loadRef <- newIORef Map.empty
        return m { dmInitCache = initRef
                 , dmLoadCache = loadRef
                 }
      -- Return result
      return $ Just (BitTerm ptr, mem2)
  where bytes = sliceIntoBytes flatBytes
        byteCount = toInteger (V.length bytes)
        nextData = dmData mem + byteCount
        ptr = beVectorFromInt be ptrWidth (dmData mem)
        ptrEnd = beVectorFromInt be ptrWidth nextData

dmAddDefine :: (Ord l, LV.Storable l)
            => BitEngine l
            -> Int -- ^ width of pointers
            -> Addr -- ^ code end
            -> IORef (DMDag l)
            -> DagMemory l -- ^ Memory
            -> LLVM.Symbol -- ^ Definition
            -> V.Vector LLVM.BlockLabel -- ^ Labels for blocks
            -> IO (Maybe (BitTerm l, DagMemory l))
dmAddDefine be ptrWidth codeEnd ref mem def blocks
   -- TODO: Alignment and overlap checks?
  | remaining >= bytesReq = do
      -- Get new memory
      m <- dmGetMem ref mem (DMAddDefine def blocks mem) $ \m ->
        return m { dmCode = ptr + bytesReq
                 , dmDefineMap = Map.insert ptr def (dmDefineMap m)
                 , dmBasicBlockMap = addBlockLabels ptrWidth be def blocks ptr (dmBasicBlockMap m)
                 }
      -- Return result
      return $ Just (BitTerm (beVectorFromInt be ptrWidth ptr), m)
  | otherwise = return Nothing
  where ptr = dmCode mem
        remaining = codeEnd - ptr
        bytesReq = 1 + toInteger (V.length blocks)

dmLookupDefine :: (Eq l, LV.Storable l)
               => BitEngine l
               -> DagMemory l
               -> BitTerm l
               -> LookupDefineResult
dmLookupDefine be mem (BitTerm a) = do
  case beVectorToMaybeInt be a of
    Nothing -> Indeterminate
    Just (_w, v) ->
      case Map.lookup v (dmDefineMap mem) of
        Nothing -> Invalid
        Just d -> Result d

dmStackAlloca :: (Ord l, LV.Storable l)
              => BitEngine l -- ^ Bit engine
              -> Int -- ^ Width of pointer in bits
              -> Bool -- ^ Flag indicates stack grows up
              -> LV.Vector l -- ^ End of stack
              -> IORef (DMDag l)
              -> DagMemory l
              -> Integer
              -> BitTerm l
              -> Int
              -> IO (StackAllocaResult (BitTerm l) (DagMemory l))
dmStackAlloca be ptrWidth stackGrowsUp stackEnd ref mem eltSize (BitTerm eltCount) align = do
  --TODO: Check alignment and overflow
  let stack = dmStack mem
      eltCountSize = LV.length eltCount
  newSizeExt <- beFullMulIntConstant be eltCount (ptrWidth,eltSize)
  let ?be = be
  let extVector = (LV.++ LV.replicate eltCountSize lFalse)
  let truncVector = LV.take ptrWidth
  let stackEndExt = extVector stackEnd
  let (.&&) = lAnd
  let (.<=) = lUnsignedLeq
  let (c,truncVector -> newStack)
        | stackGrowsUp = 
           let (ac, aStack) = lAlignUp stack align
               (ao, newStackExt) = extVector aStack `lFullAdd` newSizeExt
            in ( lNeg ac .&& lNeg ao .&& (newStackExt .<= stackEndExt)
               , newStackExt)
        | otherwise = 
           let aStack = lAlignDn stack align
               (ab, newStackExt) = extVector aStack `lFullSub` newSizeExt
            in ( lNeg ab .&& (stackEndExt .<= newStackExt)
               , newStackExt)
  let mkMem app =
        dmGetMem ref mem app $ \m -> do
          allocRef <- newIORef Map.empty
          return m { dmStack = newStack, dmAllocCache = allocRef }
  case () of
   _ | c == lFalse -> do
        return (SAResult (termFromLit c) (BitTerm stack) mem)
     | stackGrowsUp -> do
        m <- mkMem (DMAlloc stack newStack mem)
        return (SAResult (termFromLit c) (BitTerm stack) m)
     | otherwise -> do
        m <- mkMem (DMAlloc newStack stack mem)
        return (SAResult (termFromLit c) (BitTerm newStack) m)

-- | Push stack frame to memory.
-- N.B. To avoid empty deallocations in stack pop, we always add a byte to
-- the stack when pushing.
dmStackPushFrame :: (LV.Storable l, Ord l)
                 => BitEngine l -> IORef (DMDag l)
                 -> DagMemory l -> IO (BitTerm l, DagMemory l)
dmStackPushFrame be ref mem = do
  r <- dmGetMem ref mem (DMStackPush mem) $ \m ->
         return m { dmStackFrames = dmStack mem : dmStackFrames mem }
  return (termFromLit (beTrue be), r)

-- | Pop stack frame in memory and invalidate old addresses.
dmStackPopFrame :: (LV.Storable l, Ord l)
                => Bool -- ^ Flag indicating if stack should grow up in memory.
                -> IORef (DMDag l)
                -> DagMemory l
                -> IO (DagMemory l)
dmStackPopFrame stackGrowsUp ref mem =
  case dmStackFrames mem of
   [] -> bmError "internal: Attempted to pop stack frame from memory when no stack frames have been pushed."
   f : fl -> do
     let (ptr,ptrEnd) = if stackGrowsUp then (f,dmStack mem) else (dmStack mem,f)
     dmGetMem ref mem (DMStackPop ptr ptrEnd mem) $ \m -> do
       allocRef <- newIORef Map.empty
       initRef <- newIORef Map.empty
       return m { dmAllocCache = allocRef
                , dmInitCache = initRef
                , dmStack = f
                , dmStackFrames = fl
                }

dmHeapAlloc :: (Ord l, LV.Storable l)
            => BitEngine l -- ^ Bit engine
            -> Int -- ^ Width of pointer in bits
            -> LV.Vector l -- ^ End of heap
            -> IORef (DMDag l)
            -> DagMemory l
            -> Integer
            -> BitTerm l
            -> Int
            -> IO (HeapAllocResult (BitTerm l) (DagMemory l))
dmHeapAlloc be ptrWidth heapEnd ref mem eltSize (BitTerm eltCount) _align = do
  let heap = dmHeap mem
      eltCountSize = LV.length eltCount
  newSizeExt <- beFullMulIntConstant be eltCount (ptrWidth,eltSize)
  spaceRem <- beSubInt be heapEnd heap
  let spaceRemExt = spaceRem LV.++ LV.replicate eltCountSize (beFalse be)
  c <- beUnsignedLeq be newSizeExt spaceRemExt
  case () of
   _ | c == beFalse be -> do
        return (HAResult (termFromLit c) (BitTerm heap) mem)
     | otherwise -> do
        let newSize = LV.take ptrWidth newSizeExt
        newHeap <- beAddInt be heap newSize
        m <- dmGetMem ref mem (DMAlloc heap newHeap mem) $ \m -> do
               allocRef <- newIORef Map.empty
               return m { dmHeap = newHeap, dmAllocCache = allocRef }
        return (HAResult (termFromLit c) (BitTerm heap) m)

-- | Store bytes in memory
dmMemCopy :: (Ord l, LV.Storable l)
          => BitEngine l
          -> Int -- ^ Pointer width
          -> IORef (DMDag l)
          -> DagMemory l
          -> BitTerm l   -- ^ Destination pointer
          -> BitTerm l   -- ^ Source pointer
          -> BitTerm l   -- ^ Length value
          -> BitTerm l   -- ^ Alignment in bytes
          -> IO (BitTerm l, DagMemory l)
dmMemCopy be ptrWidth ref mem (BitTerm dest) (BitTerm src) (BitTerm l) (BitTerm _)
 | LV.length src /= ptrWidth = bmError "internal: src pointer size does not match pointer width."
 | LV.length dest /= ptrWidth = bmError "internal: dest pointer size does not match pointer width"
 | otherwise = do
    let lWidth = LV.length l
    let lext | lWidth == ptrWidth = l
             | lWidth >= ptrWidth = LV.drop (lWidth - ptrWidth) l
             | otherwise = LV.replicate (ptrWidth - lWidth) (beFalse be) LV.++ l
    -- TODO: Alignment and overlap checks?
    -- Check overflows
    (srcOverflow,srcEnd) <- beFullAddInt be src lext
    (destOverflow,destEnd) <- beFullAddInt be dest lext
    let lenOverflow | lWidth >= ptrWidth = beIsNonZero be (LV.take (lWidth - ptrWidth) l)
                    | otherwise = return (beFalse be)
    let addrOverflow = beOrM be lenOverflow (beOr be srcOverflow destOverflow)
                             -- Check src is readable and dest is writable
    let memValid = beAndM be (dmIsInitialized be mem (src, srcEnd))
                             (dmIsAllocated be mem (dest, destEnd))
    c <- beOrM be (beIsZero be l)
                  (beAndM be (beNeg be <$> addrOverflow) memValid)
    -- Get new memory
    m <- dmGetMem ref mem (DMMemCopy dest destEnd src mem) $ \m -> do
        initRef <- newIORef Map.empty
        loadRef <- newIORef Map.empty
        return m { dmInitCache = initRef
                 , dmLoadCache = loadRef
                 }
    -- Return result
    return (termFromLit c,m)

createDagMemModel :: (Ord l, LV.Storable l)
                  => LLVMContext
                  -> BitEngine l
                  -> MemGeom
                  -> IO (BitBlastMemModel (DagMemory l) l, DagMemory l)
createDagMemModel lc be mg = do
  let ptrWidth = llvmAddrWidthBits lc
  let stackGrowsUp = not (decreasing (mgStack mg))
  ref <- newIORef DMDag { dmAppNodeMap = Map.empty, dmNodeCount = 1 }
  let ptrEnd range = beVectorFromInt be ptrWidth (end range)
  let mm = MemModel {
               mmLoad = dmLoadBytes be
             , mmStore = dmStoreBytes be ref
             , mmMux = dmMux be ref
             , mmInitGlobal = dmInitGlobal be ptrWidth (end (mgData mg)) ref
             , mmDump = dmDump be
             , mmAddDefine = dmAddDefine be ptrWidth (end (mgCode mg)) ref
             , mmBlockAddress = blockAddress . dmBasicBlockMap
             , mmLookupDefine = dmLookupDefine be
             , mmStackAlloca = dmStackAlloca be ptrWidth stackGrowsUp (ptrEnd (mgStack mg)) ref
             , mmStackPush = dmStackPushFrame be ref
             , mmStackPop = dmStackPopFrame stackGrowsUp ref
             , mmHeapAlloc = dmHeapAlloc be ptrWidth (ptrEnd (mgHeap mg)) ref
             , mmMemCopy = dmMemCopy be ptrWidth ref
             }
  allocRef <- newIORef Map.empty
  initRef <- newIORef Map.empty
  loadRef <- newIORef Map.empty
  let mem = DagMemory { dmNodeIdx = 0
                      , dmNodeApp = DMInitial
                      , dmStack = beVectorFromInt be ptrWidth (start (mgStack mg))
                      , dmStackFrames = []
                      , dmCode = start (mgCode mg)
                      , dmDefineMap = Map.empty
                      , dmBasicBlockMap = Map.empty
                      , dmData = start (mgData mg)
                      , dmHeap = beVectorFromInt be ptrWidth (start (mgHeap mg))
                      , dmAllocCache = allocRef
                      , dmInitCache = initRef
                      , dmLoadCache = loadRef
                      }
  return (mm, mem)

-- Aiger operations {{{1

evalAigerImpl :: (LV.Storable l, Eq l) =>
                 BitEngine l -> [Bool] -> BitTerm l
              -> IO (BitTerm l)
evalAigerImpl be inps (BitTerm t) = BitTerm <$> do
  LV.map (beLitFromBool be) <$> beEvalAigV be (LV.fromList inps) t

-- Arithmetic and logical operations {{{1

bitIte :: (LV.Storable l, Eq l) =>
          BitEngine l -> BitTerm l -> BitTerm l -> BitTerm l
       -> IO (BitTerm l)
bitIte be (BitTerm c) (BitTerm a) (BitTerm b) = BitTerm <$> do
  let zero = LV.replicate (LV.length c) (beFalse be)
  cbit <- beEqVector be c zero
  beIteVector be cbit (return b) (return a)

bitICmp :: (LV.Storable l, Eq l) =>
           BitEngine l -> LLVM.ICmpOp
        -> BitTerm l -> BitTerm l
        -> IO (BitTerm l)
bitICmp be op (BitTerm a) (BitTerm b) =
   (BitTerm . LV.singleton) <$> f be a b
  where f = case op of
              LLVM.Ieq -> beEqVector
              LLVM.Ine -> neg beEqVector
              LLVM.Iugt -> neg beUnsignedLeq
              LLVM.Iuge -> neg beUnsignedLt
              LLVM.Iult -> beUnsignedLt
              LLVM.Iule -> beUnsignedLeq
              LLVM.Isgt -> neg beSignedLeq
              LLVM.Isge -> neg beSignedLt
              LLVM.Islt -> beSignedLt
              LLVM.Isle -> beSignedLeq
        neg fn bend x y = beNeg bend <$> fn bend x y

bitBitwise :: (LV.Storable l, Eq l) =>
              BitEngine l -> LLVM.BitOp
           -> BitTerm l -> BitTerm l
           -> IO (BitTerm l)
bitBitwise be op (BitTerm a) (BitTerm b) = BitTerm <$> f be a b
  where f = case op of
              LLVM.And -> beAndInt
              LLVM.Or -> beOrInt
              LLVM.Xor -> beXorInt
              LLVM.Shl -> beShl
              LLVM.Lshr -> beUnsignedShr
              LLVM.Ashr -> beSignedShr

bitArith :: (LV.Storable l, Eq l) =>
            BitEngine l -> LLVM.ArithOp
         -> BitTerm l -> BitTerm l
         -> IO (BitTerm l)
bitArith be op (BitTerm a) (BitTerm b) = BitTerm <$> f be a b
  where f = case op of
              LLVM.Add  -> beAddInt
              LLVM.Mul  -> beMulInt
              LLVM.Sub  -> beSubInt
              LLVM.SDiv -> beQuot
              LLVM.SRem -> beRem
              LLVM.UDiv -> beQuotUnsigned
              LLVM.URem -> beRemUnsigned
              LLVM.FAdd -> noFloats
              LLVM.FSub -> noFloats
              LLVM.FMul -> noFloats
              LLVM.FDiv -> noFloats
              LLVM.FRem -> noFloats
        noFloats = bmError "floating point arithmetic not currently supported"

bitConv :: (LV.Storable l, Eq l) =>
           BitEngine l -> LLVM.ConvOp
        -> BitTerm l -> LLVM.Type
        -> IO (BitTerm l)
bitConv be op (BitTerm x) (LLVM.PrimType (LLVM.Integer (fromIntegral -> w))) =
  return $ BitTerm (f be w x)
  where f = case op of
              LLVM.Trunc -> assert (w < LV.length x) beTrunc
              LLVM.ZExt  -> assert (w > LV.length x) beZext
              LLVM.SExt  -> assert (w > LV.length x) beSext
              _ -> bmError $ "Unsupported conv op: " ++ show op
bitConv _ _ _ ty = bmError $ "Unsupported conv target type: " ++ show (LLVM.ppType ty)

bitBNot :: (LV.Storable l, Eq l) =>
           BitEngine l -> BitTerm l
        -> IO (BitTerm l)
bitBNot be (BitTerm bv) = BitTerm <$> return (LV.map (beNeg be) bv)

--  SBE Definition {{{1

newtype BitIO m l v = BitIO { liftSBEBitBlast :: IO v }
  deriving (Monad, MonadIO, Functor)

type instance SBETerm (BitIO m l)       = BitTerm l
type instance SBEClosedTerm (BitIO m l) = BitTermClosed l
type instance SBEMemory (BitIO m l)     = m

type BitBlastSBE m l = SBE (BitIO m l)

sbeBitBlast :: (S.PrettyTerm (BitTermClosed l), Eq l, LV.Storable l)
            => LLVMContext
            -> BitEngine l
            -> BitBlastMemModel m l
            -> SBE (BitIO m l)
sbeBitBlast lc be mm = sbe
  where
    sbe = SBE
          { termInt          = (return . BitTerm) `c2` beVectorFromInt be
          , freshInt         = BitIO . fmap BitTerm . beInputVector be
          , termBool         = return . BitTerm . LV.singleton . beLitFromBool be
          , termArray        = return . BitTerm . termArrayImpl
          , termDecomp       = return `c2` termDecompImpl lc be
          , applyIte         = BitIO `c3` bitIte be
          , applyICmp        = BitIO `c3` bitICmp be
          , applyBitwise     = BitIO `c3` bitBitwise be
          , applyArith       = BitIO `c3` bitArith be
          , applyConv        = BitIO `c3` bitConv be
          , applyBNot        = BitIO . bitBNot be
          , termWidth        = fromIntegral . LV.length . btVector
          , closeTerm        = BitTermClosed . (,) be
          , prettyTermD      = S.prettyTermD . closeTerm sbe
          , memDump          = BitIO `c2` mmDump mm True
          , memLoad          = BitIO `c2` loadTerm lc mm
          , memStore         = BitIO `c3` storeTerm lc be mm
          , memMerge         = BitIO `c3` mmMux mm
          , memAddDefine     = \mem d vl -> BitIO $ mmAddDefine mm mem d (V.fromList vl)
          , memInitGlobal    = \m (LLVM.Typed ty gd) ->
                                 BitIO $ mmInitGlobal mm m (termToBytes lc be ty gd)
          , codeBlockAddress = return `c3` mmBlockAddress mm
          , codeLookupDefine = return `c2` mmLookupDefine mm
          , stackAlloca = \m eltTp cnt ->
              BitIO . mmStackAlloca mm m (llvmAllocSizeOf lc eltTp) (LLVM.typedValue cnt)
          , stackPushFrame   = BitIO . mmStackPush mm
          , stackPopFrame    = BitIO . mmStackPop mm
          , heapAlloc        = \m eltTp (LLVM.typedValue -> cnt) ->
              BitIO . mmHeapAlloc mm m (llvmAllocSizeOf lc eltTp) cnt
          , memCopy          = BitIO `c5` mmMemCopy mm
          , writeAiger       = \f t -> BitIO $ beWriteAigerV be f (btVector t)
          , evalAiger        = BitIO `c2` evalAigerImpl be
          }
    termArrayImpl [] = bmError "sbeBitBlast: termArray: empty term list"
    termArrayImpl ts = foldr1 (LV.++) (map btVector ts)

termDecompImpl :: (LV.Storable l, Eq l)
               => LLVMContext
               -> BitEngine l
               -> [LLVM.Type]
               -> BitTerm l
               -> [LLVM.Typed (BitTerm l)]
termDecompImpl lc _be tys0 (BitTerm t)
  | sum (map (llvmMinBitSizeOf lc) tys0) /= fromIntegral (LV.length t)
  = error "termDecompImpl: sum of type sizes must equal bitvector length"
  | otherwise
  = unfoldr slice (t, tys0)
  where
    slice (bv, [])       = assert (LV.null bv) Nothing
    slice (bv, (ty:tys)) = Just (bt $ LV.take sz bv, (LV.drop sz bv, tys))
      where
        bt = LLVM.Typed ty . BitTerm
        sz = fromIntegral $ llvmMinBitSizeOf lc ty

-- Test code {{{1
testSBEBitBlast :: IO ()
testSBEBitBlast = do
  let lc = buildLLVMContext
             (error "no type alias resolution defined")
             []
  be <- createBitEngine
  let mm = buddyMemModel lc be
      m0 = buddyInitMemory (MemGeom (0x10,0x0) (0x0,0x0) (0x0, 0x0) (0x0,0x0))
  -- (mm,m0) <- dagMemModel lc be (0x10,0x0) (0x0,0x0) (0x0, 0x0) (0x0,0x0)
  let sbe = sbeBitBlast lc be mm
  liftSBEBitBlast $ do
    let i32 = LLVM.PrimType (LLVM.Integer 32)
    let ptr = LLVM.PtrTo
    l1 <- termInt sbe 32 1
    liftIO $ putStrLn "m0:"
    memDump sbe m0 Nothing
    SAResult _ sp m1 <- stackAlloca sbe m0 i32 (LLVM.Typed i32 l1) 1
    liftIO $ putStrLn "m1:"
    memDump sbe m1 Nothing
    liftIO $ putStrLn $ show $ beVectorToMaybeInt be (btVector sp)
    lv <- termInt sbe 32 0x12345678
    (_,m2) <- memStore sbe m1 (LLVM.Typed i32 lv) sp
    liftIO $ putStrLn "m2:"
    memDump sbe m2 Nothing
    (BitTerm lc2, BitTerm lv2) <- memLoad sbe m2 (LLVM.Typed (ptr i32) sp)
    liftIO $ putStrLn $ show $ (0x12345678 :: Integer)
    liftIO $ putStrLn $ render $ text "Load condition:" <+> bePrettyLV be lc2
    liftIO $ putStrLn $ render $ text "Load value:    " <+> bePrettyLV be lv2
    return ()

__nowarn_unused :: a
__nowarn_unused = undefined testSBEBitBlast allocBlock trace c4
