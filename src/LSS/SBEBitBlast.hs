{- |
Module           : $Header$
Description      : A symbolic backend that bitblasts
Stability        : provisional
Point-of-contact : atomb, jhendrix
-}

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , sbeBitBlastMem
  , liftSBEBitBlast
  , buddyMemModel
  , dagMemModel
  -- for testing only
  , BitMemory
  , BitIO
  , bmDataAddr
  , bmDataEnd
  ) where

import           Control.Applicative       ((<$>))
import           Control.Exception         (assert)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bits
import           Data.Int
import           Data.IORef
import           Data.LLVM.Memory
import           Data.List
import           Data.Map                  (Map)
import           Debug.Trace
import           LSS.Execution.Utils
import           LSS.SBEInterface
import           Numeric                   (showHex)
import           Text.PrettyPrint.HughesPJ
import           Verinf.Symbolic.Common    (ConstantProjection(..),
                                            createBitEngine,
                                            CValue(..))
import           Verinf.Symbolic.Lit
import qualified Data.Map                  as Map
import qualified Data.Vector               as V
import qualified Data.Vector.Storable      as LV
import qualified Text.LLVM.AST             as LLVM
import qualified Verinf.Symbolic           as S

c2 :: (r -> s) -> (a -> b -> r) -> a -> b -> s
g `c2` f = \x y -> g (f x y)

c3 :: (r -> s) -> (a -> b -> c -> r) -> a -> b -> c -> s
g `c3` f = \x y z -> g (f x y z)

c4 :: (r -> s) -> (a -> b -> c -> d -> r) -> a -> b -> c -> d -> s
g `c4` f = \w x y z -> g (f w x y z)

c5 :: (r -> s) -> (a -> b -> c -> d -> e -> r) -> a -> b -> c -> d -> e -> s
g `c5` f = \v w x y z -> g (f v w x y z)

-- | Dont care bit in bit engine.
-- TODO: Figure out if this is a useful primitive to add to bit engine (e.g., does abc support ti).
beDontCare :: BitEngine l -> l
beDontCare be = beFalse be

beIteM :: Eq l => BitEngine l -> l -> IO l -> IO l -> IO l
beIteM be c tm fm 
  | c == beTrue be = tm
  | c == beFalse be = fm
  | otherwise = tm >>= \t -> fm >>= \f -> beIte be c t f

--------------------------------------------------------------------------------
-- Symbolic backend

newtype BitTerm l = BitTerm { btVector :: LV.Vector l }
  deriving (Eq)
newtype BitTermClosed l = BitTermClosed (BitEngine l, BitTerm l)

instance (Eq l, LV.Storable l) => S.PrettyTerm (BitTermClosed l) where
  prettyTermWithD _ppconf ct@(BitTermClosed (be, BitTerm bv)) =
    let str      = LV.foldr (\lit acc -> acc ++ [toChar lit]) "" bv
        toChar x = if x == beFalse be then '0' else if x == beTrue be then '1' else '?'
    in
      if 1 == LV.length bv
      then text (maybe "?:[1]" show $ getBool ct)
      else text str <> colon <>  brackets (text $ show $ LV.length bv)
           <+> maybe empty cvt (getSVal ct)
    where
      cvt x = parens (integer x)
              <+> if x >= 0
                  then hex x
                  else case getUVal ct of
                         Nothing -> empty
                         Just u  -> hex u
      hex x = parens $ text "0x" <> text (showHex x "")

instance (LV.Storable l, Eq l) => ConstantProjection (BitTermClosed l) where
  getSVal (BitTermClosed (be, t)) =
    case beVectorToMaybeInt be (btVector t) of
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

prettyLV :: (Eq l, LV.Storable l) => BitEngine l -> LV.Vector l -> Doc
prettyLV be bv = S.prettyTermWithD S.defaultPPConfig $ BitTermClosed (be, BitTerm bv)

-- | Returns number of bytes.
byteSize :: LV.Storable l => LV.Vector l -> Int
byteSize v = LV.length v `shiftR` 3

-- | Slice a vector into a list of vectors, one for each byte.
sliceIntoBytes :: LV.Storable l => LV.Vector l -> V.Vector (LV.Vector l)
sliceIntoBytes v = V.generate (byteSize v) $ \i -> LV.slice (i `shiftL` 3) 8 v

type Addr = Integer

-- | @alignUp addr i@ returns the smallest multiple of @2^i@ that it
-- at least @addr@.
alignUp :: Addr -> Int -> Addr
alignUp addr i
  | (addr .&. mask) /= 0 = ((addr `shiftR` i) + 1) `shiftL` i
  | otherwise = (addr `shiftR` i) `shiftL` i
 where mask = (setBit 0 i) - 1

-- | @alignDn addr i@ returns the largest multiple of @2^i@ that it
-- at most @addr@.
alignDn :: Addr -> Int -> Addr
alignDn addr i = addr .&. (complement mask)
 where mask = (setBit 0 i) - 1

bmError :: String -> a
bmError = error

-- MemModel {{{1
data MemModel mem ptr int bytes cond = MemModel {
    mmDump :: Bool -> mem -> Maybe [Range Addr] -> IO ()
  , mmLoad :: mem -> ptr -> Integer -> IO (cond, bytes)
    -- | @mmStore mem value addr@
  , mmStore :: mem -> bytes -> ptr -> IO (cond, mem)
  , mmMux :: cond -> mem -> mem -> IO mem
  , mmInitGlobal :: mem -> bytes -> IO (Maybe (ptr, mem))
  , mmAddDefine :: mem -> LLVM.Symbol -> [LLVM.BlockLabel] -> IO (Maybe (ptr, mem))
  , mmBlockAddress :: mem -> LLVM.Symbol -> LLVM.BlockLabel -> ptr
  , mmLookupDefine :: mem -> ptr -> LookupDefineResult
  , mmStackAlloca :: mem -> Integer -> int -> Int -> StackAllocaResult ptr mem
  , mmHeapAlloc :: mem -> Integer -> int -> Int -> HeapAllocResult ptr mem
  , mmStackPush :: mem -> mem
  , mmStackPop :: mem -> mem
  , mmMemCopy :: mem
              -> ptr            -- ^ Destination pointer
              -> ptr            -- ^ Source pointer
              -> int            -- ^ Length value
              -> int            -- ^ Alignment in bytes
              -> IO (cond, mem) -- ^ Condition and new value.
  }

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
      val LV.++ LV.replicate (fromIntegral (w .&. 0x7)) (beDontCare be)
    -- Treat other types as same.
    _ -> val

-- | Load memory using 
loadTerm :: (Eq l, LV.Storable l)
         => LLVMContext
         -> MemModel m ptr (BitTerm l) (LV.Vector l) l
         -> m
         -> LLVM.Typed ptr
         -> IO (BitTerm l, BitTerm l)
loadTerm lc mm bm ptr
  | LLVM.PtrTo tp <- resolveType lc (LLVM.typedType ptr) = do
      (c, bits) <- mmLoad mm bm (LLVM.typedValue ptr) (llvmByteSizeOf lc tp)
      return (BitTerm (LV.singleton c), bytesToTerm lc tp bits)
  | otherwise = bmError "internal: Illegal type given to load"

-- | Store term in memory model.
storeTerm :: (Eq l, LV.Storable l)
          => LLVMContext
          -> BitEngine l
          -> MemModel mem ptr (BitTerm l) (LV.Vector l) l
          -> mem
          -> LLVM.Typed (BitTerm l)
          -> ptr
          -> IO (BitTerm l, mem)
storeTerm lc be mm m v ptr = do
  let bytes = termToBytes lc be (LLVM.typedType v) (LLVM.typedValue v)
  (c,m') <- mmStore mm m bytes ptr
  return (BitTerm (LV.singleton c), m')
 
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
  | SValue (LV.Vector l) -- ^ SValue validBit definedBit value
  | SDefine LLVM.Symbol -- ^ Memory value for function definition.
  | SBlock LLVM.Symbol LLVM.BlockLabel -- ^ Memory value for block within function.
  | SUninitialized -- ^ A memory byte that has not been initialized by LLVM.
  | SUnallocated -- ^ A memory section that has not been allocated to the program.

-- A derived(Show)-like pretty printer for the Storage type
ppStorageShow :: (Eq l, LV.Storable l) => BitEngine l -> Storage l -> Doc
ppStorageShow be (SBranch f t) = text "SBranch" <+> parens (ppStorage be f) <+> parens (ppStorage be t)
ppStorageShow be (SValue lv) = text "SValue" <+> parens (prettyLV be lv)
ppStorageShow _ (SDefine d) = text ("SDefine " ++ show d)
ppStorageShow _ (SBlock d b) = text ("SBlock " ++ show d ++ " " ++ show b)
ppStorageShow _ SUninitialized = text "SUninitialized"
ppStorageShow _ SUnallocated = text "SUnallocated"

-- A "sparse" pretty printer for the Storage type; skips unallocated regions and
-- shows addresses explicitly.

ppStorage :: (Eq l, LV.Storable l) => BitEngine l -> Storage l -> Doc
ppStorage = ppStorage' Nothing

ppStorage' :: (Eq l, LV.Storable l) => Maybe [Range Addr] -> BitEngine l -> Storage l -> Doc
ppStorage' mranges be = impl 0 Nothing
  where
    impl _ Nothing SUnallocated      = text "empty memory"
    impl a Nothing s                 = impl a (Just empty) s
    impl a mdoc (SBranch f t)        = let la = a `shiftL` 1
                                           ra = la `setBit` 0
                                       in impl ra (Just $ impl la mdoc f) t
    impl a (Just doc) (SValue v)     = whenInRange a $ item doc a (prettyLV be v)
    impl a (Just doc) (SDefine sym)  = whenInRange a $ item doc a $ LLVM.ppSymbol sym
    impl a (Just doc) (SBlock s l)   = whenInRange a
                                       $ item doc a
                                         $ LLVM.ppSymbol s
                                           <> char '/'
                                           <> LLVM.ppLabel l
    impl a (Just doc) SUninitialized = whenInRange a $ item doc a $ text "uninitialized"
    impl _ (Just doc) SUnallocated   = doc
    item doc addr desc               = doc $+$ integer addr <> colon <+> desc
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
        impl (SValue vx) (SValue vy) =
          SValue <$> beIteVector be c (return vx) (return vy)
        impl (SDefine dx) (SDefine dy)
          | dx == dy = return (SDefine dx)
          | otherwise = bmError "Attempt to merge memories with incompatible definitions."
        impl (SBlock dx bx) (SBlock dy by)
          | dx == dy && bx == by = return (SBlock dx bx)
          | otherwise = bmError "Attempt to merge memories with incompatible block values."
        impl SUninitialized SUninitialized = return SUninitialized
        impl SUnallocated SUnallocated = return SUnallocated
        impl _ _ = bmError "Attempt to merge incompatible valid addresses."

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

-- | Return storage with individual byte changed.
storeByte :: (Eq l, LV.Storable l)
          => BitEngine l -- ^ Bit engine for bits
          -> Storage l -- ^ Memory to update
          -> LV.Vector l -- ^ Value to write
          -> LV.Vector l -- ^ Address to write to
          -> IO (Storage l)
storeByte be mem new ptr = impl mem (LV.length ptr) (beTrue be)
  where impl (SBranch f t) i c
          | lo == beFalse be = do
            fr <- impl f (i-1) c
            return (SBranch fr t)
          | lo == beTrue be = do
            tr <- impl t (i-1) c
            return (SBranch f tr)
          | otherwise = do
            fr <- impl f (i-1) =<< beAnd be c (beNeg be lo)
            tr <- impl t (i-1) =<< beAnd be c lo
            return (SBranch fr tr)
          where lo = ptr LV.! (i-1)
        impl (SValue old) _ c = SValue <$> beIteVector be c (return new) (return old)
        impl SUninitialized  _ c
          | c == beTrue be = return (SValue new)
          | otherwise =
              bmError "Attempt to store symbolically to address that has not been initialized."
        impl (SDefine d) _ _ =
          bmError $ "Attempt to store to address that may point to definition of " ++ show d ++ "."
        impl (SBlock d b) _ _ =
          bmError $ "Attempt to store to address that may point to block " ++ show b ++ " in " ++ show d ++ "."
        impl SUnallocated _ _ =
          bmError $ "Attempt to store to address that is invalid: " ++ show (prettyLV be ptr)

storeBytes :: (Eq l, LV.Storable l)
           => BitEngine l
           -> Storage l      -- ^ Base storage
           -> LV.Vector l    -- ^ Value to store
           -> LV.Vector l    -- ^ Address to store value in
           -> IO (Storage l) -- ^ Storage with value written and condition under which it is true.
storeBytes be mem value ptr =
  let bv = sliceIntoBytes value
      fn mm i = do
        m <- mm
        p <- beAddIntConstant be ptr (toInteger i)
        storeByte be m (bv V.! i) p
   in V.foldl fn (return mem) (V.enumFromN 0 (byteSize value))

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
   | otherwise = {- trace ("setBytes " ++ show low ++ " " ++ show high) $ -} impl mem (w-1) 0
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

-- @uninitRegion w low high@ marks all bytes in [low..high) as uninitialized.
-- N.B. @w@ is the width of pointers in bits.
uninitRegion :: Int -> Addr -> Addr -> Storage l -> Storage l
uninitRegion w low high =
  setBytes w low high (\_ -> SUninitialized)

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
  , bmBasicBlockMap :: Map (LLVM.Symbol,LLVM.BlockLabel) (BitTerm l)
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
    $+$ text "Stack Range:" <+> text (show (bmStackAddr bm, bmStackEnd bm))
    $+$ text "Code Range:" <+> text (show (bmCodeAddr bm, bmCodeEnd bm))
    $+$ text "Data Range:" <+> text (show (bmDataAddr bm, bmDataEnd bm))
    $+$ text "Frame pointers:" <+> text (show (bmStackFrames bm))
    $+$ text "Storage:"
    $+$ (if sparse then ppStorage' mranges else ppStorageShow) be (bmStorage bm)

-- | @loadBytes be mem ptr size@ returns term representing all the bits with given size.
bmLoadByte :: (Eq l, LV.Storable l)
         => BitEngine l
         -> BitMemory l
         -> LV.Vector l
         -> IO (l, LV.Vector l)
bmLoadByte be bm vi = 
  let load (SBranch f t) i =
        mergeCondVector be (vi LV.! i) (load t (i-1)) (load f (i-1))
      load (SValue v) _ = return (beTrue be, v)
      load _ _ = return (beFalse be, LV.replicate 8 (beDontCare be))
   in load (bmStorage bm) (LV.length vi - 1)

bmMerge :: (Eq l, LV.Storable l)
        => BitEngine l
        -> l -> BitMemory l -> BitMemory l -> IO (BitMemory l)
bmMerge be c m m' = do
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
  newStorage <- mergeStorage be c (bmStorage m) (bmStorage m')
  -- Free lists should be implicitly equivalent if storages are compatible.
  return m { bmStorage = newStorage }

bmInitGlobalBytes :: (Eq l, LV.Storable l)
                  => Int         -- ^ Width of pointers in bits.
                  -> BitEngine l -- ^ Bit engine for literals.
                  -> BitMemory l
                  -> LV.Vector l
                  -> IO (Maybe (BitTerm l, BitMemory l))
bmInitGlobalBytes ptrWidth be m bytes
  | newDataAddr > bmDataEnd m = return Nothing
  | otherwise = do
      let ptrv = beVectorFromInt be ptrWidth dataAddr
          mem  = uninitRegion ptrWidth dataAddr newDataAddr (bmStorage m)
      newStorage <- storeBytes be mem bytes ptrv
      return $ Just ( BitTerm ptrv
                    , m { bmStorage = newStorage, bmDataAddr = newDataAddr })
  where
    dataAddr    = bmDataAddr m
    newDataAddr = dataAddr + toInteger (byteSize bytes)

bmMemAddDefine :: (Eq l, LV.Storable l)
               => Int -- ^ Width of pointers
               -> BitEngine l -- ^ Bit engine for literals.
               -> BitMemory l -- ^ Memory
               -> LLVM.Symbol -- ^ Definition
               -> [LLVM.BlockLabel] -- ^ Labels for blocks
               -> Maybe (BitTerm l, BitMemory l)
bmMemAddDefine ptrWidth be m def labs
    | newCodeAddr > bmCodeEnd m
    = Nothing
    | otherwise
    = Just ( BitTerm (beVectorFromInt be ptrWidth codeAddr)
           , m { bmStorage = newStorage
               , bmCodeAddr = newCodeAddr
                 -- Include new addresses in memory
               , bmBasicBlockMap =
                   foldl insertAddr
                         (bmBasicBlockMap m)
                         (labs `zip` [1..bbcnt])
               }
           )
  where bbcnt = toInteger (length labs)
        newSpaceReq = 1 + bbcnt
        codeAddr = bmCodeAddr m
        newCodeAddr = codeAddr + newSpaceReq
        insertAddr bmap (bb,idx) = Map.insert (def,bb) (BitTerm v) bmap
          where v = beVectorFromInt be ptrWidth (codeAddr + idx)
        updateAddr a | a == codeAddr = SDefine def
                     | otherwise     = SBlock def (labs !! fromInteger (a - codeAddr - 1))
        newStorage = setBytes ptrWidth codeAddr newCodeAddr updateAddr (bmStorage m)

bmCodeBlockAddress :: (Eq l, LV.Storable l)
                   => BitMemory l -- ^ Memory
                   -> LLVM.Symbol -- ^ Definition
                   -> LLVM.BlockLabel -- ^ Block label
                   -> BitTerm l
bmCodeBlockAddress m d b =
  let errMsg = "internal: Failed to find block " ++ show b ++ " in " ++ show d ++ "."
   in maybe (bmError errMsg) id $ Map.lookup (d,b) (bmBasicBlockMap m)

-- | Return symbol as given address in memory.
bmCodeLookupDefine :: (Eq l, LV.Storable l)
                   => BitEngine l
                   -> BitMemory l
                   -> BitTerm l
                   -> LookupDefineResult
bmCodeLookupDefine be m (BitTerm a) = do
  case beVectorToMaybeInt be a of
    Nothing -> Indeterminate
    Just (w, v) ->
      case loadDef (bmStorage m) w v of
        Nothing -> Invalid
        Just d -> Result d

bmStackAlloca :: (Eq l, LV.Storable l)
              => Int       -- ^ Width of pointer in bits.
              -> BitEngine l
              -> BitMemory l
              -> Integer   -- ^ Element size
              -> BitTerm l -- ^ Number of elements
              -> Int       -- ^ Alignment constraint
              -> StackAllocaResult (BitTerm l) (BitMemory l)
bmStackAlloca ptrWidth be bm eltSize (BitTerm cntVector) a =
  case beVectorToMaybeInt be cntVector of
    Nothing -> SASymbolicCountUnsupported
    Just (_,cnt) ->
      let -- Get number requested.
          -- @nm x y@ computes the next multiple m of y s.t. m >= y
          nm x y = ((y + x - 1) `div` x) * x
          -- Pad up to the end of the aligned region; we do this because any
          -- global data that gets copied into this space will be padded to this
          -- size by LLVM.
          padBytes = fromIntegral $ nm (2 ^ a :: Int) sz - sz
                       where sz = fromIntegral (eltSize * cnt)
          mkRes c res endAddr newAddr = 
            let newStorage = uninitRegion ptrWidth res endAddr (bmStorage bm)
             in SAResult (BitTerm (LV.singleton (beLitFromBool be c)))
                         (BitTerm (beVectorFromInt be ptrWidth res))
                         bm { bmStorage = newStorage, bmStackAddr = newAddr }
          -- Get new bit memory.
          r | bmStackGrowsUp bm =
                let alignedAddr  = alignUp (bmStackAddr bm) a
                    newStackAddr = alignedAddr + eltSize * cnt + padBytes
                 in mkRes (newStackAddr <= bmStackEnd bm)
                          alignedAddr
                          newStackAddr
                          newStackAddr
            | otherwise =
                let sz = eltSize * cnt + padBytes
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
         let (l,h) = if bmStackGrowsUp bm then (f, bmStackAddr bm) else (bmStackAddr bm, f)
          in setBytes ptrWidth l h (\_ -> SUnallocated) (bmStorage bm)
     , bmStackAddr = f
     , bmStackFrames = fl
     }

blockPower :: Integer -> Int
blockPower v = go 0 1
  where go p n | n >= v = p
               | otherwise = go (p + 1) (n `shiftL` 1)

bmHeapAlloc :: (Eq l, LV.Storable l)
            => Int       -- ^ Width of pointer in bits.
            -> BitEngine l
            -> BitMemory l
            -> Integer   -- ^ Element size
            -> BitTerm l -- ^ Number of elements
            -> Int       -- ^ Alignment constraint
            -> HeapAllocResult (BitTerm l) (BitMemory l)
bmHeapAlloc ptrWidth be bm eltSize (BitTerm cntVector) a =
  case beVectorToMaybeInt be cntVector of
    Nothing -> HASymbolicCountUnsupported
    Just (_, cnt) ->
      let pwr = blockPower (cnt * eltSize)
          size = 2 ^ pwr
          mres = allocBlock (bmFreeList bm) pwr
          false = BitTerm $ beVectorFromInt be 1 0
          true = BitTerm $ beVectorFromInt be 1 1
          zeroTerm = BitTerm (beVectorFromInt be (LV.length cntVector) 0) in
      case mres of
        Just (freeList, addr) ->
          let endAddr = addr + size
              addrTerm = BitTerm $ beVectorFromInt be ptrWidth addr
              newStorage = uninitRegion ptrWidth addr endAddr (bmStorage bm) in
          HAResult true addrTerm size
                   (bm { bmFreeList = freeList
                       , bmStorage = newStorage
                       })
        Nothing -> HAResult false zeroTerm 0 bm

bmMemCopy :: (Eq l, LV.Storable l)
          => BitEngine l -- ^ Bit engine for literals
          -> BitMemory l
          -> BitTerm l   -- ^ Destination pointer
          -> BitTerm l   -- ^ Source pointer
          -> BitTerm l   -- ^ Length value
          -> BitTerm l   -- ^ Alignment in bytes
          -> IO (l, BitMemory l)
bmMemCopy be m (BitTerm dst) src (BitTerm len0) (BitTerm _align) = do
  -- TODO: Alignment checks?
  (c, bytes) <- loadBytes be (bmLoadByte be m) src len
  newStorage <- storeBytes be (bmStorage m) bytes dst
  return (c, m { bmStorage = newStorage })
  where
    len = case beVectorToMaybeInt be len0 of
            Nothing    -> bmError $ "Symbolic memcpy len not supported"
            Just (_,x) -> x


-- | Memory model for explicit buddy allocation scheme.
buddyMemModel :: (Eq l, LV.Storable l)
              => LLVMContext
              -> BitEngine l
              -> MemModel (BitMemory l) (BitTerm l) (BitTerm l) (LV.Vector l) l
buddyMemModel lc be =
  let ptrWidth = llvmAddrWidthBits lc
   in MemModel {
          mmDump = bmDump be
        , mmLoad = loadBytes be . bmLoadByte be
        , mmStore = \m bytes (BitTerm ptr) -> do
           newStorage <- storeBytes be (bmStorage m) bytes ptr
           return (beTrue be, m { bmStorage = newStorage })
        , mmMux = bmMerge be
        , mmInitGlobal = bmInitGlobalBytes ptrWidth be
        , mmAddDefine = return `c3` bmMemAddDefine ptrWidth be
        , mmBlockAddress = bmCodeBlockAddress 
        , mmLookupDefine = bmCodeLookupDefine be
        , mmStackAlloca = bmStackAlloca ptrWidth be
        , mmStackPush = bmStackPush
        , mmStackPop = bmStackPop (llvmAddrWidthBits lc)
        , mmHeapAlloc = bmHeapAlloc ptrWidth be
        , mmMemCopy = bmMemCopy be 
        }


-- DagMemory {{{1

data DMDag l = DMDag {
    dmAppNodeMap :: !(Map (DMApp l) (DMMemory l))
  , dmNodeCount :: !Int
  }

data DMCache l = DMCache {
     dmcAlloc :: Map (LV.Vector l) l
      -- | Maps byte addresses to (initialized,value) tuple.
   , dmcLoad :: Map (LV.Vector l) (l, LV.Vector l)
   }

data DMMemory l = DMMemory {
    dmNodeIdx :: Int 
  , dmNodeApp :: DMApp l
  , dmBasicBlockMap :: Map (LLVM.Symbol,LLVM.BlockLabel) (BitTerm l)
    -- | Current address for stack.
  , dmStack :: LV.Vector l
    -- | Frames on stack.
  , dmStackFrames :: [LV.Vector l]
    -- | Address for next value in code segment.
  , dmCode :: Addr
    -- | Address for next value in data segment.
  , dmData :: Addr
    -- | Address for next value in heap.
  , dmHeap :: LV.Vector l
    -- | Maps byte addresses to (allocated,initialized,value) tuple.
  , dmCache :: IORef (DMCache l)
  }

instance Eq (DMMemory l) where
  x == y = dmNodeIdx x == dmNodeIdx y

instance Ord (DMMemory l) where
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
   | DMAddDefine LLVM.Symbol (V.Vector LLVM.BlockLabel) (DMMemory l)
     -- | @DMAlloc base end prev@ denotes the memory obtained by
     -- allocating bytes in @[base,end)@ to @prev@.
   | DMAlloc (SymAddr l) (SymAddr l) (DMMemory l) 
     -- | @DMInitial denotes an initial completely undefined memory.
   | DMDealloc (SymAddr l) (SymAddr l) (DMMemory l)
     -- | @DMStore addr end valueBytes prev@
   | DMStore (SymAddr l) (SymAddr l) (V.Vector (Byte l)) (DMMemory l)
     -- | @DMMemCopy dest destEnd src prev@
   | DMMemCopy (SymAddr l) (SymAddr l) (SymAddr l) (DMMemory l)

   | DMMerge l (DMMemory l) (DMMemory l)
  deriving (Eq, Ord)

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

-- | Returns predicate indicating if memory can be written to.
dmIsWritable :: (LV.Storable l, Ord l) => BitEngine l -> LV.Vector l -> DMMemory l -> IO l
dmIsWritable be ptr = parseMem
  where parseMem mem = do
          allocCache <- dmcAlloc <$> readIORef (dmCache mem)
          case Map.lookup ptr allocCache of
            Just rVal -> return rVal
            Nothing -> do
              rVal <- parseApp (dmNodeApp mem)
              -- Add rVal to cache for future lookups
              modifyIORef (dmCache mem) $ \c -> 
                c { dmcAlloc = Map.insert ptr rVal (dmcAlloc c) }
              return rVal
        parseApp DMInitial             = return (beFalse be)
        parseApp (DMAddDefine _ _ mem) = parseMem mem
        parseApp (DMAlloc s e mem)     = beOrM be (beInRange be ptr (s,e)) (parseMem mem)
        parseApp (DMDealloc s e mem)   = beAndM be (beNeg be <$> beInRange be ptr (s,e)) (parseMem mem)
        parseApp (DMStore _ _ _ mem)   = parseMem mem
        parseApp (DMMemCopy _ _ _ mem) = parseMem mem
        parseApp (DMMerge dmc tm fm)   = beIteM be dmc (parseMem tm) (parseMem fm)

dmLoadByte :: (Ord l, LV.Storable l)
           => BitEngine l
           -> DMMemory l
           -> LV.Vector l
           -> IO (l, LV.Vector l) -- ^ Verification condition and value.
dmLoadByte be = parseMem
  where -- Perform mux of (Lit,LitVector) pairs
        parseMem n rPtr = do
          loadCache <- dmcLoad <$> readIORef (dmCache n)
          case Map.lookup rPtr loadCache of
            Just rVal -> return rVal
            Nothing -> do
              rVal <- parseApp (dmNodeApp n) rPtr
              -- Add rVal to cache for future lookups
              modifyIORef (dmCache n) $ \c -> 
                c { dmcLoad = Map.insert rPtr rVal (dmcLoad c) }
              return rVal
        invalidResult = (beFalse be, beDontCareByte be)
        -- Parse application
        parseApp DMInitial _ptr = return invalidResult
        parseApp (DMAddDefine _ _ mem) ptr = parseMem mem ptr
        -- | Allocation does not modify reading.
        parseApp (DMAlloc _ _ mem) ptr = parseMem mem ptr
        -- | Check that memory has not been deallocated
        parseApp (DMDealloc s e mem) ptr = do
          inRange <- beInRange be ptr (s,e)
          (c,r) <- parseMem mem ptr
          c' <- beAnd be (beNeg be inRange) c
          return (c', r)
        parseApp (DMStore s e bytes mem) ptr = do
          let byteCount = V.length bytes
          let idxFn i | i == byteCount = invalidResult
                      | otherwise = (beTrue be, bytes V.! i)
          inRange <- beInRange be ptr (s, e)
          let storedValue = do
                vidx <- beSubInt be s ptr
                beMuxGeneral (mergeCondVector be)
                             (toInteger byteCount)
                             vidx
                             (return . idxFn . fromInteger)
          mergeCondVector be inRange storedValue (parseMem mem ptr)
        parseApp (DMMemCopy d e src mem) ptr = do
          inRange <- beInRange be ptr (d,e)
          let prevPtr = beAddInt be src =<< beSubInt be ptr d
          parseMem mem =<< beIteVector be inRange prevPtr (return ptr)
        parseApp (DMMerge dmc t f) ptr = mergeCondVector be dmc (parseMem t ptr) (parseMem f ptr)

-- | Returns node with given app, creating it if necessary.  The function passed
-- in gives the opportunity to modify the node before it is cached.
dmGetNode :: (Ord l, LV.Storable l)
          => IORef (DMDag l)
          -> DMMemory l -> DMApp l -> (DMMemory l -> IO (DMMemory l)) -> IO (DMMemory l)
dmGetNode ref base app nodeFn = do
  dg <- readIORef ref
  case Map.lookup app (dmAppNodeMap dg) of
    Just r -> return r
    Nothing -> do
      let c = dmNodeCount dg
      cacheRef <- newIORef (DMCache Map.empty Map.empty)
      r <- nodeFn base { dmNodeIdx = c
                       , dmNodeApp = app
                       , dmCache = cacheRef } 
      let dg' = DMDag { dmAppNodeMap = Map.insert app r (dmAppNodeMap dg)
                      , dmNodeCount = 1 + c }
      dg' `seq` writeIORef ref dg'
      return r

dmStoreBytes :: (Ord l, LV.Storable l)
             => BitEngine l -> IORef (DMDag l)
             -> DMMemory l -> LV.Vector l -> BitTerm l -> IO (l, DMMemory l)
dmStoreBytes be ref mem flatBytes (BitTerm ptr) = do
  -- TODO: Verify bytes have been allocated.
  let bytes = sliceIntoBytes flatBytes
  wEnd <- beAddIntConstant be ptr (toInteger (V.length bytes))
  return (,) `ap` dmIsWritable be ptr mem
             `ap` dmGetNode ref mem (DMStore ptr wEnd bytes mem) return

dmMerge :: (Ord l, LV.Storable l)
        => BitEngine l -> IORef (DMDag l)
        -> l -> DMMemory l -> DMMemory l -> IO (DMMemory l)
dmMerge be ref c t f = do
  unless (dmBasicBlockMap t == dmBasicBlockMap f) $
    fail "internal: Attempt to merge memories with different block addresses."
  unless (length (dmStackFrames t) == length (dmStackFrames f)) $
    fail "internal: Attempt to merge memories with different numbers of stacks pushed."
  unless (dmCode t == dmCode f) $
    fail "internal: Attempt to merge memories with different code addresses."
  unless (dmData t == dmCode f) $
    fail "Attempt to merge memories with different data segment addresses."
  let mux = LV.zipWithM (beIte be c)
  dmGetNode ref t (DMMerge c t f) $ \m -> do
    newStack <- mux (dmStack t) (dmStack f)
    newStackFrames <- zipWithM mux (dmStackFrames t) (dmStackFrames f)
    newHeap <- mux (dmHeap t) (dmHeap f)
    return m { dmStack = newStack
             , dmStackFrames = newStackFrames
             , dmHeap = newHeap
             }

-- 
dmInitGlobal :: (Ord l, LV.Storable l) 
             => BitEngine l -> Int 
             -> Addr -- | End of data region
             -> IORef (DMDag l) 
             -> DMMemory l -> LV.Vector l -> IO (Maybe (BitTerm l, DMMemory l))
dmInitGlobal be ptrWidth dataEnd ref mem flatBytes 
  | dataEnd - dmData mem < byteCount = return Nothing
  | otherwise = do
    -- Allocate space in data segment
    mem1 <- dmGetNode ref mem (DMAlloc ptr ptrEnd mem) $ \m -> return m { dmData = nextData }
    -- Store bytes 
    mem2 <- dmGetNode ref mem1 (DMStore ptr ptrEnd bytes mem) return
    -- Return result
    return $ Just (BitTerm ptr, mem2)
  where bytes = sliceIntoBytes flatBytes
        byteCount = toInteger (V.length bytes)
        nextData = dmData mem + byteCount
        ptr = beVectorFromInt be ptrWidth (dmData mem)
        ptrEnd = beVectorFromInt be ptrWidth nextData
  
type DagMemModel l = MemModel (DMMemory l) (BitTerm l) (BitTerm l) (LV.Vector l) l

dagMemModel :: (Ord l, LV.Storable l)
            => LLVMContext
            -> BitEngine l
            -> Range Addr -- ^ Stack start and end address
            -> Range Addr -- ^ Code start and end address
            -> Range Addr -- ^ Data start and end addresses
            -> Range Addr -- ^ Heap start and end address
            -> IO (DagMemModel l, DMMemory l)
dagMemModel lc be stack code gdata heap = do
  cacheRef <- newIORef (DMCache Map.empty Map.empty)
  let ptrWidth = llvmAddrWidthBits lc
  let mem = DMMemory { dmNodeIdx = 0
                     , dmNodeApp = DMInitial
                     , dmBasicBlockMap = Map.empty
                     , dmStack = beVectorFromInt be ptrWidth (start stack)
                     , dmStackFrames = []
                     , dmCode = start code
                     , dmData = start gdata
                     , dmHeap = beVectorFromInt be ptrWidth (start heap)
                     , dmCache = cacheRef
                     }
  ref <- newIORef DMDag { dmAppNodeMap = Map.empty, dmNodeCount = 1 }
  let mm = MemModel {
          mmLoad = loadBytes be . dmLoadByte be
        , mmStore = dmStoreBytes be ref
        , mmMux = dmMerge be ref
        , mmInitGlobal = dmInitGlobal be (llvmAddrWidthBits lc) (end gdata) ref 
        , mmDump = undefined
        , mmAddDefine = undefined
        , mmBlockAddress = undefined
        , mmLookupDefine = undefined
        , mmStackAlloca = undefined
        , mmStackPush = undefined
        , mmStackPop = undefined
        , mmHeapAlloc = undefined
        , mmMemCopy = undefined
        {-
          mmDump = \_space _m -> return ()
        , mmAddDefine = \_mem _def _lbls -> return Nothing
        , mmBlockAddress = \m d b -> 
            let errMsg = "internal: Failed to find block " ++ show b ++ " in " ++ show d ++ "."
             in maybe (bmError errMsg) id $ Map.lookup (d,b) (dmBasicBlockMap m)
        , mmLookupDefine = \m p -> undefined
        , mmStackAlloca = \m p :: mem -> Integer -> int -> Int -> StackAllocaResult ptr mem
        , mmStackPush :: mem -> mem
        , mmStackPop :: mem -> mem
        , mmMemCopy :: mem
                  -> ptr            -- ^ Destination pointer
                  -> ptr            -- ^ Source pointer
                  -> int            -- ^ Length value
                  -> int            -- ^ Alignment in bytes
                  -> IO (cond, mem) -- ^ Condition and new value.
                          -}
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
  BitTerm <$> f be w x
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

type BitBlastSBE l = SBE (BitIO (BitMemory l) l)

sbeBitBlast :: (S.PrettyTerm (BitTermClosed l), Eq l, LV.Storable l)
            => LLVMContext
            -> BitEngine l
            -> MemModel m (BitTerm l) (BitTerm l) (LV.Vector l) l
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
          , memMerge         = \(BitTerm cv) m m' ->
                                 assert (LV.length cv == 1) $
                                   BitIO $ mmMux mm (cv LV.! 0) m m'
          , memAddDefine     = BitIO `c3` mmAddDefine mm
          , memInitGlobal    = \m (LLVM.Typed ty gd) ->
                                 BitIO $ mmInitGlobal mm m (termToBytes lc be ty gd)
          , codeBlockAddress = return `c3` mmBlockAddress mm
          , codeLookupDefine = return `c2` mmLookupDefine mm
          , stackAlloca = \m eltTp (LLVM.typedValue -> cnt) ->
              return . mmStackAlloca mm m (llvmByteSizeOf lc eltTp) cnt
          , stackPushFrame   = return . mmStackPush mm
          , stackPopFrame    = return . mmStackPop mm
          , heapAlloc        = \m eltTp (LLVM.typedValue -> cnt) ->
              return . mmHeapAlloc mm m (llvmByteSizeOf lc eltTp) cnt
          , memCopy          = (BitIO . fmap (\(c,m) -> (BitTerm (LV.singleton c), m)))
                                `c5` mmMemCopy mm
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
  | sum (map llvmBitSizeOf tys0) /= LV.length t
  = error "termDecompImpl: sum of type sizes must equal bitvector length"
  | otherwise
  = unfoldr slice (t, tys0)
  where
    llvmBitSizeOf ty = fromIntegral $ llvmByteSizeOf lc ty `shiftL` 3
    slice (bv, [])       = assert (LV.null bv) Nothing
    slice (bv, (ty:tys)) = Just (bt $ LV.take sz bv, (LV.drop sz bv, tys))
      where
        bt = LLVM.Typed ty . BitTerm
        sz = llvmBitSizeOf ty

type Range t = (t,t)

norm :: Range Addr -> Range Addr
norm (x,y) = (min x y, max x y)

overlap :: Range Addr -> Range Addr -> Bool
overlap (s1,e1) (s2,e2) = if s1 <= s2 then s2 < e2 && s2 < e1 else s1 < e1 && s1 < e2

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

sbeBitBlastMem :: LV.Storable l
               => (Addr, Addr) -- ^ Stack start and end address
               -> (Addr, Addr) -- ^ Code start and end address
               -> (Addr, Addr) -- ^ Data start and end addresses
               -> (Addr, Addr) -- ^ Heap start and end address
               -> BitMemory l
sbeBitBlastMem stack code gdata heap
  | decreasing code = bmError "internal: Code segment start and end are in wrong order."
  | decreasing gdata = bmError "internal: Data segment start and end are in wrong order."
  | decreasing heap = bmError "internal: Heap segment start and end are in wrong order."
  | norm stack `overlap` code  = bmError "internal: Stack and code segments overlap."
  | norm stack `overlap` gdata = bmError "internal: Stack and data segments overlap."
  | norm stack `overlap` heap  = bmError "internal: Stack and heap segments overlap."
  | code `overlap` gdata = bmError "internal: Code and data segments overlap."
  | code `overlap` heap = bmError "internal: Code and heap segments overlap."
  | gdata `overlap` heap = bmError "internal: Data and code segments overlap."
  | otherwise =
      BitMemory {
          bmStorage = SUnallocated
        , bmBasicBlockMap = Map.empty
        , bmStackAddr = start stack
        , bmStackEnd = end stack
        , bmStackFrames = []
        , bmCodeAddr = start code
        , bmCodeEnd = end code
        , bmDataAddr = start gdata
        , bmDataEnd = end gdata
        , bmFreeList = initFreeList (start heap) (end heap)
        }

testSBEBitBlast :: IO ()
testSBEBitBlast = do
  let lc = LLVMContext 5 undefined
  be <- createBitEngine
  let sbe = sbeBitBlast lc be (buddyMemModel lc be)
  let m0 = sbeBitBlastMem (0x10,0x0) (0x0,0x0) (0x0, 0x0) (0x0,0x0)
  liftSBEBitBlast $ do
    let i32 = LLVM.PrimType (LLVM.Integer 32)
    let ptr = LLVM.PtrTo
    l1 <- termInt sbe 32 1
    liftIO $ putStrLn (render (text "m0:" <+> ppStorage be (bmStorage m0)))
    SAResult _ sp m1 <- stackAlloca sbe m0 i32 (LLVM.Typed i32 l1) 1
    liftIO $ putStrLn (render (text "m1:" <+> ppStorage be (bmStorage m1)))
    liftIO $ putStrLn $ show $ beVectorToMaybeInt be (btVector sp)
    lv <- termInt sbe 32 0x12345678
    (_,m2) <- memStore sbe m1 (LLVM.Typed i32 lv) sp
    liftIO $ putStrLn (render (text "m2:" <+> ppStorage be (bmStorage m2)))
    (BitTerm actualValue,_) <- memLoad sbe m2 (LLVM.Typed (ptr i32) sp)
    liftIO $ putStrLn $ show $ (0x12345678 :: Integer)
    liftIO $ putStrLn $ show $ beVectorToMaybeInt be actualValue
    return ()

__nowarn_unused :: a
__nowarn_unused = undefined testSBEBitBlast allocBlock trace c4
