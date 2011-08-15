{- |
Module           : $Header$
Description      : A symbolic backend that bitblasts
Stability        : provisional
Point-of-contact : atomb, jhendrix
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module LSS.SBEBitBlast where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector.Storable as LV
import qualified Data.Vector as V

import qualified Text.LLVM.AST as LLVM
import Verinf.Symbolic.Lit
import LSS.SBEInterface


c2 :: (r -> s) -> (a -> b -> r) -> a -> b -> s
g `c2` f = \x y -> g (f x y)

c3 :: (r -> s) -> (a -> b -> c -> r) -> a -> b -> c -> s
g `c3` f = \x y z -> g (f x y z)

c4 :: (r -> s) -> (a -> b -> c -> d -> r) -> a -> b -> c -> d -> s
g `c4` f = \w x y z -> g (f w x y z)

--------------------------------------------------------------------------------
-- Symbolic backend

newtype BitTerm l = BitTerm { btVector :: LV.Vector l }

-- | Returns number of bytes.
byteSize :: LV.Storable l => LV.Vector l -> Int
byteSize v = LV.length v `shiftR` 3

-- | Slice a vector into a list of vectors, one for each byte.
sliceIntoBytes :: LV.Storable l => LV.Vector l -> V.Vector (LV.Vector l)
sliceIntoBytes v =
   V.map (\i -> LV.slice (i `shiftL` 3) 8 v) (V.enumFromN 0 (byteSize v))

-- | Tree-based data structure for representing value of bytes in memory.
data Storage l
  = MTBranch (Storage l) (Storage l) -- ^ Branch falseBranch trueBranch
  | MTValue (LV.Vector l) -- ^ MTValue validBit definedBit value
  | MTDefine LLVM.Symbol -- ^ Memory value for function definition.
  | MTBlock LLVM.Symbol LLVM.Ident -- ^ Memory value for block within function.
  | MTUninitialized -- ^ A memory byte that has not been initialized by LLVM.
  | MTUnallocated -- ^ A memory section that has not been allocated to the program.

mergeStorage :: (Eq l, LV.Storable l) => BitEngine l -> l -> Storage l -> Storage l -> IO (Storage l)
mergeStorage be c x y = impl x y
  where impl (MTBranch fx tx) (MTBranch fy ty) = do
          f <- impl fx fy
          t <- impl tx ty
          return (MTBranch f t)
        impl (MTValue vx) (MTValue vy) =
          MTValue <$> beIteVector be c (return vx) (return vy)
        impl (MTDefine dx) (MTDefine dy)
          | dx == dy = return (MTDefine dx)
          | otherwise = bmError "Attempt to merge memories with incompatible definitions."
        impl (MTBlock dx bx) (MTBlock dy by)
          | dx == dy && bx == by = return (MTBlock dx bx)
          | otherwise = bmError "Attempt to merge memories with incompatible block values."
        impl MTUninitialized MTUninitialized = return MTUninitialized
        impl MTUnallocated MTUnallocated = return MTUnallocated
        impl _ _ = bmError "Attempt to merge incompatible valid addresses."

-- | @loadByte be mem ptr@ returns a term representing the value of ptr in @mem@.
loadByte :: (Eq l, LV.Storable l)
         => BitEngine l
         -> Storage l
         -> BitTerm l
         -> IO (BitTerm l)
loadByte be mem (BitTerm vi) = BitTerm <$> impl mem (LV.length vi - 1)
  where impl (MTBranch f t) i =
           beIteVector be (vi LV.! i) (impl t (i+1)) (impl f (i+1))
        impl (MTValue v) _ = return v
        impl (MTDefine d) _ = 
          bmError $ "Attempt to read address that may point to definition of " ++ show d ++ "."
        impl (MTBlock d b) _ =
          bmError $ "Attempt to read address that may point to block " ++ show b ++ " in " ++ show d ++ "."
        impl MTUninitialized  _ =
          bmError $ "Attempt to read address that has not been initialized."
        impl MTUnallocated _ =
          bmError $ "Attempt to read address that is invalid."

-- | @loadBytes be mem ptr size@ returns term representing all the bits with given size.
loadBytes :: (Eq l, LV.Storable l)
          => BitEngine l
          -> Storage l
          -> BitTerm l
          -> Integer
          -> IO (BitTerm l)
loadBytes be mem (BitTerm vi) sz = do
  terms <- forM [1..sz] $ \i -> do
    t <- BitTerm <$> beAddIntConstant be vi (i-1)
    btVector <$> loadByte be mem t
  return $ BitTerm (LV.concat terms)

-- | Return storage with individual byte changed.
storeByte :: (Eq l, LV.Storable l)
          => BitEngine l -- ^ Bit engine for bits
          -> Storage l -- ^ Memory to update
          -> LV.Vector l -- ^ Value to write
          -> LV.Vector l -- ^ Address to write to
          -> IO (Storage l)
storeByte be mem new ptr = impl mem 0 (beTrue be)
  where impl (MTBranch f t) i c
           | b == beTrue be = do
             tr <- impl t (i+1) c
             return (MTBranch f tr)
           | b == beFalse be = do
             fr <- impl f (i+1) c
             return (MTBranch fr t)
           | otherwise = do
             tr <- impl t (i+1) =<< beAnd be c b
             fr <- impl f (i+1) =<< beAnd be c (beNeg be b)
             return (MTBranch fr tr)
          where b = ptr LV.! i
        impl (MTValue old) _ c = MTValue <$> beIteVector be c (return new) (return old)
        impl MTUninitialized  _ c
          | c == beTrue be = return (MTValue new)
          | otherwise =
              bmError "Attempt to store symbolically to address that has not been initialized."
        impl (MTDefine d) _ _ = 
          bmError $ "Attempt to store to address that may point to definition of " ++ show d ++ "."
        impl (MTBlock d b) _ _ =
          bmError $ "Attempt to store to address that may point to block " ++ show b ++ " in " ++ show d ++ "."
        impl MTUnallocated _ _ =
          bmError $ "Attempt to store to address that is invalid."

storeBytes :: (Eq l, LV.Storable l)
           => BitEngine l
           -> Storage l -- ^ Base storage
           -> BitTerm l -- ^ Value to store
           -> BitTerm l -- ^ Address to store value in
           -> IO (Storage l) -- ^ Storage with value written.
storeBytes be mem (BitTerm value) (BitTerm ptr) =
  let bv = sliceIntoBytes value
      fn mm i = do
        m <- mm
        p <- beAddIntConstant be ptr (toInteger i)
        storeByte be m (bv V.! i) p
   in V.foldl fn (return mem) (V.enumFromN 0 (byteSize value))

loadDef :: Storage l -> Int -> Addr -> Maybe LLVM.Symbol
loadDef s w a = impl s (w-1)
  where impl (MTBranch f t) i = impl (if testBit a i then t else f) (i-1)
        impl (MTDefine d) _ = Just d
        impl _ _ = Nothing

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

-- @setBytes w low high val mem@ sets all bytes in [low .. high) to @val@.
setBytes :: Int -> Addr -> Addr -> (Addr -> Storage l) -> Storage l -> Storage l
setBytes w low high fn mem = impl mem 0 (2 ^ w)
  where impl m _ maxAddr | maxAddr < low = m
        impl m minAddr _ | high <= minAddr = m
        impl (MTBranch f t) minAddr maxAddr =
          let midAddr = (minAddr + maxAddr) `shiftR` 1
           in MTBranch (impl f minAddr midAddr) (impl t midAddr maxAddr)
        impl MTUnallocated minAddr maxAddr =
          let midAddr = (minAddr + maxAddr) `shiftR` 1
           in MTBranch (impl MTUnallocated minAddr midAddr)
                       (impl MTUnallocated midAddr maxAddr)
        impl _ minAddr maxAddr
          | minAddr == maxAddr = fn minAddr
          | otherwise = bmError "internal: Malformed storage"

type Addr = Integer

data BitMemory l = BitMemory {
    -- | Bitwidth of address.
    bmAddrWidth :: Int
    -- | Stores state of memory.
  , bmStorage :: Storage l
    -- | Current address of stack
  , bmStackAddr :: Addr
    -- | Maximum address for stack.
  , bmStackEnd :: Addr
    -- | Frames on stack.
  , bmStackFrames :: [Integer]
    -- | Address for initial code pointers.
  , bmCodeAddr :: Addr
    -- | Maximum address for code pointers.
  , bmCodeEnd :: Addr
    -- | Maps (def,block) pairs to associated address.
  , bmCodeBlockMap :: Map (LLVM.Symbol,LLVM.Ident) Addr
  , bmFreeLists :: V.Vector [Addr]
  }

-- | Free list maps each index i to the address of blocks with 2^i
-- eleements that are free.
type FreeList = V.Vector [Addr]


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

-- | Returns true if stack addresses increase as more elements are pushed on
-- stack.
bmStackGrowsUp :: BitMemory l -> Bool
bmStackGrowsUp bm = bmStackAddr bm <= bmStackEnd bm

data LLVMContext = LLVMContext {
    llvmLookupAlias :: LLVM.Ident -> LLVM.Type
  }

 -- | Returns number of bits for an LLVM.Type
llvmSizeOf :: LLVMContext -> LLVM.Type -> Addr
llvmSizeOf _ _ = undefined

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

bmLoad :: (Eq l, LV.Storable l)
       => LLVMContext
       -> BitEngine l
       -> BitMemory l
       -> LLVM.Typed (BitTerm l)
       -> IO (BitTerm l)
bmLoad lc be bm ptr = impl (LLVM.typedType ptr)
  where impl (LLVM.Alias nm) = impl (llvmLookupAlias lc nm)
        impl (LLVM.PtrTo tp) =
          let eltSize = llvmSizeOf lc tp
           in loadBytes be (bmStorage bm) (LLVM.typedValue ptr) eltSize
        impl _ = bmError "Illegal type given to load"

bmStore :: (Eq l, LV.Storable l)
        => BitEngine l
        -> BitMemory l
        -> LLVM.Typed (BitTerm l)
        -> BitTerm l
        -> IO (BitMemory l)
bmStore be bm v ptr = do
  newStorage <- storeBytes be (bmStorage bm) (LLVM.typedValue v) ptr
  return (bm { bmStorage = newStorage })

bmStackAlloca :: (Eq l, LV.Storable l)
              => LLVMContext
              -> BitEngine l
              -> BitMemory l
              -> LLVM.Type
              -> LLVM.Typed (BitTerm l)
              -> Int
              -> (BitTerm l, BitMemory l)
bmStackAlloca lc be bm eltTp typedCnt a
    | newStackAddr > bmStackEnd bm = bmError "Stack limit exceeded in alloca."
    | otherwise = (res, newBM)
  where -- Number of bytes in element type.
        eltSize = llvmSizeOf lc eltTp 
        BitTerm cntVector = LLVM.typedValue typedCnt
        -- Get number requested.
        cnt = case beVectorToMaybeInt be cntVector of
                Nothing -> 
                  bmError $ "Symbolic number of elements requested with alloca;"
                               ++ " when current implementation only supports concrete"
                Just v -> v
        w = bmAddrWidth bm
        -- Stack pointer adjusted to alignment boundary.
        addr = bmStackAddr bm
        alignFn = if bmStackGrowsUp bm then alignUp else alignDn
        moveFn :: Addr -> Addr -> Addr 
        moveFn = if bmStackGrowsUp bm then (+) else (-)
        alignedAddr = alignFn (bmStackAddr bm) a
        -- Get new top of stack integer.
        newStackAddr = alignedAddr `moveFn` eltSize * cnt
        -- Get result pointer
        res = BitTerm (beVectorFromInt be w alignedAddr)
        -- Get new bit memory.
        newBM = bm { bmStorage = setBytes w addr newStackAddr (\_ -> MTUninitialized) (bmStorage bm)
                   , bmStackAddr = newStackAddr
                   }

-- | Push stack frame to memory.
bmStackPush :: BitMemory l -> BitMemory l
bmStackPush bm = bm { bmStackFrames = bmStackAddr bm : bmStackFrames bm }

-- | Pop stack frame in memory and invalidate old addresses.
bmStackPop :: BitMemory l -> BitMemory l
bmStackPop BitMemory { bmStackFrames = [] } =
  bmError "internal: Attempted to pop stack frame from memory when no stack frames have been pushed."
bmStackPop bm@BitMemory { bmStackFrames = f : fl } =
  bm { bmStorage = 
         let (l,h) = if bmStackGrowsUp bm then (f, bmStackAddr bm) else (bmStackAddr bm, f)
          in setBytes (bmAddrWidth bm) l h (\_ -> MTUnallocated) (bmStorage bm)
     , bmStackAddr = f
     , bmStackFrames = fl }

bmMerge :: (Eq l, LV.Storable l)
        => BitEngine l
        -> BitTerm l
        -> BitMemory l
        -> BitMemory l
        -> IO (BitMemory l)
bmMerge be (BitTerm cv) m m'
  | LV.length cv == 1 = do
      unless (bmAddrWidth m == bmAddrWidth m') $
        fail "internal: Attempt to merge memories with incompatible address width"
      unless (bmStackGrowsUp m == bmStackGrowsUp m') $
        fail "internal: Attempt to merge memories with incompatible stack growth."
      newStorage <- mergeStorage be (cv LV.! 0) (bmStorage m) (bmStorage m')
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
      unless (bmCodeBlockMap m == bmCodeBlockMap m') $
        fail "internal: Attempt to merge memories with different block addresses."
      -- Free lists should be implicitly equivalent if storages are compatible.
      return BitMemory
        { bmAddrWidth = bmAddrWidth m
        , bmStorage = newStorage 
        , bmStackAddr = bmStackAddr m
        , bmStackEnd = bmStackEnd m
        , bmStackFrames = bmStackFrames m
        , bmCodeAddr = bmCodeAddr m
        , bmCodeEnd = bmCodeEnd m
        , bmCodeBlockMap = bmCodeBlockMap m
        , bmFreeLists = bmFreeLists m
        }
  | otherwise = bmError "internal: Malformed condition given to bmMerge."

bmCodeBlockAddress :: (Eq l, LV.Storable l)
                   => BitEngine l -- ^ Bit engine for literals.
                   -> BitMemory l -- ^ Memory 
                   -> LLVM.Symbol -- ^ Definition
                   -> LLVM.Ident -- ^ Block identifier
                   -> BitTerm l
bmCodeBlockAddress be m d b = do
  case Map.lookup (d,b) (bmCodeBlockMap m) of
    Nothing -> bmError $ "Failed to find block " ++ show b ++ " in " ++ show d ++ "."
    Just a -> BitTerm (beVectorFromInt be (bmAddrWidth m) a)

-- | Return symbol as given address in memory.
bmCodeLookupDefine :: (Eq l, LV.Storable l) 
   => BitEngine l -> BitMemory l -> BitTerm l -> PartialResult LLVM.Symbol
bmCodeLookupDefine be m (BitTerm a) = do
  case beVectorToMaybeInt be a of
    Nothing -> Indeterminate
    Just v -> 
      case loadDef (bmStorage m) (bmAddrWidth m) v of
        Nothing -> Invalid
        Just d -> Result d

--  SBE Definition {{{1

newtype BitIO l a = BitIO { runBitIO :: IO a }
  deriving (Monad, MonadIO)

type instance SBETerm (BitIO l) = BitTerm l
type instance SBEMemory (BitIO l) = BitMemory l

sbeBitBlast :: (Eq l, LV.Storable l) => LLVMContext -> BitEngine l -> SBE (BitIO l)
sbeBitBlast lc be = SBE
  { termInt  = (return . BitTerm) `c2` beVectorFromInt be
  , termBool = return . BitTerm . LV.singleton . beLitFromBool be
  , applyIte = undefined
  , applyICmp = undefined
  , applyBitwise = undefined
  , applyArith = undefined
  , memLoad = BitIO `c2` bmLoad lc be
  , memStore = BitIO `c3` bmStore be 
  , memMerge = BitIO `c3` bmMerge be
  , memAddDefine = \m def idl -> do
      let bbcnt = toInteger (length idl)
      let newSpaceReq = 1 + bbcnt
      let codeAddr = bmCodeAddr m
      let newCodeAddr = codeAddr + newSpaceReq
      when (newCodeAddr > bmCodeEnd m) $
        fail "Not enough space in code memory to allocate new definition."
      let insertAddr bmap (bb,idx) = Map.insert (def,bb) (codeAddr + idx) bmap
          updateAddr a | a == codeAddr = MTDefine def
                       | otherwise = MTBlock def (idl !! fromInteger (a - codeAddr))
          newStorage = setBytes (bmAddrWidth m) codeAddr newCodeAddr updateAddr (bmStorage m)
      return ( BitTerm (beVectorFromInt be (bmAddrWidth m) codeAddr)
             , m { bmStorage = newStorage
                 , bmCodeAddr = newCodeAddr
                   -- Include new addresses in memory
                 , bmCodeBlockMap =
                     foldl insertAddr
                           (bmCodeBlockMap m)
                           (idl `zip` [1..bbcnt])
                 }
             )
  , codeBlockAddress = return `c3` bmCodeBlockAddress be
  , codeLookupDefine = return `c2` bmCodeLookupDefine be
  , stackAlloca = return `c4` bmStackAlloca lc be
  , stackPushFrame = return . bmStackPush
  , stackPopFrame = return . bmStackPop
  }

overlap :: (Addr,Addr) -> (Addr,Addr) -> Bool 
overlap (s1,e1) (s2,e2) = if s1 <= s2 then s2 < e1 else s1 < e2

sbeBitBlastMem :: LV.Storable l
               => Int -- ^ Width of address in bits.
               -> (Addr, Addr) -- ^ Stack start and end address
               -> (Addr, Addr) -- ^ Code start and end address
               -> (Addr, Addr) -- ^ Heap start and end address
               -> BitMemory l
sbeBitBlastMem w (stackStart, stackEnd) (codeStart, codeEnd) (heapStart, heapEnd)
  | (stackStart, stackEnd) `overlap` (codeStart, codeEnd)
  = bmError "Stack and code segments overlap."
  | (stackStart, stackEnd) `overlap` (heapStart, heapEnd)
  = bmError "Stack and heap segments overlap."
  | (codeStart, codeEnd) `overlap` (heapStart, heapEnd)
  = bmError "Code and heap segments overlap."
  | otherwise =
      BitMemory {
          bmAddrWidth = w
        , bmStorage = MTUnallocated
        , bmStackAddr = stackStart
        , bmStackEnd = stackEnd
        , bmStackFrames = []
        , bmCodeAddr = codeStart
        , bmCodeEnd = codeEnd
        , bmCodeBlockMap = Map.empty
        , bmFreeLists = initFreeList heapStart heapEnd
        }
