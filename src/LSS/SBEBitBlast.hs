{- |
Module           : $Header$
Description      : A symbolic backend that bitblasts
Stability        : provisional
Point-of-contact : atomb, jhendrix
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module LSS.SBEBitBlast
  ( module LSS.SBEInterface
  , BitTerm
  , BitTermClosed(..)
  , LLVMContext(..)
  , sbeBitBlast
  , sbeBitBlastMem
  , liftSBEBitBlast
  ) where

import           Control.Applicative       ((<$>))
import           Control.Exception         (assert)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bits
import           Data.Int
import           Data.Map                  (Map)
import           Debug.Trace
import           LSS.Execution.Utils
import           LSS.SBEInterface
import           Text.PrettyPrint.HughesPJ
import           Verinf.Symbolic.Common    (ConstantProjection(..), createBitEngine)
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

--------------------------------------------------------------------------------
-- Symbolic backend

newtype BitTerm l = BitTerm { btVector :: LV.Vector l }
newtype BitTermClosed l = BitTermClosed (BitEngine l, BitTerm l)

instance (Eq l, LV.Storable l) => S.PrettyTerm (BitTermClosed l) where
  prettyTermWithD _ppconf ct@(BitTermClosed (be, BitTerm bv)) =
    let str      = LV.foldr (\lit acc -> acc ++ [toChar lit]) "" bv
        toChar x = if x == beFalse be then '0' else if x == beTrue be then '1' else '?'
    in
      if 1 == LV.length bv
      then text (maybe "?:[1]" show $ getBool ct)
      else text str <> colon <>  brackets (text $ show $ LV.length bv)
           <+> maybe empty (parens . integer) (getSVal ct)

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
          | otherwise -> error $ "BitTermClosed/getSVal: unsupported integer width " ++ show w
        where
          i8 :: Integer -> Int8
          i8 x = fromIntegral x :: Int8

  getUVal (BitTermClosed (be, t)) = snd <$> beVectorToMaybeInt be (btVector t)

  getBool (BitTermClosed (be, t)) =
    case beVectorToMaybeInt be (btVector t) of
      Nothing     -> Nothing
      Just (w, v) -> Just $ case w of
                              1 -> toEnum (fromIntegral v) :: Bool
                              _ -> error "BitTermClosed/getBool: term bit width not 1"

  termConst = error "ConstantProjection (BitTermClosed): termConst BitTerm nyi"

  isConst = error "ConstantProjection (BitTermClosed): isConst BitTerm nyi"

-- | Returns number of bytes.
byteSize :: LV.Storable l => LV.Vector l -> Int
byteSize v = LV.length v `shiftR` 3

-- | Slice a vector into a list of vectors, one for each byte.
sliceIntoBytes :: LV.Storable l => LV.Vector l -> V.Vector (LV.Vector l)
sliceIntoBytes v =
   V.map (\i -> LV.slice (i `shiftL` 3) 8 v) (V.enumFromN 0 (byteSize v))

-- | Tree-based data structure for representing value of bytes in memory.
data Storage l
  = SBranch (Storage l) (Storage l) -- ^ Branch falseBranch trueBranch
  | SValue (LV.Vector l) -- ^ SValue validBit definedBit value
  | SDefine LLVM.Symbol -- ^ Memory value for function definition.
  | SBlock LLVM.Symbol LLVM.Ident -- ^ Memory value for block within function.
  | SUninitialized -- ^ A memory byte that has not been initialized by LLVM.
  | SUnallocated -- ^ A memory section that has not been allocated to the program.

-- TODO: Write an alternate pretty printer that only shows address ranges and
-- contents of allocated regions
ppStorage :: (Eq l, LV.Storable l) => BitEngine l -> Storage l -> Doc
ppStorage be (SBranch f t) = text "SBranch" <+> parens (ppStorage be f) <+> parens (ppStorage be t)
ppStorage be (SValue lv) = text "SValue"
                           <+> parens (S.prettyTermWithD S.defaultPPConfig
                                       $ BitTermClosed (be, BitTerm lv))
ppStorage _ (SDefine d) = text ("SDefine " ++ show d)
ppStorage _ (SBlock d b) = text ("SBlock " ++ show d ++ " " ++ show b)
ppStorage _ SUninitialized = text "SUninitialized"
ppStorage _ SUnallocated = text "SUnallocated"

mergeStorage :: (Eq l, LV.Storable l) => BitEngine l -> l -> Storage l -> Storage l -> IO (Storage l)
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

-- | @loadByte be mem ptr@ returns a term representing the value of ptr in @mem@.
loadByte :: (Eq l, LV.Storable l)
         => BitEngine l -> Storage l -> LV.Vector l -> IO (LV.Vector l)
loadByte be mem vi = impl mem (LV.length vi - 1)
  where impl (SBranch f t) i =
          assert (0 <= i && i < LV.length vi) $
            beIteVector be (vi LV.! i) (impl t (i-1)) (impl f (i-1))
        impl (SValue v) _ = return v
        impl (SDefine d) _ =
          bmError $ "Attempt to read address that may point to definition of " ++ show d ++ "."
        impl (SBlock d b) _ =
          bmError $ "Attempt to read address that may point to block " ++ show b ++ " in " ++ show d ++ "."
        impl SUninitialized  _ =
          bmError $ "Attempt to read address that has not been initialized."
        impl SUnallocated _ =
          bmError $ "Attempt to read address that is invalid."

-- | @loadBytes be mem ptr size@ returns term representing all the bits with given size.
loadBytes :: (Eq l, LV.Storable l)
          => BitEngine l -> Storage l -> LV.Vector l -> Integer -> IO (LV.Vector l)
loadBytes be mem vi sz = LV.concat <$> mapM lb [1..sz]
  where lb i = loadByte be mem =<< beAddIntConstant be vi (i-1)

-- | Return storage with individual byte changed.
storeByte :: (Eq l, LV.Storable l)
          => BitEngine l -- ^ Bit engine for bits
          -> Storage l -- ^ Memory to update
          -> LV.Vector l -- ^ Value to write
          -> LV.Vector l -- ^ Address to write to
          -> IO (Storage l)
storeByte be mem new ptr = impl mem (LV.length ptr) (beTrue be)
  where impl (SBranch f t) i c
           | b == beFalse be = do
             fr <- impl f (i-1) c
             return (SBranch fr t)
           | b == beTrue be = do
             tr <- impl t (i-1) c
             return (SBranch f tr)
           | otherwise = do
             tr <- impl t (i-1) =<< beAnd be c b
             fr <- impl f (i-1) =<< beAnd be c (beNeg be b)
             return (SBranch fr tr)
          where b = ptr LV.! (i-1)
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
          bmError $ "Attempt to store to address that is invalid."

storeBytes :: (Eq l, LV.Storable l)
           => BitEngine l
           -> Storage l -- ^ Base storage
           -> LV.Vector l -- ^ Value to store
           -> LV.Vector l -- ^ Address to store value in
           -> IO (Storage l) -- ^ Storage with value written.
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

type Addr = Integer

data BitMemory l = BitMemory {
    -- | Stores state of memory.
    bmStorage :: Storage l
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
    llvmAddrWidthBits :: Int
  , llvmLookupAlias :: LLVM.Ident -> LLVM.Type
  }

resolveType :: LLVMContext -> LLVM.Type -> LLVM.Type
resolveType lc (LLVM.Alias nm) = resolveType lc (llvmLookupAlias lc nm)
resolveType _ tp = tp


-- | Returns size of type in memory.
llvmPrimSizeOf :: LLVM.PrimType -> Integer
llvmPrimSizeOf LLVM.Label = bmError "internal: Cannot get size of label."
llvmPrimSizeOf LLVM.Void = bmError "internal: Cannot get size of void."
llvmPrimSizeOf (LLVM.Integer w) = (toInteger w + 7) `shiftR` 3
llvmPrimSizeOf (LLVM.FloatType LLVM.Float) = 4
llvmPrimSizeOf (LLVM.FloatType LLVM.Double) = 8
llvmPrimSizeOf (LLVM.FloatType LLVM.Fp128) = 16
llvmPrimSizeOf (LLVM.FloatType LLVM.X86_fp80) = 10
llvmPrimSizeOf (LLVM.FloatType LLVM.PPC_fp128) = 16
llvmPrimSizeOf LLVM.X86mmx = bmError "internal: X86MMX memory size is undefined."
llvmPrimSizeOf LLVM.Metadata = bmError "internal: Cannnot get size of metadata."

 -- | Returns number of bits for an LLVM.Type
llvmByteSizeOf :: LLVMContext -> LLVM.Type -> Integer
llvmByteSizeOf lc tp =
  case tp of
    LLVM.PrimType pt -> llvmPrimSizeOf pt
    LLVM.Alias a -> llvmByteSizeOf lc (llvmLookupAlias lc a)
    LLVM.Array l eTp -> toInteger l * llvmByteSizeOf lc eTp
    LLVM.FunTy _retTp _argTpl _ -> bmError "internal: Cannot get size of function type."
    LLVM.PtrTo _tp -> toInteger (llvmAddrWidthBits lc `shiftR` 3)
    --TODO: support alignment based on targetdata string in module.
    LLVM.Struct argTypes -> sum [ llvmByteSizeOf lc atp | atp <- argTypes ]
    LLVM.PackedStruct argTypes -> sum [ llvmByteSizeOf lc atp | atp <- argTypes ]
    LLVM.Vector l pt -> toInteger l * llvmPrimSizeOf pt
    LLVM.Opaque -> bmError "internal: Cannot get size of function type."

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

bmDump :: (Eq l, LV.Storable l) => BitEngine l -> BitMemory l -> IO ()
bmDump be bm = do
  banners $ render $
    text "Memory Model Dump"
    $+$ text "Stack Range:" <+> text (show (bmStackAddr bm, bmStackEnd bm))
    $+$ text "Code Range:" <+> text (show (bmCodeAddr bm, bmCodeEnd bm))
    $+$ text "Frame pointers:" <+> text (show (bmStackFrames bm))
    $+$ ppStorage be (bmStorage bm)

bmLoad :: (Eq l, LV.Storable l)
       => LLVMContext
       -> BitEngine l
       -> BitMemory l
       -> LLVM.Typed (BitTerm l)
       -> IO (BitTerm l)
bmLoad lc be bm ptr
  | LV.length ptrVal /= llvmAddrWidthBits lc = bmError "internal: Illegal pointer given to load"
  | otherwise =
      case resolveType lc (LLVM.typedType ptr) of
        LLVM.PtrTo tp -> do
          bits <-
              loadBytes be (bmStorage bm) ptrVal (llvmByteSizeOf lc tp)
          return (BitTerm (loadPtr tp bits))
        _ -> bmError "Illegal type given to load"
  where BitTerm ptrVal = LLVM.typedValue ptr
        loadPtr (LLVM.PrimType (LLVM.Integer w)) bits = LV.take (fromIntegral w) bits
        loadPtr _ bits = bits

bmStore :: (Eq l, LV.Storable l)
        => LLVMContext
        -> BitEngine l
        -> BitMemory l
        -> LLVM.Typed (BitTerm l)
        -> BitTerm l
        -> IO (BitMemory l)
bmStore lc be bm v (BitTerm ptr) = do
  let BitTerm val = LLVM.typedValue v
      bsz = llvmByteSizeOf lc (LLVM.typedType v)
      extVal = case resolveType lc (LLVM.typedType v) of
                 -- Extend integer types to full width.
                 LLVM.PrimType (LLVM.Integer w) ->
                   val LV.++ LV.replicate (fromIntegral (bsz `shiftL` 3 - toInteger w)) (beFalse be)
                 -- Treat other types as same.
                 _ -> val
  newStorage <- storeBytes be (bmStorage bm) extVal ptr
  return (bm { bmStorage = newStorage })

bmStackAlloca :: (Eq l, LV.Storable l)
              => LLVMContext
              -> BitEngine l
              -> BitMemory l
              -> LLVM.Type
              -> LLVM.Typed (BitTerm l)
              -> Int
              -> (BitTerm l, BitMemory l)
bmStackAlloca lc be bm eltTp typedCnt a =
    ( BitTerm (beVectorFromInt be w res)
    , bm { bmStorage = newStorage, bmStackAddr = newAddr })
  where -- Number of bytes in element type.
        eltSize = llvmByteSizeOf lc eltTp
        BitTerm cntVector = LLVM.typedValue typedCnt
        -- Get number requested.
        cnt = case beVectorToMaybeInt be cntVector of
                Nothing ->
                  bmError $ "Symbolic number of elements requested with alloca;"
                               ++ " when current implementation only supports concrete"
                Just (_,v) -> v
        w = llvmAddrWidthBits lc
        -- Stack pointer adjusted to alignment boundary.
        errorMsg = bmError "Stack limit exceeded in alloca."
        -- Get result pointer
        -- Get new bit memory.
        (res,newStorage,newAddr)
          | bmStackGrowsUp bm =
              let alignedAddr = alignUp (bmStackAddr bm) a
                  newStackAddr = alignedAddr + eltSize * cnt
               in if newStackAddr > bmStackEnd bm
                    then errorMsg
                    else ( alignedAddr
                         , setBytes w alignedAddr newStackAddr (\_ -> SUninitialized) (bmStorage bm)
                         , newStackAddr)
          | otherwise =
              let sz = eltSize * cnt
                  alignedAddr = alignDn (bmStackAddr bm - sz) a
               in if alignedAddr < bmStackEnd bm
                    then errorMsg
                    else ( alignedAddr
                         , setBytes w alignedAddr (alignedAddr + sz) (\_ -> SUninitialized) (bmStorage bm)
                         , alignedAddr)

-- | Push stack frame to memory.
bmStackPush :: BitMemory l -> BitMemory l
bmStackPush bm = bm { bmStackFrames = bmStackAddr bm : bmStackFrames bm }

-- | Pop stack frame in memory and invalidate old addresses.
bmStackPop :: LLVMContext -> BitMemory l -> BitMemory l
bmStackPop _ BitMemory { bmStackFrames = [] } =
  bmError "internal: Attempted to pop stack frame from memory when no stack frames have been pushed."
bmStackPop lc bm@BitMemory { bmStackFrames = f : fl } =
  bm { bmStorage =
         let (l,h) = if bmStackGrowsUp bm then (f, bmStackAddr bm) else (bmStackAddr bm, f)
          in setBytes (llvmAddrWidthBits lc) l h (\_ -> SUnallocated) (bmStorage bm)
     , bmStackAddr = f
     , bmStackFrames = fl
     }

bmMerge :: (Eq l, LV.Storable l)
        => BitEngine l
        -> BitTerm l
        -> BitMemory l
        -> BitMemory l
        -> IO (BitMemory l)
bmMerge be (BitTerm cv) m m'
  | LV.length cv == 1 = do
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
        { bmStorage = newStorage
        , bmStackAddr = bmStackAddr m
        , bmStackEnd = bmStackEnd m
        , bmStackFrames = bmStackFrames m
        , bmCodeAddr = bmCodeAddr m
        , bmCodeEnd = bmCodeEnd m
        , bmCodeBlockMap = bmCodeBlockMap m
        , bmFreeLists = bmFreeLists m
        }
  | otherwise = bmError "internal: Malformed condition given to bmMerge."

bmMemAddDefine :: (Eq l, LV.Storable l)
                => LLVMContext
                -> BitEngine l -- ^ Bit engine for literals.
                -> BitMemory l -- ^ Memory
                -> LLVM.Symbol -- ^ Definition
                -> [LLVM.Ident] -- ^ Identifiers for blocks
                -> (BitTerm l, BitMemory l)
bmMemAddDefine lc be m def idl
    | newCodeAddr > bmCodeEnd m
    = bmError "Not enough space in code memory to allocate new definition."
    | otherwise
    = ( BitTerm (beVectorFromInt be (llvmAddrWidthBits lc) codeAddr)
      , m { bmStorage = newStorage
          , bmCodeAddr = newCodeAddr
            -- Include new addresses in memory
          , bmCodeBlockMap =
              foldl insertAddr
                    (bmCodeBlockMap m)
                    (idl `zip` [1..bbcnt])
          }
      )
  where bbcnt = toInteger (length idl)
        newSpaceReq = 1 + bbcnt
        codeAddr = bmCodeAddr m
        newCodeAddr = codeAddr + newSpaceReq
        insertAddr bmap (bb,idx) = Map.insert (def,bb) (codeAddr + idx) bmap
        updateAddr a | a == codeAddr = SDefine def
                     | otherwise     = SBlock def (idl !! fromInteger (a - codeAddr - 1))
        newStorage = setBytes (llvmAddrWidthBits lc) codeAddr newCodeAddr updateAddr (bmStorage m)

bmCodeBlockAddress :: (Eq l, LV.Storable l)
                   => LLVMContext
                   -> BitEngine l -- ^ Bit engine for literals.
                   -> BitMemory l -- ^ Memory
                   -> LLVM.Symbol -- ^ Definition
                   -> LLVM.Ident -- ^ Block identifier
                   -> BitTerm l
bmCodeBlockAddress lc be m d b = do
  case Map.lookup (d,b) (bmCodeBlockMap m) of
    Nothing -> bmError $ "Failed to find block " ++ show b ++ " in " ++ show d ++ "."
    Just a -> BitTerm (beVectorFromInt be (llvmAddrWidthBits lc) a)

-- | Return symbol as given address in memory.
bmCodeLookupDefine :: (Eq l, LV.Storable l)
                   => LLVMContext
                   -> BitEngine l
                   -> BitMemory l
                   -> BitTerm l
                   -> PartialResult LLVM.Symbol
bmCodeLookupDefine lc be m (BitTerm a) = do
  case beVectorToMaybeInt be a of
    Nothing -> Indeterminate
    Just (_,v) ->
      case loadDef (bmStorage m) (llvmAddrWidthBits lc) v of
        Nothing -> Invalid
        Just d -> Result d

-- Arithmetic and logical operations {{{1

bitIte :: (LV.Storable l, Eq l) =>
          BitEngine l -> BitTerm l -> BitTerm l -> BitTerm l
       -> BitIO l (BitTerm l)
bitIte be (BitTerm c) (BitTerm a) (BitTerm b) = BitIO $ BitTerm <$> do
  let zero = LV.replicate (LV.length c) (beFalse be)
  cbit <- beEqVector be c zero
  beIteVector be cbit (return b) (return a)

bitICmp :: (LV.Storable l, Eq l) =>
           BitEngine l -> LLVM.ICmpOp
        -> BitTerm l -> BitTerm l
        -> BitIO l (BitTerm l)
bitICmp be op (BitTerm a) (BitTerm b) =
  BitIO $ (BitTerm . LV.singleton) <$> f be a b
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
           -> BitIO l (BitTerm l)
bitBitwise be op (BitTerm a) (BitTerm b) = BitIO $ BitTerm <$> f be a b
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
         -> BitIO l (BitTerm l)
bitArith be op (BitTerm a) (BitTerm b) = BitIO $ BitTerm <$> f be a b
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
        -> BitIO l (BitTerm l)
bitConv be op (BitTerm x) (LLVM.PrimType (LLVM.Integer (fromIntegral -> w))) =
  BitIO $ BitTerm <$> f be w x
  where f = case op of
              LLVM.Trunc -> assert (w < LV.length x) beTrunc
              LLVM.ZExt  -> assert (w > LV.length x) beZext
              _ -> bmError $ "Unsupported conv op: " ++ show op
bitConv _ _ _ ty = bmError $ "Unsupported conv target type: " ++ show (LLVM.ppType ty)

--  SBE Definition {{{1

newtype BitIO l a = BitIO { liftSBEBitBlast :: IO a }
  deriving (Monad, MonadIO, Functor)

type instance SBETerm (BitIO l)       = BitTerm l
type instance SBEClosedTerm (BitIO l) = BitTermClosed l
type instance SBEMemory (BitIO l)     = BitMemory l

sbeBitBlast :: (S.PrettyTerm (BitTermClosed l), Eq l, LV.Storable l)
  => LLVMContext -> BitEngine l -> SBE (BitIO l)
sbeBitBlast lc be = sbe
  where sbe = SBE
              { termInt          = (return . BitTerm) `c2` beVectorFromInt be
              , termBool         = return . BitTerm . LV.singleton . beLitFromBool be
              , applyIte         = bitIte be
              , applyICmp        = bitICmp be
              , applyBitwise     = bitBitwise be
              , applyArith       = bitArith be
              , applyConv        = bitConv be
              , closeTerm        = BitTermClosed . (,) be
              , prettyTermD      = S.prettyTermD . closeTerm sbe
              , memDump          = BitIO . bmDump be
              , memLoad          = BitIO `c2` bmLoad lc be
              , memStore         = BitIO `c3` bmStore lc be
              , memMerge         = BitIO `c3` bmMerge be
              , memAddDefine     = return `c3` bmMemAddDefine lc be
              , codeBlockAddress = return `c3` bmCodeBlockAddress lc be
              , codeLookupDefine = return `c2` bmCodeLookupDefine lc be
              , stackAlloca      = return `c4` bmStackAlloca lc be
              , stackPushFrame   = return . bmStackPush
              , stackPopFrame    = return . bmStackPop lc
              , writeAiger       = \f t -> BitIO $ beWriteAigerV be f (btVector t)
              }

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

sbeBitBlastMem :: LV.Storable l
               => (Addr, Addr) -- ^ Stack start and end address
               -> (Addr, Addr) -- ^ Code start and end address
               -> (Addr, Addr) -- ^ Heap start and end address
               -> BitMemory l
sbeBitBlastMem stack code heap
  | decreasing code = bmError "Code segment start and end are in wrong order."
  | decreasing heap = bmError "Heap segment start and end are in wrong order."
  | norm stack `overlap` code = bmError "Stack and code segments overlap."
  | norm stack `overlap` heap = bmError "Stack and heap segments overlap."
  | code `overlap` heap = bmError "Code and heap segments overlap."
  | otherwise =
      BitMemory {
          bmStorage = SUnallocated
        , bmStackAddr = start stack
        , bmStackEnd = end stack
        , bmStackFrames = []
        , bmCodeAddr = start code
        , bmCodeEnd = end code
        , bmCodeBlockMap = Map.empty
        , bmFreeLists = initFreeList (start heap) (end heap)
        }

testSBEBitBlast :: IO ()
testSBEBitBlast = do
  let lc = LLVMContext 4 undefined
  be <- createBitEngine
  let sbe = sbeBitBlast lc be
  let m0 = sbeBitBlastMem (0x10,0x0) (0x0,0x0)  (0x0,0x0)
  liftSBEBitBlast $ do
    let i32 = LLVM.PrimType (LLVM.Integer 32)
    let ptr = LLVM.PtrTo
    l1 <- termInt sbe 32 1
    (sp,m1) <- stackAlloca sbe m0 i32 (LLVM.Typed i32 l1) 1
    liftIO $ putStrLn (render (ppStorage be (bmStorage m1)))
    liftIO $ putStrLn $ show $ beVectorToMaybeInt be (btVector sp)
    lv <- termInt sbe 32 0x12345678
    m2 <- memStore sbe m1 (LLVM.Typed i32 lv) sp
    BitTerm actualValue <- memLoad sbe m2 (LLVM.Typed (ptr i32) sp)
    liftIO $ putStrLn $ show $ (0x12345678 :: Integer)
    liftIO $ putStrLn $ show $ beVectorToMaybeInt be actualValue
    return ()

__nowarn_unused :: a
__nowarn_unused = undefined testSBEBitBlast allocBlock trace
