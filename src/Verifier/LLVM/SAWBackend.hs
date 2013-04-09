{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Verifier.LLVM.SAWBackend
  ( SAWBackend
  , SAWMemory
  , createSAWBackend
  ) where

import Control.Applicative hiding (empty)
import Control.Exception (assert)
import Control.Lens hiding (op, iact)
import Control.Monad
import Control.Monad.State
import Data.Bits
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V

import Verifier.LLVM.AST
import Verifier.LLVM.Backend
import Verifier.LLVM.MemModel

import Verifier.SAW
import Verifier.SAW.ParserUtils
import Verifier.SAW.Prelude

#if __GLASGOW_HASKELL__ < 706
-- | Strict version of modifyIORef
-- Added for compatibility with GHC base 4.5 and 4.6
modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' r f = do
  v <- readIORef r
  writeIORef r $! f v 
#endif


nyi :: String -> a
nyi nm = error $ "Not yet implemented: " ++ show nm

scBitwidth :: SharedContext s -> BitWidth -> IO (SharedTerm s)
scBitwidth sc w = scNat sc (toInteger w)

scBitvectorType :: SharedContext s -> SharedTerm s -> IO (SharedTerm s)
scBitvectorType sc wt = ($ wt) =<< scApplyPreludeBitvector sc

scFreshInt :: SharedContext s -> BitWidth -> IO (SharedTerm s)
scFreshInt sc w =
  scFreshGlobal sc "_" =<< scBitvectorType sc =<< scBitwidth sc w

preludeTrueTermF :: TermF t
preludeTrueTermF = FTermF $ CtorApp (mkIdent preludeModuleName "True") []

preludeFalseTermF :: TermF t
preludeFalseTermF = FTermF $ CtorApp (mkIdent preludeModuleName "False") []

preludeBVNatTermF :: TermF t
preludeBVNatTermF = FTermF $ GlobalDef (mkIdent preludeModuleName "bvNat")

termAsBool :: SharedTerm s -> Maybe Bool
termAsBool (asTermF -> Just a)
  | a == preludeTrueTermF  = return True
  | a == preludeFalseTermF = return False
termAsBool _ = Nothing

asUnsignedBitvector :: BitWidth -> SharedTerm s -> Maybe Integer
asUnsignedBitvector w s2 = do
  (s1, vt) <- asApp s2
  (s0, wt) <- asApp s1
  when (asTermF  s0 /= Just preludeBVNatTermF) Nothing
  when (asNatLit wt /= Just (toInteger w)) Nothing
  asNatLit vt

scBool :: SharedContext s -> Bool -> IO (SharedTerm s)
scBool sc True = scApplyPreludeTrue sc
scBool sc False = scApplyPreludeFalse sc

scFloat :: SharedContext s -> Float -> IO (SharedTerm s)
scFloat sc v = scTermF sc (FTermF (FloatLit v))

scDouble :: SharedContext s -> Double -> IO (SharedTerm s)
scDouble sc v = scTermF sc (FTermF (DoubleLit v))

-- | Create a vector from a term representing its element types and the element.
scVecLit :: SharedContext s
         -> SharedTerm s -- ^ Type of vector elments.
         -> V.Vector (SharedTerm s) -- ^ Elements
         -> IO (SharedTerm s)
scVecLit sc tp v = scTermF sc (FTermF (ArrayValue tp v))

$(runDecWriter $ do
    prelude <- defineImport [|preludeModule|] preludeModule
    llvm <- defineModuleFromFile [prelude] "llvmModule" "saw/LLVM.sawcore"
    declareDefTermF prelude "ite"
    declareDefTermF llvm "llvmAdd"
    declareSharedModuleFns "LLVM" (decVal llvm)
 )

scResizeTerm :: SharedContext s
             -> BitWidth -- ^ Input width
             -> (BitWidth, SharedTerm s) -- ^ Result bitwith and term representing it.
             -> SharedTerm s
             -> IO (SharedTerm s)
scResizeTerm sc iw (rw,rt) v
  | iw < rw = do
      fn <- scApplyLLVMLlvmZExt sc
      dt <- scBitwidth sc (rw - iw)
      fn dt rt v
  | iw > rw = do
      fn <- scApplyLLVMLlvmTrunc sc
      dt <- scBitwidth sc (iw - rw)
      fn dt rt v
  | otherwise = return v

-- | Create a bitvector from a constant.
scBitvectorConst :: SharedContext s
                 -> BitWidth -- ^ Result width with corresponding term.
                 -> Integer -- ^ Value of bitvector.
                 -> IO (SharedTerm s)
scBitvectorConst sc w v = do
  wt <- scBitwidth sc w
  scBitvectorConst' sc (w,wt) v

-- | Create a bitvector from a constant.
scBitvectorConst' :: SharedContext s
                  -> (BitWidth, SharedTerm s) -- ^ Result width with corresponding term.
                  -> Integer -- ^ Value of bitvector.
                  -> IO (SharedTerm s)
scBitvectorConst' sc (w,wt) v = do
  cfn <- scApplyLLVMLlvmIntConstant sc 
  cfn wt =<< scNat sc (v `mod` 2^(toInteger w))

{-
Essay on how allocations are done:

We represent the result of an allocation as a fresh variable.  Arithmetic
on terms with these variables is designed to normalize the term into the
form "Var + offset" where feasible.

There are implicit assumptions on disjoinness between allocations.


Operations that attempt to exploit this normalization include:
  Integer arithmetic add, sub.
  Pointer add.
  UAddWithOverflow.
  Integer comparison operations.
-}

scFreshPtr :: SharedContext s -> DataLayout -> IO (SharedTerm s)
scFreshPtr sc dl = scFreshInt sc (ptrBitwidth dl)

{-
-- | A state of memory as of a stack push, branch, or merge.
data MemState d
  = -- | Represents initial memory and changes since then.
    -- Changes are stored in order, with more recent changes closer to the head
    -- of the list.
    EmptyMem [d]
    -- | Represents a push of a stack frame,  andchanges since that stack push.
    -- Changes are stored in order, with more recent changes closer to the head
    -- of the list.
  | StackFrame  [d] (MemState d) 
    -- | Represents a push of a branch frame, and changes since that branch.
    -- Changes are stored in order, with more recent changes closer to the head
    -- of the list.
  | BranchFrame [d] (MemState d)

-- | Lens to changes in mem state since last branch or stack push event.
lastMemChanges :: Simple Lens (MemState d) [d]
lastMemChanges f (EmptyMem l)      = EmptyMem <$> f l 
lastMemChanges f (StackFrame  l m) = flip StackFrame  m <$> f l
lastMemChanges f (BranchFrame l m) = flip BranchFrame m <$> f l

-- | Lens to previous memory in last branch or stack push event.
prevMem :: Simple Traversal (MemState d) (MemState d)
prevMem _ m@EmptyMem{} = pure m
prevMem f (StackFrame  l m) = StackFrame l  <$> f m
prevMem f (BranchFrame l m) = BranchFrame l <$> f m

-- | Returns all changes in memstate as a list.
memChanges :: MemState d -> [d]
memChanges m = m^.lastMemChanges ++ maybe [] memChanges (m^?prevMem) 

-- | Add a memory change to the state.
consMemChange :: d -> MemState d -> MemState d
consMemChange d = over lastMemChanges (d:)

data MemAlloc s
    -- | Represents a stack allocation with base, alignment of base, and number of bytes.
  = StackAlloc (SharedTerm s) Alignment (SharedTerm s)
    -- | Represents a heap allocation with base, alignment of base, and number of bytes.
  | HeapAlloc (SharedTerm s) Alignment (SharedTerm s)
  | AllocMerge (SharedTerm s) [MemAlloc s] [MemAlloc s]

type MemAllocState s = MemState (MemAlloc s)

data MemWrite s
    -- | @MemCopy dstRange src len@ represents a copy from [src..len) to
    -- dstRange.  Both source and dest can be assumed to satisfy the
    -- given alignment.  The length of each range is guaranteed to be the same.
  = MemCopy (StoreRange s) (SharedTerm s) 
    -- | Memstore is given address, type of value, and value.
  | MemStore (StoreRange s) MemType (SharedTerm s)
  | WriteMerge (SharedTerm s) [MemWrite s] [MemWrite s]

type MemWriteState s = MemState (MemWrite s)
-}

-- | Set of shared term variables that refer to allocations.
type Allocations s = IORef (Set (SharedTerm s))

asApp2 :: SharedTerm s -> Maybe (SharedTerm s, SharedTerm s, SharedTerm s)
asApp2 t = do
  (t1,a2) <- asApp t
  (t0,a1) <- asApp t1
  return (t0,a1,a2)

data SAWBackendState s =
       SBS { sbsDataLayout :: DataLayout
           , sbsContext :: SharedContext s
           , sbsAllocations :: Allocations s
             -- | Width of pointers in bits as a nat.
           , sbsPtrWidth :: SharedTerm s
             -- | Function for adding two pointers.
           , sbsPtrAddFn :: SharedTerm s
             -- | LLVM Type of a pointer
           , sbsPtrType    :: SharedTerm s
             -- | LLVM Type of floats.
           , sbsFloatType  :: SharedTerm s
             -- | LLVM Type for double
           , sbsDoubleType :: SharedTerm s
              -- | Creates LLVM type for arrays
           , sbsArrayTypeFn :: SharedTerm s -> SharedTerm s -> IO (SharedTerm s)
              -- | Creates LLVM type for vectors
           , sbsVecTypeFn :: SharedTerm s -> SharedTerm s -> IO (SharedTerm s)
              -- | Creates LLVM type for structs
           , sbsStructTypeFn :: SharedTerm s -> SharedTerm s -> IO (SharedTerm s)
              -- | Fieldtype constant
           , sbsFieldType :: SharedTerm s
           , smGenerator :: TermGenerator IO (SharedTerm s) (SharedTerm s) (SharedTerm s)
           }

mkBackendState :: forall s . DataLayout -> SharedContext s -> IO (SAWBackendState s)
mkBackendState dl sc = do
  ptrWidth <- scBitwidth sc (ptrBitwidth dl)
  addFn <- join $ scApplyLLVMLlvmAdd sc ?? ptrWidth
  subFn <- join $ scApplyLLVMLlvmSub sc ?? ptrWidth
  allocs <- newIORef Set.empty

  -- LLVM Type imports
  ptrType <- join $ scApplyLLVMPtrType sc <*> scNat sc (toInteger (dl^.ptrSize))
  floatType  <- scApplyLLVMFloatType sc
  doubleType <- scApplyLLVMDoubleType sc
  arrayTypeFn  <- scApplyLLVMArrayType sc 
  vecTypeFn    <- scApplyLLVMVectorType sc 
  structTypeFn <- scApplyLLVMStructType sc
  fieldType <- scApplyLLVMFieldType sc

  let decomposePtr :: SharedTerm s -> IO (Maybe (SharedTerm s, SharedTerm s))
      decomposePtr ptr =
        case asApp2 ptr of
          Just (f,b,o) | f == addFn -> do
            s <- readIORef allocs
            return $ if Set.member b s then Just (b,o) else Nothing
          _ -> return Nothing
      decomposeOffset = scIntAsConst' sc ptrWidth

  t <- scApplyPreludeTrue  sc
  f <- scApplyPreludeFalse sc
  let addPtr x y = do
        mx <- decomposePtr x
        let addPrim = scApply2 sc addFn
        case mx of
          Just (b,o) -> addPrim b =<< addPrim o y
          Nothing -> do
            my <- decomposePtr y
            case my of
              Just (b,o) -> addPrim b =<< addPrim x o
              Nothing -> addPrim x y
  let subPtr x y = do
        mx <- decomposePtr x
        my <- decomposePtr y
        let subPrim = scApply2 sc subFn
        case (,) <$> mx <*> my of
          Just ((bx,ox),(by,oy)) | bx == by -> subPrim ox oy
          _ -> subPrim x y

  muxOp <- scTermF sc iteTermF

  andFn <- scApplyPreludeAnd sc
  orFn  <- scApplyPreludeOr  sc
  boolMuxOp <- join $ scApply sc muxOp <$> scPreludeBool sc

  let mkTypeTerm :: Type -> IO (SharedTerm s)
      mkTypeTerm _ = undefined
  
  intToFloat  <- scApplyLLVMLlvmIntToFloat sc
  intToDouble <- scApplyLLVMLlvmIntToDouble sc

  appendInt <- scApplyLLVMLlvmAppendInt sc
  let tg = TG { tgPtrWidth = dl^.ptrSize
              , tgPtrDecompose = \ptr -> do
                  mr <- decomposePtr ptr
                  case mr of
                    Nothing -> return $ Symbolic ptr
                    Just (b,o) -> do
                      mo <- decomposeOffset o
                      return $ case mo of
                                 Just o' -> ConcreteOffset b o'
                                 Nothing -> SymbolicOffset b o
              , tgPtrSizeDecompose = decomposeOffset
              , tgConstPtr = scBitvectorConst' sc (ptrBitwidth dl, ptrWidth) . fromIntegral
              , tgAddPtr = addPtr
              , tgCheckedAddPtr = \x y -> (t,) <$> addPtr x y
              , tgSubPtr = subPtr

              , tgTrue = t
              , tgFalse = f
              , tgEq = \x y -> undefined x y
              , tgLe = \x y -> undefined x y
              , tgAnd = andFn
              , tgOr  = orFn
              , tgMuxCond = scApply3 sc boolMuxOp

              , tgConstBitvector = \w -> scBitvectorConst sc (8*fromIntegral w)
              , tgConstFloat  = scFloat sc
              , tgConstDouble = scDouble sc
              , tgApplyCtorF = \vcf ->
                 case vcf of
                   ConcatBV xw x yw y -> do
                     xwt <- scNat sc (toInteger xw)
                     ywt <- scNat sc (toInteger yw)
                     case dl^.intLayout of
                       BigEndian    -> appendInt xwt x ywt y
                       LittleEndian -> appendInt ywt y xwt x         
                   BVToFloat x -> intToFloat x 
                   BVToDouble x -> intToDouble x
                   ConsArray tp x n y -> do
                     consFn <- scApplyPreludeConsVec sc
                     tpt <- mkTypeTerm tp
                     nt <- scNat sc n
                     consFn tpt x nt y
                   AppendArray tp xn x yn y -> nyi ""
                   SingletonArray tp x -> nyi ""
                   MkArray tp v -> nyi ""
                   MkStruct v -> nyi ""
              , tgApplyViewF = nyi ""
              , tgMuxTerm = \c tp x y -> do
                  tpt <- mkTypeTerm tp
                  scApply4 sc muxOp tpt c x y
              }

  return SBS { sbsDataLayout = dl
             , sbsContext = sc
             , sbsAllocations = allocs
             , sbsPtrWidth = ptrWidth
             , sbsPtrAddFn = addFn
             , sbsPtrType      = ptrType
             , sbsFloatType    = floatType
             , sbsDoubleType   = doubleType
             , sbsArrayTypeFn  = arrayTypeFn
             , sbsVecTypeFn    = vecTypeFn
             , sbsStructTypeFn = structTypeFn
             , sbsFieldType    = fieldType
             , smGenerator = tg
             }

sbsMkPtrConstant :: SAWBackendState s -> Integer -> IO (SharedTerm s)
sbsMkPtrConstant sbs =
  scBitvectorConst' (sbsContext sbs)
                    (ptrBitwidth (sbsDataLayout sbs), sbsPtrWidth sbs)

-- | Attempts to decompose a term into an allocation and an offset.
sbsDecomposePtr :: SAWBackendState s -> SharedTerm s -> IO (Maybe (SharedTerm s, SharedTerm s))
sbsDecomposePtr sbs ptr =
  case asApp2 ptr of
    Just (f,b,o) | f == sbsPtrAddFn sbs -> do
      s <- readIORef (sbsAllocations sbs)
      return $ if Set.member b s then Just (b,o) else Nothing
    _ -> return Nothing

-- | Adds two terms together (which must be pointer-sized integers).
sbsPtrAdd :: SAWBackendState s -> SharedTerm s -> SharedTerm s -> IO (SharedTerm s)
sbsPtrAdd = tgAddPtr . smGenerator

sbsPtrSub :: SAWBackendState s -> SharedTerm s -> SharedTerm s -> IO (SharedTerm s)
sbsPtrSub = tgSubPtr . smGenerator


-- | Attempt to parse the term as a constant integer.
sbsIntAsConst :: SAWBackendState s -> BitWidth -> SharedTerm s -> IO (Maybe Integer)
sbsIntAsConst sbs w t = scBitwidth sc w >>= \wt -> scIntAsConst' sc wt t
  where sc = sbsContext sbs

-- | Attempt to parse the term as a constant integer.
sbsPtrOffsetAsConst :: SAWBackendState s -> SharedTerm s -> IO (Maybe Integer)
sbsPtrOffsetAsConst sbs = scIntAsConst' (sbsContext sbs) (sbsPtrWidth sbs)

-- | Attempt to parse the second term as a constant integer.
-- The first term is the width of the term.
scIntAsConst' :: SharedContext s -> SharedTerm s -> SharedTerm s -> IO (Maybe Integer)
scIntAsConst' sc w t =
  fmap asNatLit $ join $
    scApplyLLVMLlvmIntValueNat sc ?? w ?? t

type AllocInfo s = Maybe (SharedTerm s, SharedTerm s, SharedTerm s)

-- | Attempt to decompose range.
sbsAllocInfo :: SAWBackendState s -> SharedTerm s -> SharedTerm s -> IO (AllocInfo s)
sbsAllocInfo sbs ptr sizeTerm = do
  -- Attempt to decompose pointer.
  dptr <- sbsDecomposePtr sbs ptr
  -- Get allocation info if pointer can be decomposed
  case dptr of
    Just (b,o) -> do
      (\e -> Just (b,o,e)) <$> sbsPtrAdd sbs o sizeTerm
    Nothing -> return Nothing

{-
data LoadRange s =
    LoadRange { lrPtr :: SharedTerm s
                -- | End of shared term.
              , lrEnd :: SharedTerm s
              , lrAlloc :: AllocInfo s
              , lrStartOffsetAsConst :: Maybe Integer
              , lrEndOffsetAsConst :: Maybe Integer
              , lrType :: MemType
                -- | Offset since load defined.
              , lrOffset :: Int
              , lrBaseAlignment :: Alignment
              }

mkLoadRange :: SAWBackendState s
            -> MemType -> SharedTerm s -> Int -> Alignment -> IO (LoadRange s)
mkLoadRange sbs tp ptr o a = do
  let sz = toInteger (memTypeSize (sbsDataLayout sbs) tp)
  -- Get size of term.
  sizeTerm <- sbsMkPtrConstant sbs sz
  -- Get end pointer
  endPtr <- sbsPtrAdd sbs ptr sizeTerm
  -- Attempt to decompose pointer.
  dptr <- sbsDecomposePtr sbs ptr
  -- Get allocation info if pointer can be decomposed
  alloc <- sbsAllocInfo sbs ptr sizeTerm
  -- Get start offset if pointer can be decomposed
  s <- join <$> traverse (sbsPtrOffsetAsConst sbs . view _2) dptr
  -- Return final load range.
  return $ LoadRange { lrPtr = ptr
                     , lrEnd = endPtr
                     , lrAlloc = alloc
                     , lrStartOffsetAsConst = s
                     , lrEndOffsetAsConst = (sz+) <$> s
                     , lrType = tp
                     , lrOffset = o
                     , lrBaseAlignment = a
                     }

-- | Size of load range in bytes.
lrSize :: DataLayout -> LoadRange s -> Integer
lrSize dl lr = toInteger (memTypeSize dl (lrType lr))

-- | @subrange ptr off tp@ updates the range to point to a term of type tp at (ptr+off).
subrange :: SAWBackendState s -> LoadRange s -> Int -> MemType -> IO (LoadRange s)
subrange sbs lr o tp = do
  ptr <- sbsPtrAdd sbs (lrPtr lr) =<< sbsMkPtrConstant sbs (toInteger o)
  mkLoadRange sbs tp ptr (lrOffset lr + o) (lrBaseAlignment lr)
-}

adjustAlignment :: Int -> Alignment -> Alignment
adjustAlignment off a = checkAlign 0
  where checkAlign i | i == a = i
                       -- Stop if offset is set at this bit. 
                     | off `testBit` fromIntegral i = i
                     | otherwise = checkAlign (i+1)

{-
-- | Returns alignment for load range.
lrAlignment :: LoadRange s -> Alignment
lrAlignment lr = adjustAlignment (lrOffset lr) (lrBaseAlignment lr)
-}

{-
-- | Denotes a contiguous range of memory.
data StoreRange s = StoreRange { srPtr   :: SharedTerm s
                               , srEnd :: SharedTerm s
                               , srAlloc :: Maybe (SharedTerm s, SharedTerm s, SharedTerm s)
                               , srStartOffsetAsConst :: Maybe Integer
                               , srSizeAsConst :: Maybe Integer
                               , srEndOffsetAsConst :: Maybe Integer
                                 -- | Offset since load defined.
                               , srOffset :: Int
                               , srBaseAlignment :: Alignment
                               }


mkStoreRange :: SAWBackendState s
             -> SharedTerm s
             -> SharedTerm s
             -> Alignment
             -> IO (StoreRange s)
mkStoreRange sbs ptr sizeTerm a = do
  -- Get end pointer
  endPtr <- sbsPtrAdd sbs ptr sizeTerm
  -- Get allocation info if pointer can be decomposed
  alloc <- sbsAllocInfo sbs ptr sizeTerm
  -- Get start offset if pointer can be decomposed
  s <- join <$> traverse (sbsPtrOffsetAsConst sbs . view _2) alloc
  -- Get size of pointer
  sz <- sbsPtrOffsetAsConst sbs sizeTerm
  -- Return final load range.
  return $ StoreRange { srPtr = ptr
                      , srEnd = endPtr
                      , srAlloc = alloc
                      , srStartOffsetAsConst = s
                      , srSizeAsConst = sz
                      , srEndOffsetAsConst = (+) <$> s <*> sz
                      , srOffset = 0
                      , srBaseAlignment = a
                      }

-- | Returns true if store size is at least a specified amount.
srSizeAtLeast :: StoreRange s -> Integer -> Bool
srSizeAtLeast sr sz = maybe False (>= sz) (srSizeAsConst sr)

srAlignment :: StoreRange s -> Alignment
srAlignment sr = adjustAlignment (srOffset sr) (srBaseAlignment sr)
-}

{-
-- | Returns term indicating if two ranges are definitely disjoint, and false if they may overlap
offsetsDisjoint :: LoadRange s -> StoreRange s -> Bool
offsetsDisjoint lr sr = do
  case (==) <$> (view _1 <$> lrAlloc lr) <*> (view _1 <$> srAlloc sr) of
    -- If allocations are same, check ranges do not overlap.
    Just True ->
      let lrBeforeSr = (<=) <$> lrEndOffsetAsConst lr <*> srStartOffsetAsConst sr
          srBeforeLr = (<=) <$> srEndOffsetAsConst sr <*> lrStartOffsetAsConst lr
       in lrBeforeSr == Just True || srBeforeLr == Just True
    -- If allocations are different return true. 
    Just False -> True
    -- Otherwise, nothing can be said.
    Nothing -> False

-- | Returns true if first range is completely contained within second.
offsetIsContainedBy :: SAWBackendState s
                    -> LoadRange s
                    -> StoreRange s
                    -> IO (SharedTerm s)
offsetIsContainedBy sbs lr sr = do
  let dl = sbsDataLayout sbs
  let sc = sbsContext sbs
  -- Checks that store size is greater than load size before trying next
  --computation. 
  let checkSize m = do
         case srSizeAsConst sr of
           Just s | s < lrSize dl lr -> scApplyPreludeFalse sc
           _ -> m
  case (,) <$> lrAlloc lr <*> srAlloc sr of
    -- If allocations are same, check one range is contained in another
    Just ((lv,ls,le), (sv,ss,se))
      | lv == sv -> checkSize $ do
        uleFn <- join $ scApplyLLVMLlvmIule sc <*> scBitwidth sc (ptrBitwidth dl)
        join $ scApplyPreludeAnd sc
              <*> scApply2 sc uleFn ss ls
              <*> scApply2 sc uleFn le se
        -- If allocations are distinct, they must be disjoint.
      | otherwise -> scApplyPreludeFalse sc
    -- Allocations could not be identified, so we create formula.
    Nothing -> checkSize $ do
      uleFn <- join $ scApplyLLVMLlvmIule sc <*> scBitwidth sc (ptrBitwidth dl)
      join $ scApplyPreludeAnd sc
            <*> scApply2 sc uleFn (srPtr sr) (lrPtr lr)
            <*> scApply2 sc uleFn (lrEnd lr) (srEnd sr) 

-- | @rebasePtr sbs lr oldBase newBase@ returns offset pointer equal to newBase + (lr - oldPbase).
rebasePtr :: SAWBackendState s
          -> LoadRange s
          -> StoreRange s
          -> SharedTerm s
          -> IO (LoadRange s)
rebasePtr sbs lr sr newBase = do
  ptr <- sbsPtrAdd sbs newBase =<< sbsPtrSub sbs (lrPtr lr) (srPtr sr)
  -- TODO: Fix the alignment and offset to be more accurate.
  mkLoadRange sbs (lrType lr) ptr 0 0
-}

type SAWMem s = Mem (SharedTerm s) (SharedTerm s) (SharedTerm s)

data SAWMemory s = SAWMemory { _memSymbols :: Map (SharedTerm s) Symbol
                             , _memState :: Mem (SharedTerm s) (SharedTerm s) (SharedTerm s)
                             }

emptySAWMemory :: SAWMemory s
emptySAWMemory = SAWMemory { _memSymbols = Map.empty
                           , _memState = emptyMem
                           }

memSymbols :: Simple Lens (SAWMemory s) (Map (SharedTerm s) Symbol)
memSymbols = lens _memSymbols (\s v -> s { _memSymbols = v })

memState :: Simple Lens (SAWMemory s) (SAWMem s)
memState = lens _memState (\s v -> s { _memState = v })

smAddDefine :: DataLayout
            -> SharedContext s
            -> SAWMemory s
            -> Symbol
            -> [BlockLabel]
            -> IO (Maybe (SharedTerm s, [SharedTerm s], SAWMemory s))
smAddDefine dl sc m sym lbls = do
  symt <- scFreshPtr sc dl
  lblst <- traverse (\_ -> scFreshPtr sc dl) lbls
  let m' = m & memSymbols . at symt ?~ sym
  return $ Just (symt, lblst, m')

-- | Return symbol associated with address if any.
smLookupSymbol :: SAWMemory s -> SharedTerm s -> LookupSymbolResult
smLookupSymbol m t = 
  case m^.memSymbols^.at t of
    Just r -> LookupResult r
    Nothing -> Indeterminate

smAlloc :: SAWBackendState s
        -> AllocType
        -> SAWMemory s
        -> MemType
        -> BitWidth -- ^ Width of count.
        -> SharedTerm s -- ^ Count
        -> Alignment
        -> IO (AllocResult (SAWBackend s))
smAlloc sbs atp m mtp w cnt _ = do
  let sc = sbsContext sbs
  -- Create new variable for base address.
  base <- scFreshGlobal sc "_" (sbsPtrType sbs)
  modifyIORef' (sbsAllocations sbs) (Set.insert base)
  -- Get size of tp in bytes.
  let dl = sbsDataLayout sbs
  let pw = ptrBitwidth dl
  let pwt = sbsPtrWidth sbs
  tpSize <- scBitvectorConst' sc (pw,pwt) (toInteger (memTypeSize dl mtp))
  -- Convert count to have same bitwidth as pointer.
  cnt' <- scResizeTerm sc w (pw,pwt) cnt
  -- Get total number of bytes.
  mulFn <- scApplyLLVMLlvmMul sc
  mulOp <- mulFn pwt
  totalSize <- scApplyAll sc mulOp [cnt', tpSize]
  -- Get true predicate.
  t <- scApplyPreludeTrue sc
  -- Return memory with new change.
  let m' = m & memState %~ allocMem atp base totalSize
  -- Return successful allocation.
  return $ AResult t base m'   

{-
-- | A read action takes a pointer to 
type ReadAction s = LoadRange s -> IO (SharedTerm s, SharedTerm s)
-}

-- | This is a function which allows a single read to be split into two disjoint
-- reads and then rejoined.
type RejoinCont s m a = MemType -- Type of first read.
                      -> Int     -- Offset for second read.
                      -> MemType -- Type of second read.
                         -- Action to merge read results
                      -> (SharedTerm s -> SharedTerm s -> IO (SharedTerm s))
                      -> m a

{-
-- Reads pointer values at disjoint addresses and merges result.
mergeLoads :: SharedContext s
              -- Action for reading subranges.
           -> (MemType -> Int -> IO (SharedTerm s, SharedTerm s))
           -> RejoinCont s IO (SharedTerm s, SharedTerm s)
mergeLoads sc subFn tp1 o2 tp2 mergeFn = do
  (c1,r1) <- subFn tp1 0
  (c2,r2) <- subFn tp2 o2
  c <- join $ scApplyPreludeAnd sc ?? c1 ?? c2
  (c,) <$> mergeFn r1 r2
-}

{-
-- | Split int load divides the read of a single integer into two reads of
-- smaller integers and appends the result together.
splitIntLoad :: SAWBackendState s
             -> Integer -- ^ Number of bytes in int to load. 
             -> Integer -- ^ Index of byte offset to split at.
                -- | Action for merging reads.
             -> RejoinCont s m a
             -> m a
splitIntLoad sbs w l mergeFn = assert (0 < l && l < w) $ do
  let dl = sbsDataLayout sbs
  let sc = sbsContext sbs
  let w1 = 8*fromInteger l         -- Low-order aligned read width
      w2 = 8*(fromInteger w) - w1 -- High-order aligned read width
  mergeFn (IntType w1) (fromInteger l) (IntType w2) $ \r1 r2 -> do
    appendFn <- scApplyLLVMLlvmAppendInt sc
    wt1 <- scNat sc (toInteger w1)
    wt2 <- scNat sc (toInteger w2)
    case dl^.intLayout of
      BigEndian    -> appendFn wt1 wt2 r1 r2
      LittleEndian -> appendFn wt2 wt1 r2 r1
-}

{-
-- | Divides read of a vector or array into reads of smaller elements.
splitVecLoad :: SAWBackendState s
                -- | Number of elements 
             -> Int
                -- | Type of vector elements
             -> MemType
                -- | Range to read.
             -> LoadRange s
                -- | Action for merging reads.
             -> RejoinCont s IO (SharedTerm s, SharedTerm s)
             -> IO (SharedTerm s, SharedTerm s)
splitVecLoad sbs 0 tp _ _ = do
  let sc = sbsContext sbs
  (,) <$> scApplyPreludeTrue sc
      <*> join (scApplyPreludeEmptyVec sc <*> sbsMemType sbs tp)
splitVecLoad sbs n etp ptr mergeFn = assert (n > 0) $ do
  let dl = sbsDataLayout sbs
  let sc = sbsContext sbs
  let sz = fromIntegral $ memTypeSize dl etp
  mergeFn (ArrayType (n-1) (lrType ptr)) ((n-1) * sz) etp $ \vec e -> do
    join $ scApplyLLVMSnocVec sc
             <*> sbsMemType sbs etp
             <*> scNat sc (toInteger (n-1))
             ?? vec
             ?? e

splitStructLoad :: SAWBackendState s
                -> StructInfo
                   -- | Action for reading subranges.
                   -- Takes subtype and offset in bytes from range to read.
                -> (MemType -> Int -> IO (SharedTerm s, SharedTerm s))
                -> IO (SharedTerm s, SharedTerm s)
splitStructLoad sbs si subFn = do
  let sc = sbsContext sbs
  case siDropLastField si of
    Nothing -> do
      (,) <$> scApplyPreludeTrue sc
          <*> scApplyLLVMEmptyStruct sc
    Just (si', fi) -> do
      let offset = fromIntegral (fiOffset fi)
      mergeLoads sc subFn (StructType si') offset (fiType fi) $ \r1 r2 -> do
        fieldType <- scApplyLLVMFieldType sc
        fldV <- traverse (sbsFieldInfo sbs) (siFields si')
        join $ scApplyLLVMSnocStruct sc
                 <*> scNat sc (toInteger (siFieldCount si'))
                 <*> scVecLit sc fieldType fldV
                 <*> pure r1
                 <*> sbsMemType sbs (fiType fi)
                 <*> pure r2
                 <*> scNat sc (toInteger (fiPadding fi))

splitRead :: forall s .
             SAWBackendState s
             -- | Range of previous write.
          -> StoreRange s
             -- | Action to perform for when range to read from is
             -- completely in range of previous write.
          -> ReadAction s
             -- | Action to perform for when range to read from is
             -- completely out of range of previous write.
          -> ReadAction s
          -> ReadAction s
splitRead sbs sr iact oact ptr
  | offsetsDisjoint ptr sr = oact ptr
  | otherwise = do
    let dl = sbsDataLayout sbs
    let sc = sbsContext sbs
    cbt <- offsetIsContainedBy sbs ptr sr
    --  Indicates if load should be treated as atomic.
    -- This occurs if size is at most a byte, or if alignment and store size means it must read
    -- value.
    let isAtomic = lsz <= 1 || (2^a >= lsz && srSizeAtLeast sr lsz)
          where a = min (srAlignment sr) (lrAlignment ptr)
                lsz = lrSize dl ptr
    -- Split action
    case termAsBool cbt of
      Just True -> iact ptr
      Just False | isAtomic -> oact ptr
      Nothing    | isAtomic -> do
        iteFn <- scApplyPreludeIte sc
        boolType <- scPreludeBool sc
        (ct,rt) <- iact ptr
        (cf,rf) <- oact ptr
        (,) <$> iteFn boolType cbt ct cf 
            <*> join (iteFn <$> sbsMemType sbs (lrType ptr) ?? cbt ?? rt ?? rf)
      -- Decompose read into multiple smaller reads depending on type.
      _ -> do
        let splitRead' tp' o' = splitRead sbs sr iact oact =<< subrange sbs ptr o' tp'
        let mergeFn = mergeLoads sc splitRead'
        let readInt w = splitIntLoad sbs w (w-1) mergeFn
        case lrType ptr of
          IntType w -> readInt (bitsToBytes w)
          FloatType -> do
            convertFn <- scApplyLLVMLlvmIntToFloat sc
            _2 convertFn =<< readInt 4
          DoubleType -> do
            convertFn <- scApplyLLVMLlvmIntToDouble sc
            _2 convertFn =<< readInt 8
          PtrType{}  -> readInt (toInteger (dl^.ptrSize))
          ArrayType n etp -> splitVecLoad sbs n etp ptr mergeFn
          VecType n etp   -> splitVecLoad sbs n etp ptr mergeFn
          StructType si ->
                case siDropLastField si of
                  Nothing -> do
                    (,) <$> scApplyPreludeTrue sc
                        <*> scApplyLLVMEmptyStruct sc
                  Just (si', fi) -> do
                    let offset = fromIntegral (fiOffset fi)
                    mergeLoads sc splitRead' (StructType si') offset (fiType fi) $ \r1 r2 -> do
                      structFn <- scApplyLLVMSnocStruct sc
                      fieldType <- scApplyLLVMFieldType sc
                      fldV <- traverse (sbsFieldInfo sbs) (siFields si')
                      join $ structFn
                              <$> scNat sc (toInteger (siFieldCount si'))
                              <*> scVecLit sc fieldType fldV
                              <*> pure r1
                              <*> sbsMemType sbs (fiType fi)
                              <*> pure r2
                              <*> scNat sc (toInteger (fiPadding fi))
-}

mergeEq :: (Ord k, Eq a) => Map k a -> Map k a -> Map k a
mergeEq mx = Map.filterWithKey p
  where p k u = Map.lookup k mx == Just u

smMerge :: SharedTerm s -> SAWMemory s -> SAWMemory s -> SAWMemory s
smMerge c x y =
  SAWMemory { _memSymbols = mergeEq (x^.memSymbols) (y^.memSymbols)
            , _memState = mergeMem c (x^.memState) (y^.memState)
            }

 

-- | Return term value, length of fields, and vector with the types of the fields.
createStructValue :: forall s v
                   . SAWBackendState s
                  -> V.Vector (FieldInfo, v)
                  -> IO (ExprEvalFn v (SharedTerm s))
createStructValue sbs flds = do
  let dl = sbsDataLayout sbs
  let sc = sbsContext sbs
  fieldType <- scApplyLLVMFieldType sc
  let foldFn :: (FieldInfo, v)
             -> (Integer, ExprEvalFn v (SharedTerm s), SharedTerm s)
             -> IO (Integer, ExprEvalFn v (SharedTerm s), SharedTerm s)
      foldFn (fi,expr) (n,ExprEvalFn reval, rvtp) = do
        mtp <- sbsMemType sbs (fiType fi)
        padding <- scNat sc (toInteger (fiPadding fi))
        nt <- scNat sc n
        consStruct <- scApplyLLVMConsStruct sc
        let cfn = consStruct mtp padding nt rvtp
        let reval' = ExprEvalFn $ \eval ->
                      join $ ((liftIO .) . cfn) <$> eval expr <*> reval eval
        consVecFn <- scApplyPreludeConsVec sc
        entry <- scTuple sc [mtp,padding] 
        (n+1,reval',) <$> consVecFn fieldType entry nt rvtp
  -- Get initial value and type.
  empty <- scApplyLLVMEmptyStruct sc
  let eval0 = ExprEvalFn $ \_ -> return empty
  emptyFn <- scApplyPreludeEmptyVec sc
  tp0 <- emptyFn fieldType
  view _2 <$> foldrMOf folded foldFn (0, eval0, tp0) flds

{-
-- | Create a dummy value (of all zeros) for the given memtype.
dummyValue :: SAWBackendState s -> MemType -> IO (SharedTerm s)
dummyValue sbs tp0 = do
  let dl = sbsDataLayout sbs
  let sc = sbsContext sbs
  let vecFn n tp = do
       fn <- scApplyPreludeReplicate sc
       join $ fn <$> scNat sc (toInteger n)
                 <*> sbsMemType sbs tp
                 <*> dummyValue sbs tp
  case tp0 of
    IntType w -> scBitvectorConst sc w 0
    FloatType -> scFloat sc 0
    DoubleType -> scDouble sc 0
    PtrType{} -> scBitvectorConst sc (ptrBitwidth dl) 0
    ArrayType n tp -> vecFn n tp
    VecType n tp -> vecFn n tp
    StructType si -> do
      let valueFn fi = (fi, fiType fi)
      ExprEvalFn evalFn <- createStructValue sbs (valueFn <$> siFields si)
      evalFn (dummyValue sbs)
-}

scApply2 :: SharedContext s
         -> SharedTerm s
         -> SharedTerm s
         -> SharedTerm s
         -> IO (SharedTerm s)
scApply2 sc f x y = do
  g <- scApply sc f x 
  scApply sc g y

scApply3 :: SharedContext s
         -> SharedTerm s
         -> SharedTerm s
         -> SharedTerm s
         -> SharedTerm s
         -> IO (SharedTerm s)
scApply3 sc f x y z = do
  g <- scApply2 sc f x y 
  scApply sc g z

scApply4 :: SharedContext s
         -> SharedTerm s
         -> SharedTerm s
         -> SharedTerm s
         -> SharedTerm s
         -> SharedTerm s
         -> IO (SharedTerm s)
scApply4 sc f w x y z = do
  g <- scApply3 sc f w x y 
  scApply sc g z

scLazyIte :: SharedContext s
          -> SharedTerm s -- ^ Condition
          -> IO (SharedTerm s) -- ^ Type of result
          -> IO (SharedTerm s) -- ^ Result if condition is true.
          -> IO (SharedTerm s) -- ^ Result if condition is false.
          -> IO (SharedTerm s) -- ^ Result if condition is false.
scLazyIte sc c mtp mx my = do
  case scViewAsBool c of
    Just True -> mx
    Just False -> my
    Nothing -> join $ scApplyPreludeIte sc <*> mtp <*> pure c <*> mx <*> my

{-
-- | sliceIntFromInt 
sliceIntFromInt :: SAWBackendState s
                -> Integer      -- ^ Number of bytes to select.
                -> Integer      -- ^ Number of bytes in value.
                -> SharedTerm s -- ^ Value written (must be an integer).
                -> Integer      -- ^ Byte offset to read at.
                -> IO (SharedTerm s)
sliceIntFromInt sbs lw sw v o
   | lw == sw = assert (o == 0) $ return v
   | otherwise = assert (o + lw <= sw) $ do
       join $ scApplyLLVMLlvmIntSlice sc
              <*> scNat sc (8*l)
              <*> scNat sc (toInteger lw)
              <*> scNat sc (8*e)
              <*> pure v
  where dl = sbsDataLayout sbs
        sc = sbsContext sbs
        -- l is number of bytes to drop from beginning.
        l = case dl^.intLayout of
              LittleEndian -> o
              BigEndian    -> (sw-lw)-o
        -- Number of bytes to drop from end.
        e = (sw-lw) - l
-}

{-
-- | Select a subrange out of a value, and merge results.
selectOffset :: forall s
              . SAWBackendState s
             -> Integer      -- ^ Width of read in bytes
             -> Alignment    -- ^ Alignment constraint in reading.
             -> SharedTerm s -- ^ Offset to read.
             -> Integer      -- ^ Width of write in bytes.
             -> SharedTerm s -- ^ Type of result.
                -- | Given a concrete offset, this returns the selected term.
             -> (Integer -> IO (SharedTerm s))
             -> IO (SharedTerm s)
selectOffset sbs lw a offset sw resType sliceAt =  do
     let dl = sbsDataLayout sbs
     let sc = sbsContext sbs
     -- Get bitwidth of alignment on ptr'
     let wt = sbsPtrWidth sbs
     let -- This function considers whether the value can belong to a particular offset.
         mergeOffset o r = do
           -- Offset is in bytes
           cond <- join $ scApply2 sc
                          <$> (join $ scApplyLLVMLlvmIeq sc ?? wt)
                          <*> pure offset
                          <*> scBitvectorConst' sc (ptrBitwidth dl,wt) o
           -- Note: This code assumes that integers are stored with least-significant
           -- bits first.
           scLazyIte sc cond
                        (pure resType)
                        (sliceAt o)
                        (pure r)           
     v0 <- sliceAt 0
     let offsets = takeWhile (\j -> j <= (sw - lw)) [ 2^a*i | i <-[1..]]
     foldrMOf folded mergeOffset v0 offsets
-}

-- | Convert bits to bytes.
bitsToBytes :: BitWidth -> Integer
bitsToBytes w | w .&. 0x7 == 0 = toInteger w `shiftR` 3
              | otherwise = error "SAW Backend only supports full byte memory accesses."

-- | Returns true if two types have compatible SAWCore types.
compatTypes :: DataLayout -> MemType -> MemType -> Bool
compatTypes dl tp0 tp1 =
  case (tp0, tp1) of
    (IntType w0, IntType w1) -> w0 == w1
    (IntType w0, PtrType{}) -> w0 == ptrBitwidth dl
    (PtrType{}, IntType w1) -> ptrBitwidth dl == w1
    (PtrType{}, PtrType{}) -> True
    (FloatType, FloatType) -> True
    (DoubleType, DoubleType) -> True
    (ArrayType m tp0', ArrayType n tp1') -> m == n && compatTypes dl tp0' tp1'
    (ArrayType m tp0',   VecType n tp1') -> m == n && compatTypes dl tp0' tp1'
    (VecType m tp0',   ArrayType n tp1') -> m == n && compatTypes dl tp0' tp1'
    (VecType m tp0',     VecType n tp1') -> m == n && compatTypes dl tp0' tp1'
    (StructType si0, StructType si1) -> V.and $ V.zipWith fieldFn (siFields si0) (siFields si1)
      where fieldFn fi0 fi1 = fiOffset fi0 == fiOffset fi1
                           && compatTypes dl (fiType fi0) (fiType fi1)
                           && fiPadding fi0 == fiPadding fi1
    _ -> False

scIntValueType :: SharedContext s -> BitWidth -> IO (SharedTerm s) 
scIntValueType sc w = 
  join $ scApplyLLVMValue sc <*> scIntType sc w

{-
-- | Returns type of values associated with given type.
sbsTypeValue :: SAWBackendState s -> MemType -> IO (SharedTerm s)
sbsTypeValue sbs tp = join $ scApplyLLVMValue (sbsContext sbs) <*> sbsMemType sbs tp
-}

{-
sliceVec :: SAWBackendState s
         -> Integer -- ^ Number of bytes to read
         -> Int -- ^ Number of elements in vector
         -> MemType -- ^ Type of each element in vector
         -> SharedTerm s -- ^ Vector value
         -> Integer -- ^ Offset to read from.
            -- ^ Returns lift of values Number of 
         -> IO [(SharedTerm s, Integer, Integer)]
sliceVec sbs _ n etp v offset = do
  let dl = sbsDataLayout sbs
  let sc = sbsContext sbs
  let (i,r) = offset `divMod` memTypeSize dl etp
  fmap (,r) $ join $ scApplyPreludeGet sc
                       <*> scNat sc (toInteger n)
                       <*> sbsTypeValue sbs etp
                       <*> pure v
                       <*> scFinConst sc i (toInteger n)
-}

{-
sliceIntFromValue :: forall s .
                     SAWBackendState s
                  -> Integer -- ^ Number of bytes to select.
                  -> MemType -- ^ Type of value
                  -> SharedTerm s -- ^ Value written
                  -> Integer      -- ^ Number of bytes to drop from beginning.
                  -> IO (SharedTerm s)
sliceIntFromValue sbs lw stp v offset = do
  let dl = sbsDataLayout sbs
  let sc = sbsContext sbs
  let sw = toInteger $ memTypeSize dl stp
  assert (lw <= sw) $ do
    let sliceInt :: SharedTerm s -> IO (SharedTerm s)
        sliceInt v' = sliceIntFromInt sbs lw sw v' offset
    --let sliceVec' n etp = uncurry sliceInt =<< sliceVec sbs n etp v offset
    case stp of
      IntType{} -> sliceInt v
      FloatType -> sliceInt =<< join (scApplyLLVMLlvmFloatToInt sc ?? v)
      DoubleType -> sliceInt =<< join (scApplyLLVMLlvmDoubleToInt sc ?? v)
      PtrType{} -> sliceInt v
      --ArrayType n etp -> sliceVec' n etp
      --VecType n etp   -> sliceVec' n etp
      StructType si -> do
        let Just i = siIndexOfOffset si (fromIntegral offset)
        nyi "StructType" si i
-}

{-
-- | Extract an int value with  a given number of bytes from  another term.
loadIntValue :: SAWBackendState s
             -> Integer -- ^ Number of bytes to load 
             -> SharedTerm s -- ^ Offset of load.
             -> Alignment -- ^ Alignment for read.
             -> MemType -- ^ Type of value
             -> SharedTerm s -- ^ Value
             -> IO (SharedTerm s)
loadIntValue sbs lw offset a stp v = do
  let dl = sbsDataLayout sbs
  let sc = sbsContext sbs
  let sw = toInteger $ memTypeSize dl stp
  resType <- scIntValueType sc (8*fromInteger lw)
  let sliceFn = sliceIntFromValue sbs lw stp v
  selectOffset sbs lw a offset sw resType sliceFn
-}

{-
-- | Returns true if this mem type only contains types to read that occupy
-- full bytes. 
byteLevelMemType :: MemType -> Bool
byteLevelMemType tp =
  case tp of
    IntType w -> w `mod` 8 == 0
    PtrType _ -> True
    FloatType -> True
    DoubleType -> True
    ArrayType _ etp -> byteLevelMemType etp
    VecType _ etp   -> byteLevelMemType etp
    StructType si   -> V.all byteLevelMemType (siFieldTypes si)
-}

convertMemType :: DataLayout -> MemType -> Maybe Type
convertMemType dl tp0 =
  case tp0 of
    IntType w
        | r == 0 -> return (bitvectorType (fromIntegral n))
        | otherwise -> Nothing
      where (n,r) = w `divMod` 8
    PtrType{} -> return (bitvectorType (dl^.ptrSize))
    FloatType -> return floatType
    DoubleType -> return doubleType
    ArrayType n etp -> arrayType (fromIntegral n) <$> convertMemType dl etp
    VecType n etp   -> arrayType (fromIntegral n) <$> convertMemType dl etp
    StructType si   -> mkStruct <$> traverse fldFn (siFields si)
      where fldFn f = (,fiPadding f) <$> convertMemType dl (fiType f)

smLoad :: forall s .
          SAWBackendState s
       -> SAWMemory s
       -> MemType
       -> SharedTerm s
       -> Alignment
       -> IO (SharedTerm s, SharedTerm s) -- Validity predicate and result.
smLoad sbs m tp0 ptr0 _a0 =
  case convertMemType (sbsDataLayout sbs) tp0 of
    Just tp -> readMem (smGenerator sbs) ptr0 tp (m^.memState)
    Nothing -> fail "smLoad must be given types that are even byte size."

{-
smAddWrite :: SAWMemory s
           -> StoreRange s
           -> MemWrite s
           -> IO (SharedTerm s, SAWMemory s) -- Predicate and new memory.
smAddWrite = undefined
smAddWrite m sr wr = do
  let m' = m & writeState %~ consMemChange wr
  (,m') <$> validStoreFn (memChanges (m^.allocState)) sr
-}

smStore :: SAWBackendState s
        -> SAWMemory s
        -> SharedTerm s -- ^ Address to store value at. 
        -> MemType      -- ^ Type of value
        -> SharedTerm s -- ^ Value to store
        -> Alignment
        -> IO (SharedTerm s, SAWMemory s) -- Predicate and new memory.
smStore sbs m p mtp v _ = do
  case convertMemType (sbsDataLayout sbs) mtp of
    Nothing -> fail "memtype given to smStore must be an even byte size."
    Just tp -> do
      (c,ms) <- writeMem (smGenerator sbs) p tp v (m^.memState)
      return (c,m & memState .~ ms)

-- | @memcpy mem dst src len align@ copies @len@ bytes from @src@ to @dst@,
-- both of which must be aligned according to @align@ and must refer to
-- non-overlapping regions.
smCopy :: SAWBackendState s
       -> SAWMemory s
       -> SharedTerm s -- ^ Destination pointer
       -> SharedTerm s -- ^ Source pointer
       -> BitWidth  -- ^ Bitwidth for counting number of bits.
       -> SharedTerm s -- ^ Number of bytes to copy.
       -> SharedTerm s -- ^ Alignment in bytes (should have 32-bit bits)
       -> IO (SharedTerm s, SAWMemory s)
smCopy sbs m dst src w sz0 _ = do
  sz <- scResizeTerm (sbsContext sbs) w
           (ptrBitwidth (sbsDataLayout sbs), sbsPtrWidth sbs) sz0 
  (c,ms) <- copyMem (smGenerator sbs) dst src sz (m^.memState)
  return (c, m & memState .~ ms)

data SAWBackend s a = SAWBackend { runSAWBackend :: IO a }
  deriving (Functor)

type instance SBETerm (SAWBackend s) = SharedTerm s
type instance SBEPred (SAWBackend s) = SharedTerm s
type instance SBEMemory (SAWBackend s) = SAWMemory s

lift1 :: (x -> IO r) -> (x -> SAWBackend s r)
lift1 = (SAWBackend .)

lift2 :: (x -> y -> IO r)
      -> (x -> y -> SAWBackend s r)
lift2 = (lift1 .)

lift3 :: (x -> y -> z -> IO r)
      -> (x -> y -> z -> SAWBackend s r)
lift3 = (lift2 .)

lift4 :: (w -> x -> y -> z -> IO r)
      -> (w -> x -> y -> z -> SAWBackend s r)
lift4 = (lift3 .)

lift5 :: (v -> w -> x -> y -> z -> IO r)
      -> (v -> w -> x -> y -> z -> SAWBackend s r)
lift5 = (lift4 .)

lift6 :: (u -> v -> w -> x -> y -> z -> IO r)
      -> (u -> v -> w -> x -> y -> z -> SAWBackend s r)
lift6 = (lift5 .)

-- | Returns share term representing given state.
sbsMemType :: SAWBackendState s -> MemType -> IO (SharedTerm s)
sbsMemType sbs btp = do
  let sc = sbsContext sbs
  case btp of
    IntType w -> scIntType sc w
    FloatType  -> pure (sbsFloatType sbs)
    DoubleType -> pure (sbsDoubleType sbs)
    PtrType _  -> pure (sbsPtrType sbs)
    ArrayType n tp -> 
      join $ sbsArrayTypeFn sbs <$> scNat sc (toInteger n)
                                <*> sbsMemType sbs tp
    VecType n tp ->
      join $ sbsVecTypeFn sbs <$> scNat sc (toInteger n)
                              <*> sbsMemType sbs tp
    StructType si -> do
      nt <- scNat sc (toInteger (siFieldCount si))
      fldV <- traverse (sbsFieldInfo sbs) (siFields si)
      sbsStructTypeFn sbs nt =<< scVecLit sc (sbsFieldType sbs) fldV

-- | Returns term (tp,padding) for the given field info. 
sbsFieldInfo :: SAWBackendState s
             -> FieldInfo
             -> IO (SharedTerm s)
sbsFieldInfo sbs fi = do
  tp <- sbsMemType sbs (fiType fi)
  let sc = sbsContext sbs
  p <- scNat sc (toInteger (fiPadding fi))
  scTuple sc [tp,p]

scFinConst :: SharedContext s
           -> Integer -- ^ Index
           -> Integer -- ^ Bound n
           -> IO (SharedTerm s)
scFinConst sc i n | i < n = do
  fv <- scApplyPreludeFinVal sc
  join $ fv <$> scNat sc i <*> scNat sc (n - (i + 1))
scFinConst _ _ _ = error "illegal arguments to scFinConst"


scIntType :: SharedContext s -> BitWidth -> IO (SharedTerm s)
scIntType sc w = join $ scApplyLLVMIntType sc <*> scBitwidth sc w

{-
mkEvalMemType :: SAWBackendState s -> IO (MemType -> IO (SharedTerm s))
mkEvalMemType sbs = do
  return (sbsMemType sbs)
-}

typedExprEvalFn :: forall s v 
                 . SAWBackendState s
                -> TypedExpr v
                -> IO (ExprEvalFn v (SharedTerm s))
typedExprEvalFn sbs expr0 = do
  let dl = sbsDataLayout sbs
  let sc = sbsContext sbs
  let eval1 :: v
            -> (SharedTerm s -> IO (SharedTerm s))
            -> ExprEvalFn v (SharedTerm s)
      eval1 v fn = ExprEvalFn $ \eval -> liftIO . fn =<< eval v       
  let evalBin x y op = ExprEvalFn $ \eval -> liftIO . scApplyAll sc op =<< traverse eval [x,y]
  let mkVecLit mtp v = do
        tp <- join $ scApplyLLVMValue sc <*> sbsMemType sbs mtp
        return $ ExprEvalFn $ \eval -> liftIO . scVecLit sc tp =<< traverse eval v
  let constEvalFn v = ExprEvalFn $ \_ -> return v
      -- | Apply truncation or extension ops to term. 
  let extOp :: (SharedContext s
                    -> IO (SharedTerm s -> SharedTerm s
                                        -> SharedTerm s
                                        -> IO (SharedTerm s)))
            -> (SharedContext s
                    -> IO (SharedTerm s -> SharedTerm s
                                        -> SharedTerm s
                                        -> SharedTerm s
                                        -> IO (SharedTerm s)))
            -> OptVectorLength
            -> BitWidth -- ^ First constant argument to op
            -> BitWidth -- ^ Second constant width argument.
            -> v
            -> IO (ExprEvalFn v (SharedTerm s))
      extOp fn fnV mn dw rw v = do
        dt <- scBitwidth sc dw
        rt <- scBitwidth sc rw
        case mn of
          Nothing -> do
            f <- fn sc
            return $ eval1 v (f dt rt)
          Just n  -> do
            f <- fnV sc
            nt <- scNat sc (toInteger n)
            return $ eval1 v (f nt dt rt)
  let resizeOp :: OptVectorLength
               -> BitWidth -- ^ Input width
               -> BitWidth -- ^ Result bitwith
               -> v
               -> IO (ExprEvalFn v (SharedTerm s))
      resizeOp mn iw rw v
        | iw < rw =
          extOp scApplyLLVMLlvmZExt  scApplyLLVMLlvmZExtV mn  (rw - iw) iw v
        | iw > rw =
          extOp scApplyLLVMLlvmTrunc scApplyLLVMLlvmTruncV mn (iw - rw) rw v
        | otherwise = return $ ExprEvalFn $ (\eval -> eval v)
  case expr0 of
    SValInteger w v ->      
      constEvalFn <$> scBitvectorConst sc w v
    SValFloat v  -> constEvalFn <$> scFloat sc v
    SValDouble v -> constEvalFn <$> scDouble sc v
    SValNull{} -> do
      nullPtrFn <- scApplyLLVMLlvmNullPtr sc
      constEvalFn <$> (nullPtrFn =<< scNat sc (toInteger (dl^.ptrSize)))
    SValArray  mtp v -> mkVecLit mtp v
    SValVector mtp v -> mkVecLit mtp v
    SValStruct si vals -> assert (siFieldCount si == V.length vals) $ do
      createStructValue sbs (siFields si `V.zip` vals)
    IntArith op mn w x y -> do
        case mn of
          Nothing ->
            fmap (evalBin x y) $ join $ mkFn sc <*> scBitwidth sc w
          Just n  ->
            fmap (evalBin x y) $ join $
              mkFnv sc <*> scNat sc (toInteger n)
                       <*> scBitwidth sc w
      where (mkFn, mkFnv) =
              case op of
                Add{}  -> (scApplyLLVMLlvmAdd,  scApplyLLVMLlvmAddV)
                Sub{}  -> (scApplyLLVMLlvmSub,  scApplyLLVMLlvmSubV)
                Mul{}  -> (scApplyLLVMLlvmMul,  scApplyLLVMLlvmMulV)
                UDiv{} -> (scApplyLLVMLlvmUDiv, scApplyLLVMLlvmUDivV)
                SDiv{} -> (scApplyLLVMLlvmSDiv, scApplyLLVMLlvmSDivV)
                URem   -> (scApplyLLVMLlvmURem, scApplyLLVMLlvmURemV)
                SRem   -> (scApplyLLVMLlvmSRem, scApplyLLVMLlvmSRemV)
                Shl{}  -> (scApplyLLVMLlvmShl,  scApplyLLVMLlvmShlV)
                Lshr{} -> (scApplyLLVMLlvmLShr, scApplyLLVMLlvmLShrV)
                Ashr{} -> (scApplyLLVMLlvmAShr, scApplyLLVMLlvmAShrV)
                And    -> (scApplyLLVMLlvmAnd,  scApplyLLVMLlvmAndV)
                Or     -> (scApplyLLVMLlvmOr,   scApplyLLVMLlvmOrV)
                Xor    -> (scApplyLLVMLlvmXor,  scApplyLLVMLlvmXorV)   
    PtrAdd x y -> do
      let mkFn = scApplyLLVMLlvmAddPtr
      fmap (evalBin x y) $ join $
        mkFn sc <*> scNat sc (toInteger (dl^.ptrSize))
    UAddWithOverflow w x y -> do
      let si = mkStructInfo dl False [IntType 1, IntType w]
      let [p0,p1] = V.toList $ fiPadding <$> siFields si
      fn <- scApplyLLVMLlvmAddWithOverflow sc
              <*> scBitwidth sc w
              <*> scNat sc (toInteger p0)
              <*> scNat sc (toInteger p1)
      return $ ExprEvalFn $ \eval -> join $ (\xv yv -> liftIO $ fn xv yv) <$> eval x <*> eval y
    IntCmp op mn w x y -> do
        case mn of
          Nothing ->
            fmap (evalBin x y) $ join $ mkFn sc <*> scBitwidth sc w
          Just n  ->
            fmap (evalBin x y) $ join $
              mkFnV sc <*> scNat sc (toInteger n)
                       <*> scBitwidth sc w
      where (mkFn, mkFnV) =
              case op of 
                Ieq  -> (scApplyLLVMLlvmIeq,  scApplyLLVMLlvmIeqV)
                Ine  -> (scApplyLLVMLlvmIne,  scApplyLLVMLlvmIneV)
                Iugt -> (scApplyLLVMLlvmIugt, scApplyLLVMLlvmIugtV)
                Iuge -> (scApplyLLVMLlvmIuge, scApplyLLVMLlvmIugeV)
                Iult -> (scApplyLLVMLlvmIult, scApplyLLVMLlvmIultV)
                Iule -> (scApplyLLVMLlvmIule, scApplyLLVMLlvmIuleV)
                Isgt -> (scApplyLLVMLlvmIsgt, scApplyLLVMLlvmIsgtV)
                Isge -> (scApplyLLVMLlvmIsge, scApplyLLVMLlvmIsgeV)
                Islt -> (scApplyLLVMLlvmIslt, scApplyLLVMLlvmIsltV)
                Isle -> (scApplyLLVMLlvmIsle, scApplyLLVMLlvmIsleV)
    Trunc mn iw v rw -> assert (iw >= rw) $
      extOp scApplyLLVMLlvmTrunc scApplyLLVMLlvmTruncV mn (iw - rw) rw v
    ZExt mn iw v rw -> assert (iw <= rw) $
      extOp scApplyLLVMLlvmZExt  scApplyLLVMLlvmZExtV  mn (rw - iw) iw v
    SExt mn iw v rw -> assert (iw <= rw) $
      extOp scApplyLLVMLlvmSExt scApplyLLVMLlvmSExtV mn (rw - iw) iw v
    PtrToInt mn _ v rw -> resizeOp mn (ptrBitwidth dl) rw v
    IntToPtr mn iw v _ -> resizeOp mn iw (ptrBitwidth dl) v
    Select mn c tp x y -> do 
      fn <- case mn of 
              Nothing -> scApplyLLVMLlvmSelect sc
              Just n -> do
                fn <- scApplyLLVMLlvmSelectV sc
                fn <$> scNat sc (toInteger n)
      mtp <- sbsMemType sbs tp       
      return $ ExprEvalFn $ \eval -> do
         join $ (\cv xv yv -> liftIO $ fn mtp cv xv yv) <$> eval c <*> eval x <*> eval y 
    GetStructField si v i -> assert (i < siFieldCount si) $ do
      fn <- scApplyLLVMLlvmStructElt sc
      nt <- scNat sc (toInteger (siFieldCount si))
      fieldType <- scApplyLLVMFieldType sc
      flds <- traverse (sbsFieldInfo sbs) (siFields si)
      tps <- scVecLit sc fieldType flds
      -- Get index
      ft <- scFinConst sc (toInteger i) (toInteger (siFieldCount si))
      return $ ExprEvalFn $ \eval -> (\val -> liftIO $ fn nt tps val ft) =<< eval v
    GetConstArrayElt n tp a i -> assert (i < n) $ do
      fn <- scApplyLLVMLlvmArrayElt sc
      nt <- scNat sc (toInteger n)
      mtp <- sbsMemType sbs tp
      it <- scFinConst sc (toInteger i) (toInteger n)
      return $ ExprEvalFn $ \eval -> (\val -> liftIO $ fn nt mtp val it) =<< eval a

-- | Create a SAW backend.
createSAWBackend :: DataLayout
                 -> MemGeom
                 -> IO (SBE (SAWBackend s), SAWMemory s)
createSAWBackend dl _mg = do

  sc <- mkSharedContext llvmModule
  boolType <- scPreludeBool sc
  t    <- scApplyPreludeTrue sc
  pNot <- scApplyPreludeNot sc
  pAnd <- scApplyPreludeAnd sc
  iteFn <- scApplyPreludeIte sc

  let pIte = iteFn boolType

  bvEq <- scApplyPreludeBvEq sc
  sbs <- mkBackendState dl sc
  let sbe = SBE { sbeTruePred = t
                , applyIEq = \w x y -> SAWBackend $
                   join $ bvEq <$> scBitwidth sc w ?? x ?? y
                , applyAnd  = lift2 pAnd
                , applyBNot = lift1 pNot
                , applyPredIte = lift3 pIte
                , applyIte = \tp x y z -> SAWBackend $ do
                    fmap Right $ join $
                      iteFn <$> (join $ scApplyLLVMValue sc <*> sbsMemType sbs tp) 
                            ?? x
                            ?? y
                            ?? z
                , asBool = termAsBool
                , prettyPredD = nyi "prettyPredD"
                , evalPred = nyi "evalPred"
                , freshInt = SAWBackend . scFreshInt sc
                , typedExprEval = typedExprEvalFn sbs
                , applyTypedExpr = \expr -> SAWBackend $ do
                    ExprEvalFn fn <- typedExprEvalFn sbs expr
                    fn return
                , prettyTermD = nyi "prettyTermD"
                , asUnsignedInteger = asUnsignedBitvector
                , asConcretePtr     = asUnsignedBitvector (ptrBitwidth dl)
                , memDump      = nyi "memDump"
                , memLoad      = lift4 (smLoad sbs)
                , memStore     = lift5 (smStore sbs)
                , memCopy      = lift6 (smCopy sbs)
                , memAddDefine = lift3 (smAddDefine dl sc) 
                , memInitGlobal = nyi "memInitGlobal"
                , codeLookupSymbol = ((SAWBackend . return) .) . smLookupSymbol
                , stackAlloc     = lift5 (smAlloc sbs StackAlloc)
                , stackPushFrame = \m ->
                             SAWBackend $ return (t, m & memState %~ pushStackFrameMem)
                , stackPopFrame  = SAWBackend . return . (memState %~ popStackFrameMem)
                , heapAlloc      = lift5 (smAlloc sbs HeapAlloc)
                , memBranch      = SAWBackend . return . (memState %~ branchMem)
                , memBranchAbort = SAWBackend . return . (memState %~ branchAbortMem)
                , memMerge = \c x y -> SAWBackend $ return $ smMerge c x y
                , writeAiger = nyi "writeAiger"
                , writeCnf   = nyi "writeCnf"
                , evalAiger  = nyi "evalAiger"
                , sbeRunIO   = runSAWBackend
                }
  return (sbe, emptySAWMemory)