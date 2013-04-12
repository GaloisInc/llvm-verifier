{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Verifier.LLVM.MemModel
 ( Type
 , TypeF(..)
 , bitvectorType
 , floatType
 , doubleType
 , arrayType
 , mkStruct
 , typeF
 , Field
 , fieldVal ,fieldPad

 , ValueCtorF(..)
 , ViewF(..)

 , TermGenerator(..)
 , AddrDecomposeResult(..)

 , Mem
 , emptyMem
 , AllocType(..)
 , allocMem
 , allocAndWriteMem
 , readMem
 , writeMem
 , writeMem'
 , copyMem
 , pushStackFrameMem
 , popStackFrameMem
 , branchMem
 , branchAbortMem
 , mergeMem
 ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Monoid
import qualified Data.Vector as V

import Verifier.LLVM.MemModel.Common

data AddrDecomposeResult t
  = Symbolic t
  | ConcreteOffset t Integer
  | SymbolicOffset t t

adrVar :: AddrDecomposeResult t -> Maybe t
adrVar Symbolic{} = Nothing
adrVar (ConcreteOffset v _) = Just v
adrVar (SymbolicOffset v _) = Just v

data AllocType = StackAlloc | HeapAlloc

-- | Stores writeable memory allocations.
data MemAlloc p c
     -- | Allocation with given base and number of bytes.
   = Alloc AllocType p p
     -- | The merger of two allocations.
   | AllocMerge c [MemAlloc p c] [MemAlloc p c]

data MemWrite p c t
    -- | @MemCopy dst src len@ represents a copy from [src..src+len) to
    -- [dst..dst+len).
  = MemCopy (p,AddrDecomposeResult p) p (p,Maybe Integer) 
    -- | Memstore is given address, type of value, and value.
  | MemStore (p,AddrDecomposeResult p) t Type
    -- | The merger of two memories.
  | WriteMerge c [MemWrite p c t] [MemWrite p c t]

data TermGenerator m p c t = TG {
         tgPtrWidth :: Size

       , tgPtrDecompose :: p -> m (AddrDecomposeResult p)
       , tgPtrSizeDecompose :: p -> m (Maybe Integer)

       , tgConstPtr :: Size -> m p
       , tgAddPtr :: p -> p -> m p
         -- | Adds two pointers, returning value along with condition
         -- that holds if arithmetic did not overflow.
       , tgCheckedAddPtr :: p -> p -> m (c,p)
       , tgSubPtr :: p -> p -> m p

       , tgFalse :: c
       , tgTrue :: c
       , tgPtrEq :: p -> p -> m c
       , tgPtrLe :: p -> p -> m c
       , tgAnd :: c -> c -> m c
       , tgOr  :: c -> c -> m c
       , tgMuxCond :: c -> c -> c -> m c


       , tgConstBitvector :: Size -> Integer -> m t
       , tgConstFloat  :: Float -> m t
       , tgConstDouble :: Double -> m t
       , tgApplyCtorF  :: ValueCtorF t -> m t
       , tgApplyViewF  :: ViewF t -> m t
       , tgMuxTerm :: c -> Type -> t -> t -> m t
       } 

tgAddPtrC :: Monad m => TermGenerator m p c t -> p -> Addr -> m p
tgAddPtrC tg x y = tgAddPtr tg x =<< tgConstPtr tg y

tgApplyValueF :: TermGenerator m p c t -> ValueF p -> m p
tgApplyValueF tg (Add x y) = tgAddPtr tg x y
tgApplyValueF tg (Sub x y) = tgSubPtr tg x y
tgApplyValueF tg (CValue c) = tgConstPtr tg (fromInteger c)

tgMkArray :: TermGenerator m p c t -> Type -> V.Vector t -> m t
tgMkArray tg tp v = tgApplyCtorF tg (MkArray tp v)

tgMkStruct :: TermGenerator m p c t -> V.Vector (Field Type, t) -> m t
tgMkStruct tg flds = tgApplyCtorF tg (MkStruct flds)

badLoad :: (Applicative m, Monad m) => TermGenerator m p c t -> Type -> m (c,t)
badLoad tg tp = (tgFalse tg,) <$> defaultTerm tg tp

defaultTerm :: (Applicative m, Monad m) => TermGenerator m p c t -> Type -> m t
defaultTerm tg tp =
  case typeF tp of
    Bitvector w -> tgConstBitvector tg w 0
    Float  -> tgConstFloat tg 0
    Double -> tgConstDouble tg 0
    Array n etp -> tgMkArray tg etp . V.replicate (fromIntegral n) =<< defaultTerm tg etp
    Struct flds -> tgMkStruct tg =<< traverse fldFn flds
 where fldFn f = (f,) <$> defaultTerm tg (f^.fieldVal)

genValue :: (Applicative m, Monad m) => TermGenerator m p c t -> (v -> p) -> Value v -> m p
genValue tg f = foldTermM (return . f) (tgApplyValueF tg) 

genCondVar :: (Applicative m, Monad m) => TermGenerator m p c t -> (v -> p) -> Cond v -> m c
genCondVar tg f c =
  case c of
    Eq x y  -> join $ tgPtrEq tg <$> genValue tg f x <*> genValue tg f y
    Le x y  -> join $ tgPtrLe tg <$> genValue tg f x <*> genValue tg f y
    And x y -> join $ tgAnd tg <$> genCondVar tg f x <*> genCondVar tg f y

genValueCtor :: (Applicative m, Monad m)
             => TermGenerator m p c t -> ValueCtor t -> m t
genValueCtor tg = foldTermM return (tgApplyCtorF tg)

applyView :: (Applicative m, Monad m) 
          => TermGenerator m p c t -> t -> ValueView Type -> m t
applyView tg t = foldTermM (\_ -> return t) (tgApplyViewF tg)

-- | Join all conditions in fold together.
tgAll :: Monad m
      => TermGenerator m p c t
      -> Getting (Dual (Endo (c -> m c))) s c
      -> s
      -> m c
tgAll tg fld = foldrMOf fld (tgAnd tg) (tgTrue tg)

tgMuxPair :: Applicative m
          => TermGenerator m p c t
          -> c
          -> Type
          -> (c,t)
          -> (c,t)
          -> m (c,t)
tgMuxPair tg c tp (xc,xt) (yc,yt) =
  (,) <$> tgMuxCond tg c xc yc
      <*> tgMuxTerm tg c tp xt yt

evalValueCtor :: (Applicative m, Monad m )
              => TermGenerator m p c t
              -> ValueCtor (c,t)
              -> m (c,t)
evalValueCtor tg vc =
   (,) <$> tgAll tg (traverse . _1) vc
       <*> genValueCtor tg (snd <$> vc)

evalMuxValueCtor :: forall m u p c t .
                    (Applicative m, Monad m)
                 => TermGenerator m p c t
                    -- Type for value returned
                 -> Type
                    -- Evaluation function
                 -> (Var -> p)
                    -- Function for reading specific subranges.
                 -> (u -> m (c,t))
                 -> Mux (Cond Var) (ValueCtor u)
                 -> m (c,t)
evalMuxValueCtor tg tp vf subFn =
  reduceMux (\c -> tgMuxPair tg c tp)
    <=< muxLeaf (evalValueCtor tg)
    <=< muxCond (genCondVar tg vf)
    <=< muxLeaf (traverse subFn)

readMemCopy :: forall m p c t .
               (Applicative m, Monad m, Eq p)
            => TermGenerator m p c t
            -> p
            -> Type
            -> (p,AddrDecomposeResult p)
            -> p
            -> (p,Maybe Integer)
            -> (Type -> p -> m (c,t))
            -> m (c,t)
readMemCopy tg l tp (d,dd) src (sz,szd) readPrev = do
  ld <- tgPtrDecompose tg l
  let varFn :: Var -> p
      varFn Load = l
      varFn Store = d
      varFn StoreSize = sz
  case (ld, dd) of
    -- Offset if known
    ( ConcreteOffset lv lo
      , ConcreteOffset sv so
      ) 
      | lv == sv -> do
      let subFn :: RangeLoad Addr -> m (c,t)
          subFn (OutOfRange o tp') = readPrev tp' =<< tgAddPtrC tg lv o
          subFn (InRange    o tp') = readPrev tp' =<< tgAddPtrC tg src o
      case szd of
        Just csz -> do
          let s = R (fromInteger so) (fromInteger (so + csz))
          let vcr = rangeLoad (fromInteger lo) tp s
          evalValueCtor tg =<< traverse subFn vcr
        _ ->
          evalMuxValueCtor tg tp varFn subFn $
            fixedOffsetRangeLoad (fromInteger lo) tp (fromInteger so)
    -- We know variables are disjoint.
    _ | Just lv <- adrVar ld
      , Just sv <- adrVar dd
      , lv /= sv -> readPrev tp l
      -- Symbolic offsets
    _ -> do
      let subFn :: RangeLoad (Value Var) -> m (c,t)
          subFn (OutOfRange o tp') =
            readPrev tp' =<< genValue tg varFn o
          subFn (InRange o tp') =
            readPrev tp' =<< tgAddPtr tg src =<< genValue tg varFn o
      let pref | ConcreteOffset{} <- dd = FixedStore
               | ConcreteOffset{} <- ld = FixedLoad
               | otherwise = NeitherFixed
      let mux0 | Just csz <- szd =
                   fixedSizeRangeLoad pref tp (fromInteger csz) 
               | otherwise =
                   symbolicRangeLoad pref tp
      evalMuxValueCtor tg tp varFn subFn mux0

readMemStore :: forall m p c t .
               (Applicative m, Monad m, Eq p)
            => TermGenerator m p c t
            -> p
            -> Type
            -> (p,AddrDecomposeResult p)
            -> t
            -> Type
            -> (Type -> p -> m (c,t))
            -> m (c,t)
readMemStore tg l ltp (d,dd) t stp readPrev = do
  ld <- tgPtrDecompose tg l
  ssz <- tgConstPtr tg (typeSize stp)
  let varFn :: Var -> p
      varFn Load = l
      varFn Store = d
      varFn StoreSize = ssz
  case (ld, dd) of
    -- Offset if known
    ( ConcreteOffset lv lo
      , ConcreteOffset sv so
      ) 
      | lv == sv -> do
      let subFn :: ValueLoad Addr -> m (c,t)
          subFn (OldMemory o tp')   = readPrev tp' =<< tgAddPtrC tg lv o
          subFn (LastStore v)       = (tgTrue tg,) <$> applyView tg t v
          subFn (InvalidMemory tp') = badLoad tg tp'
      let vcr = valueLoad (fromInteger lo) ltp (fromInteger so) (Var stp)
      evalValueCtor tg =<< traverse subFn vcr
    -- We know variables are disjoint.
    _ | Just lv <- adrVar ld
      , Just sv <- adrVar dd
      , lv /= sv -> readPrev ltp l
      -- Symbolic offsets
    _ -> do
      let subFn :: ValueLoad (Value Var) -> m (c,t)
          subFn (OldMemory o tp')   = readPrev tp' =<< genValue tg varFn o
          subFn (LastStore v)       = (tgTrue tg,) <$> applyView tg t v
          subFn (InvalidMemory tp') = badLoad tg tp'
      let pref | ConcreteOffset{} <- dd = FixedStore
               | ConcreteOffset{} <- ld = FixedLoad
               | otherwise = NeitherFixed
      evalMuxValueCtor tg ltp varFn subFn $ 
        symbolicValueLoad pref ltp (Var stp)

readMem :: (Applicative m, Monad m, Eq p)
        => TermGenerator m p c t
        -> p
        -> Type
        -> Mem p c t
        -> m (c,t)
readMem tg l tp m = readMem' tg l tp (memWrites m)

readMem' :: (Applicative m, Monad m, Eq p)
         => TermGenerator m p c t
         -> p
         -> Type
         -> [MemWrite p c t]
         -> m (c,t)
readMem' tg _ tp [] = badLoad tg tp
readMem' tg l tp (h:r) = do
  let readPrev tp' l' = readMem' tg l' tp' r
  case h of
    MemCopy dst src sz ->
      readMemCopy tg l tp dst src sz readPrev
    MemStore dst v stp ->
      readMemStore tg l tp dst v stp readPrev
    WriteMerge c xr yr -> do
      join $ tgMuxPair tg c tp
               <$> readMem' tg l tp (xr++r)
               <*> readMem' tg l tp (yr++r)

-- | A state of memory as of a stack push, branch, or merge.
data MemState d =
    -- | Represents initial memory and changes since then.
    -- Changes are stored in order, with more recent changes closer to the head
    -- of the list.
    EmptyMem d
    -- | Represents a push of a stack frame,  andchanges since that stack push.
    -- Changes are stored in order, with more recent changes closer to the head
    -- of the list.
  | StackFrame d (MemState d) 
    -- | Represents a push of a branch frame, and changes since that branch.
    -- Changes are stored in order, with more recent changes closer to the head
    -- of the list.
  | BranchFrame d (MemState d)

memStateLastChanges :: Simple Lens (MemState d) d
memStateLastChanges f s0 =
  case s0 of
    EmptyMem l -> EmptyMem <$> f l
    StackFrame l s  -> flip StackFrame s  <$> f l
    BranchFrame l s -> flip BranchFrame s <$> f l

type MemChanges p c t = ([MemAlloc p c], [MemWrite p c t])

prependChanges :: MemChanges p c t -> MemChanges p c t -> MemChanges p c t
prependChanges (xa,xw) (ya,yw) = (xa ++ ya, xw ++ yw)

muxChanges :: c -> MemChanges p c t -> MemChanges p c t -> MemChanges p c t
muxChanges c (xa,xw) (ya,yw) = ([AllocMerge c xa ya],[WriteMerge c xw yw])

data Mem p c t = Mem { _memState :: MemState (MemChanges p c t)
                     }

memState :: Simple Lens (Mem p c t) (MemState ([MemAlloc p c],[MemWrite p c t]))
memState = lens _memState (\s v -> s { _memState = v })

memChanges :: (MemChanges p c t -> [d]) -> Mem p c t -> [d]
memChanges f m = go (m^.memState)
  where go (EmptyMem l)      = f l
        go (StackFrame l s)  = f l ++ go s
        go (BranchFrame l s) = f l ++ go s

memAllocs :: Mem p c t -> [MemAlloc p c]
memAllocs = memChanges fst

memWrites :: Mem p c t -> [MemWrite p c t]
memWrites = memChanges snd

memAddAlloc :: MemAlloc p c -> Mem p c t -> Mem p c t
memAddAlloc x = memState . memStateLastChanges . _1 %~ (x:)

memAddWrite :: MemWrite p c t -> Mem p c t -> Mem p c t
memAddWrite x = memState . memStateLastChanges . _2 %~ (x:)

emptyChanges :: MemChanges p c t
emptyChanges = ([],[])

emptyMem :: Mem p c t
emptyMem = Mem { _memState = EmptyMem emptyChanges
               }

isAllocated' :: (Applicative m, Monad m)
             => TermGenerator m p c t
                -- ^ Evaluation function that takes continuation
                -- for condition if previous check fails.
             -> (p -> p -> m c -> m c)
             -> [MemAlloc p c]
             -> m c
isAllocated' tg _ [] = pure (tgFalse tg)
isAllocated' tg step (Alloc _ a asz:r) = do
  step a asz (isAllocated' tg step r)
isAllocated' tg step (AllocMerge c xr yr:r) =
  join $ tgMuxCond tg c
         <$> isAllocated' tg step (xr ++ r)
         <*> isAllocated' tg step (yr ++ r)                


-- | @offsetisAllocated tg b o sz m@ returns condition required to prove range
-- @[b+o..b+o+sz)@ lays within a single allocation in @m@.  This code assumes
-- @sz@ is non-zero, and @b+o@ does not overflow.
offsetIsAllocated :: (Applicative m, Monad m, Eq p)
                  => TermGenerator m p c t -> p -> p -> p -> Mem p c t -> m c
offsetIsAllocated tg t o sz m = do
  (oc, oe) <- tgCheckedAddPtr tg o sz
  let step a asz fallback
        | t == a = tgPtrLe tg oe asz
        | otherwise = fallback
  tgAnd tg oc =<< isAllocated' tg step (memAllocs m)

isAllocated :: (Applicative m, Monad m, Eq p)
            => TermGenerator m p c t -> p -> p -> Mem p c t -> m c
isAllocated tg p sz m = do
  ld <- tgPtrDecompose tg p
  case ld of
    Symbolic{} -> do
      (oc,pe) <- tgCheckedAddPtr tg p sz
      let step a asz fallback =
            join $ tgOr tg 
              <$> (do ae <- tgAddPtr tg a asz
                      join $ tgAnd tg <$> tgPtrLe tg a p <*> tgPtrLe tg pe ae)
              <*> fallback
      tgAnd tg oc =<< isAllocated' tg step (memAllocs m)
    ConcreteOffset t o0 -> do
      o <- tgConstPtr tg (fromInteger o0)
      offsetIsAllocated tg t o sz m
    SymbolicOffset t o -> do
      offsetIsAllocated tg t o sz m

writeMem :: (Applicative m, Monad m, Eq p)
         => TermGenerator m p c t
         -> p
         -> Type
         -> t
         -> Mem p c t
         -> m (c,Mem p c t)
writeMem tg p tp v m = do
  (,) <$> (do sz <- tgConstPtr tg (typeEnd 0 tp)
              isAllocated tg p sz m)
      <*> writeMem' tg p tp v m

-- | Write memory without checking if it is allocated.
writeMem' :: (Applicative m, Monad m, Eq p)
          => TermGenerator m p c t
          -> p
          -> Type
          -> t
          -> Mem p c t
          -> m (Mem p c t)
writeMem' tg p tp v m = addWrite <$> tgPtrDecompose tg p
  where addWrite pd = m & memAddWrite (MemStore (p,pd) v tp)

-- | Perform a mem copy.
copyMem :: (Applicative m, Monad m, Eq p)
         => TermGenerator m p c t
         -> p -- ^ Dest
         -> p -- ^ Source 
         -> p -- ^ Size
         -> Mem p c t
         -> m (c,Mem p c t)
copyMem tg dst src sz m = do
  (,) <$> (join $ tgAnd tg <$> isAllocated tg dst sz m
                           <*> isAllocated tg src sz m)
      <*> (do dstd <- tgPtrDecompose tg dst
              szd <- tgPtrSizeDecompose tg sz
              return $ m & memAddWrite (MemCopy (dst,dstd) src (sz,szd)))

-- | Allocate space for memory
allocMem :: AllocType -- ^ Type of allocation 
         -> p -- ^ Base for allocation
         -> p -- ^ Size
         -> Mem p c t 
         -> Mem p c t
allocMem a b sz = memAddAlloc (Alloc a b sz)

-- | Allocate space for memory
allocAndWriteMem :: (Applicative m, Monad m, Eq p)
                 => TermGenerator m p c t
                 -> AllocType -- ^ Type of allocation 
                 -> p -- ^ Base for allocation
                 -> Type
                 -> t -- Value to write
                 -> Mem p c t 
                 -> m (Mem p c t)
allocAndWriteMem tg a b tp v m = do
  sz <- tgConstPtr tg (typeEnd 0 tp)
  writeMem' tg b tp v (m & memAddAlloc (Alloc a b sz))

pushStackFrameMem :: Mem p c t -> Mem p c t
pushStackFrameMem = memState %~ StackFrame emptyChanges

popStackFrameMem :: Mem p c t -> Mem p c t
popStackFrameMem m = m & memState %~ popf
  where popf (StackFrame (a,w) s) = s & memStateLastChanges %~ prependChanges c
          where c = (mapMaybe pa a, w)
        popf _ = error "popStackFrameMem given unexpected memory"
        pa (Alloc StackAlloc _ _) = Nothing
        pa a@(Alloc HeapAlloc _ _) = Just a
        pa (AllocMerge c x y) = Just (AllocMerge c (mapMaybe pa x) (mapMaybe pa y))

branchMem :: Mem p c t -> Mem p c t
branchMem = memState %~ BranchFrame emptyChanges

branchAbortMem :: Mem p c t -> Mem p c t
branchAbortMem = memState %~ popf
  where popf (BranchFrame c s) = s & memStateLastChanges %~ prependChanges c
        popf _ = error "branchAbortMem given unexpected memory"

mergeMem :: c -> Mem p c t -> Mem p c t -> Mem p c t
mergeMem c x y =
  case (x^.memState, y^.memState) of
    (BranchFrame a s, BranchFrame b _) ->
      let s' = s & memStateLastChanges %~ prependChanges (muxChanges c a b)
       in x & memState .~ s'