{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module MonadicMemModel where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Monoid
import qualified Data.Vector as V
import SymbolicMemModel


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

data AddrDecomposeResult t
  = Symbolic t
  | ConcreteOffset t Integer
  | SymbolicOffset t t

adrVar :: AddrDecomposeResult t -> Maybe t
adrVar Symbolic{} = Nothing
adrVar (ConcreteOffset v _) = Just v
adrVar (SymbolicOffset v _) = Just v


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

       , tgPtrDecompose :: p -> AddrDecomposeResult p

       , tgConstPtr :: Size -> m p
       , tgAddPtr :: p -> p -> m p
       , tgAddPtrC :: p -> Addr -> m p
       , tgSubPtr :: p -> p -> m p

       , tgFalse :: c
       , tgTrue :: c
       , tgEq :: p -> p -> m c
       , tgLe :: p -> p -> m c
       , tgAnd :: c -> c -> m c
       , tgMuxCond :: c -> c -> c -> m c


       , tgMuxTerm :: c -> t -> t -> m t
       , tgConstBitvector :: Size -> Integer -> m t
       , tgApplyCtorF :: ValueCtorF t -> m t
       , tgApplyViewF :: ViewF t -> m t
       } 

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
    Array n etp -> tgMkArray tg etp . V.replicate (fromIntegral n) =<< defaultTerm tg etp
    Struct flds -> tgMkStruct tg =<< traverse fldFn flds
 where fldFn f = (f,) <$> defaultTerm tg (f^.fieldVal)

genValue :: (Applicative m, Monad m) => TermGenerator m p c t -> (v -> p) -> Value v -> m p
genValue tg f = foldTermM (return . f) (tgApplyValueF tg) 

genCondVar :: (Applicative m, Monad m) => TermGenerator m p c t -> (v -> p) -> Cond v -> m c
genCondVar tg f c =
  case c of
    Eq x y  -> join $ tgEq tg <$> genValue tg f x <*> genValue tg f y
    Le x y  -> join $ tgLe tg <$> genValue tg f x <*> genValue tg f y
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
      -> Getting (Dual (Endo (c -> m c))) s u c b
      -> s
      -> m c
tgAll tg fld = foldrMOf fld (tgAnd tg) (tgTrue tg)

tgMuxPair :: Applicative m => TermGenerator m p c t -> c -> (c,t) -> (c,t) -> m (c,t)
tgMuxPair tg c (xc,xt) (yc,yt) =
  (,) <$> tgMuxCond tg c xc yc
      <*> tgMuxTerm tg c xt yt

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
                    -- Evaluation function
                 -> (Var -> p)
                    -- Function for reading specific subranges.
                 -> (u -> m (c,t))
                 -> Mux (Cond Var) (ValueCtor u)
                 -> m (c,t)
evalMuxValueCtor tg vf subFn =
  reduceMux (tgMuxPair tg)
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
  let ld = tgPtrDecompose tg l
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
          evalMuxValueCtor tg varFn subFn $
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
      evalMuxValueCtor tg varFn subFn mux0

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
  let ld = tgPtrDecompose tg l
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
      evalMuxValueCtor tg varFn subFn (symbolicValueLoad pref ltp (Var stp))

readMem :: forall m p c t .
                  (Applicative m, Monad m, Eq p)
               => TermGenerator m p c t
               -> p
               -> Type
               -> [MemWrite p c t]
               -> m (c,t)
readMem tg _ tp [] = badLoad tg tp
readMem tg l tp (h:r) = do
  let readPrev tp' l' = readMem tg l' tp' r
  case h of
    MemCopy dst src sz ->
      readMemCopy tg l tp dst src sz readPrev
    MemStore dst v stp ->
      readMemStore tg l tp dst v stp readPrev
    WriteMerge c xr yr -> do
      join $ tgMuxPair tg c <$> readMem tg l tp (xr++r)
                            <*> readMem tg l tp (yr++r)