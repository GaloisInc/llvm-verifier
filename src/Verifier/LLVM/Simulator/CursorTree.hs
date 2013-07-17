{-# Language ViewPatterns #-}
module Verifier.LLVM.Simulator.CursorTree
  ( Orientation(..)
  , CursorTree(..)
  , treeParents
  , topView
  , activeValue
  , updateActiveValue
  , treePair
  , instContext
  , instContext'
  , branch
  , toSeq
  , size
  , activeIndex
  , move
  , inorderTraversal
    -- * Tree contexts.
  , TreeContext
  , onLeft
  , onRight
  , emptyContext
  ) where

import Control.Applicative
import Control.Lens
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

class Sized a where
  size :: a -> Int

-- | Describes which of the two branches is active.
data Orientation
   = LeftActive  -- ^ The left branch of the tree is active.
   | RightActive -- ^ The right branch of the tree is active.
 deriving (Show)

-- | @CurA cursor tree provides a non-empty binary-tree representation
-- where branches and leaves may be annotated (denoted by @b@ and @a@
-- respectively.  One value in the tree is specially marked as "active";
-- and can be obtained in constate time.   It is possible to find the index
-- of the active value, and change which value is active at any type using
-- "move".  The cursor tree was designed to represent the set of active
-- execution paths in the simulator.
data CursorTree b a
  = Branch (TreeContext b a) b Orientation a (CursorTree b a)
  | Singleton a 
  deriving (Show)

-- | Returns branch labels and which side is active with branch
-- immediately about active node first, and root branch last.
treeParents :: CursorTree b a -> [(b,Orientation,CursorTree b a)]
treeParents = impl . fst . asPair
  where impl Empty = []
        impl (OnLeft _ c b t) = (b,RightActive,t):impl c
        impl (OnRight _ c b t) = (b,LeftActive,t):impl c

-- | @branch c b o l r@ denotes @c[b(l,r)]@.  
-- Which branch is active is chosen by @o@.
branch :: TreeContext b a
       -> b
       -> Orientation
       -> CursorTree b a
       -> CursorTree b a
       -> CursorTree b a
branch c b LeftActive  l r = instContext (onRight c b r) l
branch c b RightActive l r = instContext (onLeft  c b l) r

-- | A cursor tree with one missing subtree.
data TreeContext b a
    -- | @Empty@ denotes the empty context.
  = Empty
    -- | @OnLeft s c b l@ denotes the context @c[b(l >< [])]@.
    -- @s@ contains the size of the context.
  | OnLeft  !Int !(TreeContext b a) b (CursorTree b a)
    -- | @OnRight s b r c@ denotes the context @c[b([] >< r)]@
    -- @s@ contains the size of the context.
  | OnRight !Int !(TreeContext b a) b (CursorTree b a)
  deriving (Show)

-- | @appendTreeContext d c@ returns @d(c([]))@.
appendContext :: TreeContext b a -> TreeContext b a -> TreeContext b a
appendContext d Empty = d
appendContext d (OnLeft  s c b l) = OnLeft  (s+size d) (appendContext d c) b l
appendContext d (OnRight s c b r) = OnRight (s+size d) (appendContext d c) b r

instContext' :: TreeContext b a -> a -> CursorTree b a
instContext' d a =
  case d of
    Empty -> Singleton a
    OnLeft  _ c b l -> Branch c b RightActive a l
    OnRight _ c b r -> Branch c b LeftActive  a r

-- | @instContext c t@ denotes the tree @c[t]@.
instContext :: TreeContext b a -> CursorTree b a -> CursorTree b a
instContext d (asPair -> (c,a)) = instContext' (appendContext d c) a

-- | Lens that allows manipulating the element of the tree that has focus.
activeValue :: Lens (CursorTree b a) (CursorTree b a) a a
activeValue f (asPair -> (c,x)) = instContext' c <$> f x

-- | Lens that allows manipulating the element of the tree that has focus.
updateActiveValue :: Lens (CursorTree b a) (CursorTree b a) a (CursorTree b a)
updateActiveValue f (asPair -> (c,x)) = instContext c <$> f x

-- | @asPair c[a]@ returns pair @(c,a)@.
asPair :: CursorTree b a -> (TreeContext b a,a)
asPair (Singleton a) = (Empty,a)
asPair (Branch c b LeftActive  l r) = (OnRight (size c+size r) c b r, l)
asPair (Branch c b RightActive r l) = (OnLeft  (size c+size l) c b l, r)

-- | Convert between trees and (context,value) pairs.
treePair :: Simple Iso (CursorTree b a) (TreeContext b a,a)
treePair = iso asPair (uncurry instContext')

instance Sized (CursorTree b a) where
  size = next . asPair
   where next (c,_) = size c + 1

instance Sized (TreeContext b a) where
  size Empty = 0
  size (OnLeft  s _ _ _) = s
  size (OnRight s _ _ _) = s

emptyContext :: TreeContext b a 
emptyContext = Empty

-- | @onRight c b r@ denotes the context @c[b([], r)]@.
onRight  :: TreeContext b a -> b -> CursorTree b a -> TreeContext b a
onRight c b r = OnRight (size r + size c) c b r

-- | @onLeft c b l@ denotes the context @c[b(l, [])]@.
onLeft :: TreeContext b a -> b -> CursorTree b a -> TreeContext b a
onLeft c b l = OnLeft (size l + size c) c b l

inorderTraversal :: Simple Traversal (CursorTree b a) a
inorderTraversal f x = fromStack <$> inorderTraversal' f (toStack x)

inorderTraversal' :: Simple Traversal (CursorStack b a) a
inorderTraversal' f (Bottom a) = Bottom <$> f a
inorderTraversal' f (ConsLeft s b l r) =
  ConsLeft s b  <$> inorderTraversal  f l
                <*> inorderTraversal' f r
inorderTraversal' f (ConsRight s b l r) =
  ConsRight s b <$> inorderTraversal' f l
                <*> inorderTraversal  f r

-- | Appends the elements in the context to the sequence in
-- left-to-right order.
seqCtx :: TreeContext b a -> Seq a -> Seq a
seqCtx Empty l = l
seqCtx (OnLeft  _ c _ l) r = c `seqCtx` (toSeq l Seq.>< r)
seqCtx (OnRight _ c _ r) l = c `seqCtx` (l Seq.>< toSeq r)

-- | Returns the elements of the tree in left-to-right order.
toSeq :: CursorTree b a -> Seq a
toSeq = next . asPair
  where next (c,a) = c `seqCtx` Seq.singleton a

-- | Return number of terms to left of root in tree.
activeIndex :: CursorTree b a -> Int
activeIndex t = contextIndex (fst (asPair t)) 0
  where contextIndex :: TreeContext b a -> Int -> Int
        contextIndex Empty i = i
        contextIndex (OnLeft _ c _ l)  i = contextIndex c $! i+size l
        contextIndex (OnRight _ c _ _) i = contextIndex c i

-- | A top-down view of a cursor tree.
data CursorStack b a
  = -- | Marks that the right side of the tree is active.
    ConsLeft !Int b (CursorTree b a) (CursorStack b a)
    -- | Marks that the left side of the tree is active.
  | ConsRight !Int b (CursorStack b a) (CursorTree b a)
  | Bottom a
 deriving (Show)

consLeft :: b -> CursorTree b a -> CursorStack b a -> CursorStack b a
consLeft b l r = ConsLeft (size l + size r) b l r

consRight :: b -> CursorStack b a -> CursorTree b a -> CursorStack b a
consRight b l r = ConsRight (size l + size r) b l r

instance Sized (CursorStack b a) where
  size (ConsLeft  s _ _ _) = s
  size (ConsRight s _ _ _) = s
  size (Bottom _) = 1

-- | Converts a cursor tree represented by a CursorStack into the
-- equivalent cursor tree represented by a CursorTree.
fromStack :: CursorStack b a -> CursorTree b a
fromStack = fromStack' Empty

fromStack' :: TreeContext b a -> CursorStack b a -> CursorTree b a 
fromStack' c (ConsLeft _ b l r) =
  fromStack' (onLeft c b l) r -- c[l >< r]
fromStack' c (ConsRight _ b l r) =
  fromStack' (onRight  c b r) l -- c[l >< r]
fromStack' c (Bottom a) = instContext' c a

toStack :: CursorTree b a -> CursorStack b a 
toStack = next . asPair
  where next (c,a) = toStack' c (Bottom a)

toStack' :: TreeContext b a -> CursorStack b a -> CursorStack b a 
toStack' Empty s = s
toStack' (OnRight _ c b r) l = toStack' c (consRight b l r) -- c[s >< u]
toStack' (OnLeft  _ c b l) r = toStack' c (consLeft b l r) -- c[l >< r]

topView :: CursorTree b a -> Either a (b, Orientation, CursorTree b a, CursorTree b a)
topView t =
  case toStack t of
    Bottom a -> Left a
    ConsLeft  _ b l r -> Right (b, RightActive, l, fromStack r)
    ConsRight _ b l r -> Right (b, LeftActive,  fromStack l, r)

-- | Move a given number of positions to left or right in tree.
move :: Int -> CursorTree b a -> Maybe (CursorTree b a)
move i t@(asPair -> (c,a))
 | i < 0     = moveLeft  (Bottom a) i c
 | i == 0    = Just t
 | otherwise = moveRight (Bottom a) (i-1) c

moveLeft :: CursorStack b a
         -> Int 
         -> TreeContext b a
         -> Maybe (CursorTree b a)
moveLeft _ _ Empty = Nothing                         
moveLeft r i (OnLeft _ c b l) =
  if i+size l >= 0 then
    Just $ selectWithin (onRight c b (fromStack r)) (toStack l) (i+size l)
  else 
    moveLeft (consLeft b l r) (i+size l) c
-- Skip over branches on right when moving left.
moveLeft l i (OnRight _ c b r) = moveLeft (consRight b l r) i c

moveRight :: CursorStack b a
          -> Int 
          -> TreeContext b a
          -> Maybe (CursorTree b a)
moveRight _ _ Empty = Nothing                         
moveRight l i (OnRight _ c b r) =
  if i < size r then
    Just $ selectWithin (onLeft c b (fromStack l)) (toStack r) i
  else
    moveRight (consRight b l r) (i-size r) c
moveRight r i (OnLeft _ c b l) = moveRight (consLeft b l r) i c

-- | @selectWithin @c t i@ returns a cursor tree equivalent to @c[t]@
-- with root at index @i@ in @t@.
selectWithin :: TreeContext b a
             -> CursorStack b a
             -- | Index to point to.
             -- (must be less than size of term). 
             -> Int         
             -> CursorTree b a
selectWithin c z i = 
  case z of
    -- C[a]
    Bottom a -> instContext' c a
    -- C[b(l, r)]
    ConsLeft _ b l r ->
      if size l <= i then  
        selectWithin (onLeft  c b l) r (i-size l)
      else
        selectWithin (onRight c b (fromStack r)) (toStack l) i
    -- C[b(l, r)]
    ConsRight _ b l r ->
      if size l <= i then
        selectWithin (onLeft  c b (fromStack l)) (toStack r) (i-size l) 
      else
        selectWithin (onRight c b r) l i