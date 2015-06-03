{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

{- |
Module           : $Header$
Description      : Symbolic execution tests
License          : Free for non-commercial use. See LICENSE.
Stability        : provisional
Point-of-contact : acfoltzer
-}
module Verifier.LLVM.Debugger.FreeApp 
 ( App(..)
 , Cont
 , atom
 , idCont
 , runCont
 , evalCont
 , contToApp
 , composeCont
 ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

-- | App is a free instance of applicative.
data App u a where
  -- @PureApp x@ denotes @pure x@. 
  PureApp :: !a -> App u a
  -- @NP x a@ denotes @x <**> a@.
  NP :: !(u a) -> Cont u a b -> App u b

-- | A continuation with type @Cont u a b@ is semantically
-- equivalent to @App u (a->b)@, but uses implementation
-- details to reduce number of thunks.
data Cont u a b where
  IdCont :: Cont u a a
  ComposeCont :: !(App u (a->b)) -> Cont u b c -> Cont u a c

contToApp :: Cont u a b -> App u (a->b)
contToApp IdCont = pure id
contToApp (ComposeCont u v) = u <&&> ((.) `fmap` contToApp v)

evalCont :: a -> Cont u a r -> App u r
evalCont x IdCont = pure x
evalCont x (ComposeCont u v) = evalApp x u `runCont` v

-- | Denotes @(pure id)@ as a continuation.
idCont :: Cont u a a
idCont = IdCont

altToCont :: App u (a->b) -> Cont u a b
altToCont a = ComposeCont a IdCont

-- | Chain two continuations in sequence.
composeCont :: Cont u a b -> Cont u b c -> Cont u a c
composeCont IdCont c = c
composeCont (ComposeCont u v) w = ComposeCont u (v `composeCont` w)

-- | @fmap f c@ is evivalent to @altToCont (fmap (f.) (contToApp c))@.
instance Functor (Cont u c) where
  fmap f c = c `composeCont` (altToCont (pure f))

-- @evalApp a b@ denotes @pure a <**> b@.
evalApp :: a -> App u (a->b) -> App u b
evalApp x (PureApp f) = PureApp (f x)
evalApp x (NP u v) = NP u (fmap ($x) v)

runCont :: App u a -> Cont u a r -> App u r
runCont (PureApp x) c = evalCont x c
runCont (NP u v) c = NP u (v `composeCont` c)

instance Functor (App u) where
  fmap f (PureApp x) = PureApp (f x)
  fmap f (NP u c) = NP u (f <$> c)
 
instance Applicative (App u) where
  pure  = PureApp
  f <*> a = f <&&> (flip ($) `fmap` a)

-- | Right associative version of (<**>)
(<&&>) :: App u a -> App u (a->b) -> App u b
PureApp x <&&> f = evalApp x f
NP u v <&&> w = NP u (composeCont v (altToCont w))

infixr 4 <&&>

atom :: u a -> App u a
atom x = NP x idCont
