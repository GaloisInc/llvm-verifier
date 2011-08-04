{- |
Module           : $Header$
Description      : The interface to a symbolic backend
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE TypeFamilies #-}

module LSS.SBEInterface where

-- | SBETerm is a function over types that yields the term type associated with
-- a particular SBE interface implementation
type family SBETerm (sbe :: * -> *)

data SBE m = SBE
  { falseTerm   :: m (SBETerm m)
  , termInteger :: Integer -> m (SBETerm m)
  , applyAdd    :: SBETerm m -> SBETerm m -> m (SBETerm m)
  }

--------------------------------------------------------------------------------
-- SBE implementations

newtype SBEStub a = SBEStub { runStub :: a }
type instance SBETerm SBEStub = Int

sbeStub :: SBE SBEStub
sbeStub = SBE
  { falseTerm   = SBEStub 0
  , termInteger = SBEStub . fromIntegral
  , applyAdd    = \x y -> SBEStub (x + y)
  }

liftStubToIO :: SBEStub a -> IO a
liftStubToIO = return . runStub

newtype SBEStubTwo a = SBEStubTwo { runStubTwo :: a }
type instance SBETerm SBEStubTwo = Integer

sbeStubTwo :: SBE SBEStubTwo
sbeStubTwo = SBE
  { falseTerm   = SBEStubTwo 0
  , termInteger = SBEStubTwo . fromIntegral
  , applyAdd    = \x y -> SBEStubTwo (x + y)
  }

liftStubTwoToIO :: SBEStubTwo a -> IO a
liftStubTwoToIO = return . runStubTwo
