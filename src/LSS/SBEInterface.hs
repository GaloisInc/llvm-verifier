{- |
Module           : $Header$
Description      : The interface to a symbolic backend
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module LSS.SBEInterface where

class SupportedSBE sbe term | sbe -> term where
  falseTerm :: sbe -> term

--------------------------------------------------------------------------------
-- SBE implementations

newtype SBEStub = SBEStub ()
sbeStub :: SBEStub
sbeStub = SBEStub ()

instance SupportedSBE SBEStub Int where
  falseTerm SBEStub{} = 0


