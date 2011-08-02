{- |
Module           : $Header$
Description      : The interface to a symbolic backend
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module LSS.SBEInterface where

data SBE m term = SBE
  { falseTerm :: m term
  }

--------------------------------------------------------------------------------
-- SBE implementations

sbeStub :: SBE IO Int
sbeStub = SBE
  { falseTerm = return 0
  }
