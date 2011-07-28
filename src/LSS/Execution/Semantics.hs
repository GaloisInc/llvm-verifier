{- |
Module           : $Header$
Description      : Provides an executable semantics for LLVM-Symbolic programs
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module LSS.Execution.Semantics where

import Control.Monad.Trans

-- | This typeclass defines the underlying execution semantics for the
-- LLVM-Symbolic instruction set.  The typeclass is parameterized over the
-- primitive types in use by a given simulator.

class (MonadIO m)
  => Semantics m int rslt | m -> int rslt where
  -----------------------------------------------------------------------------------------
  -- Integer operations

  -- | Returns the sum of two inputs
  iAdd :: int -> int -> m int

  --------------------------------------------------------------------------------
  -- Execution and control-flow operations

  -- | Single-steps the current program
  doStep :: m ()

  -- | Executes the doStep operation until the program terminates
  run :: m rslt
