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
import Data.Int
import Data.LLVM.Symbolic.AST

data AtomicValue int
  = IValue { _w :: Int32, unIValue :: int }
  deriving (Show)

-- | This typeclass defines the underlying execution semantics for the
-- LLVM-Symbolic instruction set.  The typeclass is parameterized over the
-- primitive types in use by a given simulator.
class (MonadIO m)
  => Semantics m int | m -> int where
  -----------------------------------------------------------------------------------------
  -- Integer operations

  -- | Returns the sum of two inputs
  iAdd :: int -> int -> m int

  -----------------------------------------------------------------------------------------
  -- LLVM-Sym operations

--  assign          :: Reg -> HERE: need ~AtomicValue-like
  setCurrentBlock :: SymBlockID -> m ()

  --------------------------------------------------------------------------------
  -- Execution and control-flow operations

  -- | Executes until the program terminates
  run :: m ()

