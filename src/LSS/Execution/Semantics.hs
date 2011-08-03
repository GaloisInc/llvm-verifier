{- |
Module           : $Header$
Description      : Provides an executable semantics for LLVM-Symbolic programs
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module LSS.Execution.Semantics where

import Control.Monad.Trans
import Data.Int
import Data.LLVM.Symbolic.AST

data AtomicValue int
  = IValue { _w :: Int32, unIValue :: int }
  deriving (Show)

-- | This typeclass defines the underlying execution semantics for the
-- LLVM-Symbolic instruction set.
class (MonadIO m) => Semantics sbe m | sbe -> m where
  type IntTy (sbe :: * -> *)

  -----------------------------------------------------------------------------------------
  -- Integer operations

  -- | Returns the sum of two inputs
--  iAdd :: int -> int -> m int
  iAdd :: IntTy sbe -> IntTy sbe -> m (IntTy sbe)

  -----------------------------------------------------------------------------------------
  -- LLVM-Sym operations

--  assign          :: Reg -> HERE: need ~AtomicValue-like
  setCurrentBlock :: SymBlockID -> m ()

  --------------------------------------------------------------------------------
  -- Execution and control-flow operations

  -- | Executes until the program terminates
  run :: m ()

