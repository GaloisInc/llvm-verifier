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
import qualified Text.LLVM as L

data AtomicValue int
  = IValue { _w :: Int32, unIValue :: int }
  deriving (Show)

-- | This typeclass defines the underlying execution semantics for the
-- LLVM-Symbolic instruction set.
class (MonadIO m) => Semantics sbe m | m -> sbe where
  type IntTerm      (sbe :: * -> *)
  type FrameTy      (sbe :: * -> *)
  type MergeFrameTy (sbe :: * -> *)

  -----------------------------------------------------------------------------------------
  -- Integer operations

  -- | Returns the sum of two inputs
  iAdd :: IntTerm sbe -> IntTerm sbe -> m (IntTerm sbe)

  -----------------------------------------------------------------------------------------
  -- LLVM-Sym operations

  assign          :: Reg -> AtomicValue (IntTerm sbe) -> m ()
  setCurrentBlock :: SymBlockID -> m ()

  -- | @eval expr@ evaluates @expr@ via the symbolic backend
  eval :: SymExpr -> m (AtomicValue (IntTerm sbe))

  -- | @popFrame@ removes the top entry of the call frame stack in the current
  -- path; assumes that the call frame stack is nonempty and that there is a
  -- current path defined.
  popFrame :: m (FrameTy sbe)

  -- | @popMergeFrame@ removes the top entry of the merge frame stack; assumes
  -- that the control stack is nonempty.
  popMergeFrame :: m (MergeFrameTy sbe)

  mergeReturn :: FrameTy sbe -> MergeFrameTy sbe -> Maybe (L.Typed SymValue) -> m ()

  --------------------------------------------------------------------------------
  -- Execution and control-flow operations

  -- | Executes until the program terminates
  run :: m ()

  --------------------------------------------------------------------------------
  -- Debugging

  dumpCtrlStk :: m ()
