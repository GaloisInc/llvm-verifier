{- |
Module           : $Header$
Description      : Common types for LLVM Sym execution
Stability        : provisional
Point-of-contact : jstanley
-}

module LSS.Execution.Types
  ( module LSS.Execution.Codebase
  , Simulator(..)
  , State(..)
  , Value(..)
  )
where

import Control.Monad.State (StateT(..))
import LSS.Execution.Codebase

-- | Atomic values for simulation, parameterized over primitive types
data Value int = IValue { unIValue :: int }

data State = State { codebase :: Codebase }

newtype Simulator m a = SM { runSM :: StateT State m a }


