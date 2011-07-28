{- |
Module           : $Header$
Description      : The symbolic simulator for LLVM-Symbolic programs
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE EmptyDataDecls #-}

module LSS.Simulator

where

import Control.Monad.State (evalStateT)
import Control.Monad.Trans

import LSS.Execution.Types

runSimulator :: MonadIO m => Codebase -> Simulator m a -> m a
runSimulator cb m = evalStateT (runSM m) (newSimState cb)

newSimState :: Codebase -> State
newSimState cb = State cb

