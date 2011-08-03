{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : jstanley
-}

module LSS.Execution.Stepper (step) where

import Control.Monad
import Control.Monad.Trans

import LSS.Execution.Codebase
import LSS.Execution.Semantics
import LSS.Execution.Utils
import Data.LLVM.Symbolic.AST

-- | Execute a single instruction
step :: Semantics sbe m => SymStmt -> m ()

step ClearCurrentExecution =
  error "ClearCurrentExecution nyi"

step (PushCallFrame _fn _args _mres) =
  error "PushCallFrame nyi"

step (PushInvokeFrame _fn _args _mres _e) =
  error "PushInvokeFrame nyi"

step (PushPostDominatorFrame _pdid) =
  error "PushPostDominatorFrame nyi"

step (MergePostDominator _pdid _cond) =
  error "MergePostDominator nyi"

step MergeReturnVoidAndClear =
  error "MergeReturnVoidAndClear nyi"

step (MergeReturnAndClear _resx) =
  error "MergeReturnAndClear nyi"

step (PushPendingExecution _cond) =
  error "PushPendingExecution nyi"

step (SetCurrentBlock bid) = setCurrentBlock bid

step (AddPathConstraint _cond) =
  error "AddPathConstraint nyi"

step (Assign reg expr) = assign reg =<< eval expr

step (Store _addr _val) =
  error "Store nyi"

step (IfThenElse _c _thenStms _elseStms)
  = error "IfThenElse nyi"

step Unreachable
  = error "step: Encountered 'unreachable' instruction"

step Unwind
  = error "Unwind nyi"