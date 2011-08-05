{- |
Module           : $Header$
Description      : Potential join points for symbolic execution traces
Stability        : provisional
Point-of-contact : jstanley
-}

module LSS.Execution.MergeFrame
  ( emptyCtrlStk
  , emptyExitFrame
  , emptyReturnFrame

  , pushMF
  , popMF
  , topMF

  , pushPendingPath
  , popPendingPath
  , topPendingPath
  , pendingPaths
  )

where

import           Control.Arrow             hiding ((<+>))
import           Control.Monad
import           Data.Maybe
import           Data.LLVM.Symbolic.AST
import           LSS.Execution.Common
import           LSS.Execution.Utils

emptyCtrlStk :: CtrlStk term
emptyCtrlStk = CtrlStk []

pushMF :: MergeFrame term -> CtrlStk term -> CtrlStk term
pushMF mf (CtrlStk mfs) = CtrlStk (mf:mfs)

-- | Pops the merge frame from the top of the control stack; runtime error if
-- the control stack is empty.
popMF :: CtrlStk term -> (MergeFrame term, CtrlStk term)
popMF (CtrlStk [])       = error "popMF: empty control stack"
popMF (CtrlStk (mf:mfs)) = (mf, CtrlStk mfs)

-- | Obtains the merge frame at the top of the control stack; yields Nothing
-- when the control stack is empty
topMF :: CtrlStk term -> Maybe (MergeFrame term)
topMF (CtrlStk mfs) = listToMaybe mfs

-- | Push a pending path into the topmost merge frame; runtime error if the
-- control stack is empty
pushPendingPath :: Path term -> CtrlStk term -> CtrlStk term
pushPendingPath _ (CtrlStk []) = error "pushPendingPath: empty control stack"
pushPendingPath p' (CtrlStk cs) = CtrlStk $ headf cs (pushPending p')
  where
    pushPending p mf = case mf of
      ExitFrame s ps                -> ExitFrame s (p:ps)
      PostdomFrame s ps             -> PostdomFrame s (p:ps)
      ReturnFrame rr nl ns el es ps -> ReturnFrame rr nl ns el es (p:ps)

-- | Pop a pending path from the topmost merge frame; runtime error if either
-- of the control stack or pending paths list are empty
popPendingPath :: CtrlStk term -> (Path term, CtrlStk term)
popPendingPath (CtrlStk [])     = error "popPendingPath: empty control stack"
popPendingPath (CtrlStk (x:xs)) = second (CtrlStk . (:xs)) (popPending x)
  where
    popPending mf = case mf of
      ExitFrame _ []                    -> err
      ExitFrame s (p:ps)                -> (p, ExitFrame s ps)
      PostdomFrame _ []                 -> err
      PostdomFrame s (p:ps)             -> (p, PostdomFrame s ps)
      ReturnFrame _ _ _ _ _ []          -> err
      ReturnFrame rr nl ns el es (p:ps) -> (p, ReturnFrame rr nl ns el es ps)
      where
        err = error "popPendingPath: empty path list"

-- | Obtains the top pending path in the merge frame at the top of the control
-- stack; yields Nothing when either of the control stack or the pending frames
-- stack of the topmost merge frame on the control are empty.
topPendingPath :: CtrlStk term -> Maybe (Path term)
topPendingPath = topMF >=> topPending

topPending :: MergeFrame term -> Maybe (Path term)
topPending mf = listToMaybe (pendingPaths mf)

pendingPaths :: MergeFrame term -> [Path term]
pendingPaths (ExitFrame _ ps)           = ps
pendingPaths (PostdomFrame _ ps)        = ps
pendingPaths (ReturnFrame _ _ _ _ _ ps) = ps

emptyReturnFrame :: MergeFrame term
emptyReturnFrame = ReturnFrame Nothing initSymBlockID Nothing Nothing Nothing []

emptyExitFrame :: Path term -> MergeFrame term
emptyExitFrame = flip ExitFrame []
