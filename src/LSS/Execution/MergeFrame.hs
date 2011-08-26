{- |
Module           : $Header$
Description      : Potential join points for symbolic execution traces
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE ViewPatterns #-}

module LSS.Execution.MergeFrame
  ( emptyCtrlStk
  , emptyExitFrame
  , emptyReturnFrame
  , emptyPdomFrame

  -- * Control stack manipulation/query
  , pushMF
  , popMF
  , topMF
  , pushPendingPath
  , popPendingPath
  , topPendingPath

  -- * Merge frame manipulation/query
  , modifyPending
  , pushPending
  , popPending
  , topPending
  , pendingPaths
  , getMergedState
  , setMergedState
  , modifyMergedState
  , getExcMergedState
  , setExcMergedState
  , isExitFrame
  , isReturnFrame
  , finalizeExit

  -- * Path manipulation/query
  , modifyCallFrame
  , pc
  , setCurrentBlock
  , setReturnValue
  , withCallFrame
  )

where

import Control.Arrow          hiding ((<+>))
import Control.Monad
import Data.LLVM.Symbolic.AST
import LSS.Execution.Common
import LSS.Execution.Utils

emptyCtrlStk :: CtrlStk term mem
emptyCtrlStk = CtrlStk []

-- | Push a merge frame to the given control stack
pushMF :: MergeFrame term mem -> CtrlStk term mem -> CtrlStk term mem
pushMF mf (CtrlStk mfs) = CtrlStk (mf:mfs)

-- | Pops a merge frame from the given control stack; runtime error if the
-- control stack is empty.
popMF :: CtrlStk term mem -> (MergeFrame term mem, CtrlStk term mem)
popMF (CtrlStk [])       = error "popMF: empty control stack"
popMF (CtrlStk (mf:mfs)) = (mf, CtrlStk mfs)

-- | Obtains the merge frame at the top of the control stack; yields Nothing
-- when the control stack is empty
topMF :: CtrlStk term mem -> Maybe (MergeFrame term mem)
topMF = safeHead . mergeFrames

-- | Push a pending path into the topmost merge frame of the given control
-- stack; runtime error if the control stack is empty
pushPendingPath :: Path' term mem -> CtrlStk term mem -> CtrlStk term mem
pushPendingPath _ (CtrlStk []) = error "pushPendingPath: empty control stack"
pushPendingPath p' (CtrlStk cs) = CtrlStk $ headf cs (pushPending p')

-- | Pop a pending path from the topmost merge frame of the given control stack;
-- runtime error if either of the control stack or pending paths list are empty
popPendingPath :: CtrlStk term mem -> (Path' term mem, CtrlStk term mem)
popPendingPath (CtrlStk [])     = error "popPendingPath: empty control stack"
popPendingPath (CtrlStk (x:xs)) = second (CtrlStk . (:xs)) (popPending x)

-- | Obtains the first pending path in the merge frame at the top of the given
-- control stack; yields Nothing when either (a) the control stack is empty or
-- (b) the pending frames list of the topmost merge frame is empty.
topPendingPath :: CtrlStk term mem -> Maybe (Path' term mem)
topPendingPath = topMF >=> topPending

-- | Modify the current pending path in the given merge frame
modifyPending :: (Path' term mem -> Path' term mem) -> MergeFrame term mem -> MergeFrame term mem
modifyPending f mf = let (p, mf') = popPending mf in pushPending (f p) mf'

-- | Push a pending path to the given non-exit merge frame
pushPending :: Path' term mem -> MergeFrame term mem -> MergeFrame term mem
pushPending p mf = case mf of
  ExitFrame{}                   -> error "pushPending not supported on exit frames"
  PostdomFrame s ps bid         -> PostdomFrame s (p:ps) bid
  ReturnFrame rr nl ns el es ps -> ReturnFrame rr nl ns el es (p:ps)

-- | Pop a pending path from the given non-exit merge frame; runtime error if
-- | the merge frame's pending path list is empty.
popPending :: MergeFrame term mem -> (Path' term mem, MergeFrame term mem)
popPending mf = case mf of
  ExitFrame{}                       -> error "popPending not supported on exit frames"
  PostdomFrame _ [] _               -> err
  PostdomFrame s (p:ps) bid         -> (p, PostdomFrame s ps bid)
  ReturnFrame _ _ _ _ _ []          -> err
  ReturnFrame rr nl ns el es (p:ps) -> (p, ReturnFrame rr nl ns el es ps)
  where
    err = error "popPendingPath: empty path list"

isReturnFrame :: MergeFrame term mem -> Bool
isReturnFrame ReturnFrame{} = True
isReturnFrame _             = False

isExitFrame :: MergeFrame term mem -> Bool
isExitFrame ExitFrame{} = True
isExitFrame _           = False

-- | Modifies the merged state in a merge frame; in the case of return merge
-- frames, this function modifies the merged state for a normal function call
-- return.
modifyMergedState ::
     (Maybe (Path' term mem) -> Maybe (Path' term mem))
  -> MergeFrame term mem
  -> MergeFrame term mem
modifyMergedState f mf = setMergedState (f (getMergedState mf)) mf

-- | Obtains the merged state from a merge frame; in the case of return merge
-- frames, this function yields the merged state for normal function call
-- return.
getMergedState :: MergeFrame term mem -> Maybe (Path' term mem)
getMergedState (ExitFrame ms _ )          = ms
getMergedState (PostdomFrame ms _ _)      = ms
getMergedState (ReturnFrame _ _ ms _ _ _) = ms

-- | Sets the merged state in the given merge frame; in the case of return merge
-- frames, this function sets the merged state for normal function call return.
setMergedState :: Maybe (Path' term mem) -> MergeFrame term mem -> MergeFrame term mem
setMergedState ms (ExitFrame _ mrv)              = ExitFrame ms mrv
setMergedState ms (PostdomFrame _ ps bid)        = PostdomFrame ms ps bid
setMergedState ms (ReturnFrame rr nl _ el es ps) = ReturnFrame rr nl ms el es ps

-- | Obtains the merged exception state after function call return from a return
-- merge frame.
getExcMergedState :: MergeFrame term mem -> Maybe (Path' term mem)
getExcMergedState (ReturnFrame _ _ _ _ ms _) = ms
getExcMergedState _ = error "getExcMergedState: obtained non-return merge frame"

-- | In the given return merge frame, sets the merged exception state after
-- function call return.
setExcMergedState :: Maybe (Path' term mem) -> MergeFrame term mem -> MergeFrame term mem
setExcMergedState ms (ReturnFrame rr nl ns el _ ps) = ReturnFrame rr nl ns el ms ps
setExcMergedState _ _ = error "setExcMergedState: given non-return merge frame"

-- | Sets the return value in the given path
setReturnValue :: Maybe term -> Path' term mem -> Path' term mem
setReturnValue mrv p = p{ pathRetVal = mrv }

-- | Obtains the first pending path in the given merge frame; yields Nothing if
-- | the pending frames list is empty.
topPending :: MergeFrame term mem -> Maybe (Path' term mem)
topPending = safeHead . pendingPaths

-- | Yields all pending paths in the given non-exit merge frame
pendingPaths :: MergeFrame term mem -> [Path' term mem]
pendingPaths ExitFrame{}                = error "pendingPaths not supported for exit merge frames"
pendingPaths (PostdomFrame _ ps _)      = ps
pendingPaths (ReturnFrame _ _ _ _ _ ps) = ps

setCurrentBlock :: SymBlockID -> Path' term mem -> Path' term mem
setCurrentBlock blk p = p{ pathCB = Just blk }

setCallFrame :: CallFrame term -> Path' term mem -> Path' term mem
setCallFrame cf p = p{ pathCallFrame = cf }

modifyCallFrame :: (CallFrame term -> CallFrame term) -> Path' term mem -> Path' term mem
modifyCallFrame f p = setCallFrame (f $ pathCallFrame p) p

withCallFrame :: Path' term mem -> (CallFrame term -> a) -> a
withCallFrame (pathCallFrame -> cf) f = f cf

emptyReturnFrame :: MergeFrame term mem
emptyReturnFrame = ReturnFrame Nothing initSymBlockID Nothing Nothing Nothing []

emptyExitFrame :: MergeFrame term mem
emptyExitFrame = ExitFrame Nothing Nothing

emptyPdomFrame :: SymBlockID -> MergeFrame term mem
emptyPdomFrame = PostdomFrame Nothing []

finalizeExit :: MergeFrame term mem -> MergeFrame term mem
finalizeExit (ExitFrame _ Just{})      = error "finalizeExitFrame: frame already finalized"
finalizeExit ef@(ExitFrame Nothing _)  = ef
finalizeExit (ExitFrame (Just mrgd) _) = ExitFrame Nothing $ pathRetVal mrgd
finalizeExit _                         = error "finalizeExitFrame: non-exit frame"

pc :: Constraint term -> term
pc (Constraint _ t) = t

