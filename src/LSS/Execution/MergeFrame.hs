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
  , setCurrentBlock
  , setReturnValue

  -- * AtomicValue utility functions
  , fromAV
  , toAV
  )

where

import Control.Arrow          hiding ((<+>))
import Control.Monad
import Data.LLVM.Symbolic.AST
import LSS.Execution.Common
import LSS.Execution.Utils
import qualified Text.LLVM as L

fromAV :: AtomicValue term -> term
fromAV (IValue _ t) = t

toAV :: L.Typed term -> AtomicValue term
toAV (L.Typed (L.PrimType (L.Integer (fromIntegral -> w))) t) = IValue w t
toAV (L.Typed t _) = error $ "toAV: unsupported type: " ++ show (L.ppType t)

emptyCtrlStk :: CtrlStk term
emptyCtrlStk = CtrlStk []

-- | Push a merge frame to the given control stack
pushMF :: MergeFrame term -> CtrlStk term -> CtrlStk term
pushMF mf (CtrlStk mfs) = CtrlStk (mf:mfs)

-- | Pops a merge frame from the given control stack; runtime error if the
-- control stack is empty.
popMF :: CtrlStk term -> (MergeFrame term, CtrlStk term)
popMF (CtrlStk [])       = error "popMF: empty control stack"
popMF (CtrlStk (mf:mfs)) = (mf, CtrlStk mfs)

-- | Obtains the merge frame at the top of the control stack; yields Nothing
-- when the control stack is empty
topMF :: CtrlStk term -> Maybe (MergeFrame term)
topMF = safeHead . mergeFrames

-- | Push a pending path into the topmost merge frame of the given control
-- stack; runtime error if the control stack is empty
pushPendingPath :: Path term -> CtrlStk term -> CtrlStk term
pushPendingPath _ (CtrlStk []) = error "pushPendingPath: empty control stack"
pushPendingPath p' (CtrlStk cs) = CtrlStk $ headf cs (pushPending p')

-- | Pop a pending path from the topmost merge frame of the given control stack;
-- runtime error if either of the control stack or pending paths list are empty
popPendingPath :: CtrlStk term -> (Path term, CtrlStk term)
popPendingPath (CtrlStk [])     = error "popPendingPath: empty control stack"
popPendingPath (CtrlStk (x:xs)) = second (CtrlStk . (:xs)) (popPending x)

-- | Obtains the first pending path in the merge frame at the top of the given
-- control stack; yields Nothing when either (a) the control stack is empty or
-- (b) the pending frames list of the topmost merge frame is empty.
topPendingPath :: CtrlStk term -> Maybe (Path term)
topPendingPath = topMF >=> topPending

-- | Modify the current pending path in the given merge frame
modifyPending :: (Path term -> Path term) -> MergeFrame term -> MergeFrame term
modifyPending f mf = let (p, mf') = popPending mf in pushPending (f p) mf'

-- | Push a pending path to the given non-exit merge frame
pushPending :: Path term -> MergeFrame term -> MergeFrame term
pushPending p mf = case mf of
  ExitFrame{}                   -> error "pushPending not supported on exit frames"
  PostdomFrame s ps             -> PostdomFrame s (p:ps)
  ReturnFrame rr nl ns el es ps -> ReturnFrame rr nl ns el es (p:ps)

-- | Pop a pending path from the given non-exit merge frame; runtime error if
-- | the merge frame's pending path list is empty.
popPending :: MergeFrame term -> (Path term, MergeFrame term)
popPending mf = case mf of
  ExitFrame{}                       -> error "popPending not supported on exit frames"
  PostdomFrame _ []                 -> err
  PostdomFrame s (p:ps)             -> (p, PostdomFrame s ps)
  ReturnFrame _ _ _ _ _ []          -> err
  ReturnFrame rr nl ns el es (p:ps) -> (p, ReturnFrame rr nl ns el es ps)
  where
    err = error "popPendingPath: empty path list"

isReturnFrame :: MergeFrame term -> Bool
isReturnFrame ReturnFrame{} = True
isReturnFrame _             = False

isExitFrame :: MergeFrame term -> Bool
isExitFrame ExitFrame{} = True
isExitFrame _           = False

-- | Modifies the merged state in a merge frame; in the case of return merge
-- frames, this function modifies the merged state for a normal function call
-- return.
modifyMergedState ::
     (Maybe (Path term) -> Maybe (Path term))
  -> MergeFrame term
  -> MergeFrame term
modifyMergedState f mf = setMergedState (f (getMergedState mf)) mf

-- | Obtains the merged state from a merge frame; in the case of return merge
-- frames, this function yields the merged state for normal function call
-- return.
getMergedState :: MergeFrame term -> Maybe (Path term)
getMergedState (ExitFrame ms _ )          = ms
getMergedState (PostdomFrame ms _)        = ms
getMergedState (ReturnFrame _ _ ms _ _ _) = ms

-- | Sets the merged state in the given merge frame; in the case of return merge
-- frames, this function sets the merged state for normal function call return.
setMergedState :: Maybe (Path term) -> MergeFrame term -> MergeFrame term
setMergedState ms (ExitFrame _ mrv)              = ExitFrame ms mrv
setMergedState ms (PostdomFrame _ ps)            = PostdomFrame ms ps
setMergedState ms (ReturnFrame rr nl _ el es ps) = ReturnFrame rr nl ms el es ps

-- | Obtains the merged exception state after function call return from a return
-- merge frame.
getExcMergedState :: MergeFrame term -> Maybe (Path term)
getExcMergedState (ReturnFrame _ _ _ _ ms _) = ms
getExcMergedState _ = error "getExcMergedState: obtained non-return merge frame"

-- | In the given return merge frame, sets the merged exception state after
-- function call return.
setExcMergedState :: Maybe (Path term) -> MergeFrame term -> MergeFrame term
setExcMergedState ms (ReturnFrame rr nl ns el _ ps) = ReturnFrame rr nl ns el ms ps
setExcMergedState _ _ = error "setExcMergedState: given non-return merge frame"

-- | Sets the return value in the given path
setReturnValue :: Maybe term -> Path term -> Path term
setReturnValue mrv p = p{ pathRetVal = mrv }

-- | Obtains the first pending path in the given merge frame; yields Nothing if
-- | the pending frames list is empty.
topPending :: MergeFrame term -> Maybe (Path term)
topPending = safeHead . pendingPaths

-- | Yields all pending paths in the given non-exit merge frame
pendingPaths :: MergeFrame term -> [Path term]
pendingPaths ExitFrame{}                = error "pendingPaths not supported for exit merge frames"
pendingPaths (PostdomFrame _ ps)        = ps
pendingPaths (ReturnFrame _ _ _ _ _ ps) = ps

setCurrentBlock :: SymBlockID -> Path term -> Path term
setCurrentBlock blk p = p{ pathCB = Just blk }

setCallFrame :: CallFrame term -> Path term -> Path term
setCallFrame cf p = p{ pathCallFrame = cf }

modifyCallFrame :: (CallFrame term -> CallFrame term) -> Path term -> Path term
modifyCallFrame f p = setCallFrame (f $ pathCallFrame p) p

emptyReturnFrame :: MergeFrame term
emptyReturnFrame = ReturnFrame Nothing initSymBlockID Nothing Nothing Nothing []

emptyExitFrame :: MergeFrame term
emptyExitFrame = ExitFrame Nothing Nothing

finalizeExit :: MergeFrame term -> MergeFrame term
finalizeExit (ExitFrame _ Just{})      = error "finalizeExitFrame: frame already finalized"
finalizeExit ef@(ExitFrame Nothing _)  = ef
finalizeExit (ExitFrame (Just mrgd) _) = ExitFrame Nothing $ pathRetVal mrgd
finalizeExit _                         = error "finalizeExitFrame: non-exit frame"
