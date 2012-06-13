{- |
Module           : $Header$
Description      : Potential join points for symbolic execution traces
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE ViewPatterns #-}

module LSS.Execution.MergeFrame
  ( 
  -- * Control stack manipulation/query
    pushMF
  , popMF
  , topMF
  -- * Merge frame manipulation/query
  , pushPending
  , popPending
  , pendingPaths
  , getMergedState
  , setMergedState
  , isExitFrame

  -- * Path manipulation/query
  , modifyPathRegs
  , pcTerm
  )

where

import LSS.Execution.Common
import LSS.Execution.Utils

-- | Push a merge frame to the given control stack
pushMF :: MergeFrame term mem -> CtrlStk term mem -> CtrlStk term mem
pushMF mf (CtrlStk mfs) = CtrlStk (mf:mfs)

-- | Pops a merge frame from the given control stack; runtime error if the
-- control stack is empty.
popMF :: CtrlStk term mem -> Maybe (MergeFrame term mem, CtrlStk term mem)
popMF (CtrlStk [])       = Nothing
popMF (CtrlStk (mf:mfs)) = Just (mf, CtrlStk mfs)

-- | Obtains the merge frame at the top of the control stack; yields Nothing
-- when the control stack is empty
topMF :: CtrlStk term mem -> Maybe (MergeFrame term mem)
topMF = safeHead . mergeFrames

isExitFrame :: MergeFrame term mem -> Bool
isExitFrame ExitMergeFrame{} = True
isExitFrame _                = False

-- | Obtains the merged state from a merge frame; in the case of return merge
-- frames, this function yields the merged state for normal function call
-- return.
getMergedState :: String -> MergeFrame term mem -> MergedState term mem
getMergedState nm (ExitMergeFrame _)     = error $ nm ++ ": ExitFrame has no merged state"
getMergedState _ (PostdomMergeFrame pdf) = pdfMergedState pdf
getMergedState _ (ReturnMergeFrame rf)   = rfNormalState rf

-- | Sets the merged state in the given merge frame; in the case of return merge
-- frames, this function sets the merged state for normal function call return.
setMergedState :: MergedState term mem -> MergeFrame term mem -> MergeFrame term mem
setMergedState _ (ExitMergeFrame _)       = error "setMergedState: ExitFrame has no merged state"
setMergedState ms (PostdomMergeFrame pdf) = PostdomMergeFrame pdf { pdfMergedState = ms }
setMergedState ms (ReturnMergeFrame rf)   = ReturnMergeFrame rf { rfNormalState = ms }

-- | Yields all pending paths in the given non-exit merge frame
pendingPaths :: MergeFrame term mem -> [Path' term mem]
pendingPaths (ExitMergeFrame ef)     = efPending ef
pendingPaths (PostdomMergeFrame pdf) = pdfPending pdf
pendingPaths (ReturnMergeFrame rf)   = rfPending rf 

-- | Push a pending path to the given non-exit merge frame
pushPending :: Path' term mem -> MergeFrame term mem -> MergeFrame term mem
pushPending p mf = case mf of
  ExitMergeFrame ef     -> ExitMergeFrame     ef { efPending  = p : efPending ef }
  PostdomMergeFrame pdf -> PostdomMergeFrame pdf { pdfPending = p : pdfPending pdf }
  ReturnMergeFrame rf   -> ReturnMergeFrame   rf { rfPending  = p : rfPending rf }

-- | Pop a pending path from the given non-exit merge frame; runtime error if
-- | the merge frame's pending path list is empty.
popPending :: MergeFrame term mem -> Maybe (Path' term mem, MergeFrame term mem)
popPending mf = case mf of
  ExitMergeFrame ef -> 
    case efPending ef of
      [] -> Nothing
      p:ps -> Just (p, ExitMergeFrame ef { efPending = ps })
  PostdomMergeFrame pdf -> 
    case pdfPending pdf of
      [] -> Nothing
      p:ps -> Just (p, PostdomMergeFrame pdf { pdfPending = ps })
  ReturnMergeFrame rf ->
    case rfPending rf of
      [] -> Nothing
      p:ps -> Just (p, ReturnMergeFrame rf { rfPending = ps })

modifyPathRegs :: (RegMap term -> RegMap term) -> Path' term mem -> Path' term mem
modifyPathRegs f p = p { pathRegs = f (pathRegs p) }