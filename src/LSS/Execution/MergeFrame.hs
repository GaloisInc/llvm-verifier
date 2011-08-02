{- |
Module           : $Header$
Description      : Potential join points for symbolic execution traces
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE ViewPatterns #-}

module LSS.Execution.MergeFrame
  ( CtrlStk'(..)
  , MergeFrame'(..)

  , emptyCtrlStk
  , emptyExitFrame
  , emptyReturnFrame

  , pushMF
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
import           LSS.Execution.Utils

import           Text.PrettyPrint.Pretty
import           Text.PrettyPrint.HughesPJ
import qualified Text.PrettyPrint.HughesPJ as PP

data CtrlStk' p t = CtrlStk { mergeFrames :: [MergeFrame' p t] }

emptyCtrlStk :: CtrlStk' p t
emptyCtrlStk = CtrlStk []

pushMF :: MergeFrame' p t -> CtrlStk' p t -> CtrlStk' p t
pushMF mf (CtrlStk mfs) = CtrlStk (mf:mfs)

-- | Obtains the merge frame at the top of the control stack; yields Nothing
-- when the control stack is empty
topMF :: CtrlStk' p t -> Maybe (MergeFrame' p t)
topMF (CtrlStk mfs) = listToMaybe mfs

-- | Push a pending path into the topmost merge frame; runtime error if the
-- control stack is empty
pushPendingPath :: p -> CtrlStk' p t -> CtrlStk' p t
pushPendingPath _ (CtrlStk []) = error "pushPendingPath: empty control stack"
pushPendingPath p' (CtrlStk cs) = CtrlStk $ headf cs (pushPending p')
  where
    pushPending p mf = case mf of
      ExitFrame s ps                -> ExitFrame s (p:ps)
      PostdomFrame s ps             -> PostdomFrame s (p:ps)
      ReturnFrame rr nl ns el es ps -> ReturnFrame rr nl ns el es (p:ps)

-- | Pop a pending path from the topmost merge frame; runtime error if either
-- of the control stack or pending paths list are empty
popPendingPath :: CtrlStk' p t -> (p, CtrlStk' p t)
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
topPendingPath :: CtrlStk' p t -> Maybe p
topPendingPath = topMF >=> topPending

topPending :: MergeFrame' p t -> Maybe p
topPending mf = listToMaybe (pendingPaths mf)

-- | There are three types of merge frames: (1) postdominator frames, which are
-- typical intraprocedural merge points and correspond to basic blocks which are
-- immediate postdominators of earlier-executing basic blocks; (2) return
-- frames, which capture points immediately following function calls; and (3)
-- exit frames, which represent program exit points.
data MergeFrame' path term
  = ExitFrame    { _mergedState :: path, _pending :: [path] }
  | PostdomFrame { _mergedState :: path, _pending :: [path] }
  | ReturnFrame
    { _retReg         :: Maybe Reg        -- ^ Register to store return value (if any)
    , _normalLabel    :: SymBlockID       -- ^ Label for normal path
    , _normalState    :: Maybe path       -- ^ Merged state after function call return
    , _exceptLabel    :: Maybe SymBlockID -- ^ Label for exception path
    , _exceptionState :: Maybe path       -- ^ Merged exception state after function call return
    , _pending        :: [path]
    }

pendingPaths :: MergeFrame' p t -> [p]
pendingPaths (ExitFrame _ ps)           = ps
pendingPaths (PostdomFrame _ ps)        = ps
pendingPaths (ReturnFrame _ _ _ _ _ ps) = ps

emptyReturnFrame :: MergeFrame' p t
emptyReturnFrame = ReturnFrame Nothing initSymBlockID Nothing Nothing Nothing []

emptyExitFrame :: p -> MergeFrame' p t
emptyExitFrame = flip ExitFrame []

-----------------------------------------------------------------------------------------
-- Pretty printing

instance Pretty path => Pretty (MergeFrame' path term) where
  pp mf = case mf of
    ExitFrame p pps ->
      text "MF(Exit):" $+$ nest 2 (pp p) $+$ nest 2 (ppPendingPaths pps)
    PostdomFrame p pps ->
      text "MF(Pdom):" $+$ nest 2 (pp p) $+$ nest 2 (ppPendingPaths pps)
    ReturnFrame _mr nl mns mel mes pps ->
      text "MF(Retn):" $+$ nest 2 rest
        where
          mpath str = nest 2 . maybe (parens $ text str) pp
          rest      = text "Normal@" <> ppSymBlockID nl <> colon
                      $+$ mpath "no normal path set" mns
                      $+$ maybe PP.empty
                            ( \el ->
                                text "Exc@" <> ppSymBlockID el
                                $+$ mpath "no exception path set" mes
                            )
                            mel
                      $+$ ppPendingPaths pps
    where
      ppPendingPaths :: Pretty path => [path] -> Doc
      ppPendingPaths pps =
        text "Pending paths:"
        $+$ nest 2 (if null pps then text "(none)" else vcat (map pp pps))

instance Pretty path => Pretty (CtrlStk' path term) where
  pp (CtrlStk mfs) =
    hang (text "CS" <+> lbrace) 2 (vcat (map pp mfs)) $+$ rbrace
