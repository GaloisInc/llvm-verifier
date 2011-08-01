{- |
Module           : $Header$
Description      : Potential join points for symbolic execution traces
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE ViewPatterns #-}

module LSS.Execution.MergeFrame where

import Data.LLVM.Symbolic.AST
import LSS.Execution.Utils

data CtrlStk' path term = CtrlStk { csEntries :: [CSEnt' path term] }

emptyCtrlStk :: CtrlStk' path term
emptyCtrlStk = CtrlStk []

pushMF :: MergeFrame' p t -> CtrlStk' p t -> CtrlStk' p t
pushMF mf (CtrlStk cs) = CtrlStk $ CSEnt mf [] : cs

-- | Push a pending path into the topmost control stack entry
pushPendingPath :: p -> CtrlStk' p t -> CtrlStk' p t
pushPendingPath _ (CtrlStk []) = error "pushPendingPath: empty control stack"
pushPendingPath p (CtrlStk cs) = CtrlStk $ headf cs (csePushPending p)

-- | A control stack entry associates a merge frame with paths that still need
-- to be executed on top of of it.

data CSEnt' path term = CSEnt
  { cseMergeFrame   :: MergeFrame' path term
  , csePendingPaths :: [path]
  }

csePushPending :: path -> CSEnt' path term -> CSEnt' path term
csePushPending p (CSEnt mf pps) = CSEnt mf (p:pps)

-- | There are three types of merge frames: (1) postdominator frames, which are
-- typical intraprocedural merge points and correspond to basic blocks which are
-- immediate postdominators of earlier-executing basic blocks; (2) return
-- frames, which capture points immediately following function calls; and (3)
-- exit frames, which represent program exit points.

data MergeFrame' path term
  = ExitFrame    { _mergedState :: path }
  | PostdomFrame { _mergedState :: path }
  | ReturnFrame
    { _retReg         :: Maybe Reg        -- ^ Register to store return value (if any)
    , _normalLabel    :: SymBlockID       -- ^ Label for normal path
    , _normalState    :: Maybe path       -- ^ Merged state after function call return
    , _exceptLabel    :: Maybe SymBlockID -- ^ Label for exception path
    , _exceptionState :: Maybe path       -- ^ Merged exception state after function call return
    }

emptyReturnFrame :: MergeFrame' path term
emptyReturnFrame = ReturnFrame Nothing initSymBlockID Nothing Nothing Nothing
