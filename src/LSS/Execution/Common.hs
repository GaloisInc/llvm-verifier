{- |
Module           : $Header$
Description      : Common type definitions and helper functions for LSS
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module LSS.Execution.Common where

import           Control.Monad.State      hiding (State)
import           Data.LLVM.Symbolic.AST
import           LSS.Execution.Codebase
import           LSS.SBEInterface
import           LSS.Execution.Utils
import           Text.PrettyPrint.HughesPJ
import           Text.PrettyPrint.Pretty
import           Verinf.Symbolic.Common
import qualified Data.Map                 as M
import qualified Text.LLVM                as L
import qualified Text.PrettyPrint.HughesPJ as PP

newtype Simulator sbe m a = SM { runSM :: StateT (State sbe m) m a }
  deriving ( Functor
           , Monad
           , MonadIO
           , MonadState (State sbe m)
           )

type LiftSBE sbe m = forall a. sbe a -> Simulator sbe m a

-- | Symbolic simulator state
data State sbe m = State
  { codebase  :: Codebase              -- ^ LLVM code, post-transformation to sym ast
  , symBE     :: SBE sbe               -- ^ Symbolic backend interface
  , liftSymBE :: LiftSBE sbe m         -- ^ Lift SBE operations into the Simulator monad
  , ctrlStk   :: CtrlStk (SBETerm sbe) -- ^ Control stack for tracking merge points
  }

data CtrlStk term = CtrlStk { mergeFrames :: [MergeFrame term] }

-- | There are three types of merge frames: (1) postdominator frames, which are
-- typical intraprocedural merge points and correspond to basic blocks which are
-- immediate postdominators of earlier-executing basic blocks; (2) return
-- frames, which capture points immediately following function calls; and (3)
-- exit frames, which represent program exit points.
data MergeFrame term
  = ExitFrame    { _mergedState :: Path term, _pending :: [Path term] }
  | PostdomFrame { _mergedState :: Path term, _pending :: [Path term] }
  | ReturnFrame
    { _retReg         :: Maybe Reg         -- ^ Register to store return value (if any)
    , _normalLabel    :: SymBlockID        -- ^ Label for normal path
    , _normalState    :: Maybe (Path term) -- ^ Merged state after function call return
    , _exceptLabel    :: Maybe SymBlockID  -- ^ Label for exception path
    , _exceptionState :: Maybe (Path term) -- ^ Merged exception state after function call return
    , _pending        :: [Path term]
    }

-- | Captures all symbolic execution state for a unique control-flow path (as
-- specified by the recorded path constraints)
data Path term = Path
  { pathCallFrames  :: [CallFrame term]   -- ^ The dynamic call stack for this
                                          -- path
  , pathException   :: Maybe term         -- ^ When handling an exception along
                                          -- this path, a pointer to the
                                          -- exception structure; Nothing
                                          -- otherwise
  , pathReturnValue :: Maybe term         -- ^ The return value along this path,
                                          -- if any.
  , pathCB          :: Maybe SymBlockID   -- ^ The currently-executing basic
                                          -- block along this path, if any.
  , pathConstraints :: term               -- ^ The constraints necessary for
                                          -- execution of this path
  }

type RegMap term = M.Map Reg (AtomicValue term)

-- | A frame (activation record) in the program being simulated
data CallFrame term = CallFrame
  { frmFuncSym :: L.Symbol
  , frmRegs    :: RegMap term
  }
  deriving Show

entryCallFrame :: CallFrame term
entryCallFrame = CallFrame (L.Symbol "_galois_lss_entry") M.empty

data AtomicValue intTerm
  = IValue { _w :: Int, unIValue :: intTerm }

instance PrettyTerm intTerm => Show (AtomicValue intTerm) where
  show (IValue w term) = "IValue {_w = " ++ show w ++ ", unIValue = " ++ prettyTerm term ++ "}"

-----------------------------------------------------------------------------------------
-- Pretty printing

instance Pretty (Path term) => Pretty (MergeFrame term) where
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
                      $+$ mpath "no normal-return merged state set" mns
                      $+$ maybe PP.empty
                            ( \el ->
                                text "Exc@" <> ppSymBlockID el
                                $+$ mpath "no exception path set" mes
                            )
                            mel
                      $+$ ppPendingPaths pps
    where
      ppPendingPaths pps =
        text "Pending paths:"
        $+$ nest 2 (if null pps then text "(none)" else vcat (map pp pps))

instance Pretty (Path term) => Pretty (CtrlStk term) where
  pp (CtrlStk mfs) =
    hang (text "CS" <+> lbrace) 2 (vcat (map pp mfs)) $+$ rbrace

--------------------------------------------------------------------------------
-- Pretty printing

instance PrettyTerm term => Pretty (RegMap term) where
  pp mp = vcat [ text (show r ++ " => " ++ show v) | (r,v) <- M.toList mp]

instance PrettyTerm term => Pretty (CallFrame term) where
  pp (CallFrame sym regMap) =
    text "CF" <> parens (L.ppSymbol sym) <> colon $+$ nest 2 (pp regMap)

instance (PrettyTerm term) => Pretty (Path term) where
  pp (Path frms _mexc _mrv mcb pathConstraint) =
    text "Path"
    <>  brackets (maybe (text "none") ppSymBlockID mcb)
    <>  colon <+> (parens $ text "Constraint:" <+> text (prettyTerm pathConstraint) )
    $+$ nest 2 (vcat $ map pp frms)

-----------------------------------------------------------------------------------------
-- Debugging

dumpCtrlStk :: (MonadIO m, PrettyTerm (SBETerm sbe)) => Simulator sbe m ()
dumpCtrlStk = banners . show . pp =<< gets ctrlStk

