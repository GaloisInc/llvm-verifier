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
import           Verinf.Utils.LogMonad
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
  , verbosity :: Int
  }

data CtrlStk term = CtrlStk { mergeFrames :: [MergeFrame term] }

-- | There are three types of merge frames: (1) postdominator frames, which are
-- typical intraprocedural merge points and correspond to basic blocks which are
-- immediate postdominators of earlier-executing basic blocks; (2) return
-- frames, which capture points immediately following function calls; and (3)
-- exit frames, which represent program exit points.
data MergeFrame term
  = ExitFrame
    { _mergedState  :: Maybe (Path term)
    , programRetVal :: Maybe term
    }
  | PostdomFrame
    { _mergedState :: Maybe (Path term)
    , _pending :: [Path term]
    }
  | ReturnFrame
    { rfRetReg        :: Maybe (L.Typed Reg) -- ^ Register to store return value (if any)
    , rfNormalLabel   :: SymBlockID          -- ^ Label for normal path
    , _normalState    :: Maybe (Path term)   -- ^ Merged state after function call return
    , _exceptLabel    :: Maybe SymBlockID    -- ^ Label for exception path
    , _exceptionState :: Maybe (Path term)   -- ^ Merged exception state after function call return
    , _pending        :: [Path term]
    }

-- | Captures all symbolic execution state for a unique control-flow path (as
-- specified by the recorded path constraints)
data Path term = Path
  { pathCallFrame   :: CallFrame term     -- ^ The top call frame of the dynamic
                                          -- call stack along this path
  , pathRetVal      :: Maybe term         -- ^ The return value along this path
                                          -- after normal function call return,
                                          -- if any.
  , pathException   :: Maybe term         -- ^ When handling an exception along
                                          -- this path, a pointer to the
                                          -- exception structure; Nothing
                                          -- otherwise
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

data AtomicValue intTerm
  = IValue { _w :: Int, unIValue :: intTerm }

--------------------------------------------------------------------------------
-- Misc typeclass instances

instance MonadIO m => LogMonad (Simulator sbe m) where
  getVerbosity   = gets verbosity
  setVerbosity v = modify $ \s -> s{ verbosity = v }

-----------------------------------------------------------------------------------------
-- Pretty printing

instance PrettyTerm intTerm => Show (AtomicValue intTerm) where
  show (IValue w term) =
    "IValue {_w = " ++ show w ++ ", unIValue = " ++ prettyTerm term ++ "}"

instance (PrettyTerm term, Pretty (Path term)) => Pretty (MergeFrame term) where
  pp mf = case mf of
    ExitFrame mp mrv ->
      text "MF(Exit):"
      $+$ mpath "no merged state set" mp
      $+$ nest 2 ( maybe (parens $ text "no return value set")
                         (\rv -> text "Return value:" <+> text (prettyTerm rv))
                         mrv
                 )
    PostdomFrame p pps ->
      text "MF(Pdom):"
      $+$ nest 2 (text "Merged:" <+> pp p) $+$ nest 2 (ppPendingPaths pps)
    ReturnFrame _mr nl mns mel mes pps ->
      text "MF(Retn):" $+$ nest 2 rest
        where
          rest = text "Normal" <+> text "~>" <+> ppSymBlockID nl <> colon
                 $+$ mpath "no normal-return merged state set" mns
                 $+$ maybe PP.empty
                       ( \el ->
                           text "Exc" <+> text "~>" <+> ppSymBlockID el <> colon
                           $+$ mpath "no exception path set" mes
                       )
                       mel
                 $+$ ppPendingPaths pps
    where
      mpath str = nest 2 . maybe (parens $ text $ "Merged: " ++ str) pp
      ppPendingPaths pps =
        text "Pending paths:"
        $+$ nest 2 (if null pps then text "(none)" else vcat (map pp pps))

instance (PrettyTerm term, Pretty (Path term)) => Pretty (CtrlStk term) where
  pp (CtrlStk mfs) =
    hang (text "CS" <+> lbrace) 2 (vcat (map pp mfs)) $+$ rbrace

--------------------------------------------------------------------------------
-- Pretty printing

instance PrettyTerm term => Pretty (RegMap term) where
  pp mp = vcat [ L.ppIdent r <+> (text $ "=> " ++ show v) | (r,v) <- M.toList mp]

instance PrettyTerm term => Pretty (CallFrame term) where
  pp (CallFrame sym regMap) =
    text "CF" <> parens (L.ppSymbol sym) <> colon $+$ nest 2 (pp regMap)

instance (PrettyTerm term) => Pretty (Path term) where
  pp (Path cf mrv _mexc mcb pathConstraint) =
    text "Path"
    <>  brackets (maybe (text "none") ppSymBlockID mcb)
    <>  colon <+> (parens $ text "Constraint:" <+> text (prettyTerm pathConstraint) )
    $+$ nest 2 ( text "Return value:"
                 <+> maybe (parens . text $ "not set") (text . prettyTerm) mrv
               )
    $+$ nest 2 (pp cf)

-----------------------------------------------------------------------------------------
-- Debugging

dumpCtrlStk :: (MonadIO m, PrettyTerm (SBETerm sbe)) => Simulator sbe m ()
dumpCtrlStk = banners . show . pp =<< gets ctrlStk

dumpCtrlStk' :: (MonadIO m, LogMonad m, PrettyTerm (SBETerm sbe)) => Int -> Simulator sbe m ()
dumpCtrlStk' lvl = whenVerbosity (>=lvl) dumpCtrlStk

instance (LogMonad IO) where
  setVerbosity _ = return ()
  getVerbosity   = return 1