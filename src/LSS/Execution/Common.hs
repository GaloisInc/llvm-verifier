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
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module LSS.Execution.Common where

import           Control.Applicative
import           Control.Arrow             hiding ((<+>))
import           Control.Monad.State       hiding (State)
import           Data.LLVM.Symbolic.AST
import           Debug.Trace
import           LSS.Execution.Codebase
import           LSS.Execution.Utils
import           LSS.SBEInterface
import           Language.Haskell.TH
import           Text.LLVM                 (Typed(..))
import           Text.PrettyPrint.HughesPJ
import           Verinf.Utils.LogMonad
import qualified Data.Map                  as M
import qualified Text.LLVM                 as L
import qualified Text.PrettyPrint.HughesPJ as PP

newtype Simulator sbe m a = SM { runSM :: StateT (State sbe m) m a }
  deriving ( Functor
           , Monad
           , MonadIO
           , MonadState (State sbe m)
           )

type LiftSBE sbe m = forall a. sbe a -> Simulator sbe m a
type GFPMap sbe    = M.Map (L.Symbol, [L.Type]) (SBETerm sbe)

-- | Symbolic simulator state
data State sbe m = State
  { codebase  :: Codebase              -- ^ LLVM code, post-transformation to sym ast
  , symBE     :: SBE sbe               -- ^ Symbolic backend interface
  , memModel  :: SBEMemory sbe         -- ^ The SBE's LLVM memory model
  , liftSymBE :: LiftSBE sbe m         -- ^ Lift SBE operations into the Simulator monad
  , ctrlStk   :: CtrlStk (SBETerm sbe) -- ^ Control stack for tracking merge points
  , gfpTerms  :: GFPMap sbe            -- ^ Global function pointer terms
  , verbosity :: Int                   -- ^ Verbosity level
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
    , _pending     :: [Path term]
    , pdfLabel     :: SymBlockID
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

type RegMap term = M.Map Reg (Typed term)

-- | A frame (activation record) in the program being simulated
data CallFrame term = CallFrame
  { frmFuncSym :: L.Symbol
  , frmRegs    :: RegMap term
  }
  deriving Show

--------------------------------------------------------------------------------
-- Misc typeclass instances

instance MonadIO m => LogMonad (Simulator sbe m) where
  getVerbosity   = gets verbosity
  setVerbosity v = modify $ \s -> s{ verbosity = v }

instance (Monad m, Functor m) => Applicative (Simulator sbe m) where
  pure      = return
  af <*> aa = af >>= \f -> f <$> aa

-----------------------------------------------------------------------------------------
-- Pretty printing

ppCtrlStk :: SBE sbe -> CtrlStk (SBETerm sbe) -> Doc
ppCtrlStk sbe (CtrlStk mfs) =
  hang (text "CS" <+> lbrace) 2 (vcat (map (ppMergeFrame sbe) mfs)) $+$ rbrace

ppMergeFrame :: SBE sbe -> MergeFrame (SBETerm sbe) -> Doc
ppMergeFrame sbe mf = case mf of
  ExitFrame mp mrv ->
    text "MF(Exit):"
    $+$ mpath "no merged state set" mp
    $+$ nest 2 ( maybe (parens $ text "no return value set")
                       (\rv -> text "Return value:" <+> prettyTermD sbe rv)
                       mrv
               )
  PostdomFrame p pps bid ->
    text "MF(Pdom|" <>  ppSymBlockID bid <> text "):"
    $+$ nest 2 (text "Merged:" <+> maybe PP.empty (ppPath sbe) p)
    $+$ nest 2 (ppPendingPaths pps)
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
    mpath str = nest 2 . maybe (parens $ text $ "Merged: " ++ str) (ppPath sbe)
    ppPendingPaths pps =
      text "Pending paths:"
      $+$ nest 2 (if null pps then text "(none)" else vcat (map (ppPath sbe) pps))

ppCallFrame :: SBE sbe -> CallFrame (SBETerm sbe) -> Doc
ppCallFrame sbe (CallFrame sym regMap) =
  text "CF" <> parens (L.ppSymbol sym) <> colon $+$ nest 2 (ppRegMap sbe regMap)


ppPath :: SBE sbe -> Path (SBETerm sbe) -> Doc
ppPath sbe (Path cf mrv _mexc mcb pathConstraint) =
  text "Path"
  <>  brackets (maybe (text "none") ppSymBlockID mcb)
  <>  colon <+> (parens $ text "Constraint:" <+> prettyTermD sbe pathConstraint)
  $+$ nest 2 ( text "Return value:"
               <+> maybe (parens . text $ "not set") (prettyTermD sbe) mrv
             )
  $+$ nest 2 (ppCallFrame sbe cf)

ppRegMap :: SBE sbe -> RegMap (SBETerm sbe) -> Doc
ppRegMap sbe mp =
    vcat [ ppIdentAssoc r <> (L.ppTyped (prettyTermD sbe) v) | (r,v) <- as ]
    where
      ppIdentAssoc r = L.ppIdent r
                       <> text (replicate (maxLen - identLen r) ' ')
                       <> text " => "
      maxLen         = foldr max 0 $ map (identLen . fst) as
      identLen       = length . show . L.ppIdent
      as             = M.toList mp

-----------------------------------------------------------------------------------------
-- Debugging

dumpCtrlStk :: (MonadIO m) => Simulator sbe m ()
dumpCtrlStk = do
  (sbe, cs) <- gets (symBE &&& ctrlStk)
  banners $ show $ ppCtrlStk sbe cs

dumpCtrlStk' :: (MonadIO m, LogMonad m) => Int -> Simulator sbe m ()
dumpCtrlStk' lvl = whenVerbosity (>=lvl) dumpCtrlStk

instance (LogMonad IO) where
  setVerbosity _ = return ()
  getVerbosity   = return 1

dbugV :: (MonadIO m, Show a) => String -> a -> m ()
dbugV desc v = dbugM $ desc ++ ": " ++ show v

dt, dV, nmStr :: Name -> Q Exp
dt name    = [| trace ( $(nmStr name) ++ ": " ++ show $(varE name)) $(varE name) |]
dV name    = [| dbugV $(nmStr name) $(varE name) |]
nmStr name = [| $(litE . StringL $ nameBase name) |]