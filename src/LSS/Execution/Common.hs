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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module LSS.Execution.Common where

import           Control.Applicative
import           Control.Arrow             hiding ((<+>))
import           Control.Monad.State       hiding (State)
import           Data.LLVM.Memory
import           Data.LLVM.Symbolic.AST
import           LSS.Execution.Codebase
import           LSS.Execution.Utils
import           LSS.SBEInterface
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
type GlobalMap sbe = M.Map (L.Symbol, Maybe [L.Type]) (Typed (SBETerm sbe))
type CS sbe        = CtrlStk (SBETerm sbe) (SBEMemory sbe)
type MF sbe        = MergeFrame (SBETerm sbe) (SBEMemory sbe)
type Path sbe      = Path' (SBETerm sbe) (SBEMemory sbe)
type CF sbe        = CallFrame (SBETerm sbe)

-- | Symbolic simulator state
data State sbe m = State
  { codebase     :: Codebase      -- ^ LLVM code, post-transformation to sym ast
  , llvmCtx      :: LLVMContext   -- ^ Memory alignment and type aliasing info
  , symBE        :: SBE sbe       -- ^ Symbolic backend interface
  , initMemModel :: SBEMemory sbe -- ^ The SBE's initially-provided LLVM memory
                                  -- model; mutated models are carried in path
                                  -- states.
  , liftSymBE    :: LiftSBE sbe m -- ^ Lift SBE operations into the Simulator monad
  , ctrlStk      :: CS sbe        -- ^ Control stack for tracking merge points
  , globalTerms  :: GlobalMap sbe -- ^ Global ptr terms
  , verbosity    :: Int           -- ^ Verbosity level
  }

data CtrlStk term mem = CtrlStk { mergeFrames :: [MergeFrame term mem] }

-- | There are three types of merge frames: (1) postdominator frames, which are
-- typical intraprocedural merge points and correspond to basic blocks which are
-- immediate postdominators of earlier-executing basic blocks; (2) return
-- frames, which capture points immediately following function calls; and (3)
-- exit frames, which represent program exit points.
data MergeFrame term mem
  = ExitFrame
    { _mergedState  :: Maybe (Path' term mem)
    , programRetVal :: Maybe term
    }
  | PostdomFrame
    { _mergedState :: Maybe (Path' term mem)
    , _pending     :: [Path' term mem]
    , pdfLabel     :: SymBlockID
    }
  | ReturnFrame
    { rfRetReg        :: Maybe (L.Typed Reg)     -- ^ Register to store return value (if any)
    , rfNormalLabel   :: SymBlockID              -- ^ Label for normal path
    , _normalState    :: Maybe (Path' term mem ) -- ^ Merged state after function call return
    , _exceptLabel    :: Maybe SymBlockID        -- ^ Label for exception path
    , _exceptionState :: Maybe (Path' term mem)  -- ^ Merged exception state
                                                 -- after function call return
    , _pending        :: [Path' term mem]
    }

-- | Captures all symbolic execution state for a unique control-flow path (as
-- specified by the recorded path constraints)
data Path' term mem = Path
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
  , pathMem         :: mem                -- ^ The memory model along this path
  , pathConstraints :: Constraint term    -- ^ The constraints necessary for
                                          -- execution of this path
  }

-- Carry a list of SymConds for pretty-printing; useful when symbolic term
-- contents are not visible.
data Constraint term = Constraint [SymCond] term

type RegMap term = M.Map Reg (Typed term)

-- | A frame (activation record) in the program being simulated
data CallFrame term = CallFrame
  { frmFuncSym :: L.Symbol
  , frmRegs    :: RegMap term
  }
  deriving Show

--------------------------------------------------------------------------------
-- Misc typeclass instances

instance Monad m => LogMonad (Simulator sbe m) where
  getVerbosity   = gets verbosity
  setVerbosity v = modify $ \s -> s{ verbosity = v }

instance (Monad m, Functor m) => Applicative (Simulator sbe m) where
  pure      = return
  af <*> aa = af >>= \f -> f <$> aa

-----------------------------------------------------------------------------------------
-- Pretty printing

ppCtrlStk :: SBE sbe -> CS sbe -> Doc
ppCtrlStk sbe (CtrlStk mfs) =
  hang (text "CS" <+> lbrace) 2 (vcat (map (ppMergeFrame sbe) mfs)) $+$ rbrace

ppMergeFrame :: SBE sbe -> MF sbe -> Doc
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
ppCallFrame sbe (CallFrame _sym regMap) =
  --text "CF" <> parens (L.ppSymbol sym) <> colon $+$ nest 2 (ppRegMap sbe regMap)
  if M.null regMap then PP.empty else text "Locals:" $+$ nest 2 (ppRegMap sbe regMap)

ppPath :: SBE sbe -> Path sbe -> Doc
ppPath sbe (Path cf mrv _mexc mcb _mem (Constraint conds pathConstraint)) =
  text "Path"
  <>  brackets ( text (show $ L.ppSymbol $ frmFuncSym cf)
                 <> char '/'
                 <> maybe (text "none") ppSymBlockID mcb
               )
  <>  colon
  <+> ( parens
          $ text "PC:"
            <+> prettyTermD sbe pathConstraint
            <+> ( if null conds
                    then PP.empty
                    else text "SCs:"
                         <+> (hcat $ punctuate comma $ map ppSymCond conds)
                )
      )
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
