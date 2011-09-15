{- |
Module           : $Header$
Description      : Common type definitions and helper functions for LSS
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE DeriveDataTypeable         #-}
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
import           Control.Monad.Error
import           Control.Monad.State       hiding (State)
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

newtype Simulator sbe m a =
  SM { runSM :: ErrorT (InternalExc sbe m) (StateT (State sbe m) m) a }
  deriving
    ( Functor
    , Monad
    , MonadIO
    , MonadState (State sbe m)
    , MonadError (InternalExc sbe m)
    )

type LiftSBE sbe m = forall a. sbe a -> Simulator sbe m a
type GlobalMap sbe = M.Map (L.Symbol, Maybe [L.Type]) (Typed (SBETerm sbe))
type CS sbe        = CtrlStk (SBETerm sbe) (SBEMemory sbe)
type MF sbe        = MergeFrame (SBETerm sbe) (SBEMemory sbe)
type Path sbe      = Path' (SBETerm sbe) (SBEMemory sbe)
type CF sbe        = CallFrame (SBETerm sbe)
type OvrMap sbe m  = M.Map L.Symbol (Override sbe m)

-- | Symbolic simulator options
data LSSOpts = LSSOpts {
    optsErrorPathDetails :: Bool
  }
defaultLSSOpts :: LSSOpts
defaultLSSOpts = LSSOpts False

-- | Symbolic simulator state
data State sbe m = State
  { codebase     :: Codebase        -- ^ LLVM code, post-transformation to sym ast
  , symBE        :: SBE sbe         -- ^ Symbolic backend interface
  , initMemModel :: SBEMemory sbe   -- ^ The SBE's initial LLVM memory model;
                                    -- mutated models are carried in path states.
  , liftSymBE    :: LiftSBE sbe m   -- ^ Lift SBE operations into the Simulator monad
  , ctrlStk      :: CS sbe          -- ^ Control stack for tracking merge points
  , globalTerms  :: GlobalMap sbe   -- ^ Global ptr terms
  , overrides    :: OvrMap sbe m    -- ^ Function override table
  , verbosity    :: Int             -- ^ Verbosity level
  , evHandlers   :: SEH sbe m       -- ^ Simulation event handlers
  , errorPaths   :: [ErrorPath sbe] -- ^ Terminated paths due to errors.
  , lssOpts      :: LSSOpts         -- ^ Options passed to simulator
  , pathCounter  :: Integer         -- ^ Name supply for paths
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
    , programMem    :: Maybe mem
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
  , prevPathCB      :: Maybe SymBlockID   -- ^ The basic block this path
                                          -- executed before the current block,
                                          -- if any.
  , pathMem         :: mem                -- ^ The memory model along this path
  , pathConstraint  :: Constraint term    -- ^ The aggregated constraints
                                          -- necessary for execution of this
                                          -- path
  , pathName        :: Integer            -- A unique name for this path
  }

data FailRsn       = FailRsn String deriving (Show)
data ErrorPath sbe = EP { epRsn :: FailRsn, epPath :: Path sbe }

-- | The exception type for errors that are both thrown and caught within the
-- simulator.
data InternalExc sbe m
  = ErrorPathExc FailRsn (State sbe m)
  | UnknownExc (Maybe FailRsn)
instance Error (InternalExc sbe m) where
  noMsg  = UnknownExc Nothing
  strMsg = UnknownExc . Just . FailRsn

-- | Simulation event handlers, useful for debugging nontrivial codes.
data SEH sbe m = SEH
  {
    -- | Invoked after function overrides have been registered
    onPostOverrideReg :: Simulator sbe m ()
    -- | Invoked before each instruction executes
  , onPreStep         :: SymStmt  -> Simulator sbe m ()
    -- | Invoked after each instruction executes
  , onPostStep        :: SymStmt  -> Simulator sbe m ()
    -- | Invoked before construction of a global term value
  , onMkGlobTerm      :: L.Global -> Simulator sbe m ()
    -- | Invoked before memory model initialization of global data
  , onPreGlobInit     :: L.Global -> Typed (SBETerm sbe) -> Simulator sbe m ()
    -- | Invoked after memory model initialization of global data
  , onPostGlobInit    :: L.Global -> Typed (SBETerm sbe) -> Simulator sbe m ()
  }

-- Carry aggregated symconds for pretty-printing; useful when symbolic term
-- contents are not visible.
data Constraint term = Constraint { symConds :: SCExpr term, pcTerm :: term }
data SCExpr term
  = SCAtom SymCond
  | SCTerm term
  | SCEAnd (SCExpr term) (SCExpr term)
  | SCEOr (SCExpr term) (SCExpr term)

type RegMap term = M.Map Reg (Typed term)

-- | A frame (activation record) in the program being simulated
data CallFrame term = CallFrame
  { frmFuncSym :: L.Symbol
  , frmRegs    :: RegMap term
  }
  deriving Show

-- | A handler for a function override. This gets the function symbol as an
-- argument so that one function can potentially be used to override multiple
-- symbols.
type OverrideHandler sbe m
  =  L.Symbol              -- ^ Callee symbol
  -> Maybe (Typed Reg)     -- ^ Callee return register
  -> [Typed (SBETerm sbe)] -- ^ Callee arguments
  -> Simulator sbe m (Maybe (SBETerm sbe))

-- | An override may specify a function to run within the simulator,
-- or alternatively a symbol to look up and execute in its place.
data Override sbe m
  = Override (OverrideHandler sbe m)
  | Redirect L.Symbol

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

ppFailRsn :: FailRsn -> Doc
ppFailRsn (FailRsn msg) = text msg

ppCtrlStk :: SBE sbe -> CS sbe -> Doc
ppCtrlStk sbe (CtrlStk mfs) =
  hang (text "CS" <+> lbrace) 2 (vcat (map (ppMergeFrame sbe) mfs)) $+$ rbrace

ppMergeFrame :: SBE sbe -> MF sbe -> Doc
ppMergeFrame sbe mf = case mf of
  ExitFrame mp mrv mm ->
    text "MF(Exit):"
    $+$ mpath "no merged state set" mp
    $+$ nest 2 ( maybe (parens $ text "no return value set")
                       (\rv -> text "Return value:" <+> prettyTermD sbe rv)
                       mrv
               )
    $+$ nest 2 ( maybe (parens $ text "no final memory set")
                       (const $ parens $ text "final mem set")
                       mm
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
ppPath sbe (Path cf mrv _mexc mcb _mpcb _mem c name) =
  text "Path #"
  <>  integer name
  <>  brackets ( text (show $ L.ppSymbol $ frmFuncSym cf)
                 <> char '/'
                 <> maybe (text "none") ppSymBlockID mcb
               )
  <>  colon
  <+> (parens $ text "PC:" <+> ppPC sbe c)
  $+$ nest 2 ( text "Return value:"
               <+> maybe (parens . text $ "not set") (prettyTermD sbe) mrv
             )
  $+$ nest 2 (ppCallFrame sbe cf)

-- Prints just the path's location and path constraints
ppPathLoc :: SBE sbe -> Path sbe -> Doc
ppPathLoc sbe (Path cf _ _ mcb _mpcb _ c name) =
  text "Path #"
  <>  integer name
  <>  brackets ( text (show $ L.ppSymbol $ frmFuncSym cf)
                 <> char '/'
                 <> maybe (text "none") ppSymBlockID mcb
               )
  <>  colon
  <+> (parens $ text "PC:" <+> ppPC sbe c)

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

ppPC :: SBE sbe -> Constraint (SBETerm sbe) -> Doc
ppPC sbe (Constraint conds pc) =
  prettyTermD sbe pc
  <+> ( text "SCs:" <+> ppSCExpr sbe conds
      )

ppSymConds :: [SymCond] -> Doc
ppSymConds = hcat . punctuate comma . map ppSymCond

ppSCExpr :: SBE sbe -> SCExpr (SBETerm sbe) -> Doc
ppSCExpr _   (SCAtom sc)                      = ppSymCond sc
ppSCExpr sbe (SCTerm t)                       = parens $ text "?term:" <+> prettyTermD sbe t
ppSCExpr sbe (SCEAnd (SCAtom TrueSymCond) b)  = ppSCExpr sbe b
ppSCExpr sbe (SCEAnd a (SCAtom TrueSymCond))  = ppSCExpr sbe a
ppSCExpr sbe (SCEAnd a b)                     = ppSCExpr sbe a <+> text "&" <+> ppSCExpr sbe b
ppSCExpr sbe (SCEOr a@(SCAtom TrueSymCond) _) = ppSCExpr sbe a
ppSCExpr sbe (SCEOr _ b@(SCAtom TrueSymCond)) = ppSCExpr sbe b
ppSCExpr sbe (SCEOr a b)                      = ppSCExpr sbe a <+> text "|" <+> ppSCExpr sbe b

ppTuple :: [Doc] -> Doc
ppTuple = parens . hcat . punctuate comma

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
