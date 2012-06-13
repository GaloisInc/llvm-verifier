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
import           LSS.SBEBitBlast           (BitIO(..))
import           Text.LLVM                 (Typed(..))
import           Text.PrettyPrint.HughesPJ
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

liftBitBlastSim :: BitIO m l a -> Simulator sbe IO a
liftBitBlastSim = SM . lift . lift . liftSBEBitBlast

type LiftSBE sbe m = forall a. sbe a -> Simulator sbe m a
type GlobalMap sbe = M.Map (L.Symbol, Maybe [L.Type]) (Typed (SBETerm sbe))
type CS sbe        = CtrlStk (SBETerm sbe) (SBEMemory sbe)
type MF sbe        = MergeFrame (SBETerm sbe) (SBEMemory sbe)
type CF sbe        = CallFrame (SBETerm sbe)
type OvrMap sbe m  = M.Map L.Symbol (Override sbe m, Bool {- user override? -})

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
  , liftSymBE    :: LiftSBE sbe m   -- ^ Lift SBE operations into the Simulator monad
  , ctrlStk      :: CS sbe          -- ^ Control stack for tracking merge points
  , globalTerms  :: GlobalMap sbe   -- ^ Global ptr terms
  , fnOverrides  :: OvrMap sbe m    -- ^ Function override table
  , verbosity    :: Int             -- ^ Verbosity level
  , evHandlers   :: SEH sbe m       -- ^ Simulation event handlers
  , errorPaths   :: [ErrorPath sbe] -- ^ Terminated paths due to errors.
  , lssOpts      :: LSSOpts         -- ^ Options passed to simulator
  , pathCounter  :: Integer         -- ^ Name supply for paths
  , aigOutputs   :: [SBETerm sbe]   -- ^ Current list of AIG outputs, discharged
                                    -- via lss_write_aiger() sym api calls
  }
  
newtype CtrlStk term mem = CtrlStk { mergeFrames :: [MergeFrame term mem] }

emptyCtrlStk :: CtrlStk term mem
emptyCtrlStk = CtrlStk []

data MergedState term mem 
  = EmptyState term term -- ^ Assumptions and assertions on path before entering merge frame.
  | PathState (Path' term mem) term -- ^ Path and current assertions on finished paths. 

-- | Captures all symbolic execution state for a unique control-flow path (as
-- specified by the recorded path constraints)
data Path' term mem = Path
  { pathCallFrame   :: CallFrame term     -- ^ The top call frame of the dynamic
                                          -- call stack along this path
  , pathException   :: Maybe term         -- ^ When handling an exception along
                                          -- this path, a pointer to the
                                          -- exception structure; Nothing
                                          -- otherwise
  , pathCB          :: Maybe SymBlockID   -- ^ The currently-executing basic
                                          -- block along this path, if any.
  , pathMem         :: mem                -- ^ The memory model along this path
  , pathName        :: Integer            -- ^ A unique name for this path
  , pathAssumptions :: term               -- ^ Terms assumed to be true since last merge frame.
  , pathAssertions  :: term               -- ^ Condition on path since last merge frame. 
  }

initialMergedState :: Functor sbe => SBE sbe -> sbe (MergedState (SBETerm sbe) (SBEMemory sbe))
initialMergedState sbe = fn <$> termBool sbe True
  where fn t = EmptyState t t

pathMergedState :: Path' term mem -> MergedState term mem
pathMergedState p = EmptyState (pathAssumptions p) (pathAssertions p)  

type Path sbe      = Path' (SBETerm sbe) (SBEMemory sbe)

addPathAssumption :: Functor sbe
                  => SBE sbe -> SBETerm sbe -> Path sbe -> sbe (Path sbe) 
addPathAssumption sbe t p = set <$> applyAnd sbe (pathAssumptions p) t
  where set a = p { pathAssumptions = a }

addPathAssertion :: Functor sbe
                 => SBE sbe -> SBETerm sbe -> Path sbe -> sbe (Path sbe)
addPathAssertion sbe t p = set <$> applyAnd sbe (pathAssertions p) t
  where set a = p { pathAssertions = a }

data ExitFrame term mem = ExitFrame {
       programMergedState  :: MergedState term mem
     , efPending :: [Path' term mem]
     }  

data PostdomFrame term mem = PostdomFrame { 
       pdfMergedState :: MergedState term mem
     , pdfPending     :: [Path' term mem]
     , pdfLabel       :: SymBlockID
     }

data ReturnFrame term mem = ReturnFrame {
       rfCallFrame     :: CallFrame term       -- ^ Call frame for path when it arrives.
     , rfRetReg        :: Maybe (L.Typed Reg)  -- ^ Register to store return value (if any)
     , rfNormalLabel   :: SymBlockID           -- ^ Label for normal path
     , rfExceptLabel   :: Maybe SymBlockID     -- ^ Label for exception path
     , rfNormalState   :: MergedState term mem -- ^ Merged state after function call return.
     , rfExceptState   :: MergedState term mem -- ^ Merged exception state after function call return.
     , rfPending       :: [Path' term mem]
     }

-- | There are three types of merge frames: (1) postdominator frames, which are
-- typical intraprocedural merge points and correspond to basic blocks which are
-- immediate postdominators of earlier-executing basic blocks; (2) return
-- frames, which capture points immediately following function calls; and (3)
-- exit frames, which represent program exit points.
data MergeFrame term mem
  = ExitMergeFrame (ExitFrame term mem)
  | PostdomMergeFrame (PostdomFrame term mem)
  | ReturnMergeFrame (ReturnFrame term mem)

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

instance MonadIO m => LogMonad (Simulator sbe m) where
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
  ExitMergeFrame ef ->
    text "MF(Exit):"
    $+$ nest 2 (mpath "no merged state set" (programMergedState ef))
  PostdomMergeFrame pdf ->
    text "MF(Pdom|" <>  ppSymBlockID (pdfLabel pdf) <> text "):"
    $+$ nest 2 (mpath "" (pdfMergedState pdf))
    $+$ nest 2 (ppPendingPaths (pdfPending pdf))
  ReturnMergeFrame rf ->
    text "MF(Retn):" $+$ nest 2 rest
      where
        rest = text "Normal" <+> text "~>" <+> ppSymBlockID (rfNormalLabel rf) <> colon
               $+$ nest 2 (mpath "no normal-return merged state set" (rfNormalState rf))
               $+$ maybe PP.empty
                     ( \el ->
                         text "Exc" <+> text "~>" <+> ppSymBlockID el <> colon
                         $+$ mpath "no exception path set" (rfExceptState rf)
                     )
                     (rfExceptLabel rf)
               $+$ ppPendingPaths (rfPending rf)
  where
    mpath str (EmptyState _ _) = parens $ text ("Merged: " ++ str)
    mpath _ (PathState p _) = ppPath sbe p
    ppPendingPaths pps =
      text "Pending paths:"
      $+$ nest 2 (if null pps then text "(none)" else vcat (map (ppPath sbe) pps))

pathFuncSym :: Path' term mem -> L.Symbol
pathFuncSym = frmFuncSym . pathCallFrame

pathRegs :: Path' term mem -> RegMap term
pathRegs = frmRegs . pathCallFrame
               
ppPath :: SBE sbe -> Path sbe -> Doc
ppPath sbe p =
  text "Path #"
  <>  integer (pathName p)
  <>  brackets ( text (show $ L.ppSymbol $ pathFuncSym p)
                 <> char '/'
                 <> maybe (text "none") ppSymBlockID (pathCB p)
               )
  <>  colon
  $+$ nest 2 (text "Locals:" $+$ nest 2 (ppRegMap sbe (pathRegs p)))
-- <+> (parens $ text "PC:" <+> ppPC sbe c)

--(Path cf mrv _mexc mcb _mpcb _mem c name) =

-- Prints just the path's location and path constraints
ppPathLoc :: SBE sbe -> Path sbe -> Doc
ppPathLoc _ p =
  text "Path #"
  <>  integer (pathName p)
  <>  brackets ( text (show $ L.ppSymbol $ frmFuncSym (pathCallFrame p))
                 <> char '/'
                 <> maybe (text "none") ppSymBlockID (pathCB p)
               )
--  <>  colon
--  <+> (parens $ text "PC:" <+> ppPC sbe )

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
