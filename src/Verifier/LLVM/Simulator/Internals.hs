{- |
Module           : $Header$
Description      : Common type definitions and helper functions for LSS
Stability        : provisional
Point-of-contact : jhendrix
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
module Verifier.LLVM.Simulator.Internals
  ( Simulator(SM)
  , runSM
  , dumpCtrlStk

  , GlobalMap

  , LSSOpts(LSSOpts, optsErrorPathDetails, optsSatAtBranches)
  , defaultLSSOpts

  , State(..)
  , ctrlStk
  , globalTerms
  , blockPtrs
  , fnOverrides
  , evHandlers
  , errorPaths
  , pathCounter
  , aigOutputs
  , breakpoints
  , trBreakpoints

  , Breakpoint(..)
  , TransientBreakpoint(..)
  , addBreakpoint
  , removeBreakpoint

  , CS(..)
  , ActiveCS
  , activePath
  , initialCtrlStk
  , currentPath
  , modifyCurrentPathM
  , pushCallFrame
  , addCtrlBranch
  , jumpCurrentPath
  , returnCurrentPath
  , markCurrentPathAsError

  , currentPathOfState
  , currentPathMem

  , SymBlockID

  , RegMap
  , setReturnValue
  , ppRegMap

  , Path
  , pathFuncSym
  , pathCB
  , pathName
  , pathRegs
  , pathMem
  , pathAssertions
  , pathStack
  , ppPath
  , ppPathLoc

    -- * Symbolic helpers
  , liftSBE
  , withSBE
  , withSBE'
  , getDL
  , withDL
  , unlessQuiet
  , tellUser
    
    -- * Term helpers
  , ptrInc
  , strTy
   
  , FailRsn(FailRsn)
  , ppFailRsn

  , Override(Override, Redirect)
  , VoidOverrideHandler
  , StdOvd
  , checkTypeCompat
  , voidOverride
  , override
  , registerOverride
  , tryRegisterOverride
  , OverrideEntry
  , registerOverrides

  , ErrorPath(EP, epRsn, epPath)
  , InternalExc(ErrorPathExc, UnknownExc)
  , SEH(..)
  , onPreStep
  , onBlockEntry
  , errorPath
  , wrongArguments
 
    -- * Memory primitives
  , memFailRsn
  , processMemCond
  , alloca
  , malloc
  , load
  , loadString
  , resolveFunPtrTerm
  , store
  , dumpMem

  , ppStackTrace
  , ppTuple
  , ppBreakpoints
  ) where

import Control.Applicative hiding (empty)
import qualified Control.Arrow as A
import Control.Exception (assert)
import Control.Lens hiding (act)
import Control.Monad.Error
import Control.Monad.State.Class
import Control.Monad.Trans.State.Strict (StateT)

import qualified Data.Map  as M
import Data.Maybe
import qualified Data.Set  as S

import System.Console.Haskeline.MonadException (MonadException)

import Text.PrettyPrint.Leijen hiding ((<$>))

import Verifier.LLVM.AST
import Verifier.LLVM.Backend
import Verifier.LLVM.Codebase
import Verifier.LLVM.Simulator.SimUtils

newtype Simulator sbe m a =
  SM { runSM :: ErrorT (InternalExc sbe m) (StateT (State sbe m) m) a }
  deriving
    ( Functor
    , Monad
    , MonadIO
    , MonadState (State sbe m)
    , MonadError (InternalExc sbe m)
    , MonadException
    )

type LiftSBE sbe m = forall a. sbe a -> Simulator sbe m a
type GlobalMap sbe = M.Map Symbol (SBETerm sbe)
type BlockMap sbe = M.Map (Symbol, BlockLabel) (SBETerm sbe)
type OvrMap sbe m  = M.Map Symbol (Override sbe m, Bool {- user override? -})

-- | Symbolic simulator options
data LSSOpts = LSSOpts {
    optsErrorPathDetails :: Bool
  , optsSatAtBranches    :: Bool
  -- ^ use a SAT-checking backend at branches, pruning unfeasable paths
  }

-- | Default simulator options
defaultLSSOpts :: LSSOpts
defaultLSSOpts = LSSOpts { optsErrorPathDetails = False
                         , optsSatAtBranches    = False
                         }

-- | Symbolic simulator state
data State sbe m = State
  { codebase     :: Codebase sbe    -- ^ LLVM code, post-transformation to sym ast
  , symBE        :: SBE sbe         -- ^ Symbolic backend interface
  , liftSymBE    :: LiftSBE sbe m   -- ^ Lift SBE operations into the Simulator monad
  , _ctrlStk     :: !(Maybe (CS sbe))  -- ^ Control stack for controlling simulator.
  , _globalTerms :: GlobalMap sbe   -- ^ Global ptr terms
  , _blockPtrs   :: BlockMap sbe
  , _fnOverrides  :: OvrMap sbe m    -- ^ Function override table
  , verbosity    :: Int             -- ^ Verbosity level
  , _evHandlers   :: SEH sbe m       -- ^ Simulation event handlers
  , _errorPaths   :: [ErrorPath sbe] -- ^ Terminated paths due to errors.
  , lssOpts      :: LSSOpts         -- ^ Options passed to simulator
  , _pathCounter  :: Integer         -- ^ Name supply for paths
  , _aigOutputs   :: [(MemType,SBETerm sbe)]   -- ^ Current list of AIG outputs, discharged
                                     -- via lss_write_aiger() sym api calls
  , _breakpoints :: M.Map Symbol (S.Set Breakpoint)
  , _trBreakpoints :: S.Set TransientBreakpoint
  }

ctrlStk :: Simple Lens (State sbe m) (Maybe (CS sbe))
ctrlStk f s = (\v -> s { _ctrlStk = v }) <$> f (_ctrlStk s)

globalTerms :: Simple Lens (State sbe m) (GlobalMap sbe)
globalTerms f s = (\v -> s { _globalTerms = v }) <$> f (_globalTerms s)

blockPtrs :: Simple Lens (State sbe m) (BlockMap sbe)
blockPtrs f s = (\v -> s { _blockPtrs = v }) <$> f (_blockPtrs s)

fnOverrides :: Simple Lens (State sbe m) (OvrMap sbe m)
fnOverrides f s = (\v -> s { _fnOverrides = v }) <$> f (_fnOverrides s)

evHandlers :: Simple Lens (State sbe m) (SEH sbe m)
evHandlers f s = (\v -> s { _evHandlers = v }) <$> f (_evHandlers s)

errorPaths :: Simple Lens (State sbe m) [ErrorPath sbe]
errorPaths f s = (\v -> s { _errorPaths = v }) <$> f (_errorPaths s)

pathCounter :: Simple Lens (State sbe m) Integer
pathCounter f s = (\v -> s { _pathCounter = v }) <$> f (_pathCounter s)

aigOutputs :: Simple Lens (State sbe m) [(MemType,SBETerm sbe)]
aigOutputs f s = (\v -> s { _aigOutputs = v }) <$> f (_aigOutputs s)

breakpoints :: Simple Lens (State sbe m) (M.Map Symbol (S.Set Breakpoint))
breakpoints f s = (\v -> s { _breakpoints = v }) <$> f (_breakpoints s)

trBreakpoints :: Simple Lens (State sbe m) (S.Set TransientBreakpoint)
trBreakpoints f s = (\v -> s { _trBreakpoints = v }) <$> f (_trBreakpoints s)


-- | Types of breakpoints (kind of boring for now, but maybe with a
-- DWARF parser we can do more...)
data Breakpoint = BreakEntry
                -- ^ Break when entering this symbol
                | BreakBBEntry SymBlockID
                -- ^ Break when entering a basic block
  deriving (Eq, Ord, Show)

-- | Transient breakpoints for interactive debugging
data TransientBreakpoint = BreakNextStmt
                         -- ^ Break at the next statement
                         | BreakReturnFrom Symbol
                         -- ^ Break when returning from a function
  deriving (Eq, Ord, Show)


-- | Action to perform when branch.
data BranchAction sbe
  = BARunFalse (SBEPred sbe) -- ^ Branch condition
               (Path sbe) -- ^ True path to run
  | BAFalseComplete (SBEPred sbe) -- ^ Assertions before merge
                    (SBEPred sbe) -- ^ Branch condition
                    (Path sbe) -- ^ Completed true path

data MergeInfo
  = ReturnInfo Int (Maybe Ident)
    -- | Contains the 
  | PostdomInfo Int SymBlockID

data PathHandler sbe
  = BranchHandler MergeInfo -- ^ Number of call frames and block id to merge to.
                  (BranchAction sbe) -- ^ Action to get new control stack when current
                                      -- path reached merge location.
                  (PathHandler sbe) -- ^ Handler once this handler is done.
  | StopHandler

-- | A control stack that is still active.
data ActiveCS sbe = ACS (Path sbe) (PathHandler sbe)

activePath :: Simple Lens (ActiveCS sbe) (Path sbe)
activePath f (ACS p h) = flip ACS h <$> f p

data CS sbe
  = FinishedCS (Path sbe)
  | ActiveCS (ActiveCS sbe)

currentPath :: Simple Lens (CS sbe) (Path sbe)
currentPath f (FinishedCS p) = FinishedCS <$> f p
currentPath f (ActiveCS acs) = ActiveCS <$> activePath f acs

-- | Traversal for current path of simulator state.
currentPathOfState :: Simple Traversal (State sbe m) (Path sbe)
currentPathOfState = ctrlStk . _Just . currentPath

-- | Traversal for current path memory if any.
currentPathMem :: Simple Traversal (State sbe m) (SBEMemory sbe)
currentPathMem = currentPathOfState . pathMem

initialCtrlStk :: SBE sbe -> SBEMemory sbe -> CS sbe
initialCtrlStk sbe mem = FinishedCS p
  where p = Path { pathFuncSym = entrySymbol
                 , pathCB = Nothing
                 , pathName = 0
                 , _pathRegs = M.empty
                 , _pathMem = mem
                 , _pathAssertions = sbeTruePred sbe
                 , pathStackHt = 0
                 , pathStack = []
                 }

-- | Modify current path in control stack.
modifyCurrentPathM :: forall m sbe a .
                      Functor m
                   => CS sbe
                   -> (Path sbe -> m (a,Path sbe))
                   -> m (a,CS sbe)
modifyCurrentPathM cs f = 
  over _2 (\p -> set currentPath p cs) <$> f (cs^.currentPath)

pushCallFrame :: SBE sbe
              -> Symbol -- ^ Function we are jumping to. 
              -> SymBlockID -- ^ Block to return to.
              -> Maybe (MemType, Ident) -- ^ Where to write return value to (if any).
              -> CS sbe -- ^ Current control stack
              -> ActiveCS sbe
pushCallFrame sbe calleeSym returnBlock retReg cs =
    case cs of
      FinishedCS p       -> ACS (newPath p) StopHandler
      ActiveCS (ACS p h) -> ACS (newPath p) h
  where newPath p = p'
          where cf = CallFrame { cfFuncSym = pathFuncSym p
                               , cfReturnBlock = Just returnBlock
                               , cfRegs = p^.pathRegs
                               , cfRetReg = retReg
                               , cfAssertions = p^.pathAssertions
                               }
                p' = p { pathFuncSym = calleeSym
                       , pathCB = Just initSymBlockID
                       , pathName = pathName p
                       , _pathRegs = M.empty
                       , _pathAssertions = sbeTruePred sbe
                       , pathStackHt = pathStackHt p + 1
                       , pathStack   = cf : pathStack p
                       }

-- | Add a control branch
addCtrlBranch :: SBE sbe
              -> SBEPred sbe -- ^ Condition to branch on.
              -> SymBlockID -- ^ Location for newly branched paused path to start at.
              -> Integer -- ^ Name of path
              -> MergeLocation -- ^ Control point to merge at.
              -> ActiveCS sbe -- ^  Current control stack.
              -> IO (ActiveCS sbe)
addCtrlBranch sbe c nb nm ml (ACS p h) = do
    fmap fn $ sbeRunIO sbe $ memBranch sbe (p^.pathMem)
  where fn mem = ACS pf $ BranchHandler info (BARunFalse c pt) h
          where pf = p & pathMem .~ mem
                       & pathAssertions .~ sbeTruePred sbe
                pt = p { pathCB = Just nb
                       , pathName = nm 
                       } & pathMem .~ mem
        info = case ml of
                 Just b -> PostdomInfo (pathStackHt p) b
                 Nothing -> ReturnInfo (pathStackHt p - 1) (snd <$> cfRetReg cf)
                   where cf : _ = pathStack p
              
postdomMerge :: SBE sbe
             -> Path sbe
             -> PathHandler sbe
             -> IO (ActiveCS sbe)
postdomMerge sbe p (BranchHandler info@(PostdomInfo n b) act h)
   | n == pathStackHt p && Just b == pathCB p =
  case act of
    BARunFalse c tp -> do
      let tp' = tp & pathAssertions .~ sbeTruePred sbe
      let act' = BAFalseComplete (tp^.pathAssertions) c p
      return $ ACS tp' (BranchHandler info act' h)
    BAFalseComplete a c pf -> do
      -- TODO: Collect and print merge errors.
      let mergeTyped (tt,tp) (ft,_) =
            (,tp) . either (const tt) id <$> sbeRunIO sbe (applyIte sbe tp c tt ft)
      -- Merge path regs
      mergedRegs <- traverse id $
        M.intersectionWith mergeTyped (p^.pathRegs) (pf^.pathRegs)
      -- Merge memory
      mergedMemory <- sbeRunIO sbe $ memMerge sbe c (p^.pathMem) (pf^.pathMem)
      -- Merge assertions
      mergedAssertions <- sbeRunIO sbe $
        applyPredIte sbe c (p^.pathAssertions) (pf^.pathAssertions)
      a' <- sbeRunIO sbe $ applyAnd sbe a mergedAssertions
      assert (pathFuncSym p == pathFuncSym pf && pathCB p == pathCB pf) $ do
      let p' = p & pathRegs .~ mergedRegs
                 & pathMem  .~ mergedMemory
                 & pathAssertions .~ a'
      -- Recurse to check if more merges should be performed.
      postdomMerge sbe p' h
postdomMerge _ p h = return (ACS p h)

-- | Move current path to target block.
jumpCurrentPath :: SBE sbe -> SymBlockID -> ActiveCS sbe -> IO (ActiveCS sbe)
jumpCurrentPath sbe b (ACS p h) = postdomMerge sbe p { pathCB = Just b } h

-- | Handle merge of paths.
-- The first element of the return pair contains a mesage if error(s) occured
-- during merging.
returnMerge :: SBE sbe
            -> Path sbe
            -> PathHandler sbe
            -> IO ([String], CS sbe)
returnMerge _ p StopHandler | pathStackHt p == 0 = return ([], FinishedCS p)
returnMerge sbe p (BranchHandler info@(ReturnInfo n mr) act h) | n == pathStackHt p =
  case act of
    BARunFalse c tp -> do
      let tp' = tp & pathAssertions .~ sbeTruePred sbe
      let act' = BAFalseComplete (tp^.pathAssertions) c p
      return $ ([], ActiveCS (ACS tp' (BranchHandler info act' h)))
    BAFalseComplete a c pf -> do
      -- Merge return value
      (errs, mergedRegs) <-
        case mr of
          Nothing -> return ([], p^.pathRegs)
          Just r -> do
            let Just (vt,tp) =  p^.pathRegs^.at r
            let Just (vf,_)  = pf^.pathRegs^.at r
            mv <- sbeRunIO sbe $ applyIte sbe tp c vt vf
            let v = either (const vt) id mv
            return (mv ^.. _Left, p^.pathRegs & at r ?~ (v, tp))

      -- Merge memory
      mergedMemory <- sbeRunIO sbe $ memMerge sbe c (p^.pathMem) (pf^.pathMem)
      -- Merge assertions
      mergedAssertions <- sbeRunIO sbe $
        applyPredIte sbe c (p^.pathAssertions) (pf^.pathAssertions)
      a' <- sbeRunIO sbe $ applyAnd sbe a mergedAssertions
      let p' = p & pathRegs .~ mergedRegs
                 & pathMem .~ mergedMemory
                 & pathAssertions .~ a'
      A.first (errs ++) <$> returnMerge sbe p' h
returnMerge _ p h = return ([], ActiveCS (ACS p h))

-- | Return from current path
returnCurrentPath :: SBE sbe -> Maybe (SBETerm sbe) -> ActiveCS sbe -> IO (CS sbe)
returnCurrentPath sbe rt (ACS p h) = do
  let cf : cfs = pathStack p
  m <- sbeRunIO sbe $ stackPopFrame sbe (p^.pathMem)
  let p' = Path { pathFuncSym = cfFuncSym cf
                , pathCB   = cfReturnBlock cf
                , pathName = pathName p
                , _pathRegs = setReturnValue "returnCurrentpath" (cfRetReg cf) rt (cfRegs cf)
                , _pathMem  = m
                , _pathAssertions = cfAssertions cf
                , pathStackHt = pathStackHt p - 1
                , pathStack  = cfs
                }
  snd <$> returnMerge sbe p' h

branchError :: SBE sbe
            -> MergeInfo -- Info for current merge point.
            -> BranchAction sbe -- Action to run if branch occurs.
            -> PathHandler sbe -- Previous path handler.
            -> IO (CS sbe) 
branchError sbe _ (BARunFalse c pt) h = do
  a2 <- sbeRunIO sbe $ applyAnd sbe (pt^.pathAssertions) c
  mem' <- sbeRunIO sbe $ memBranchAbort sbe (pt^.pathMem)
  let pt' = pt & pathMem .~ mem'
               & pathAssertions .~ a2
  return $ ActiveCS $ ACS pt' h
branchError sbe mi (BAFalseComplete a c pf) h = do
  -- Update assertions on current path
  a1   <- sbeRunIO sbe $ applyAnd sbe a (pf^.pathAssertions)
  cNot <- sbeRunIO sbe $ applyBNot sbe c
  a2   <- sbeRunIO sbe $ applyAnd sbe a1 cNot
  mem' <- sbeRunIO sbe $ memBranchAbort sbe (pf^.pathMem)
  let pf' = pf & pathMem .~ mem'
               & pathAssertions .~ a2
  -- Try to merge states that may have been waiting for the current path to terminate.
  case (mi,h) of
    ( ReturnInfo n _, BranchHandler (ReturnInfo pn _) _ _) | n == pn ->
      snd <$> returnMerge sbe pf' h
    ( PostdomInfo n _, BranchHandler (PostdomInfo pn _) _ _) | n == pn ->
      ActiveCS <$> postdomMerge sbe pf' h
    _ -> return $ ActiveCS $ ACS pf' h

-- | Mark the current path as an error path.
markCurrentPathAsError :: SBE sbe -> ActiveCS sbe -> IO (Maybe (CS sbe))
markCurrentPathAsError sbe (ACS _ (BranchHandler mi a h)) =
  Just <$> branchError sbe mi a h
markCurrentPathAsError _ (ACS _ StopHandler) = return Nothing

type RegMap term = M.Map Ident (term, MemType)

setReturnValue :: String -> Maybe (MemType, Ident) -> Maybe t
               ->  RegMap t -> RegMap t
setReturnValue _n (Just (tp, r)) (Just rv) rm = M.insert r (rv, tp) rm
setReturnValue _n Nothing   Nothing   rm = rm
setReturnValue nm Nothing   (Just _) _  =
  error $ nm ++ ": Return value where non expected"
setReturnValue nm (Just (_,tr)) Nothing   _  =
  error $ nm ++ ": Missing return value for "  ++ show (ppIdent tr)

-- | A Call frame for returning.
data CallFrame sbe = CallFrame { cfFuncSym :: Symbol
                               , cfReturnBlock :: Maybe SymBlockID
                               , cfRegs :: RegMap (SBETerm sbe)
                               , cfRetReg :: Maybe (MemType, Ident)
                               , cfAssertions :: SBEPred sbe
                               }

-- | Captures all symbolic execution state for a unique control-flow path (as
-- specified by the recorded path constraints)
data Path sbe = Path
  { pathFuncSym      :: !Symbol
  , pathCB           :: !(Maybe SymBlockID) -- ^ The currently-executing basic
                                         -- block along this path, if any.
  , pathName         :: !(Integer)       -- ^ A unique name for this path
  , _pathRegs        :: !(RegMap (SBETerm sbe))
  , _pathMem          :: SBEMemory sbe    -- ^ The memory model along this path
  , _pathAssertions   :: SBEPred sbe      -- ^ Condition on path since last branch.
  , pathStackHt      :: !Int             -- ^ Number of call frames count.
  , pathStack        :: [CallFrame sbe] -- ^ Return registers for current calls.
  }

pathRegs :: Simple Lens (Path sbe) (RegMap (SBETerm sbe))
pathRegs = lens _pathRegs (\p r -> p { _pathRegs = r })

pathMem :: Simple Lens (Path sbe) (SBEMemory sbe)
pathMem = lens _pathMem (\p r -> p { _pathMem = r })

pathAssertions :: Simple Lens (Path sbe) (SBEPred sbe)
pathAssertions = lens _pathAssertions (\p r -> p { _pathAssertions = r })

data FailRsn       = FailRsn String deriving (Show)
data ErrorPath sbe = EP { epRsn :: FailRsn, epPath :: Path sbe }

instance Error FailRsn where
  noMsg  = FailRsn ""
  strMsg = FailRsn

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
  , _onPreStep         :: SymStmt (SBETerm sbe) -> Simulator sbe m ()
    -- | Invoked after each instruction executes
  , onPostStep        :: SymStmt (SBETerm sbe) -> Simulator sbe m ()
    -- | Invoked when entering a basic block
  , _onBlockEntry      :: SymBlock (SBETerm sbe) -> Simulator sbe m ()
    -- | Invoked when leaving a basic block
  , onBlockExit       :: SymBlock (SBETerm sbe) -> Simulator sbe m ()
    -- | Invoked before construction of a global term value
  , onMkGlobTerm      :: Global (SBETerm sbe) -> Simulator sbe m ()
    -- | Invoked before memory model initialization of global data
  , onPreGlobInit     :: Global (SBETerm sbe) -> SBETerm sbe -> Simulator sbe m ()
    -- | Invoked after memory model initialization of global data
  , onPostGlobInit    :: Global (SBETerm sbe) -> SBETerm sbe -> Simulator sbe m ()
  }

onPreStep :: Simple Lens (SEH sbe m) (SymStmt (SBETerm sbe) -> Simulator sbe m ())
onPreStep = lens _onPreStep (\seh sh -> seh { _onPreStep = sh })

onBlockEntry :: Simple Lens (SEH sbe m) (SymBlock (SBETerm sbe) -> Simulator sbe m ())
onBlockEntry = lens _onBlockEntry (\seh sh -> seh { _onBlockEntry = sh })

-- | A handler for a function override. This gets the function symbol as an
-- argument so that one function can potentially be used to override multiple
-- symbols.
type OverrideHandler sbe m
  =  Symbol                  -- ^ Callee symbol
  -> Maybe (MemType, Ident)  -- ^ Callee return register
  -> [(MemType,SBETerm sbe)] -- ^ Callee arguments
  -> Simulator sbe m (Maybe (SBETerm sbe))

-- | An override may specify a function to run within the simulator,
-- or alternatively a symbol to look up and execute in its place.
data Override sbe m
  = Override (OverrideHandler sbe m)
  | Redirect Symbol

type VoidOverrideHandler sbe m
  =  [(MemType,SBETerm sbe)] -- ^ Callee arguments
  -> Simulator sbe m ()

voidOverride :: Functor m => VoidOverrideHandler sbe m -> Override sbe m
voidOverride f = Override (\_ _ a -> Nothing <$ f a)

override :: Functor m
         => ([(MemType, SBETerm sbe)] -> Simulator sbe m (SBETerm sbe))
         -> Override sbe m
override f = Override (\_ _ a -> Just <$> f a)

--------------------------------------------------------------------------------
-- Breakpoints

addBreakpoint :: (Functor m, Monad m)
              => Symbol
              -> Breakpoint
              -> Simulator sbe m ()
addBreakpoint = toggleBreakpoint S.insert

removeBreakpoint :: (Functor m, Monad m)
                 => Symbol
                 -> Breakpoint
                 -> Simulator sbe m ()
removeBreakpoint = toggleBreakpoint S.delete

toggleBreakpoint :: (Functor m, Monad m)
                 => (Breakpoint -> S.Set Breakpoint -> S.Set Breakpoint)
                 -> Symbol
                 -> Breakpoint
                 -> Simulator sbe m ()
toggleBreakpoint fn sym bp = do
  bps <- fromMaybe S.empty <$> uses breakpoints (M.lookup sym)
  breakpoints %= M.insert sym (fn bp bps)

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

ppCtrlStk :: SBE sbe -> Maybe (CS sbe) -> Doc
ppCtrlStk _ Nothing = text "All paths failed"
ppCtrlStk sbe (Just (FinishedCS p)) = ppPath sbe p
ppCtrlStk sbe (Just (ActiveCS (ACS p h))) =
  text "Active path:" <$$>
  ppPath sbe p <$$>
  ppPathHandler sbe h

ppMergeInfo :: MergeInfo -> Doc
ppMergeInfo (ReturnInfo n mr) = text "return" <> parens (int n <+> reg)
  where reg = maybe empty ppIdent mr
ppMergeInfo (PostdomInfo n b) =
    text "postdom" <> parens (int n <+> ppSymBlockID b)

ppBranchAction :: SBE sbe -> BranchAction sbe -> Doc
ppBranchAction sbe (BARunFalse c p) = 
  text "runFalse" <+> prettyPredD sbe c <$$>
  nest 2 (ppPath sbe p)
ppBranchAction sbe (BAFalseComplete a c p) =
  text "falseComplete" <+> prettyPredD sbe c <$$>
  nest 2 (text "assumptions:" <+> prettyPredD sbe a) <$$>
  nest 2 (ppPath sbe p)

ppPathHandler :: SBE sbe -> PathHandler sbe -> Doc
ppPathHandler sbe (BranchHandler info act h) = 
  text "on" <+> ppMergeInfo info <+> text "do" <$$>
  nest 2 (ppBranchAction sbe act) <$$>
  ppPathHandler sbe h
ppPathHandler _ StopHandler = text "stop"


ppPath :: SBE sbe -> Path sbe -> Doc
ppPath sbe p =
  text "Path #"
  <>  integer (pathName p)
  <>  brackets ( text (show $ ppSymbol $ pathFuncSym p)
                 <> char '/'
                 <> maybe (text "none") ppSymBlockID (pathCB p)
               )
  <>  colon
  <$$> nest 2 (text "Locals:" <$$> nest 2 (ppRegMap sbe (p^.pathRegs)))
-- <+> (parens $ text "PC:" <+> ppPC sbe c)

-- Prints just the path's location and path constraints
ppPathLoc :: SBE sbe -> Path sbe -> Doc
ppPathLoc _ p =
  text "Path #"
  <>  integer (pathName p)
  <>  brackets ( text (show $ ppSymbol $ pathFuncSym p)
                 <> char '/'
                 <> maybe (text "none") ppSymBlockID (pathCB p)
               )

ppStackTrace :: [CallFrame term] -> Doc
ppStackTrace = braces . vcat . map (ppSymbol . cfFuncSym)

ppRegMap :: SBE sbe -> RegMap (SBETerm sbe) -> Doc
ppRegMap sbe mp =
    vcat [ ppIdentAssoc r <> prettyTermD sbe v | (r,(v,_)) <- as ]
    where
      ppIdentAssoc r = ppIdent r
                       <> text (replicate (maxLen - identLen r) ' ')
                       <> text " => "
      maxLen         = foldr max 0 $ map (identLen . fst) as
      identLen       = length . show . ppIdent
      as             = M.toList mp

ppTuple :: [Doc] -> Doc
ppTuple = parens . hcat . punctuate comma

ppBreakpoint :: Breakpoint -> Doc
ppBreakpoint BreakEntry          = text "init"
ppBreakpoint (BreakBBEntry sbid) = ppSymBlockID sbid

ppBreakpoints :: M.Map Symbol (S.Set Breakpoint) -> Doc
ppBreakpoints m = text "breakpoints set:" <$$> hang 2 (vcat d)
  where d = [ text sym <> ppBreakpoint bp
            | (Symbol sym, bps) <- M.toList m
            , bp <- S.toList bps
            ]


--------------------------------------------------------------------------------
-- SBE lifters and helpers

liftSBE :: Monad m => sbe a -> Simulator sbe m a
liftSBE sa = ($ sa) =<< gets liftSymBE

withSBE :: (Functor m, Monad m) => (SBE sbe -> sbe a) -> Simulator sbe m a
withSBE f = liftSBE . f =<< gets symBE

withSBE' :: (Functor m, Monad m) => (SBE sbe -> a) -> Simulator sbe m a
withSBE' f = f <$> gets symBE

getDL :: Monad m => Simulator sbe m DataLayout
getDL = gets (cbDataLayout . codebase)

withDL :: (Functor m, MonadIO m) => (DataLayout -> a) -> Simulator sbe m a
withDL f = f <$> getDL

-----------------------------------------------------------------------------------------
-- Term operations and helpers

ptrInc :: (Functor m, MonadIO m)
        => SBETerm sbe -> Simulator sbe m (SBETerm sbe)
ptrInc x = do
  sbe <- gets symBE
  w <- ptrBitwidth <$> getDL
  y <- liftSBE $ termInt sbe w 1
  liftSBE $ applyTypedExpr sbe (PtrAdd x y)

strTy :: MemType
strTy = i8p

-----------------------------------------------------------------------------------------
-- Debugging

dumpCtrlStk :: (MonadIO m) => Simulator sbe m ()
dumpCtrlStk = do
  (sbe, cs) <- gets (symBE A.&&& view ctrlStk)
  banners $ show $ ppCtrlStk sbe cs

unlessQuiet :: MonadIO m => Simulator sbe m () -> Simulator sbe m ()
unlessQuiet act = getVerbosity >>= \v -> unless (v == 0) act

-- For user feedback that gets silenced when verbosity = 0.
tellUser :: (MonadIO m) => String -> Simulator sbe m ()
tellUser msg = unlessQuiet $ dbugM msg

--------------------------------------------------------------------------------
-- Error handling

errorPath ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  ) => String -> Simulator sbe m a
errorPath rsn = do
  s <- get
  let sbe = symBE s
  let Just (ActiveCS cs) = s^.ctrlStk
  let p = cs^.activePath
  -- Log error path  
  whenVerbosity (>=3) $ do
    dbugM $ "Error path encountered: " ++ rsn
    dbugM $ show $ ppPath sbe p
  mcs <- liftIO $ markCurrentPathAsError sbe cs
  let s' = s & ctrlStk .~ mcs
             & errorPaths %~ (EP (FailRsn rsn) p:)
  -- Merge negation of assumptions in current path into conditions on merge frame.
  -- NB: Since we've set up the control stack for the next invocation of
  -- run, and explicitly captured the error path, we need to be sure to
  -- ship that modified state back to the catch site so it execution can
  -- continue correctly.
  throwError $ ErrorPathExc (FailRsn rsn) s'

wrongArguments :: (Functor sbe, Functor m, MonadIO m) => String -> Simulator sbe m a
wrongArguments nm = errorPath $ nm ++ ": wrong number of arguments"

memFailRsn :: SBE sbe -> String -> [SBETerm sbe] -> String
memFailRsn sbe desc terms = show $ text desc <+> ppTuple pts
  --TODO: See if we can get a reasonable location for the failure.
  where pts = map (prettyTermD sbe) terms


-- | Handle a condition returned by the memory model
processMemCond ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => String -> SBEPred sbe -> Simulator sbe m ()
processMemCond rsn cond = do
  sbe <- gets symBE
  case asBool sbe cond of
    Just True  -> return ()
    Just False -> errorPath rsn
    _ -> do
      Just cs <- use ctrlStk
      let p = cs^.currentPath
      -- TODO: provide more detail here?
      whenVerbosity (>= 6) $ do
        tellUser $ "Warning: Obtained symbolic validity result from memory model."
        tellUser $ "This means that certain memory accesses were valid only on some paths."
        tellUser $ "In this case, the symbolic validity result was encountered at:"
        tellUser $ show $ ppPathLoc sbe p
        tellUser ""
      p' <- liftSBE $ pathAssertions (applyAnd sbe cond) p
      ctrlStk ?= set currentPath p' cs 

alloca ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => MemType
  -> BitWidth
  -> SBETerm sbe
  -> Alignment
  -> Simulator sbe m (SBETerm sbe)
alloca ty szw sztm a = do
  Just m <- preuse currentPathMem
  sbe <- gets symBE
  rslt <- liftSBE $ stackAlloc sbe m ty szw sztm a
  case rslt of
    ASymbolicCountUnsupported  -> errorPath $
      "Stack allocation only supports a concrete element count "
        ++ "(try a different memory model?)"
    AResult c t m' -> do
      currentPathMem .= m'
      let fr = memFailRsn sbe ("Failed alloca allocation of type " ++ show (ppMemType ty)) []
      processMemCond fr c
      return t

malloc ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => MemType
  -> BitWidth -- ^ Width of size
  -> SBETerm sbe -- ^ Size
  -> Simulator sbe m (SBETerm sbe)
malloc ty szw sztm = do
  sbe <- gets symBE
  Just m <- preuse currentPathMem
  rslt <- liftSBE $ heapAlloc sbe m ty szw sztm 0
  case rslt of
    ASymbolicCountUnsupported -> errorPath $
      "malloc only supports concrete element count "
        ++ "(try a different memory model?)"
    AResult c t m' -> do
      currentPathMem .= m'
      let fr =  memFailRsn sbe ("Failed malloc allocation of type " ++ show (ppMemType ty)) []
      t <$ processMemCond fr c

-- | Load value at addr in current path.
load ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => MemType -> SBETerm sbe -> Alignment -> Simulator sbe m (SBETerm sbe)
load tp addr a = do
  sbe <- gets symBE
  Just mem <- preuse currentPathMem
  (cond, v) <- liftSBE $ memLoad sbe mem tp addr a
  let fr = memFailRsn sbe "Invalid load address" [addr]
  processMemCond fr cond
  return v

-- | Load a null-termianted string at given address.
-- May fail if a symbolic byte is found.
loadString ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => String -> SBETerm sbe -> Simulator sbe m String
loadString nm ptr = do
    -- Load ptr, ptr+1, until zero byte, convert each into char,
    -- assemble into list
    cs <- go ptr
    return $ map (toEnum . fromEnum) $ cs
  where go addr = do
          t <- load i8 addr 0
          sbe <- gets symBE
          addr' <- ptrInc addr
          case asUnsignedInteger sbe undefined t of
            Nothing -> do
              errorPath $
                 "Encountered a symbolic byte in " ++ nm ++ "."
            Just 0 -> return []
            Just v -> (v:) <$> go addr'

resolveFunPtrTerm ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => SBETerm sbe -> Simulator sbe m LookupSymbolResult
resolveFunPtrTerm fp = do
  Just m <- preuse currentPathMem
  withSBE $ \s -> codeLookupSymbol s m fp

store ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => MemType -> SBETerm sbe -> SBETerm sbe -> Alignment -> Simulator sbe m ()
store tp val dst a = do
  sbe <- gets symBE
  -- Update memory.
  Just m <- preuse currentPathMem
  (c, m') <- liftSBE $ memStore sbe m dst tp val a
  currentPathMem .= m'
  -- Update symbolic condition.
  let fr = memFailRsn sbe "Invalid store address: " [dst]
  processMemCond fr c

dumpMem :: (Functor m, MonadIO m) => Int -> String -> Simulator sbe m ()
dumpMem v msg =
  whenVerbosity (>=v) $ do
    dbugM $ msg ++ ":"
    Just m <- preuse currentPathMem
    withSBE (\s -> memDump s m Nothing)

--------------------------------------------------------------------------------
-- Override handling

type StdOvd m sbe =
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => Override sbe m

checkTypeCompat :: Monad m
                => Symbol
                -> FunDecl -- ^ Declaration of function to be overriden.
                -> String -- ^ Name of override function
                -> FunDecl -- ^ Type of override function
                -> Simulator sbe m ()      
checkTypeCompat fnm (FunDecl frtn fargs fva) tnm (FunDecl trtn targs tva) = do
  lc <- gets (cbLLVMContext . codebase)
  let ?lc = lc
  let nm = show . ppSymbol
  let e rsn = error $ "Attempt to replace " ++ nm fnm
                     ++ " with function " ++ tnm ++ " that " ++ rsn
  let ppTypes :: [MemType] -> String
      ppTypes tys = show (parens (commas (ppMemType <$> tys)))
  unless (compatMemTypeLists fargs targs) $
    e $ "has different argument types.\n" 
      ++ "  Argument types of " ++ nm fnm ++ ": " ++ ppTypes fargs ++ "\n"
      ++ "  Argument types of " ++ tnm ++ ": " ++ ppTypes targs ++ "\n"
  unless (compatRetTypes frtn trtn) $ e $ "has a different return type.\n"
    ++ "  Return type of " ++ nm fnm ++ ": " ++ show (ppRetType frtn) ++ "\n"
    ++ "  Return type of " ++ tnm ++ ": " ++ show (ppRetType trtn) ++ "\n"
  when (fva && not tva) $ e $ "does not accept varargs.\n"
  when (not fva && tva) $ e $ "allows varargs.\n"

registerOverride :: Monad m => Symbol -> FunDecl -> Override sbe m -> Simulator sbe m ()
registerOverride sym decl handler = do
  cb <- gets codebase
  case cb^.cbFunctionType sym of
    Nothing -> return ()
    Just fd -> do
      checkTypeCompat sym fd "override" decl
      fnOverrides . at sym ?= (handler, False)

-- | Registers an override if a function with the given name has the
-- right type.
tryRegisterOverride :: MonadIO m
                    => Symbol
                       -- | Returns override if function matches expection.
                    -> (FunDecl -> Maybe (Override sbe m))
                    -> Simulator sbe m ()
tryRegisterOverride nm act = do
  cb <- gets codebase
  -- Lookup function
  case cb^.cbFunctionType nm of
    Nothing -> return ()
    Just d ->
      case act d of -- Try getting override
        Just ovd -> registerOverride nm d ovd
        Nothing -> tellUser $ "Warning: " ++ show nm ++ " has an unexpected type."

type OverrideEntry sbe m = (Symbol, FunDecl, Override sbe m)

registerOverrides :: Monad m => [OverrideEntry sbe m] -> Simulator sbe m ()
registerOverrides = mapM_ fn
  where fn (sym, fd, handler) = registerOverride sym fd handler