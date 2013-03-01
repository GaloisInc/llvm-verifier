{- |
Module           : $Header$
Description      : Common type definitions and helper functions for LSS
Stability        : provisional
Point-of-contact : jhendrix
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
module Verifier.LLVM.Simulator.Common 
  ( Simulator(SM)
  , runSM
  , dumpCtrlStk

  , GlobalMap

  , LSSOpts(LSSOpts, optsErrorPathDetails)
  , defaultLSSOpts

  , State(State)
  , codebase
  , symBE
  , liftSymBE
  , ctrlStk
  , globalTerms
  , fnOverrides
  , verbosity
  , evHandlers
  , errorPaths
  , lssOpts
  , pathCounter
  , aigOutputs
  , modifyCS

  , CS
  , initialCtrlStk
  , isFinished
  , getCurrentPath
  , modifyPath
  , modifyCurrentPathM
  , pushCallFrame
  , addCtrlBranch
  , jumpCurrentPath
  , returnCurrentPath
  , markCurrentPathAsError


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
  , ppPath
  , ppPathLoc
  
  , FailRsn(FailRsn)
  , ppFailRsn

  , Override(Override, Redirect)

  , ErrorPath(EP, epRsn, epPath)
  , InternalExc(ErrorPathExc, UnknownExc)
  , SEH( SEH
       , onPostOverrideReg
       , onPreStep
       , onPostStep
       , onMkGlobTerm
       , onPreGlobInit
       , onPostGlobInit
       )
  , ppTuple
  ) where

import Control.Applicative hiding (empty)
import qualified Control.Arrow as A
import Control.Exception (assert)
import Control.Lens hiding (act, set)
import Control.Monad.Error
import Control.Monad.State hiding (State)
import qualified Data.Map  as M
import Text.PrettyPrint.HughesPJ
import qualified Text.LLVM                 as L

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
    )

type LiftSBE sbe m = forall a. sbe a -> Simulator sbe m a
type GlobalMap sbe = M.Map L.Symbol (SBETerm sbe)
--type MF sbe        = MergeFrame (SBETerm sbe) (SBEMemory sbe)
type OvrMap sbe m  = M.Map L.Symbol (Override sbe m, Bool {- user override? -})

-- | Symbolic simulator options
data LSSOpts = LSSOpts {
    optsErrorPathDetails :: Bool
  }

-- | Default simulator options
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

-- | Manipulate the control stack
modifyCS :: (CS sbe -> CS sbe) -> (State sbe m -> State sbe m)
modifyCS f s = s { ctrlStk = f (ctrlStk s) }

-- | Action to perform when branch.
data BranchAction sbe
  = BARunFalse (SBEPred sbe) -- ^ Branch condition
               (Path sbe) -- ^ True path to run
  | BAFalseComplete (SBEPred sbe) -- ^ Assertions before merge
                    (SBEPred sbe) -- ^ Branch condition
                    (Path sbe) -- ^ Completed true path

data MergeInfo
  = ReturnInfo Int (Maybe L.Ident)
    -- | Contains the 
  | PostdomInfo Int SymBlockID

data PathHandler sbe
  = BranchHandler MergeInfo -- ^ Number of call frames and block id to merge to.
                  (BranchAction sbe) -- ^ Action to get new control stack when current
                                      -- path reached merge location.
                  (PathHandler sbe) -- ^ Handler once this handler is done.
  | StopHandler

data CS sbe
  = CompletedCS (Maybe (Path sbe))
    -- | An active control stack.
  | ActiveCS (Path sbe) -- ^ Current path
             (PathHandler sbe) -- ^ Handler that describes response once execution finishes.

initialCtrlStk :: SBE sbe -> SBEMemory sbe -> CS sbe
initialCtrlStk sbe mem = CompletedCS (Just p)
  where p = Path { pathFuncSym = entrySymbol
                 , pathCB = Nothing
                 , pathName = 0
                 , _pathRegs = M.empty
                 , _pathMem = mem
                 , _pathAssertions = sbeTruePred sbe
                 , pathStackHt = 0
                 , pathStack = []
                 }

-- | Return true if all paths in control stack have no more work.
isFinished :: CS sbe -> Bool
isFinished CompletedCS{} = True
isFinished _ = False

getCurrentPath :: CS sbe -> Maybe (Path sbe)
getCurrentPath (CompletedCS mp) = mp
getCurrentPath (ActiveCS p _) = Just p

modifyPath :: (Path sbe -> Path sbe) -> CS sbe -> Maybe (CS sbe)
modifyPath f cs = 
  case cs of
    CompletedCS mp -> (CompletedCS . Just . f) <$> mp
    ActiveCS p h -> Just (ActiveCS (f p) h)
 
-- | Modify current path in control stack.
modifyCurrentPathM :: forall m sbe a .
                      Functor m
                   => CS sbe
                   -> (Path sbe -> m (a,Path sbe))
                   -> Maybe (m (a,CS sbe))
modifyCurrentPathM cs f =
  case cs of
    CompletedCS mp -> (run (CompletedCS . Just)) <$> mp
    ActiveCS p h -> Just (run fn p)
      where fn p' = ActiveCS p' h
 where run :: (Path sbe -> CS sbe) -> Path sbe -> m (a, CS sbe) 
       run csfn = fmap (A.second csfn) . f

pushCallFrame :: SBE sbe
              -> L.Symbol -- ^ Function we are jumping to. 
              -> SymBlockID -- ^ Block to return to.
              -> Maybe (MemType, L.Ident) -- ^ Where to write return value to (if any).
              -> CS sbe -- ^ Current control stack
              -> Maybe (CS sbe)
pushCallFrame sbe calleeSym returnBlock retReg cs =
    case cs of
      CompletedCS Nothing -> fail "All paths failed"
      CompletedCS (Just p) -> do
        return $ ActiveCS (newPath p) StopHandler
      ActiveCS p h -> Just $ ActiveCS (newPath p) h
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
              -> CS sbe -- ^  Current control stack.
              -> Maybe (IO (CS sbe)) 
addCtrlBranch _ _ _ _ _ CompletedCS{} =
  fail "Path is completed"
addCtrlBranch sbe c nb nm ml (ActiveCS p h) = Just $ do
    fmap fn $ sbeRunIO sbe $ memBranch sbe (p^.pathMem)
  where fn mem = ActiveCS pf $ BranchHandler info (BARunFalse c pt) h
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
             -> IO (CS sbe)
postdomMerge sbe p (BranchHandler info@(PostdomInfo n b) act h)
   | n == pathStackHt p && Just b == pathCB p =
  case act of
    BARunFalse c tp -> do
      let tp' = tp & pathAssertions .~ sbeTruePred sbe
      let act' = BAFalseComplete (tp^.pathAssertions) c p
      return $ ActiveCS tp' (BranchHandler info act' h)
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
postdomMerge _ p h = return (ActiveCS p h)

-- | Move current path to target block.
jumpCurrentPath :: SBE sbe -> SymBlockID -> CS sbe -> Maybe (IO (CS sbe))
jumpCurrentPath _ _ CompletedCS{} = fail "Path is completed"
jumpCurrentPath sbe b (ActiveCS p h) = Just $ postdomMerge sbe p { pathCB = Just b } h

-- | Handle merge of paths.
-- The first element of the return pair contains a mesage if error(s) occured
-- during merging.
returnMerge :: SBE sbe
            -> Path sbe
            -> PathHandler sbe
            -> IO ([String], CS sbe)
returnMerge _ p StopHandler | pathStackHt p == 0 =
  return ([], CompletedCS (Just p))
returnMerge sbe p (BranchHandler info@(ReturnInfo n mr) act h) | n == pathStackHt p =
  case act of
    BARunFalse c tp -> do
      let tp' = tp & pathAssertions .~ sbeTruePred sbe
      let act' = BAFalseComplete (tp^.pathAssertions) c p
      return $ ([], ActiveCS tp' (BranchHandler info act' h))
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
returnMerge _ p h = return ([], ActiveCS p h)

-- | Return from current path
returnCurrentPath :: SBE sbe -> Maybe (SBETerm sbe) -> CS sbe -> Maybe (IO (CS sbe))
returnCurrentPath _ _ CompletedCS{} = fail "Path is completed"
returnCurrentPath sbe rt (ActiveCS p h) = Just $ do
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
  return $ ActiveCS pt' h
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
      postdomMerge sbe pf' h
    _ -> return (ActiveCS pf' h)

-- | Mark the current path as an error path.
markCurrentPathAsError :: SBE sbe -> CS sbe -> Maybe (IO (CS sbe))
markCurrentPathAsError _ CompletedCS{} = fail "Path is completed"
markCurrentPathAsError sbe (ActiveCS _ (BranchHandler mi a h)) = Just (branchError sbe mi a h)
markCurrentPathAsError _ (ActiveCS _ StopHandler) = Just (return (CompletedCS Nothing))

type RegMap term = M.Map L.Ident (term, MemType)

setReturnValue :: String -> Maybe (MemType, L.Ident) -> Maybe t
               ->  RegMap t -> RegMap t
setReturnValue _n (Just (tp, r)) (Just rv) rm = M.insert r (rv, tp) rm
setReturnValue _n Nothing   Nothing   rm = rm
setReturnValue nm Nothing   (Just _) _  =
  error $ nm ++ ": Return value where non expected"
setReturnValue nm (Just (_,tr)) Nothing   _  =
  error $ nm ++ ": Missing return value for "  ++ show (L.ppIdent tr)

-- | A Call frame for returning.
data CallFrame sbe = CallFrame { cfFuncSym :: L.Symbol
                               , cfReturnBlock :: Maybe SymBlockID
                               , cfRegs :: RegMap (SBETerm sbe)
                               , cfRetReg :: Maybe (MemType, L.Ident)
                               , cfAssertions :: SBEPred sbe
                               }

-- | Captures all symbolic execution state for a unique control-flow path (as
-- specified by the recorded path constraints)
data Path sbe = Path
  { pathFuncSym      :: !L.Symbol
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
  , onMkGlobTerm      :: Global -> Simulator sbe m ()
    -- | Invoked before memory model initialization of global data
  , onPreGlobInit     :: Global -> SBETerm sbe -> Simulator sbe m ()
    -- | Invoked after memory model initialization of global data
  , onPostGlobInit    :: Global -> SBETerm sbe -> Simulator sbe m ()
  }

-- | A handler for a function override. This gets the function symbol as an
-- argument so that one function can potentially be used to override multiple
-- symbols.
type OverrideHandler sbe m
  =  L.Symbol              -- ^ Callee symbol
  -> Maybe (MemType, L.Ident)     -- ^ Callee return register
  -> [SBETerm sbe] -- ^ Callee arguments
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
ppCtrlStk sbe (CompletedCS mp) =
  maybe (text "All paths failed") (ppPath sbe) mp
ppCtrlStk sbe (ActiveCS p h) =
  text "Active path:" $$
  ppPath sbe p $$
  ppPathHandler sbe h

ppMergeInfo :: MergeInfo -> Doc
ppMergeInfo (ReturnInfo n mr) = text "return" <> parens (int n <+> reg)
  where reg = maybe empty L.ppIdent mr
ppMergeInfo (PostdomInfo n b) =
    text "postdom" <> parens (int n <+> ppSymBlockID b)

ppBranchAction :: SBE sbe -> BranchAction sbe -> Doc
ppBranchAction sbe (BARunFalse c p) = 
  text "runFalse" <+> prettyPredD sbe c $$
  nest 2 (ppPath sbe p)
ppBranchAction sbe (BAFalseComplete a c p) =
  text "falseComplete" <+> prettyPredD sbe c $$
  nest 2 (text "assumptions:" <+> prettyPredD sbe a) $$
  nest 2 (ppPath sbe p)

ppPathHandler :: SBE sbe -> PathHandler sbe -> Doc
ppPathHandler sbe (BranchHandler info act h) = 
  text "on" <+> ppMergeInfo info <+> text "do" $$
  nest 2 (ppBranchAction sbe act) $$
  ppPathHandler sbe h
ppPathHandler _ StopHandler = text "stop"


ppPath :: SBE sbe -> Path sbe -> Doc
ppPath sbe p =
  text "Path #"
  <>  integer (pathName p)
  <>  brackets ( text (show $ L.ppSymbol $ pathFuncSym p)
                 <> char '/'
                 <> maybe (text "none") ppSymBlockID (pathCB p)
               )
  <>  colon
  $+$ nest 2 (text "Locals:" $+$ nest 2 (ppRegMap sbe (p^.pathRegs)))
-- <+> (parens $ text "PC:" <+> ppPC sbe c)

-- Prints just the path's location and path constraints
ppPathLoc :: SBE sbe -> Path sbe -> Doc
ppPathLoc _ p =
  text "Path #"
  <>  integer (pathName p)
  <>  brackets ( text (show $ L.ppSymbol $ pathFuncSym p)
                 <> char '/'
                 <> maybe (text "none") ppSymBlockID (pathCB p)
               )

ppRegMap :: SBE sbe -> RegMap (SBETerm sbe) -> Doc
ppRegMap sbe mp =
    vcat [ ppIdentAssoc r <> prettyTermD sbe v | (r,(v,_)) <- as ]
    where
      ppIdentAssoc r = L.ppIdent r
                       <> text (replicate (maxLen - identLen r) ' ')
                       <> text " => "
      maxLen         = foldr max 0 $ map (identLen . fst) as
      identLen       = length . show . L.ppIdent
      as             = M.toList mp

ppTuple :: [Doc] -> Doc
ppTuple = parens . hcat . punctuate comma

-----------------------------------------------------------------------------------------
-- Debugging

dumpCtrlStk :: (MonadIO m) => Simulator sbe m ()
dumpCtrlStk = do
  (sbe, cs) <- gets (symBE A.&&& ctrlStk)
  banners $ show $ ppCtrlStk sbe cs