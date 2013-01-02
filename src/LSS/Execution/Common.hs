{- |
Module           : $Header$
Description      : Common type definitions and helper functions for LSS
Stability        : provisional
Point-of-contact : jhendrix
-}

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-warn-orphans      #-}

module LSS.Execution.Common 
  ( Simulator(SM)
  , runSM
  , dumpCtrlStk
  , dumpCtrlStk'

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
  , Path'
  , pathFuncSym
  , pathCB
  , pathName
  , pathRegs
  , pathMem
  , pathAssertions
  , addPathAssertion
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
import Control.Arrow             hiding ((<+>))
import Control.Exception (assert)
import Control.Monad.Error hiding (sequence)
import Control.Monad.State       hiding (State, sequence)
import Data.Foldable
import Data.Traversable
import Data.LLVM.Symbolic.AST
import qualified Data.Map  as M
import LSS.Execution.Codebase
import LSS.Execution.Utils
import LSS.LLVMUtils (i1)
import Verifier.LLVM.Backend
import Text.LLVM                 (Typed(..))
import Text.PrettyPrint.HughesPJ
import qualified Text.LLVM                 as L
import Prelude hiding (foldr, sequence)

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
  = BARunFalse (SBETerm sbe) -- ^ Branch condition
               (Path sbe) -- ^ False path to run
  | BAFalseComplete (SBETerm sbe) -- ^ Assertions before merge
                    (SBETerm sbe) -- ^ Branch condition
                    (Path sbe) -- ^ Completed true path

data MergeInfo
  = ReturnInfo Int (Maybe Reg)

  | PostdomInfo Int SymBlockID

data PathHandler sbe
  = BranchHandler MergeInfo -- ^ Number of call frames and block id to merge to.
                  (BranchAction sbe) -- ^ Action to get new control stack when current
                                      -- path reached merge location.
                  (PathHandler sbe) -- ^ Handler once this handler is done.
  | StopHandler

data CS (sbe :: * -> *)
  = CompletedCS (Maybe (Path sbe))
    -- | An active control stack.
  | ActiveCS (Path sbe) -- ^ Current path
             (PathHandler sbe) -- ^ Handler that describes response once execution finishes.

initialCtrlStk :: SBE sbe -> SBEMemory sbe -> IO (CS sbe)
initialCtrlStk sbe mem = do
  true <- sbeRunIO sbe $ termBool sbe True
  let p = Path { pathFuncSym = entrySymbol
               , pathCB = Nothing
               , pathName = 0
               , pathRegs = M.empty
               , pathMem = mem
               , pathAssertions = true
               , pathStackHt = 0
               , pathStack = []
               }
  return $ CompletedCS (Just p)

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
       run csfn = fmap (id *** csfn) . f

pushCallFrame :: L.Symbol -- ^ Function we are jumping to. 
              -> SymBlockID -- ^ Block to return to.
              -> Maybe (Typed Reg) -- ^ Where to write return value to (if any).
              -> CS sbe -- ^ Current control stack
              -> Maybe (CS sbe)
pushCallFrame calleeSym returnBlock retReg cs =
    case cs of
      CompletedCS Nothing -> fail "All paths failed"
      CompletedCS (Just p) -> do
        return $ ActiveCS (newPath p) StopHandler
      ActiveCS p h -> Just $ ActiveCS (newPath p) h
  where newPath p = p'
          where cf = CallFrame { cfFuncSym = pathFuncSym p
                               , cfReturnBlock = Just returnBlock
                               , cfRegs = pathRegs p
                               , cfRetReg = retReg
                               , cfAssertions = pathAssertions p
                               }
                p' = Path { pathFuncSym = calleeSym
                          , pathCB = Just initSymBlockID
                          , pathName = pathName p
                          , pathRegs = M.empty
                          , pathMem = pathMem p
                          , pathAssertions = pathAssertions p
                          , pathStackHt = pathStackHt p + 1
                          , pathStack   = cf : pathStack p
                          }

-- | Add a control branch
addCtrlBranch :: SBETerm sbe -- ^ Condition to branch on.
              -> SymBlockID -- ^ Location for newly branched paused path to start at.
              -> Integer -- ^ Name of path
              -> MergeLocation -- ^ Control point to merge at.
              -> CS sbe -- ^  Current control stack.
              -> Maybe (CS sbe) 
addCtrlBranch c nb nm ml cs =
    case cs of
      CompletedCS{} -> fail "Path is completed"
      ActiveCS p h -> Just $ ActiveCS p $ BranchHandler info (BARunFalse c pt) h
        where info = case ml of
                       Just b -> PostdomInfo (pathStackHt p) b
                       Nothing -> ReturnInfo (pathStackHt p - 1) (typedValue <$> cfRetReg cf)
                         where cf : _ = pathStack p
              pt = p { pathCB = Just nb
                     , pathName = nm 
                     }
              
postdomMerge :: SBE sbe
             -> Path sbe
             -> PathHandler sbe
             -> IO (CS sbe)
postdomMerge sbe p (BranchHandler info@(PostdomInfo n b) act h)
   | n == pathStackHt p && Just b == pathCB p =
  case act of
    BARunFalse c tp -> do
      true <- sbeRunIO sbe $ termBool sbe True
      let tp' = tp { pathAssertions = true }
      let act' = BAFalseComplete (pathAssertions tp) c p
      return $ ActiveCS tp' (BranchHandler info act' h)
    BAFalseComplete a c pf -> do
      let mergeTyped (Typed tp tt) (Typed _ ft) =
            Typed tp <$> sbeRunIO sbe (applyIte sbe tp c tt ft)
      -- Merge path regs
      mergedRegs <- sequence $
        M.intersectionWith mergeTyped (pathRegs p) (pathRegs pf)
      -- Merge memory
      mergedMemory <- sbeRunIO sbe $ memMerge sbe c (pathMem p) (pathMem pf)
      -- Merge assertions
      mergedAssertions <- sbeRunIO sbe $
        applyIte sbe i1 c (pathAssertions p) (pathAssertions pf)
      a' <- sbeRunIO sbe $ applyAnd sbe a mergedAssertions
      assert (pathFuncSym p == pathFuncSym pf && pathCB p == pathCB pf) $ do
      let p' = p { pathRegs = mergedRegs
                 , pathMem = mergedMemory
                 , pathAssertions = a'
                 }
      -- Recurse to check if more merges should be performed.
      postdomMerge sbe p' h
postdomMerge _ p h = return (ActiveCS p h)

-- | Move current path to target block.
jumpCurrentPath :: SBE sbe -> SymBlockID -> CS sbe -> Maybe (IO (CS sbe))
jumpCurrentPath _ _ CompletedCS{} = fail "Path is completed"
jumpCurrentPath sbe b (ActiveCS p h) = Just $ postdomMerge sbe p { pathCB = Just b } h

returnMerge :: SBE sbe
            -> Path sbe
            -> PathHandler sbe
            -> IO (CS sbe)
returnMerge _ p StopHandler | pathStackHt p == 0 =
  return (CompletedCS (Just p))
returnMerge sbe p (BranchHandler info@(ReturnInfo n mr) act h) | n == pathStackHt p =
  case act of
    BARunFalse c tp -> do
      true <- sbeRunIO sbe $ termBool sbe True
      let tp' = tp { pathAssertions = true }
      let act' = BAFalseComplete (pathAssertions tp) c p
      return $ ActiveCS tp' (BranchHandler info act' h)
    BAFalseComplete a c pf -> do
      -- Merge return value
      mergedRegs <-
        case mr of
          Nothing -> return (pathRegs p)
          Just r -> do
            let Just (Typed tp vt) = M.lookup r (pathRegs p)
            let Just (Typed _ vf)  = M.lookup r (pathRegs pf)
            v <- sbeRunIO sbe $ applyIte sbe tp c vt vf
            return $ M.insert r (Typed tp v) (pathRegs p)
      -- Merge memory
      mergedMemory <- sbeRunIO sbe $ memMerge sbe c (pathMem p) (pathMem pf)
      -- Merge assertions
      mergedAssertions <- sbeRunIO sbe $
        applyIte sbe i1 c (pathAssertions p) (pathAssertions pf)
      a' <- sbeRunIO sbe $ applyAnd sbe a mergedAssertions
      let p' = p { pathRegs = mergedRegs
                 , pathMem = mergedMemory
                 , pathAssertions = a'
                 }
      returnMerge sbe p' h
returnMerge _ p h = return (ActiveCS p h)

-- | Return from current path
returnCurrentPath :: SBE sbe -> Maybe (SBETerm sbe) -> CS sbe -> Maybe (IO (CS sbe))
returnCurrentPath _ _ CompletedCS{} = fail "Path is completed"
returnCurrentPath sbe rt (ActiveCS p h) = Just $ do
  let cf : cfs = pathStack p
  m <- sbeRunIO sbe $ stackPopFrame sbe (pathMem p)
  let p' = Path { pathFuncSym = cfFuncSym cf
                , pathCB   = cfReturnBlock cf
                , pathName = pathName p
                , pathRegs = setReturnValue "returnCurrentpath" (cfRetReg cf) rt (cfRegs cf)
                , pathMem  = m
                , pathAssertions = cfAssertions cf
                , pathStackHt = pathStackHt p - 1
                , pathStack  = cfs
                }
  returnMerge sbe p' h

branchError :: SBE sbe -> BranchAction sbe -> PathHandler sbe -> IO (CS sbe) 
branchError sbe (BARunFalse c pt) h = do
  a2 <- sbeRunIO sbe $ applyAnd sbe (pathAssertions pt) c
  let pt' = pt { pathAssertions = a2 }
  return $ ActiveCS pt' h
branchError sbe (BAFalseComplete a c pf) h = do
  -- Update assertions on current path
  a1   <- sbeRunIO sbe $ applyAnd sbe a (pathAssertions pf)
  cNot <- sbeRunIO sbe $ applyBNot sbe c
  a2   <- sbeRunIO sbe $ applyAnd sbe a1 cNot
  let pf' = pf { pathAssertions = a2 }

  -- Try to merge states that may have been waiting for the current path to terminate.
  case h of
    BranchHandler ReturnInfo{} _ _ -> returnMerge sbe pf' h
    BranchHandler PostdomInfo{} _ _ -> postdomMerge sbe pf' h
    StopHandler -> return (ActiveCS pf' h)

-- | Mark the current path as an error path.
markCurrentPathAsError :: SBE sbe -> CS sbe -> Maybe (IO (CS sbe))
markCurrentPathAsError _ CompletedCS{} = fail "Path is completed"
markCurrentPathAsError sbe (ActiveCS _ (BranchHandler _ a h)) = Just (branchError sbe a h)
markCurrentPathAsError _ (ActiveCS _ StopHandler) = Just (return (CompletedCS Nothing))

type RegMap term = M.Map Reg (Typed term)

setReturnValue :: String -> Maybe (Typed Reg) -> Maybe t
               ->  RegMap t -> RegMap t
setReturnValue _n (Just (Typed tp r)) (Just rv) rm = M.insert r (Typed tp rv) rm
setReturnValue _n Nothing   Nothing   rm = rm
setReturnValue nm Nothing   (Just _) _  =
  error $ nm ++ ": Return value where non expected"
setReturnValue nm (Just tr) Nothing   _  =
  error $ nm ++ ": Missing return value for "  ++ show (L.ppIdent (typedValue tr))

-- | A Call frame for returning.
data CallFrame term = CallFrame { cfFuncSym :: L.Symbol
                                , cfReturnBlock :: Maybe SymBlockID
                                , cfRegs :: RegMap term
                                , cfRetReg :: Maybe (Typed Reg)
                                , cfAssertions :: term
                                }

-- | Captures all symbolic execution state for a unique control-flow path (as
-- specified by the recorded path constraints)
data Path' term mem = Path
  { pathFuncSym      :: !L.Symbol
  , pathCB           :: !(Maybe SymBlockID) -- ^ The currently-executing basic
                                         -- block along this path, if any.
  , pathName         :: !(Integer)       -- ^ A unique name for this path
  , pathRegs         :: !(RegMap term)
  , pathMem          :: mem              -- ^ The memory model along this path
  , pathAssertions   :: term             -- ^ Condition on path since last branch.
  , pathStackHt    :: !Int             -- ^ Number of call frames count.
  , pathStack      :: [CallFrame term]     -- ^ Return registers for current calls.
  }

type Path sbe      = Path' (SBETerm sbe) (SBEMemory sbe)

addPathAssertion :: Functor sbe
                 => SBE sbe -> SBETerm sbe -> Path sbe -> sbe (Path sbe)
addPathAssertion sbe t p = set <$> applyAnd sbe (pathAssertions p) t
  where set a = p { pathAssertions = a }

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
  , onPreGlobInit     :: L.Global -> SBETerm sbe -> Simulator sbe m ()
    -- | Invoked after memory model initialization of global data
  , onPostGlobInit    :: L.Global -> Typed (SBETerm sbe) -> Simulator sbe m ()
  }

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
  text "runFalse" <+> prettyTermD sbe c $$
  nest 2 (ppPath sbe p)
ppBranchAction sbe (BAFalseComplete a c p) =
  text "falseCompelte" <+> prettyTermD sbe c $$
  nest 2 (text "assumptions:" <+> prettyTermD sbe a) $$
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
  $+$ nest 2 (text "Locals:" $+$ nest 2 (ppRegMap sbe (pathRegs p)))
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
    vcat [ ppIdentAssoc r <> (L.ppTyped (prettyTermD sbe) v) | (r,v) <- as ]
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
  (sbe, cs) <- gets (symBE &&& ctrlStk)
  banners $ show $ ppCtrlStk sbe cs

dumpCtrlStk' :: (MonadIO m, LogMonad m) => Int -> Simulator sbe m ()
dumpCtrlStk' lvl = whenVerbosity (>=lvl) dumpCtrlStk

{-
instance (LogMonad IO) where
  setVerbosity _ = return ()
  getVerbosity   = return 1
-}