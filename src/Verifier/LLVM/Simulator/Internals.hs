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
{-# LANGUAGE ViewPatterns #-}
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
  , errorPaths
  , pathCounter
  , aigOutputs
  , breakpoints
  , onPathPosChange
  , ErrorHandler
  , onSimError
  , onUserInterrupt

  , Breakpoint
  , addBreakpoint
  , removeBreakpoint


  , CS(..)
  , ActiveCS
  , activePath
  , activePaths
  , initialCtrlStk
  , currentPath
  , modifyCurrentPathM
  , pushCallFrame
  , addCtrlBranch

  , currentPathOfState
  , currentPathMem

  , SymBlockID

  , RegMap
  , setReturnValue
  , ppRegMap

  , PathPC
  , incPathPC
  , Path
  , pathFuncSym
  , pathPC
  , pathName
  , pathRegs
  , pathMem
  , pathAssertions
  , pathStack
  , pathStackHt
  , ppPath
  , ppPathInfo
  , assumptionsForActivePath
  , jumpCurrentPath
  , returnCurrentPath
  , killCurrentPath

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
  , tryFindFunDecl
  , tryRegisterOverride
    -- Override entry.
  , OverrideEntry
  , StdOvdEntry
  , voidOverrideEntry
  , overrideEntry
  , varArgsOverrideEntry
  , registerOverrideEntry
  , registerOverrides

  , ErrorPath(EP, epRsn, epPath)
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
  , memset
  , dumpMem

  , ppStackTrace
  , ppTuple
  ) where

import Control.Applicative hiding (empty)
import qualified Control.Arrow as A
import Control.Exception (assert)
import Control.Lens hiding (act)
import Control.Monad.Error
import Control.Monad.State.Class
import Control.Monad.Trans.State.Strict (StateT, runStateT, execStateT)

import qualified Data.Map  as M
import Data.Maybe
import qualified Data.Set  as S
import qualified Data.Vector as V

import System.Console.Haskeline.MonadException (MonadException)

import Text.PrettyPrint.Leijen hiding ((<$>))

import Verifier.LLVM.AST
import Verifier.LLVM.Backend
import Verifier.LLVM.Codebase
import Verifier.LLVM.Simulator.CursorTree
import Verifier.LLVM.Simulator.SimUtils

newtype Simulator sbe m a =
  SM { runSM :: ErrorT FailRsn (StateT (State sbe m) m) a }
  deriving
    ( Functor
    , Monad
    , MonadIO
    , MonadState (State sbe m)
    , MonadError FailRsn
    , MonadException
    )

type LiftSBE sbe m = forall a. sbe a -> Simulator sbe m a

-- | Map from function names to address of their entry point.
type GlobalMap sbe = M.Map Symbol (SBETerm sbe)

-- | Map from individual blocks within function to address of block.
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

-- | Action to perform when a simulation error is encountered.
-- Parameters include simulator control stack prior to error, and
-- a description of the error.
type ErrorHandler sbe m
   = ActiveCS sbe -> FailRsn -> Simulator sbe m ()

-- | Symbolic simulator state
data State sbe m = State
  { codebase     :: Codebase sbe    -- ^ LLVM code, post-transformation to sym ast
  , symBE        :: SBE sbe         -- ^ Symbolic backend interface
  , liftSymBE    :: LiftSBE sbe m   -- ^ Lift SBE operations into the Simulator monad
  , _globalTerms :: GlobalMap sbe   -- ^ Global ptr terms (global for program).
  , _blockPtrs   :: BlockMap sbe    -- ^ Block pointers (global for program).
  , verbosity    :: Int             -- ^ Verbosity level
  , _errorPaths  :: [ErrorPath sbe] -- ^ Terminated paths due to errors.
  , _pathCounter :: Integer         -- ^ Name supply for paths

  , lssOpts      :: LSSOpts         -- ^ Options passed to simulator
  , _breakpoints :: !(M.Map Symbol (S.Set Breakpoint))

  , _onPathPosChange :: Simulator sbe m ()
  , _onSimError      :: ErrorHandler sbe m
    -- | Invoked when user presses control-C during execution.
  , _onUserInterrupt :: Simulator sbe m ()


  , _ctrlStk     :: !(Maybe (CS sbe))  -- ^ Control stack for controlling simulator.
  , _fnOverrides :: OvrMap sbe m    -- ^ Function override table
    -- | Current list of AIG outputs, discharged via lss_write_aiger() sym
    -- api calls
  , _aigOutputs  :: [(MemType,SBETerm sbe)]
  }

ctrlStk :: Simple Lens (State sbe m) (Maybe (CS sbe))
ctrlStk f s = (\v -> s { _ctrlStk = v }) <$> f (_ctrlStk s)

globalTerms :: Simple Lens (State sbe m) (GlobalMap sbe)
globalTerms f s = (\v -> s { _globalTerms = v }) <$> f (_globalTerms s)

blockPtrs :: Simple Lens (State sbe m) (BlockMap sbe)
blockPtrs f s = (\v -> s { _blockPtrs = v }) <$> f (_blockPtrs s)

fnOverrides :: Simple Lens (State sbe m) (OvrMap sbe m)
fnOverrides f s = (\v -> s { _fnOverrides = v }) <$> f (_fnOverrides s)

errorPaths :: Simple Lens (State sbe m) [ErrorPath sbe]
errorPaths f s = (\v -> s { _errorPaths = v }) <$> f (_errorPaths s)

pathCounter :: Simple Lens (State sbe m) Integer
pathCounter f s = (\v -> s { _pathCounter = v }) <$> f (_pathCounter s)

aigOutputs :: Simple Lens (State sbe m) [(MemType,SBETerm sbe)]
aigOutputs f s = (\v -> s { _aigOutputs = v }) <$> f (_aigOutputs s)

breakpoints :: Simple Lens (State sbe m) (M.Map Symbol (S.Set Breakpoint))
breakpoints f s = (\v -> s { _breakpoints = v }) <$> f (_breakpoints s)

-- | Event called when the instruction the active path is on has changed.
onPathPosChange :: Simple Lens (State sbe m) (Simulator sbe m ())
onPathPosChange =
  lens _onPathPosChange (\s v -> s { _onPathPosChange = v })

onSimError :: Simple Lens (State sbe m) (ErrorHandler sbe m)
onSimError = lens _onSimError (\s v -> s { _onSimError = v })

-- | Event called when the simulator is interrupted by a user interrupt.
onUserInterrupt :: Simple Lens (State sbe m) (Simulator sbe m ())
onUserInterrupt = lens _onUserInterrupt (\s v -> s { _onUserInterrupt = v })

-- | Types of breakpoints (kind of boring for now, but maybe with a
-- DWARF parser we can do more...)
type Breakpoint = (SymBlockID,Int)

-- | A control stack that is still active.
newtype ActiveCS sbe = ACS { unACS :: CursorTree (BranchInfo sbe) (Path sbe) }

data BranchInfo sbe = BI { biTarget :: MergeTarget 
                         , biCond :: SBEPred sbe
                         , _biAssertions :: SBEPred sbe
                         , _biAbortsAbove :: Int 
                         }

biAssertions :: Simple Lens (BranchInfo sbe) (SBEPred sbe)
biAssertions = lens _biAssertions (\s v -> s { _biAssertions = v })

biAbortsAbove :: Simple Lens (BranchInfo sbe) Int
biAbortsAbove = lens _biAbortsAbove (\s v -> s { _biAbortsAbove = v })


-- | Target location to merge to.
data MergeTarget
    -- | @OnReturn i mr@ denotes that the merge should occur when
    -- returning from a function with old stack height equal to @i@.
    -- The register to write the return value to in the calling
    -- frame is @mr@ (which may be Nothing if this is a void function).
  = OnReturn Int (Maybe Ident)
    -- | @OnPostdomJump i b@ denotes the merge should occur when
    -- jumping to block @b@ with a stack height of @i@. 
  | OnPostdomJump Int SymBlockID

activeTree :: Simple Iso
                     (ActiveCS sbe)
                     (CursorTree (BranchInfo sbe) (Path sbe))
activeTree = iso unACS ACS

activePath :: Simple Lens (ActiveCS sbe) (Path sbe)
activePath = activeTree . activeValue

activePaths :: Simple Traversal (ActiveCS sbe) (Path sbe)
activePaths = activeTree . inorderTraversal

activePair :: Simple Iso
                     (ActiveCS sbe)
                     (TreeContext (BranchInfo sbe) (Path sbe), Path sbe)
activePair = activeTree . treePair

-- | A control stack consists of a collection of execution paths
-- that will eventually be merged.
data CS sbe
  = FinishedCS (Path sbe)
  | ActiveCS (ActiveCS sbe)

-- | Returns current path
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
initialCtrlStk sbe mem = FinishedCS (initialPath sbe mem)

-- | Modify current path in control stack.
modifyCurrentPathM :: Functor m
                   => CS sbe
                   -> (Path sbe -> m (a,Path sbe))
                   -> m (a,CS sbe)
modifyCurrentPathM cs f = 
  over _2 (\p -> set currentPath p cs) <$> f (cs^.currentPath)

-- | Push a call frame to the active path
pushCallFrame :: forall sbe
               . SBE sbe
              -> Symbol -- ^ Function we are jumping to. 
              -> SymBlockID -- ^ Block to return to.
              -> Maybe (MemType, Ident) -- ^ Where to write return value to (if any).
              -> CS sbe -- ^ Current control stack
              -> ActiveCS sbe
pushCallFrame sbe calleeSym returnBlock retReg cs =
    case cs of
      FinishedCS p -> ACS (Singleton (newPath p))
      ActiveCS acs -> acs & activePath %~ newPath
  where newPath :: Path sbe -> Path sbe
        newPath p = p'
          where cf = CallFrame { cfFuncSym = p^.pathFuncSym
                               , cfReturnBlock = Just (returnBlock,0)
                               , cfRegs = p^.pathRegs
                               , cfRetReg = retReg
                               }
                p' = p & pathFuncSym    .~ calleeSym
                       & pathPC         .~ Just (initSymBlockID,0)
                       & pathRegs       .~ M.empty
                       & pathAssertions .~ sbeTruePred sbe
                       & pathStackHt    +~ 1
                       & pathStack     %~ (cf:)

-- | Add a control branch
addCtrlBranch :: SBE sbe
              -> SBEPred sbe -- ^ Condition to branch on.
                 -- | Location for newly branched paused path to start at.
              -> SymBlockID
              -> Integer -- ^ Name of new path
              -> MergeLocation -- ^ Control point to merge at.
              -> ActiveCS sbe -- ^  Current control stack.
              -> IO (ActiveCS sbe)
addCtrlBranch sbe c nb nm ml (view activePair -> (h,p)) = do
  mem <- sbeRunIO sbe (memBranch sbe (p^.pathMem))
  let tgt = case ml of
              Just b -> OnPostdomJump (p^.pathStackHt) b
              Nothing -> OnReturn (p^.pathStackHt) (snd <$> cfRetReg cf)
                where cf : _ = p^.pathStack
      info = BI tgt c (p^.pathAssertions) 0
  let pt = p & pathPC .~ Just (nb,0)
             & pathMem .~ mem
             & pathAssertions .~ sbeTruePred sbe
  let pf = p & pathName .~ nm
             & pathMem .~ mem
             & pathAssertions .~ sbeTruePred sbe
  return $ ACS (Branch h info RightActive pf (Singleton pt))
                
atTarget :: Path sbe -> MergeTarget -> Bool
atTarget p (OnPostdomJump n b) = p^.pathStackHt == n && p^.pathPC == Just (b,0)
atTarget p (OnReturn n _) = p^.pathStackHt < n

flipBranch :: TreeContext b a
           -> b
           -> Orientation
           -> a
           -> CursorTree b a
           -> CursorTree b a
flipBranch h b LeftActive  p r = branch h b RightActive (Singleton p) r
flipBranch h b RightActive p r = branch h b LeftActive  r (Singleton p)

-- | Return merged assertions in path.
mergeAssertions :: SBE sbe
                -> SBEPred sbe -- ^ Old assertions
                -> SBEPred sbe -- ^ Branch condition
                -> Path sbe -- ^ True path
                -> Path sbe -- ^ False path
                -> IO (SBEPred sbe)
mergeAssertions sbe a c pt pf = do
  a' <- sbeRunIO sbe $
    applyPredIte sbe c (pt^.pathAssertions) (pf^.pathAssertions)
  sbeRunIO sbe $ applyAnd sbe a a'

type ErrorCollector m e = StateT [e] m 

runErrorCollector :: Functor m => ErrorCollector m e a -> m ([e],a)
runErrorCollector m = finish <$> runStateT m []
  where finish (v,e) = (reverse e,v)

collectError :: Monad m => a -> m (Either e a) -> ErrorCollector m e a
collectError d a = do
  ev <- lift $ a
  case ev of
    Left e -> modify (e:) >> return d
    Right v -> return v

-- | Merge path and path handler.  Note that the new state may
-- have infeasible path assertions.
checkForMerge :: SBE sbe -> ActiveCS sbe -> IO (ActiveCS sbe)
checkForMerge sbe (ACS cs) =
  case cs of
    Branch h b o p r | atTarget p (biTarget b) -> do
      let tgt = biTarget b
          c   = biCond b
          a   = b^.biAssertions    
      case r of
        -- Merge if adjacent tree is a single path at the branch
        -- merge point.
        Singleton pf | atTarget pf tgt -> do
          let mergeTyped (tt,tp) (ft,_) =
                fmap (,tp) $ collectError tt
                           $ sbeRunIO sbe
                           $ applyIte sbe tp c tt ft
          -- Merge path regs
          (_errs, mergedRegs) <- runErrorCollector $ do
            let pr  = p^.pathRegs
                pfr = pf^.pathRegs 
            case tgt of
              OnReturn _ mr ->
                case mr of
                  Nothing -> return pr
                  Just reg -> do -- Merge return register only
                    let Just vt =  pr^.at reg
                    let Just vf = pfr^.at reg
                    (\v -> pr & at reg ?~ v) <$> mergeTyped vt vf
              OnPostdomJump{} -> -- Merge all registers
                traverse id $ M.intersectionWith mergeTyped pr pfr
          -- Merge memory
          mergedMem <- sbeRunIO sbe $
            memMerge sbe c (p^.pathMem) (pf^.pathMem)
          finalMem <- abortMemBranches sbe (b^.biAbortsAbove) mergedMem
          -- Merge assertions
          a' <- mergeAssertions sbe a c p pf
          assert (p^.pathFuncSym == pf^.pathFuncSym
                  && p^.pathPC == pf^.pathPC) $ do
            return ()  
          let p' = p & pathRegs .~ mergedRegs
                     & pathMem  .~ finalMem
                     & pathAssertions .~ a'
          -- Recurse to check if more merges should be performed.
          checkForMerge sbe (ACS ((h,p') ^.from treePair))
        _ -> return (ACS (flipBranch h b o p r))
    _ -> return (ACS cs)

-- | Move current path to target block.
-- Note that the new path may have infeasible assertions.
jumpCurrentPath :: SBE sbe -> SymBlockID -> ActiveCS sbe -> IO (ActiveCS sbe)
jumpCurrentPath sbe b cs =
  checkForMerge sbe (cs & activePath . pathPC .~ Just (b,0))

-- | Return from current path.
-- The current path may be infeasible.
returnCurrentPath :: SBE sbe
                  -> Maybe (SBETerm sbe)
                  -> ActiveCS sbe
                  -> IO (CS sbe)
returnCurrentPath sbe rt cs = do
  let p = cs^.activePath
  let cf : cfs = p^.pathStack
  m <- sbeRunIO sbe $ stackPopFrame sbe (p^.pathMem)
  let regs' = setReturnValue (cfRetReg cf) rt (cfRegs cf)
  let p' = p & pathFuncSym .~ cfFuncSym cf
             & pathPC .~ cfReturnBlock cf
             & pathRegs .~ regs'
             & pathMem .~ m
             & pathStackHt -~ 1
             & pathStack .~ cfs
  cs' <- checkForMerge sbe (cs & activePath .~ p')
  return $
    case cs' of
      ACS (Singleton pn) | null (pn^.pathStack) -> FinishedCS pn
      _ -> ActiveCS cs'

assumptionsForActivePath :: (Monad m)
                         => ActiveCS sbe
                         -> Simulator sbe m (SBEPred sbe)
assumptionsForActivePath (ACS t) = do
  sbe <- gets symBE
  let branchCond (b,LeftActive,_) = return (biCond b)
      branchCond (b,RightActive,_) = liftSBE $ applyBNot sbe (biCond b)
  conds <- mapM branchCond (treeParents t)
  let andFn x y = liftSBE (applyAnd sbe x y)
  foldM andFn (sbeTruePred sbe) conds

-- | @sbeAndIO sbe a c@ returns (a and c).
sbeAndIO :: SBE sbe -> SBEPred sbe -> SBEPred sbe -> IO (SBEPred sbe)
sbeAndIO sbe a c = sbeRunIO sbe (applyAnd sbe a c)
  
-- | List a monadic operation from a state @s@ to itself to
-- the state monad.
stateTransform :: Monad m => (s -> m s) -> StateT s m ()
stateTransform a = do
  s <- get
  put =<< lift (a s)

-- | Abort memory branches
abortMemBranches :: SBE sbe -> Int -> SBEMemory sbe -> IO (SBEMemory sbe)
abortMemBranches sbe n = execStateT runAllAborts
  where abortOnce    = sbeRunIO sbe . memBranchAbort sbe
        runAllAborts = replicateM_ n (stateTransform abortOnce)

-- | Kill the current path and add it to the list of errorPaths.
-- This function assumes the simulator has an active path.
killCurrentPath :: MonadIO m => FailRsn -> Simulator sbe m ()
killCurrentPath rsn = do
  Just (ActiveCS cs) <- use ctrlStk
  -- Get current path before last step.
  let p = cs^.activePath
  -- Merge current path as error, and update control stack.
  sbe <- gets symBE
  mcs' <- liftIO $ markCurrentPathAsError sbe cs
  ctrlStk .= mcs'
  -- Add path to list of error paths.
  errorPaths %= (EP rsn p:)

-- | Mark the current path as an error path.
-- N.B. The new current path if any could now potentially be infeasible.
markCurrentPathAsError :: SBE sbe -> ActiveCS sbe -> IO (Maybe (CS sbe))
markCurrentPathAsError _   (ACS Singleton{}) = return Nothing
markCurrentPathAsError sbe (ACS (Branch ctx b o _ rest)) = do
  -- Negate condition if the true branch (which is on the left)
  -- has failed.
  cond <- case o of
            LeftActive  -> sbeRunIO sbe $ applyBNot sbe (biCond b)
            RightActive -> pure (biCond b)
  -- Get new and assertions.
  newAssertions <- sbeAndIO sbe (b^.biAssertions) cond
  cs <- case topView rest of
          Left p -> do
            -- Set assertions
            let p' = p & pathAssertions .~ newAssertions
            -- Clean up memory aborts.
            let cnt = (b^.biAbortsAbove)+1
            instContext' ctx <$> pathMem (abortMemBranches sbe cnt) p'
          Right (b', o', l, r) -> do
            a' <- sbeAndIO sbe newAssertions (b'^.biAssertions)
            let b2 = b' & biAssertions .~ a'
                        & biAbortsAbove +~ 1
            return $ branch ctx b2 o' l r
  Just . ActiveCS <$> checkForMerge sbe (ACS cs)

type RegMap term = M.Map Ident (term, MemType)

setReturnValue :: Maybe (MemType, Ident) -> Maybe t
               -> RegMap t -> RegMap t
setReturnValue (Just (tp, r)) (Just rv) rm = M.insert r (rv, tp) rm
setReturnValue Nothing   Nothing   rm = rm
setReturnValue Nothing   (Just _) _  =
  error $ "internal: Return value where non expected"
setReturnValue (Just (_,tr)) Nothing   _  =
  error $ "internal: Missing return value for "  ++ show (ppIdent tr)

type PathPC = Maybe (SymBlockID, Int)

incPathPC :: PathPC -> PathPC
incPathPC = over (_Just . _2) (+1) 

-- | A Call frame for returning.
data CallFrame sbe = CallFrame { cfFuncSym :: Symbol
                               , cfReturnBlock :: PathPC
                               , cfRegs :: RegMap (SBETerm sbe)
                               , cfRetReg :: Maybe (MemType, Ident)
                               }

-- | A unique control flow path during symbolic execution.
data Path sbe = Path
  { _pathFuncSym     :: !Symbol
    -- | The current PC location of path if any.
  , _pathPC         :: !PathPC
  , _pathName       :: !(Integer)
  , _pathRegs       :: !(RegMap (SBETerm sbe))
    -- | The current state of memory on path.
  , _pathMem        :: SBEMemory sbe
    -- | Assertions added since last branch.
  , _pathAssertions :: SBEPred sbe
    -- | Number of call frames in stack.
  , _pathStackHt     :: !Int
    -- | List of stack frames on path previously.
  , _pathStack       :: [CallFrame sbe]
  }

-- | A path with no active state and the given initial memory.
initialPath :: SBE sbe -> SBEMemory sbe -> Path sbe
initialPath sbe mem =
  Path { _pathFuncSym = entrySymbol
       , _pathPC = Nothing
       , _pathName = 0
       , _pathRegs = M.empty
       , _pathMem = mem
       , _pathAssertions = sbeTruePred sbe
       , _pathStackHt = 0
       , _pathStack = []
       }

pathFuncSym :: Simple Lens (Path sbe) Symbol
pathFuncSym = lens _pathFuncSym (\p r -> p { _pathFuncSym = r })

pathPC :: Simple Lens (Path sbe) PathPC
pathPC = lens _pathPC (\p r -> p { _pathPC = r })

-- | A Unique identifier for this path.
pathName :: Simple Lens (Path sbe) Integer
pathName = lens _pathName (\p r -> p { _pathName = r })

pathRegs :: Simple Lens (Path sbe) (RegMap (SBETerm sbe))
pathRegs = lens _pathRegs (\p r -> p { _pathRegs = r })

pathMem :: Simple Lens (Path sbe) (SBEMemory sbe)
pathMem = lens _pathMem (\p r -> p { _pathMem = r })

pathAssertions :: Simple Lens (Path sbe) (SBEPred sbe)
pathAssertions = lens _pathAssertions (\p r -> p { _pathAssertions = r })

pathStackHt :: Simple Lens (Path sbe) Int
pathStackHt = lens _pathStackHt (\p r -> p { _pathStackHt = r })

pathStack :: Simple Lens (Path sbe) [CallFrame sbe]
pathStack = lens _pathStack (\p r -> p { _pathStack = r })

newtype FailRsn       = FailRsn String deriving (Show)
data ErrorPath sbe = EP { epRsn :: FailRsn, epPath :: Path sbe }

instance Error FailRsn where
  noMsg  = FailRsn "(no reason given)"
  strMsg = FailRsn

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
ppCtrlStk sbe (Just (ActiveCS cs)) =
  text "Active path:" <$$>
  ppPath sbe (cs^.activePath)
  --TODO: Print more information

{-
ppMergeTarget :: MergeTarget -> Doc
ppMergeTarget (OnReturn n mr) = text "return" <> parens (int n <+> reg)
  where reg = maybe empty ppIdent mr
ppMergeTarget (OnPostdomJump n b) =
    text "postdom" <> parens (int n <+> ppSymBlockID b)

ppBranchAction :: SBE sbe -> BranchAction sbe -> Doc
ppBranchAction sbe (BARunFalse p) = 
  text "runFalse" <$$>
  indent 2 (ppPath sbe p)
ppBranchAction sbe (BAFalseComplete a p) =
  text "falseComplete" <$$>
  indent 2 (text "assumptions:" <+> prettyPredD sbe a) <$$>
  indent 2 (ppPath sbe p)

ppPathHandler :: SBE sbe -> PathHandler sbe -> Doc
ppPathHandler sbe (BranchHandler tgt c act h) = 
  text "on" <+> ppMergeTarget tgt <+> text "do" <$$>
  indent 2 (text "condition:" <+> prettyPredD sbe c) <$$>
  indent 2 (ppBranchAction sbe act) <$$>
  ppPathHandler sbe h
ppPathHandler _ StopHandler = text "stop"
-}

ppPathPC :: PathPC -> Doc
ppPathPC Nothing = text "none"
ppPathPC (Just (bid,i)) = ppSymBlockID bid <> colon <> int i

ppPath :: SBE sbe -> Path sbe -> Doc
ppPath sbe p =
  text "Path" <+> ppPathInfo p <> colon <$$>
  indent 2 (text "Locals:" <$$> indent 2 (ppRegMap sbe (p^.pathRegs)))
-- <+> (parens $ text "PC:" <+> ppPC sbe c)

-- | Prints just the path's name and info.
ppPathInfo :: Path sbe -> Doc
ppPathInfo p =
  char '#' <> integer (p^.pathName) <> colon 
  <> ppSymbol (p^.pathFuncSym) <> colon
  <> ppPathPC (p^.pathPC)

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

errorPath :: Monad m => String -> Simulator sbe m a
errorPath = throwError . FailRsn

wrongArguments :: Monad m => String -> Simulator sbe m a
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
  Just cs <- use ctrlStk
  let p = cs^.currentPath
  a <- liftSBE $ applyAnd sbe cond (p^.pathAssertions)
  case asBool sbe a of
    Just True  -> return ()
    Just False -> errorPath rsn
    _ -> do
      -- TODO: provide more detail here?
      whenVerbosity (>= 6) $ do
        tellUser $ show $ text "Warning at" <+> ppPathInfo p
        tellUser $ "  Could not verify memory access was valid."
        tellUser $ "  Results may only be partially correct."
      let p' = p & pathAssertions .~ a
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
    AError msg -> errorPath msg
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
    AError msg -> errorPath msg
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
          case asUnsignedInteger sbe 8 t of
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
  sbe <- gets symBE
  Just m <- preuse currentPathMem
  liftSBE $ codeLookupSymbol sbe m fp

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

memset :: (Functor sbe, Functor m, MonadIO m)
       => String -- ^ Name of function for error purposes. 
       -> SBETerm sbe -- ^ Destination
       -> SBETerm sbe -- ^ Value (must be an i8)
       -> BitWidth    -- ^ Width of length
       -> SBETerm sbe -- ^ Length
       -> Simulator sbe m ()
memset nm dst0 val lw len = do
  sbe <- gets symBE
  case asUnsignedInteger sbe lw len of
    Nothing -> errorPath $ show nm ++ ": does not support symbolic lengths."
    Just n0 -> do
      let n = fromIntegral n0
      a <- liftSBE $ termArray sbe i8 (V.replicate n val)
      store (ArrayType n i8) a dst0 0

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
  let e rsn = errorPath $ "Attempt to replace " ++ nm fnm
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
tryFindFunDecl :: MonadIO m
               => Symbol
                  -- | Returns override if function matches expection.
               -> (FunDecl -> Simulator sbe m ())
               -> Simulator sbe m ()
tryFindFunDecl nm act = do
  cb <- gets codebase
  -- Lookup function
  case cb^.cbFunctionType nm of
    Nothing -> return ()
    Just d -> act d


-- | Registers an override if a function with the given name has the
-- right type.
tryRegisterOverride :: MonadIO m
                    => Symbol
                       -- | Returns override if function matches expection.
                    -> (FunDecl -> Maybe (Override sbe m))
                    -> Simulator sbe m ()
tryRegisterOverride nm act = do
  tryFindFunDecl nm $ \d -> do
    case act d of -- Try getting override
      Just ovd -> registerOverride nm d ovd
      Nothing -> tellUser $ "Warning: " ++ show nm ++ " has an unexpected type."

type OverrideEntry sbe m = (Symbol, FunDecl, Override sbe m)

type StdOvdEntry sbe m =
  ( Functor sbe
  , Functor m
  , MonadIO m
  )
  => OverrideEntry sbe m


voidOverrideEntry :: Functor m
                  => Symbol
                  -> [MemType]
                  -> VoidOverrideHandler sbe m
                  -> OverrideEntry sbe m
voidOverrideEntry nm tps h = (nm, voidFunDecl tps, voidOverride h)

overrideEntry :: Functor m
              => Symbol
              -> MemType
              -> [MemType]
              -> ([(MemType, SBETerm sbe)] -> Simulator sbe m (SBETerm sbe))
              -> OverrideEntry sbe m
overrideEntry nm rtp tps h = (nm, funDecl rtp tps, override h)

varArgsOverrideEntry :: Functor m
              => Symbol
              -> MemType
              -> [MemType]
              -> ([(MemType, SBETerm sbe)] -> Simulator sbe m (SBETerm sbe))
              -> OverrideEntry sbe m
varArgsOverrideEntry nm rtp tps h = (nm, varArgsFunDecl rtp tps, override h)

registerOverrideEntry :: Monad m => OverrideEntry sbe m -> Simulator sbe m ()
registerOverrideEntry (sym, fd, handler) = registerOverride sym fd handler

registerOverrides :: Monad m => [OverrideEntry sbe m] -> Simulator sbe m ()
registerOverrides = mapM_ registerOverrideEntry