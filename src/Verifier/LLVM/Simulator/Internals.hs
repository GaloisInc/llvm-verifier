{- |
Module           : $Header$
Description      : Common type definitions and helper functions for LSS
License          : BSD3
Stability        : provisional
Point-of-contact : jhendrix
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Verifier.LLVM.Simulator.Internals
  ( Simulator(SM, runSM)
  , throwSM, catchSM
  , getVerbosity
  , setVerbosity
  , whenVerbosity
  , dbugM'
  , dumpCtrlStk

  , GlobalMap

  , LSSOpts(..)
  , defaultLSSOpts

    -- * State
  , State(..)
  , ctrlStk
  , globalTerms
  , blockPtrs
  , fnOverrides
  , errorPaths
  , pathCounter
  , aigOutputs
  , breakpoints
    -- ** Event handlers
  , ErrorHandler
  , onPathPosChange
  , onSimError
  , onUserInterrupt

  , Breakpoint
  , ppLocation

  , addBreakpoint
  , removeBreakpoint

    -- * Control stack.
  , CS
  , initialCtrlStk
  , currentPath
  , currentPaths
     -- ** Constrol stack combinators
  , csHasSinglePath
  , currentPathAssertions
  , currentPathStack
  , currentPathOfState
  , currentPathMem

    -- * Path information
  , Path
  , pathName
  , pathStack
  , pathMem
  , pathAssertions

    -- ** Path combinators.
  , pathCallFrames  
  , pathStackHt
  , pathIsActive

    -- ** Path pretty printing.
  , ppPathNameAndLoc
  , ppPath

  , RegMap
  , setReturnValue
  , ppLocals

    -- * PathStack
  , PathStack(..)
  , pathStackReturnValue
  , pathStackCallFrames
  , pushCallFrame

    -- * CallStack
  , CallStack
  , topCallFrame

    -- * CallFrame
  , CallFrame
  , newCallFrame
  , cfFunc
  , cfLocation
  , cfPC
  , cfRegs
  , cfBlock
  , cfStmt
  , cfArgValues
  , cfLocalValues

    -- * Operations on the simulator.
  , assumptionsForActivePath
  , addCtrlBranch
  , setCurrentBlock
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
  , simplifyAddr
  , load
  , loadString
  , resolveFunPtrTerm
  , store
  , memset
  , dumpMem

  , ppTuple
  ) where

import qualified Control.Arrow as A
import Control.Exception (assert)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict (StateT, runStateT, execStateT)
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Data.Traversable as Trav
import qualified Data.Map  as M
import Data.Maybe
import qualified Data.Set  as S
import qualified Data.Vector as V
import Prelude ()
import Prelude.Compat hiding (mapM, mapM_)

import System.Console.Haskeline.MonadException (MonadException(..), RunIO(..))

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Verifier.LLVM.Backend
import Verifier.LLVM.Codebase
import Verifier.LLVM.Simulator.CursorTree
import Verifier.LLVM.Simulator.SimUtils

flipBranch :: TreeContext b a
           -> b
           -> Orientation
           -> a
           -> CursorTree b a
           -> CursorTree b a
flipBranch h b LeftActive  p r = branch h b RightActive (Singleton p) r
flipBranch h b RightActive p r = branch h b LeftActive  r (Singleton p)

-- | List a monadic operation from a state @s@ to itself to
-- the state monad.
stateTransform :: Monad m => (s -> m s) -> StateT s m ()
stateTransform a = do
  s <- get
  put =<< lift (a s)

------------------------------------------------------------------------
-- RegMap

type RegMap term = M.Map Ident (term, MemType)

setFinalReturn :: Maybe MemType -> Maybe t -> Maybe (t, MemType)
setFinalReturn (Just tp) (Just rv) = return (rv,tp)
setFinalReturn Nothing   Nothing   = Nothing
setFinalReturn Nothing   (Just _)  =
  error $ "internal: Return value where non expected"
setFinalReturn (Just _)  Nothing   =
  error $ "internal: Missing return value."

setReturnValue :: Maybe (MemType, Ident) -> Maybe t
               -> RegMap t -> RegMap t
setReturnValue (Just (tp, r)) (Just rv) rm = M.insert r (rv, tp) rm
setReturnValue Nothing   Nothing   rm = rm
setReturnValue Nothing   (Just _) _  =
  error $ "internal: Return value where non expected"
setReturnValue (Just (_,tr)) Nothing   _  =
  error $ "internal: Missing return value for "  ++ show (ppIdent tr)

-- | Print local variables in as a list.  One variable is on each
-- line, and an equals statement separates the variable from its value.
ppLocals :: SBE sbe -> [(Ident, SBETerm sbe)] -> Doc
ppLocals sbe locals = vcat $ ppLocal <$> locals
  where ppLocal (nm, v) = ppIdent nm <+> char '=' <+> prettyTermD sbe v

------------------------------------------------------------------------
-- CallFrame

-- | A call frame on execution path.
data CallFrame sbe
   = CallFrame { -- | Function frame is for.
                 cfFunc     :: SymDefine (SBETerm sbe)
                 -- | Block currectly executing.
               , _cfBlock   :: SymBlockID
                 -- | Index of instruction within block.
               , _cfPC      :: Int
                 -- | Registers in frame.
               , _cfRegs    :: RegMap (SBETerm sbe)
               }

-- | Make new call frame for the given definition and args.
newCallFrame :: SymDefine (SBETerm sbe)
             -> [SBETerm sbe]
             -> CallFrame sbe
newCallFrame def args = assertLength $ do
   CallFrame { cfFunc    = def
             , _cfBlock  = sdEntry def
             , _cfPC     = 0
             , _cfRegs   = M.fromList $ zipWith bindArg formals args
             }
  where formals = sdArgs def
        bindArg (r,tp) v = (r, (v,tp))
        assertLength x =
          if length formals == length args
          then x
          else error $ "Error: length formals == "++show (length formals)++
                       " != length args == "++show (length args)++" in "++
                       show (sdName def)++".\n"++
                       "Perhaps you forgot some 'llvm_var' statements in your spec?"


-- | Location currently being executed.
cfLocation :: Simple Lens (CallFrame sbe) Breakpoint
cfLocation f cf = setFn <$> f (_cfBlock cf, _cfPC cf)
  where setFn (b,pc) = cf { _cfBlock = b, _cfPC = pc }

cfPC :: Simple Lens (CallFrame sbe) Int
cfPC = lens _cfPC (\s v -> s { _cfPC = v })

cfRegs :: Simple Lens (CallFrame sbe) (RegMap (SBETerm sbe))
cfRegs = lens _cfRegs (\s v -> s { _cfRegs = v })

-- | Return block currently being executed.
cfBlock :: CallFrame sbe -> SymBlock (SBETerm sbe)
cfBlock cf = lookupSymBlock (cfFunc cf) (_cfBlock cf)

-- | Return next statement for frame to execute.
cfStmt :: CallFrame sbe -> SymStmt (SBETerm sbe)
cfStmt cf = sbStmts (cfBlock cf) V.! (_cfPC cf)

-- | Return arguments to function with value
cfArgValues :: CallFrame sbe -> [(Ident, SBETerm sbe)]
cfArgValues cf = [ (sym, binding sym) | (sym,_) <- sdArgs (cfFunc cf) ]
  where binding sym = fst $ fromMaybe (error msg) $ M.lookup sym (cf^.cfRegs)
          where msg = "internal error: cfArgValues missing argument value."

-- | Return local variables in function regmap.
cfLocalValues :: CallFrame sbe -> [(Ident, SBETerm sbe)]
cfLocalValues cf = [ (sym, v) | (sym, (v,_)) <- M.toList (cf^.cfRegs)
                   , S.notMember sym argSet
                   ]
  where argSet = S.fromList $ fmap fst $ sdArgs (cfFunc cf)

------------------------------------------------------------------------
-- ErrorCollector

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

------------------------------------------------------------------------
-- LSSOpts

-- | Symbolic simulator options
data LSSOpts = LSSOpts {
    optsErrorPathDetails :: Bool
  , optsSatAtBranches    :: Bool
  -- ^ use a SAT-checking backend at branches, pruning unfeasable paths
  , optsSimplifyAddrs    :: Bool
  -- ^ simplify address expressions before loads and stores
  }

-- | Default simulator options
defaultLSSOpts :: LSSOpts
defaultLSSOpts = LSSOpts { optsErrorPathDetails = False
                         , optsSatAtBranches    = False
                         , optsSimplifyAddrs    = False
                         }

------------------------------------------------------------------------
-- CallStack

-- | Call stack contains top frame, total stack height, and return frames.
data CallStack sbe 
   = StopReturn (CallFrame sbe) (Maybe MemType)
   | CallReturn (CallFrame sbe) (Maybe (MemType, Ident)) Int (CallStack sbe)

-- | Make new call stack with a single frame.
mkCallStack :: CallFrame sbe -> Maybe MemType -> CallStack sbe
mkCallStack cf rtp = StopReturn cf rtp

-- | Return height of call stack.
callStackHt :: CallStack sbe -> Int
callStackHt (StopReturn _ _) = 1
callStackHt (CallReturn _ _ n _) = n

-- | Push a new call frame to the call stack.
consCallFrame :: CallFrame sbe
              -> (Maybe (MemType, Ident))
              -> CallStack sbe
              -> CallStack sbe
consCallFrame cf ret stk = CallReturn cf ret (callStackHt stk + 1) stk

-- | Return top stack in call frame or throw error is none exists.
topCallFrame :: Simple Lens (CallStack sbe) (CallFrame sbe)
topCallFrame f (StopReturn cf rtp) = setFn <$> f cf
  where setFn cf' = StopReturn cf' rtp
topCallFrame f (CallReturn cf r n rs) = setFn <$> f cf
  where setFn cf' = CallReturn cf' r n rs

-- | Traverse frames in call stack.
callStackFrames :: Simple Traversal (CallStack sbe) (CallFrame sbe)
callStackFrames f (StopReturn cf mr) = (\cf' -> StopReturn cf' mr) <$> f cf
callStackFrames f (CallReturn cf r n rs) =
  (\cf' rs' -> CallReturn cf' r n rs') <$> f cf <*> callStackFrames f rs

-- | Return register of stack if this is an inner call.
returnReg :: CallStack sbe -> Maybe Ident
returnReg (StopReturn _ _) = Nothing
returnReg (CallReturn _ mr _ _) = snd <$> mr

-- | Return registers in top call frame of stack.
stackBlock :: Simple Lens (CallStack sbe) Breakpoint
stackBlock = topCallFrame . cfLocation

-- | Return registers in top call frame of stack.
stackRegs :: Simple Lens (CallStack sbe) (RegMap (SBETerm sbe))
stackRegs = topCallFrame . cfRegs

------------------------------------------------------------------------
-- PathStack

data PathStack sbe 
   = CallStack (CallStack sbe)
   | FinStack (Maybe (SBETerm sbe, MemType))

-- | Returns return value from final path stack if any.
pathStackReturnValue :: Simple Traversal (PathStack sbe) (SBETerm sbe)
pathStackReturnValue f (FinStack mr) = FinStack <$> (_Just . onOne) f mr
  where onOne k (a, b) = k a <&> \a' -> (a',b)
pathStackReturnValue _ ps = pure ps

-- | Return height of path stack.
pathStackHt' :: PathStack sbe -> Int
pathStackHt' (CallStack stk) = callStackHt stk
pathStackHt' FinStack{} = 0

-- | Performs a traversal of stack frames, starting from currently active frame.
pathStackCallFrames :: Simple Traversal (PathStack sbe) (CallFrame sbe)
pathStackCallFrames f (CallStack stk) = CallStack <$> callStackFrames f stk
pathStackCallFrames _ (FinStack mr) = pure (FinStack mr)

-- | Push a call frame to the stack.
pushCallFrame :: CallFrame sbe -- ^ Call frame to push to.
              -> Maybe (MemType, Ident) -- ^ Where to write return value to (if any).
              -> PathStack sbe -- ^ Current path
              -> PathStack sbe
pushCallFrame cf retReg = CallStack . updateStack
  where updateStack (FinStack _) = mkCallStack cf (fst <$> retReg)
        updateStack (CallStack call_stk) =
          call_stk & topCallFrame.cfPC +~ 1  -- Step to next instruction.
                   & consCallFrame cf retReg -- Push call frame and return.

------------------------------------------------------------------------
-- Path

-- | A unique control flow path during symbolic execution.
data Path sbe = Path
  { _pathName       :: !(Integer)
    -- | List of stack frames on path previously.
  , _pathStack      :: PathStack sbe
    -- | The current state of memory on path.
  , _pathMem        :: SBEMemory sbe
    -- | Assertions added since last branch.
  , _pathAssertions :: SBEPred sbe
  }

-- | A path with no active state and the given initial memory.
initialPath :: SBE sbe -> SBEMemory sbe -> Path sbe
initialPath sbe mem =
  Path { _pathName = 0
       , _pathStack = FinStack Nothing
       , _pathMem = mem
       , _pathAssertions = sbeTruePred sbe
       }

-- | A unique identifier for this path.
pathName :: Simple Lens (Path sbe) Integer
pathName = lens _pathName (\p r -> p { _pathName = r })

-- | Control stack along path.
pathStack :: Simple Lens (Path sbe) (PathStack sbe)
pathStack = lens _pathStack (\p r -> p { _pathStack = r })

pathMem :: Simple Lens (Path sbe) (SBEMemory sbe)
pathMem = lens _pathMem (\p r -> p { _pathMem = r })

pathAssertions :: Simple Lens (Path sbe) (SBEPred sbe)
pathAssertions = lens _pathAssertions (\p r -> p { _pathAssertions = r })

pathCallFrames :: Simple Traversal (Path sbe) (CallFrame sbe)
pathCallFrames = pathStack . pathStackCallFrames

pathStackHt :: Path sbe -> Int
pathStackHt = pathStackHt' . view pathStack

-- | Return if path has an active call stack.
pathIsActive :: Path sbe -> Bool
pathIsActive p = pathStackHt p > 0

------------------------------------------------------------------------
-- MergeTarget

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

-- | Return true if path is at target.
atTarget :: Path sbe -> MergeTarget -> Bool
atTarget p (OnPostdomJump n b)
  | CallStack cs <- p^.pathStack = callStackHt cs == n
                                && cs^.stackBlock == (b,0)
  | otherwise = False
atTarget p (OnReturn n _) = pathStackHt p < n

------------------------------------------------------------------------
-- BranchInfo

-- | Condition, target, previous assertions
data BranchInfo sbe = BI { biCond :: SBEPred sbe
                         , biTarget :: MergeTarget 
                         , _biAssertions :: SBEPred sbe
                         , _biAbortsAbove :: Int 
                         }

-- | Assertions on path prior to branch.
biAssertions :: Simple Lens (BranchInfo sbe) (SBEPred sbe)
biAssertions = lens _biAssertions (\s v -> s { _biAssertions = v })

-- | This counts the number of times a parent above this branch has
-- been aborted due to a path being killed.  It is used to abort
-- memory branches.
biAbortsAbove :: Simple Lens (BranchInfo sbe) Int
biAbortsAbove = lens _biAbortsAbove (\s v -> s { _biAbortsAbove = v })

------------------------------------------------------------------------
-- ControlStack

-- | A control stack consists of a collection of execution paths
-- that will eventually be merged.
newtype CS sbe = CS { unCS :: CursorTree (BranchInfo sbe) (Path sbe) }

-- | Create a constrol stack with a single path with the given memory.
initialCtrlStk :: SBE sbe -> SBEMemory sbe -> CS sbe
initialCtrlStk sbe mem = CS $ Singleton $ initialPath sbe mem

currentTree :: Simple Iso
                      (CS sbe)
                      (CursorTree (BranchInfo sbe) (Path sbe))
currentTree = iso unCS CS

-- | Returns current path
currentPath :: Simple Lens (CS sbe) (Path sbe)
currentPath = currentTree . activeValue

-- | Traverse paths in control stack.
currentPaths :: Simple Traversal (CS sbe) (Path sbe)
currentPaths = currentTree . inorderTraversal

currentPair :: Simple Iso
                      (CS sbe)
                      (TreeContext (BranchInfo sbe) (Path sbe), Path sbe)
currentPair = currentTree . treePair

-- | Returns stack of current path.
currentPathStack :: Simple Lens (CS sbe) (PathStack sbe)
currentPathStack = currentPath . pathStack

-- | Return true if there is only one path in control stack.
csHasSinglePath :: CS sbe -> Bool
csHasSinglePath (CS (Singleton _)) = True
csHasSinglePath _ = False

------------------------------------------------------------------------
-- State and Simulator definition.

type LiftSBE sbe m = forall a. sbe a -> m a

-- | Map from function names to address of their entry point.
type GlobalMap sbe = M.Map Symbol (SBETerm sbe)

-- | Map from individual blocks within function to address of block.
type BlockMap sbe = M.Map (Symbol, BlockLabel) (SBETerm sbe)

type OvrMap sbe m  = M.Map Symbol (Override sbe m, Bool {- user override? -})

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

newtype FailRsn = FailRsn String deriving (Show)

instance (MonadException m) => MonadException (ExceptT e m) where
    controlIO f = ExceptT $ controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap ExceptT . run . runExceptT)
                    in fmap runExceptT $ f run'

-- | Action to perform when a simulation error is encountered.
-- Parameters include simulator control stack prior to error, and
-- a description of the error.
type ErrorHandler sbe m
   = CS sbe -> FailRsn -> Simulator sbe m ()

-- | Types of breakpoints (kind of boring for now, but maybe with a
-- DWARF parser we can do more...)
type Breakpoint = (SymBlockID,Int)

ppLocation :: Breakpoint -> Doc
ppLocation (b, pc) = ppSymBlockID b <> colon <> int pc

data ErrorPath sbe = EP { epRsn :: FailRsn, epPath :: Path sbe }

newtype Simulator sbe m a =
  SM { runSM :: ExceptT FailRsn (StateT (State sbe m) m) a }
  deriving
    ( Functor
    , Monad
    , Applicative
    , MonadIO
    , MonadException
    )

throwSM :: Monad m => FailRsn -> Simulator sbe m a
throwSM = SM . throwE

catchSM :: Monad m
        => Simulator sbe m a
        -> (FailRsn -> Simulator sbe m a)
        -> Simulator sbe m a
catchSM (SM m) h = SM (catchE m (runSM . h))

instance Monad m => MonadState (State sbe m) (Simulator sbe m) where
  get = SM (lift Strict.get)
  put = SM . lift . Strict.put

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

-- | Traversal for current path of simulator state.
currentPathOfState :: Simple Traversal (State sbe m) (Path sbe)
currentPathOfState = ctrlStk . _Just . currentPath

-- | Traversal for current path memory if any.
currentPathMem :: Simple Traversal (State sbe m) (SBEMemory sbe)
currentPathMem = currentPathOfState . pathMem

-- | Traversal for current path assertions if any.
currentPathAssertions :: Simple Traversal (State sbe m) (SBEPred sbe)
currentPathAssertions = currentPathOfState . pathAssertions

------------------------------------------------------------------------
-- Simulator primitives

-- | Return merged assertions in path.
mergeAssertions :: Monad m
                => SBEPred sbe -- ^ Old assertions
                -> SBEPred sbe -- ^ Branch condition
                -> Path sbe -- ^ True path
                -> Path sbe -- ^ False path
                -> Simulator sbe m (SBEPred sbe)
mergeAssertions a c pt pf = do
  sbe <- gets symBE
  a' <- liftSBE $
    applyPredIte sbe c (pt^.pathAssertions) (pf^.pathAssertions)
  liftSBE $ applyAnd sbe a a'

mergePaths :: MonadIO m
           => BranchInfo sbe
           -> Path sbe -- ^ True path
           -> Path sbe -- ^ False path
           -> Simulator sbe m (Path sbe)
mergePaths b pt pf = do
  sbe <- gets symBE
  let c = biCond b
  let a = b^.biAssertions

  -- Merged typed value, the order depends on whether the active
  -- branch is the true branch or the false branch.
  let mergeTyped (tt,tp) (ft,_) =
        fmap (,tp) $ collectError tt
                   $ sbeRunIO sbe
                   $ applyIte sbe tp c tt ft
  -- Merge callStack
  (_errs, mergedStack) <- liftIO $ runErrorCollector $ do
    case biTarget b of
      OnReturn _ mr -> do
        case (pt^.pathStack, pf^.pathStack) of
          -- Main void return: no work to do
          (FinStack Nothing, FinStack Nothing) -> do
            return (pt^.pathStack)
          -- Main nonvoid return: merge the final return values
          (FinStack (Just v1), FinStack (Just v2)) -> do
            FinStack . Just <$> mergeTyped v1 v2
          -- Nonmain return
          (CallStack cs1, CallStack cs2) ->
            case mr of
              -- Nonvoid return: merge the return values in the caller's register bank
              Just reg ->
                assert (isJust (cs2^.stackRegs^.at reg)) $ do
                  let Just v1 = cs1^.stackRegs^.at reg
                  CallStack <$> (stackRegs . at reg . _Just) (mergeTyped v1) cs2
              -- Void return: no work to do
              Nothing ->
                return (pt^.pathStack)
          (_,_) -> error "internal: Unexpected form during merging" 
      OnPostdomJump{} -> -- Merge all registers
        case (pt^.pathStack, pf^.pathStack) of
          -- Update specific reg
          (CallStack cs1, CallStack cs2) -> do
            let r1 = cs1^.topCallFrame^.cfRegs
                mergeFn = Trav.sequence . M.intersectionWith mergeTyped r1
            CallStack <$> stackRegs mergeFn cs2
          (_,_) -> error "internal: Unexpected form during merging" 

  -- Merge memory
  mergedMem <- liftSBE $ memMerge sbe c (pt^.pathMem) (pf^.pathMem)
  finalMem <- abortMemBranches (b^.biAbortsAbove) mergedMem
  -- Merge assertions
  a' <- mergeAssertions a c pt pf
  -- Return new path
  return Path { _pathName       = min (_pathName pt) (_pathName pf)
              , _pathStack      = mergedStack
              , _pathMem        = finalMem
              , _pathAssertions = a'
              }

-- | Add a control branch
addCtrlBranch :: Monad m
              => SBEPred sbe -- ^ Condition to branch on.
                 -- | Location for true branch to start at.
              -> SymBlockID
              -> Integer -- ^ Name of false path
              -> MergeLocation -- ^ Control point to merge at.
              -> Simulator sbe m ()
addCtrlBranch c nb nm ml = do
  Just cs <- use ctrlStk
  let (h,p) = cs^.currentPair
  sbe <- gets symBE
  mem <- liftSBE $ memBranch sbe $ p^.pathMem
  let tgt = case ml of
              Just b -> OnPostdomJump (pathStackHt p) b
              Nothing -> OnReturn (pathStackHt p) (returnReg stk)
                where CallStack stk = p^.pathStack
  let info = BI { biCond = c
                , biTarget = tgt
                , _biAssertions = p^.pathAssertions
                , _biAbortsAbove = 0
                }
  let CallStack stk = p ^.pathStack
  let pt = p & pathStack .~ CallStack (stk & topCallFrame . cfLocation .~ (nb,0))
             & pathMem .~ mem
             & pathAssertions .~ sbeTruePred sbe
  let pf = p & pathName .~ nm
             & pathMem .~ mem
             & pathAssertions .~ sbeTruePred sbe
  ctrlStk ?= CS (Branch h info RightActive pf (Singleton pt))
 
-- | Merge path and path handler.  Note that the new state may
-- have infeasible path assertions.
checkForMerge :: MonadIO m => CS sbe -> Simulator sbe m ()
checkForMerge (CS cs) = do
  case cs of
    Branch h b o p r | atTarget p (biTarget b) -> do
      case r of
        -- Merge if adjacent tree is a single path at the branch
        -- merge point.
        Singleton po | atTarget po (biTarget b) -> do
          p' <- case o of
                  LeftActive  -> mergePaths b p po
                  RightActive -> mergePaths b po p
          -- Recurse to check if more merges should be performed.
          checkForMerge $ CS $ (h,p') ^.from treePair
        _ -> ctrlStk ?= CS (flipBranch h b o p r)
    _ -> ctrlStk ?= CS cs

-- | Move current path to target block, and check for potential merge.
-- Note that the new path may have infeasible assertions.
setCurrentBlock :: MonadIO m => SymBlockID -> Simulator sbe m ()
setCurrentBlock b = do
  Just cs <- use ctrlStk
  let CallStack stk = cs^.currentPathStack
  checkForMerge $ cs & currentPathStack .~ CallStack (stk & stackBlock .~ (b,0))

-- | Return from current path.
-- The current path may be infeasible.
returnCurrentPath :: (Functor m, MonadIO m)
                  => Maybe (SBETerm sbe)
                  -> Simulator sbe m ()
returnCurrentPath rt = do
  Just cs <- use ctrlStk
  let p0 = cs^.currentPath
  case p0^.pathStack of
    FinStack _ -> error "internal: returnCurrentPath given inactive path."
    CallStack stk -> do
      sbe <- gets symBE
      p <- pathMem (\m -> liftSBE $ stackPopFrame sbe m) p0
      let stk' = case stk of
                   StopReturn _ mtp ->
                     FinStack $ setFinalReturn mtp rt
                   CallReturn _ retReg n rstk -> assert (n > 1) $
                     -- Set return value in previous frame.
                     CallStack $ rstk & stackRegs %~ setReturnValue retReg rt
      checkForMerge $ cs & currentPath .~ (p & pathStack .~ stk')

-- | Negate predicate when condition is true.
applyNotWhen :: Monad m => Bool -> SBEPred sbe -> Simulator sbe m (SBEPred sbe)
applyNotWhen True  p = withSBE $ \sbe -> applyBNot sbe p
applyNotWhen False p = return p

-- | Return assumptions along active path.
assumptionsForActivePath :: Monad m => Simulator sbe m (SBEPred sbe)
assumptionsForActivePath = do
  Just (CS t) <- use ctrlStk
  let branchCond (b,o,_) = applyNotWhen (o == RightActive) (biCond b)
  conds <- mapM branchCond (treeParents t)
  sbe <- gets symBE
  let andFn x y = liftSBE (applyAnd sbe x y)
  foldM andFn (sbeTruePred sbe) conds

-- | Abort memory branches
abortMemBranches :: Monad m => Int -> SBEMemory sbe -> Simulator sbe m (SBEMemory sbe)
abortMemBranches n m0 = do
  sbe <- gets symBE
  let abortOnce    = liftSBE . memBranchAbort sbe
      runAllAborts = replicateM_ n (stateTransform abortOnce)
  execStateT runAllAborts m0

-- | Kill the current path and add it to the list of errorPaths.
-- This function assumes the simulator has an active path.
killCurrentPath :: (Functor m, MonadIO m) => FailRsn -> Simulator sbe m ()
killCurrentPath rsn = do
  Just cs0 <- use ctrlStk
  -- Add path to list of error paths.
  errorPaths %= (EP rsn (cs0^.currentPath):)
  -- Merge current path as error, and update control stack.
  case cs0 of
    CS Singleton{} -> ctrlStk .= Nothing
    CS (Branch ctx b o _ rest) -> do
      -- Negate condition if the true branch (which is on the left)
      -- has failed.
      cond <- applyNotWhen (o == LeftActive) (biCond b)
      -- Get assertions to add to rest.
      sbe <- gets symBE
      newAssertions <- liftSBE $ applyAnd sbe (b^.biAssertions) cond
      -- Replace parent with child.
      replaceParentWithChild ctx newAssertions (b^.biAbortsAbove) rest

-- | Replace a branch with one child killed with the other sibling.
replaceParentWithChild :: (Functor m, MonadIO m) =>
                          TreeContext (BranchInfo sbe) (Path sbe)
                          -- ^ Context for new execution tree.
                       -> SBEPred sbe
                          -- ^ Assertions to add to new
                       -> Int
                          -- ^ Number of branches killed in parent branch.
                       -> CursorTree (BranchInfo sbe) (Path sbe)
                          -- ^ New execution tree to make active.
                       -> Simulator sbe m ()
replaceParentWithChild ctx newAssertions parentAborts rest = do
  sbe <- gets symBE
  case topView rest of
    
    -- If tree is a single path, then we make a leaf active.
    Left p -> do
      -- Set assertions
      let p' = p & pathAssertions .~ newAssertions
      -- Cleanup memory in preparate for deleting branch b.
      let cnt = parentAborts+1
      p2 <- pathMem (abortMemBranches cnt) p'
      -- Replace the current branch with p2.
      checkForMerge $ CS $ instContext' ctx p2

    -- If this is a branch @b[ l >< r ]@ with o the active subtree.
    Right (b, o, l, r) -> do
      a <- liftSBE $ applyAnd sbe newAssertions (b^.biAssertions)
      let b' = b & biAssertions .~ a
                 & biAbortsAbove +~ 1
      checkForMerge $ CS $ branch ctx b' o l r

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

getVerbosity :: Monad m => Simulator sbe m Int
getVerbosity = gets verbosity

setVerbosity :: Monad m => Int -> Simulator sbe m ()
setVerbosity v = modify $ \s -> s{ verbosity = v }

whenVerbosity :: Monad m => (Int -> Bool) -> Simulator sbe m () -> Simulator sbe m ()
whenVerbosity f m = getVerbosity >>= \v -> when (f v) m

dbugM' :: (Monad m, MonadIO m) => Int -> String -> Simulator sbe m ()
dbugM' lvl = whenVerbosity (>=lvl) . dbugM

--instance (Monad m, Functor m) => Applicative (Simulator sbe m) where
--  pure      = return
--  af <*> aa = af >>= \f -> f <$> aa

-----------------------------------------------------------------------------------------
-- Pretty printing

ppFailRsn :: FailRsn -> Doc
ppFailRsn (FailRsn msg) = text msg

ppCtrlStk :: SBE sbe -> Maybe (CS sbe) -> Doc
ppCtrlStk _ Nothing = text "All paths failed"
ppCtrlStk sbe (Just cs) =
  ppPath sbe (cs^.currentPath)
  --TODO: Print more information

ppPath :: SBE sbe -> Path sbe -> Doc
ppPath _sbe p =
  text "Path" <+> ppPathNameAndLoc p
  --TODO: Fix this.
-- <> colon <$$>
--  indent 2 (text "Locals:" <$$> indent 2 (ppRegMap sbe (p^.pathRegs)))
-- <+> (parens $ text "PC:" <+> ppPC sbe c)

-- | Prints just the path's name and location.
ppPathNameAndLoc :: Path sbe -> Doc
ppPathNameAndLoc p = char '#' <> integer (p^.pathName) <> colon <> loc
  where loc = case p^.pathStack of
                FinStack _ -> colon <> text "completed"
                CallStack (view topCallFrame -> cf) ->
                  ppSymbol (sdName (cfFunc cf))
                  <> colon <> ppLocation (cf^.cfLocation)

ppTuple :: [Doc] -> Doc
ppTuple = parens . hcat . punctuate comma

--------------------------------------------------------------------------------
-- SBE lifters and helpers

liftSBE :: Monad m => sbe a -> Simulator sbe m a
liftSBE sa = SM $ lift $ do
  liftFn <- gets liftSymBE
  lift $ liftFn sa 

withSBE :: (Monad m) => (SBE sbe -> sbe a) -> Simulator sbe m a
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
unlessQuiet m = getVerbosity >>= \v -> unless (v == 0) m

-- For user feedback that gets silenced when verbosity = 0.
tellUser :: (MonadIO m) => String -> Simulator sbe m ()
tellUser msg = unlessQuiet $ dbugM msg

--------------------------------------------------------------------------------
-- Error handling

errorPath :: Monad m => String -> Simulator sbe m a
errorPath = SM . throwE . FailRsn

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
        tellUser $ show $ text "Warning at" <+> ppPathNameAndLoc p
        tellUser $ "  Could not verify memory access was valid."
        tellUser $ "  Results may only be partially correct."
      let p' = p & pathAssertions .~ a
      ctrlStk ?= set currentPath p' cs

--------------------------------------------------------------------------------
-- Memory handling

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
  sbe <- gets symBE
  Just m <- preuse currentPathMem
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

simplifyAddr :: ( Functor m , MonadIO m , Functor sbe ) =>
                SBETerm sbe -> Simulator sbe m (SBETerm sbe)
simplifyAddr addr = do
  simplifyEnabled <- gets (optsSimplifyAddrs . lssOpts)
  if simplifyEnabled
    then do
      sbe <- gets symBE
      dbugM' 3 $ show $ "Simplifying address:" <+> prettyTermD sbe addr
      assmps <- assumptionsForActivePath
      Just assns <- preuse currentPathAssertions
      dbugM' 3 $ show $ "Current assumptions:" <+> prettyPredD sbe assmps
      dbugM' 3 $ show $ "Current assertions:" <+> prettyPredD sbe assns
      context <- liftSBE $ applyAnd sbe assns assmps
      addr0 <- liftSBE $ simplifyConds sbe context addr
      addr1 <- liftSBE $ simplifyConds sbe context addr0
      addr2 <- liftSBE $ simplifyConds sbe context addr1
      dbugM' 3 $ show $ "Done simplifying address:" <+> prettyTermD sbe addr2
      return addr2
    else return addr

-- | Load value at addr in current path.
load ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => MemType -> SBETerm sbe -> Alignment -> Simulator sbe m (SBETerm sbe)
load tp addr a = do
  sbe <- gets symBE
  addr' <- simplifyAddr addr
  Just mem <- preuse currentPathMem
  (cond, v) <- liftSBE $ memLoad sbe mem tp addr' a
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
                 "Encountered a symbolic byte in " ++ nm
                 ++ " in term:\n" ++ show (prettyTermD sbe t)
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
  dst' <- simplifyAddr dst
  Just m <- preuse currentPathMem
  (c, m') <- liftSBE $ memStore sbe m dst' tp val a
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

dumpMem ::
  (Functor m, MonadIO m) =>
  Int -> String -> Maybe [(Integer, Integer)] -> Simulator sbe m ()
dumpMem v msg mranges =
  whenVerbosity (>=v) $ do
    dbugM $ msg ++ ":"
    Just m <- preuse currentPathMem
    withSBE (\s -> memDump s m mranges)

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
               -> (FunDecl -> Simulator sbe m ())
               -> Simulator sbe m ()
tryFindFunDecl nm f = do
  cb <- gets codebase
  -- Lookup function
  case cb^.cbFunctionType nm of
    Nothing -> return ()
    Just d -> f d

-- | Registers an override if a function with the given name has the
-- right type.
tryRegisterOverride :: MonadIO m
                    => Symbol
                    -> (FunDecl -> Maybe (Override sbe m)) -- ^ Checks for override.
                    -> Simulator sbe m ()
tryRegisterOverride nm f = do
  tryFindFunDecl nm $ \d -> do
    case f d of -- Try getting override
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
