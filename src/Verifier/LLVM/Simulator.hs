{- |
Module           : $Header$
Description      : The symbolic simulator for LLVM-Symbolic programs
Stability        : provisional
Point-of-contact : jstanley
-}

-- Debugging output at various verbosity levels:
-- 1: No output, the lowest verbosity level
-- 2: Instruction trace only
-- 3: Show path state details when all paths yield errors.  Memory model
--    information for error paths.  Displays error paths as they are
--    encountered rather than at the end of execution.
-- 4: Path constraints on nontrivial path merges.
-- 5: Simulator internal state (control stack dump per instruction); show
--    memory model details in addition to path state details when all paths
--    yield errors.
-- 6: Memory model dump on load/store operations only (for nontrivial codes,
--    this generates a /lot/ of output).  Complete path dumps on nontrivial path
--    merges.  Warnings on symbolic validity results from memory model
-- 7: Memory model dump pre/post every operation (for nontrivial codes, this
--    generates a /lot/ of output -- now with more output than level 6!)

{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}


module Verifier.LLVM.Simulator
  ( module Verifier.LLVM.AST
  , module Verifier.LLVM.Codebase
  , Simulator (SM)
  , State(..)
  , SEH(..)
  , callDefine
  , defaultSEH
  , getProgramReturnValue
  , getProgramFinalMem
  , evalExpr
  , evalExprInCC
  , Evaluator
  , runEvaluator
  , runSimulator
  , liftSBE
  , withSBE
  , withSBE'
  , errorPath
  -- * Memory operations
  , alloca
  , load
  , store
  , processMemCond
  -- for testing
  , dbugM
  , dbugTerm
  , dumpMem
  , getMem
  , withDL
  , warning
  , ErrorPath
  , errorPaths
  , LSSOpts(..)
  ) where

import           Control.Applicative
import Control.Exception (AsyncException(..))
import           Control.Lens hiding (act,from)
import           Control.Monad.Error
import           Control.Monad.State.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.State.Strict (evalStateT)
import           Data.List                 (isPrefixOf, nub)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import qualified Data.Vector as V
import System.Console.Haskeline.MonadException (MonadException, handle, throwIO)
import           System.Exit
import           System.IO
import Text.PrettyPrint.Leijen hiding ((<$>), align, line)

import Verifier.LLVM.AST
import Verifier.LLVM.Backend
import Verifier.LLVM.Codebase
import Verifier.LLVM.Simulator.Debugging
import Verifier.LLVM.Simulator.Internals
import Verifier.LLVM.Simulator.SimUtils

import Verifier.LLVM.LLVMIntrinsics
import Verifier.LLVM.LibcOverrides
import Verifier.LLVM.LSSOverrides

-- Utility functions

withActiveCS :: (MonadIO m)
             => (ActiveCS sbe -> IO (CS sbe))
             -> Simulator sbe m ()
withActiveCS f = do
  Just (ActiveCS cs) <- use ctrlStk
  cs' <- liftIO (f cs)
  ctrlStk ?= cs'

-- | Obtain the first pending path in the topmost merge frame; @Nothing@ means
-- that the control stack is empty or the top entry of the control stack has no
-- pending paths recorded.
getPath :: (Functor m, Monad m)
        => Simulator sbe m (Maybe (Path sbe))
getPath = preuse currentPathOfState

modifyPathRegs :: (Functor m, Monad m)
               => (RegMap (SBETerm sbe) -> RegMap (SBETerm sbe))
               -> Simulator sbe m ()
modifyPathRegs f = do
  Just p <- preuse currentPathOfState
  currentPathOfState .= over pathRegs f p

-- @getMem@ yields the memory model of the current path if any.
getMem :: (Functor m, Monad m) =>  Simulator sbe m (Maybe (SBEMemory sbe))
getMem = preuse currentPathMem

-- | Run simulator in given context.
runSimulator :: forall sbe a .
  ( Functor sbe
  , Ord (SBETerm sbe)
  )
  => Codebase sbe          -- ^ Post-transform LLVM code, memory alignment, and
                           -- type aliasing info
  -> SBE sbe               -- ^ A symbolic backend
  -> SBEMemory sbe         -- ^ The SBE's LLVM memory model
  -> SEH sbe IO            -- ^ Simulation event handlers (use defaultSEH if no
                           -- event handling is needed)
  -> Maybe LSSOpts         -- ^ Simulation options
  -> Simulator sbe IO a     -- ^ Simulator action to perform
  -> IO a
runSimulator cb sbe mem seh mopts m = do
  let lifter :: forall v . sbe v -> Simulator sbe IO v
      lifter = SM . lift . lift . sbeRunIO sbe
  let newSt = State { codebase     = cb
                    , symBE        = sbe
                    , liftSymBE    = lifter
                    , verbosity    = 6
                    , lssOpts      = fromMaybe defaultLSSOpts mopts
                    , _ctrlStk     = Just $ initialCtrlStk sbe mem
                    , _globalTerms = M.empty
                    , _blockPtrs = M.empty
                    , _fnOverrides = M.empty
                    , _evHandlers  = seh
                    , _errorPaths  = []
                    , _pathCounter = 1
                    , _aigOutputs  = []
                    , _breakpoints = M.empty
                    , _trBreakpoints = NoTransientBreakpoint
                    , _errorPolicy = EnterREPL
                    }
  ea <- flip evalStateT newSt $ runErrorT $ runSM $ do
    initGlobals
    registerLLVMIntrinsicOverrides
    registerLibcOverrides
    registerLSSOverrides
    m
  -- TODO: call exception handlers given by to-be-written SEH fields
  case ea of
    Left (FailRsn rsn) ->
      fail $ "internal: uncaught exception: " ++ rsn
    Right x -> return x

-- | Initialize all global data and register all defines.
initGlobals ::
  ( MonadIO m
  , Functor sbe
  , Functor m
  )
  => Simulator sbe m ()
initGlobals = do
  nms <- use (to codebase . cbGlobalNameMap)
  -- Register all function definitions.
  do let defines = [ d | (_,Right d) <- M.toList nms]
     forM_ defines $ \d -> do
       let idl       = nub $ mapMaybe symBlockLabel $ M.keys (sdBody d)
       insertGlobalFn (sdName d) idl
  -- Add symbol for declarations.
  do declares <- gets (cbUndefinedFns . codebase)
     forM_ declares $ \(sym,_) -> do
       insertGlobalFn sym []
  -- Initialize global data
  do let globals = [ g | (_,Left g) <- M.toList nms]
     forM_ globals $ \g -> do
       -- Run onMkGlobTerm event handler
       join $ use (evHandlers . to onMkGlobTerm) ?? g
       cdata <- evalExprInCC "addGlobal" (globalValue g)
       -- Run onPreGlobInit event handler.
       join $ use (evHandlers . to onPreGlobInit) ?? g ?? cdata
       let noDataSpc = "Not enough space in data segment to allocate new global."
       insertGlobalTerm noDataSpc (globalSym g) (MemType (globalType g)) $
          \s m -> memInitGlobal s m (globalType g) cdata
       -- Run onPostGlobInit event handler.
       join $ use (evHandlers . to onPostGlobInit) ?? g ?? cdata

-- | External entry point for a function call.  This will push a callFrame
-- for the function to the stack, and run the function until termination.
callDefine ::
  ( MonadIO m
  , Functor m
  , MonadException m
  , Functor sbe
  )
  => Symbol     -- ^ Callee symbol
  -> RetType       -- ^ Callee return type
  -> [(MemType,SBETerm sbe)] -- ^ Callee argument generator
  -> Simulator sbe m ()
callDefine calleeSym t args = do
  def <- lookupSymbolDef calleeSym
  let retReg = (,entryRsltReg) <$> sdRetType def
  lc <- gets (cbLLVMContext . codebase)
  let ?lc = lc
  unless (compatRetTypes t (sdRetType def)) $
    dbugM $ show $
      text "Warning: callDefine given incorrect return type of"
              <+> ppRetType t
              <+>  text "for" <+> ppSymbol calleeSym <+> text "when actual type is"
              <+> ppRetType (sdRetType def) <> text "."
  callDefine' False entryRetNormalID calleeSym retReg args
  cont <- atBreakpoint
  when cont debuggerREPL
  run

setCurrentBlock :: MonadIO m => SymBlockID -> Simulator sbe m ()
setCurrentBlock b = do
  sbe <- gets symBE
  withActiveCS $ \cs -> ActiveCS <$> jumpCurrentPath sbe b cs

callDefine' ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => Bool                    -- ^ Is this a redirected call?
  -> SymBlockID              -- ^ Normal call return block id
  -> Symbol                  -- ^ Callee symbol
  -> Maybe (MemType, Ident)  -- ^ Callee return type and result register
  -> [(MemType,SBETerm sbe)]           -- ^ Callee arguments
  -> Simulator sbe m ()
callDefine' isRedirected normalRetID calleeSym@(Symbol calleeName) mreg args = do
  -- NB: Check overrides before anything else so we catch overriden intrinsics
  symOver <- use (fnOverrides . at calleeSym)
  case symOver of
    Nothing -> normal
    Just (Redirect calleeSym', _)
      -- NB: We break transitive redirection to avoid cycles
      | isRedirected -> normal
      | otherwise    -> callDefine' True normalRetID calleeSym' mreg args
    Just (Override f, _) -> do
      r <- f calleeSym mreg args
      modifyPathRegs $ setReturnValue "callDefine'" mreg r
      setCurrentBlock normalRetID
  where
    normal
      | isPrefixOf "llvm." calleeName = do
          whenVerbosity (>= 1) $ do
            --TODO: Give option of stopping on warnings like this.
            tellUser $ "Warning: skipping unsupported LLVM intrinsic "
                         ++ show calleeName
          setCurrentBlock normalRetID
      | otherwise = do
          runNormalSymbol normalRetID calleeSym mreg (snd <$> args)

-- | Return symbol definition with given name or fail.
lookupSymbolDef :: (Functor m, MonadIO m, Functor sbe)
                => Symbol
                -> Simulator sbe m (SymDefine (SBETerm sbe))
lookupSymbolDef sym = do
  cb <- gets codebase
  case lookupDefine sym cb of
    Just def -> return def
    Nothing  -> do
      errorPath $ "Could not find definition for symbol " ++ show (ppSymbol sym) ++ "."

runNormalSymbol ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => SymBlockID            -- ^ Normal call return block id
  -> Symbol              -- ^ Callee symbol
  -> Maybe (MemType, Ident)     -- ^ Callee return type and result register
  -> [SBETerm sbe] -- ^ Callee arguments
  -> Simulator sbe m ()
runNormalSymbol normalRetID calleeSym mreg args = do
  def <- lookupSymbolDef calleeSym
  sbe <- gets symBE
  dbugM' 5 $ "callDefine': callee " ++ show (ppSymbol calleeSym)
  Just cs <- use ctrlStk
  let m = cs^.currentPath^.pathMem
  (c,m') <- withSBE $ \s -> stackPushFrame s m
  let cs' = cs & pushCallFrame sbe calleeSym normalRetID mreg
               & activePath . pathRegs .~ bindArgs (sdArgs def) args
               & activePath . pathMem  .~ m'
  ctrlStk ?= ActiveCS cs'
  -- Push stack frame in current process memory.
  let fr = "Stack push frame failure: insufficient stack space"
  processMemCond fr c
 where
    err doc = error $ "callDefine/bindArgs: " ++ show doc

    bindArgs formals actuals
      | length formals /= length actuals =
          err $ text "incorrect number of actual parameters"
            <$$> text "formals: " <> text (show (fst <$> formals))
      | otherwise =
          foldr (uncurry bindArg) M.empty (formals `zip` actuals)
    bindArg (r,tp) v = M.insert r (v,tp)

finalRetValOfPath :: Simple Traversal (Path sbe) (SBETerm sbe)
finalRetValOfPath = pathRegs . at entryRsltReg . _Just . _1

getProgramReturnValue :: (Monad m, Functor m)
                      => Simulator sbe m (Maybe (SBETerm sbe))
getProgramReturnValue = preuse $ currentPathOfState . finalRetValOfPath

getProgramFinalMem :: (Monad m, Functor m)
                   => Simulator sbe m (Maybe (SBEMemory sbe))
getProgramFinalMem = preuse currentPathMem

-- | Return true if simulator has a current path at a breakpoint.
atBreakpoint :: Monad m => Simulator sbe m Bool
atBreakpoint = do
  mp <- preuse currentPathOfState
  case mp of
    Nothing -> return False
    Just p -> do
      tbps <- use trBreakpoints
      case tbps of
        BreakNextStmt -> return True
        BreakReturnFrom ht | pathStackHt p < ht -> return True
        _ -> do
          mbps <- use (breakpoints . at (pathFuncSym p))
          return $ fromMaybe False $ S.member <$> (p^.pathPC) <*> mbps

-- | Resets simulator state after a failure occurs.
onSimulationError :: (Functor sbe, Functor m, MonadException m)
                  => State sbe m -- ^ State to reset to.
                  -> ActiveCS sbe -- ^ Constrol stack to use for resetting. 
                  -> FailRsn
                  -> Simulator sbe m ()
onSimulationError s cs rsn = do
  put s -- Reset state
  -- Get current path before last step.
  let p = cs^.activePath
  -- Handle error based on policy.
  ep <- use errorPolicy
  case ep of
    EnterREPL -> do
      dbugM $ show $
        text "Simulation error:" <+> ppFailRsn rsn <$$>
        indent 2 (text "at" <+> ppPathInfo p)
      debuggerREPL
      -- Resume execution
      run
    KillPath -> do
      sbe <- gets symBE
      -- Log error path  
      whenVerbosity (>=1) $ do
        dbugM $ show $ text "Simulation error:" <+> ppFailRsn rsn
      -- Just print location at verbosity 1 and 2.
      whenVerbosity (`elem` [1,2]) $ do
        dbugM $ show $ indent 2 (text "at" <+> ppPathInfo p)
      -- Print full path at verbosity 3+.
      whenVerbosity (>=3) $ do
        dbugM $ show $ ppPath sbe p
      -- Merge current path as error, and update control stack.
      mcs' <- liftIO $ markCurrentPathAsError sbe cs
      ctrlStk .= mcs'
      -- Add path to list of error paths.
      errorPaths %= (EP rsn p:)
      -- Check to see if we have hit a breakpoint from marking path as
      -- error.
      cont <- atBreakpoint
      when cont debuggerREPL
      -- Resume execution
      run

-- | Run execution until completion or a breakpoint is encountered.
run ::
  ( Functor m
  , MonadIO m
  , MonadException m
  , Functor sbe
  )
  => Simulator sbe m ()
run = do
  mcs <- use ctrlStk
  case mcs of
    Nothing -> do
      -- All paths ended in errors.
      showEPs <- gets (optsErrorPathDetails . lssOpts)
      if showEPs then
        tellUser "All paths yielded errors!" >> dumpErrorPaths
      else
        tellUser "All paths yielded errors! To see details, use --errpaths."
    Just (FinishedCS p) -> do
      -- Normal program termination on at least one path.
      -- Report termination info at appropriate verbosity levels; also,
      -- inform user about error paths when present and optionally dump
      -- them.
      whenVerbosity (>=5) $ dumpCtrlStk
      whenVerbosity (>=2) $ do
        dbugM "run terminating normally: found valid exit frame"        
        case p ^? finalRetValOfPath of
          Nothing -> dbugM "Program had no return value."
          Just rv -> dbugTerm "Program returned value" rv
        numErrs <- length <$> use errorPaths
        showEPs <- optsErrorPathDetails <$> gets lssOpts
        when (numErrs > 0 && not showEPs) $
          tellUser "Warning: Some paths yielded errors. To see details, use --errpaths."
        when (numErrs > 0 && showEPs) $ do
          dbugM $ showErrCnt numErrs
          dumpErrorPaths
    Just (ActiveCS cs) -> do
      s <- get
      let p = cs^.activePath
      let userIntHandler UserInterrupt = do
            put s
            dbugM $ show $ 
              text "Simulation interrupted: Entering debugger" <$$>
              indent 2 (text "at" <+> ppPathInfo p)
            debuggerREPL
            dbugM $ "Resuming simulation"
            liftIO $ resetInterrupt
            run
          userIntHandler e = throwIO e
      handle userIntHandler $ do
        flip catchError (onSimulationError s cs) $ do
          let Just (pcb,pc) = p^.pathPC
          -- Get name of function we are in.
          let sym = pathFuncSym p
          -- Get statement to execute
          Just def <- lookupDefine sym <$> gets codebase
          let sb = lookupSymBlock def pcb
          let stmt = sbStmts sb V.! pc
          -- Log what we're about to execute
          whenVerbosity (>=2) $ do
            dbugM $ show $
              text "Executing" <+> parens (ppPathInfo p) <> colon 
                      <+> ppStmt stmt
          -- Execute statement
          step stmt
          whenVerbosity (>=5) dumpCtrlStk
          -- Check to see if we have hit a breakpoint from previous step.
          cont <- atBreakpoint
          when cont debuggerREPL
          run
  where
    showErrCnt x
      | x == 1    = "Encountered errors on exactly one path. Details below."
      | otherwise = "Encountered errors on " ++ show x
                      ++ " paths.  Details below."
    dumpErrorPaths = do
        dbugM $ replicate 80 '-'
        eps <- use errorPaths
        sbe <- gets symBE
        forM_ eps $ \ep -> do
          let p = epPath ep
          dbugM $ "Error reason        : " ++ show (ppFailRsn (epRsn ep))
          dbugM $ "Error path state    :\n" ++ show (nest 2 $ ppPath sbe p)
          whenVerbosity (>= 3) $ do
            dbugM "Error path memory: "
          withSBE $ \s -> memDump s (p^.pathMem) Nothing
          when (length eps > 1) $ dbugM "--"
        dbugM $ replicate 80 '-'

--------------------------------------------------------------------------------
-- LLVM-Sym operations

assignReg :: (Functor m, MonadIO m)
          => Ident -> MemType -> SBETerm sbe -> Simulator sbe m ()
assignReg reg tp v = modifyPathRegs $ at reg ?~ (v,tp)

-- | Evaluate condition in current path.
evalCond :: (Functor sbe, Functor m, MonadIO m)
         => SymCond (SBETerm sbe) -> Simulator sbe m (SBEPred sbe)
evalCond (HasConstValue t w i) = do
  v <- evalExprInCC "evalCond" t
  sbe <- gets symBE
  iv <- liftSBE $ termInt sbe w i
  liftSBE $ applyIEq sbe w v iv

data EvalContext sbe = EvalContext {
       evalContextName :: String
     , evalGlobals :: GlobalMap sbe
     , evalRegs :: Maybe (RegMap (SBETerm sbe))
     , evalMemory :: Maybe (SBEMemory sbe)
     }

getCurrentEvalContext :: (Functor m, Monad m)
                      => String
                      -> Simulator sbe m (EvalContext sbe)
getCurrentEvalContext nm =do
  gm <- use globalTerms
  mr <- preuse (currentPathOfState . pathRegs)
  mmem <- preuse currentPathMem
  return EvalContext { evalContextName = nm
                     , evalGlobals = gm
                     , evalRegs = mr
                     , evalMemory = mmem
                     }

evalExprInCC :: (Functor m, MonadIO m, Functor sbe) 
         => String -> SymValue (SBETerm sbe) -> Simulator sbe m (SBETerm sbe)
evalExprInCC nm sv = runEvaluator nm $ evalExpr sv

type Evaluator sbe = ErrorT FailRsn (ReaderT (EvalContext sbe) IO)

runEvaluator ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => String
  -> Evaluator sbe a -> Simulator sbe m a
runEvaluator nm m = do
  ec <- getCurrentEvalContext nm
  mr <- liftIO $ runReaderT (runErrorT m) ec
  case mr of
    Left (FailRsn fr) -> errorPath fr
    Right v -> return v

evalExpr :: SymValue (SBETerm sbe)
         -> Evaluator sbe (SBETerm sbe)
evalExpr sv = do
  ec <- ask
  case sv of
    SValIdent i -> 
      case evalRegs ec of
        Just regs ->
          case regs^.at i of
            Just (x,_)  -> return x
            Nothing -> throwError $ FailRsn $
               "Could not find register: "
                ++ show (ppIdent i) ++ " in " 
                ++ show (M.keys regs)
                ++ " when attempting to evaluate expression."
        Nothing -> error $ "evalExpr called by " ++ evalContextName ec ++ " with missing frame."
    SValSymbol sym -> do
      case evalGlobals ec ^. at sym of
        Just t -> return t
        Nothing -> error "evalExp' called with missing global symbol."
    SValExpr _ (ExprEvalFn fn) -> fn evalExpr

insertGlobalFn ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => Symbol
  -> [BlockLabel]
  -> Simulator sbe m ()
insertGlobalFn sym blocks = do
  let errMsg = "Insufficient space for new function"
  Just m <- preuse currentPathMem
  mr <- withSBE $ \s -> memAddDefine s m sym blocks
  case mr of
    Nothing -> errorPath errMsg
    Just (r,bptrs, m')  -> do
      currentPathMem .= m'
      globalTerms . at sym ?= r
      forOf_ folded (blocks `zip` bptrs) $ \(l,p) -> do
        blockPtrs . at (sym,l) ?= p

insertGlobalTerm ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => String
  -> Symbol
  -> SymType
  -> (SBE sbe -> SBEMemory sbe -> sbe (Maybe (SBETerm sbe, SBEMemory sbe)))
  -> Simulator sbe m ()
insertGlobalTerm errMsg sym _ act = do
  Just m <- preuse currentPathMem
  mr <- withSBE $ \s -> act s m
  case mr of
    Nothing -> errorPath errMsg
    Just (r,m')  -> do
      currentPathMem .= m'
      globalTerms . at sym ?= r

--------------------------------------------------------------------------------
-- Instruction stepper and related functions

incPC :: Monad m => Simulator sbe m ()
incPC = do
  currentPathOfState . pathPC %= incPathPC

-- | Execute a single LLVM-Sym AST instruction
-- Returns true if execution should continue, and false if
-- we should enter the debugger.
step ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => SymStmt (SBETerm sbe) -> Simulator sbe m ()
step (PushCallFrame callee args mres retTgt) = do
  sbe <- gets symBE
  join $ runEvaluator "PushCallFrame" $ do
    calleeSym <- 
      case callee of
        SValSymbol sym -> return sym
        _ -> do
          fp <- evalExpr callee
          Just m <- asks evalMemory
          r <- liftIO $ sbeRunIO sbe $ codeLookupSymbol sbe m fp
          case r of
            Left e -> do
              throwError $ FailRsn $
                "PushCallFrame: Failed to resolve callee function pointer: "
                ++ show (ppSymValue callee) ++ "\n"
                ++ show e ++ "\n"
                ++ show (prettyTermD sbe fp)
            Right sym -> return sym
    argTerms <- (traverse._2) evalExpr args
    return $ callDefine' False retTgt calleeSym mres argTerms 

step (Return mtv) = do
  sbe <- gets symBE
  mrv <- traverse (evalExprInCC "mergeReturn") mtv
  withActiveCS (returnCurrentPath sbe mrv)

step (PushPendingExecution bid cond ml) = do
  sbe <- gets symBE
  c <- evalCond cond
  opts <- gets lssOpts
  Just p <- preuse currentPathOfState
  tsat <- if optsSatAtBranches opts
             then do a' <- liftSBE $ applyAnd sbe c (p^.pathAssertions)
                     liftSBE $ termSAT sbe a'
             else return Unknown
  fsat <- if optsSatAtBranches opts
             then do cnot <- liftSBE $ applyBNot sbe c
                     a' <- liftSBE $ applyAnd sbe cnot (p^.pathAssertions)
                     liftSBE $ termSAT sbe a'
             else return Unknown
  -- exclude paths which are definitely unsat. If both are unsat, we
  -- probably couldn't have gotten here, so just throw an error path
  -- right away
  when (tsat == UnSat && fsat == UnSat) $ do
    dbugM' 3 "both branch conditions unsatisfiable -- should have been caught earlier"
    errorPath "no satisfiable assertions at branch point"

  -- fall through to else branch if true condition is unsat
  let b = if tsat /= UnSat then asBool sbe c else Just False
  case b of
    -- Don't bother with elseStmts as condition is true.
    Just True  -> do
      setCurrentBlock bid
    -- Don't bother with elseStmts if negated condition is unsat
    Nothing | fsat == UnSat -> do
      setCurrentBlock bid
    -- Don't bother with pending path as condition is false.
    Just False -> incPC
    -- Don't bother with pending path as condition is unsat
    Nothing | tsat == UnSat -> incPC
    Nothing -> do
      nm <- use pathCounter
      pathCounter += 1
      withActiveCS $ \cs ->
        ActiveCS <$> addCtrlBranch sbe c bid nm ml cs
      incPC

step (SetCurrentBlock bid) = setCurrentBlock bid

step (Assign reg tp (Val tv)) = do
  assignReg reg tp =<< evalExprInCC "eval@Val" tv
  incPC

step (Assign reg tp (Alloca ty msztv a)) = do
  -- Get number of elements and with of number of elements.
  (aw,sizeTm) <- 
    case msztv of
      Just (w,v) -> (w,) <$> evalExprInCC "alloca" v
      Nothing -> do
        aw <- ptrBitwidth <$> getDL
        sbe <- gets symBE
        (aw,) <$> liftSBE (termInt sbe aw 1)
  assignReg reg tp =<< alloca ty aw sizeTm a
  incPC

step (Assign reg tp (Load v ty a)) = do
  addrTerm <- evalExprInCC "load" v
  dumpMem 6 "load pre"
  val <- load ty addrTerm a
  dumpMem 6 "load post"
  assignReg reg tp val
  incPC

step (Store valType val addr a) = do
  whenVerbosity (<=6) $ dumpMem 6 "store pre"
  join $ runEvaluator "store" $ do
    valTerm <- evalExpr val
    addrTerm <- evalExpr addr
    return $ store valType valTerm addrTerm a >> incPC
  whenVerbosity (<=6) $ dumpMem 6 "store post"

step Unreachable
  = error "step: Encountered 'unreachable' instruction"

step s@BadSymStmt{} = do
  errorPath $ "Path execution encountered unsupported statement:\n"
            ++ show (ppStmt s)

--------------------------------------------------------------------------------
-- Callbacks and event handlers

defaultSEH :: Monad m => SEH sbe m
defaultSEH = SEH { onPostOverrideReg = return ()
                 , _onPreStep = \_   -> return ()
                 , _onBlockEntry = \_   -> return ()
                 , onBlockExit = \_ -> return ()
                 , onMkGlobTerm = \_ -> return ()
                 , onPreGlobInit = \_ _ -> return ()
                 , onPostGlobInit = \_ _ -> return ()
                 }

--------------------------------------------------------------------------------
-- Misc utility functions

entryRsltReg :: Ident
entryRsltReg = Ident "__galois_final_rslt"

--------------------------------------------------------------------------------
-- Debugging



repl :: (Functor m, MonadIO m) => Simulator sbe m ()
repl = do
  liftIO $ putStr "lss> " >> hFlush stdout
  inp <- liftIO getLine
  unless (null inp) $ case inp of
    "cs"   -> dumpCtrlStk >> repl
    "mem"  -> dumpMem 1 "User request" >> repl
    "path" -> do sbe <- gets symBE
                 Just p   <- getPath
                 liftIO $ putStrLn $ show $ ppPath sbe p
                 repl
    "quit"   -> liftIO $ exitWith ExitSuccess
    other    -> case other of
                  ('v':rest) -> case reads rest of
                                  [(v, "")] -> do
                                    dbugM $ "Verbosity set to " ++ show v ++ "."
                                    setVerbosity v
                                    repl
                                  _ -> unknown
                  _          -> unknown
  where
    unknown = dbugM "Unknown command. Options are cs, mem, path, quit, vx." >> repl

dbugTerm :: (MonadIO m, Functor m) => String -> SBETerm sbe -> Simulator sbe m ()
dbugTerm desc t = do
  d <- withSBE' $ \s -> prettyTermD s t
  dbugM $ desc ++ ": " ++ show d

_nowarn_unused :: a
_nowarn_unused = undefined
  (dbugTerm undefined undefined :: Simulator IO IO ())
  (repl :: Simulator IO IO ())

--------------------------------------------------------------------------------
-- Standard overrides

warning :: (Functor m, MonadIO m) => String -> Simulator sbe m ()
warning msg = do
  mp <- getPath
  -- Get location information
  let prefix = maybe (text "Warning") 
                     (\p -> text "Warning at" <+> ppPathInfo p)
                     mp
  liftIO $ putStrLn $ show prefix ++ ". " ++ msg