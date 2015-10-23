{- |
Module           : $Header$
Description      : The symbolic simulator for LLVM-Symbolic programs
License          : Free for non-commercial use. See LICENSE.
Stability        : provisional
Point-of-contact : jhendrix
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
  ( Simulator (SM)
  , getVerbosity
  , setVerbosity
  , whenVerbosity
  , State(..)
  , onSimError
  , ErrorHandler
  , killPathOnError
  , callDefine
  , callDefine'
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
  , currentPathMem
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
  , MonadException
  , lookupSymbolDef
  , getPath
  , run
  ) where

import Control.Exception ( AsyncException(..)
                         , AssertionFailed(..)
                         , assert
                         )
import           Control.Lens hiding (from)
import           Control.Monad.State.Class
import qualified Control.Monad.State as MTL
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict (evalStateT)
import           Data.List                 (isPrefixOf, nub)
import qualified Data.Graph as G
import qualified Data.Map as M
import           Data.Maybe
import System.Console.Haskeline.MonadException (MonadException, handle, throwIO)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), align, line)
import Prelude ()
import Prelude.Compat hiding (mapM_)

import Verifier.LLVM.Backend
import Verifier.LLVM.Codebase
import Verifier.LLVM.Overrides.Intrinsics
import Verifier.LLVM.Overrides.Libc
import Verifier.LLVM.Overrides.LSS
import Verifier.LLVM.Simulator.Internals
import Verifier.LLVM.Simulator.SimUtils


-- | Obtain the first pending path in the topmost merge frame; @Nothing@ means
-- that the control stack is empty or the top entry of the control stack has no
-- pending paths recorded.
getPath :: (Functor m, Monad m)
        => Simulator sbe m (Maybe (Path sbe))
getPath = preuse currentPathOfState

withCurrentCallStack :: Monad m
                     => String
                     -> MTL.State (CallStack sbe) a
                     -> Simulator sbe m a
withCurrentCallStack _nm m = do
  Just cs <- use ctrlStk
  let CallStack stk = cs^.currentPathStack
  let (v,stk') = MTL.runState m stk
  let cs' = cs & currentPathStack .~ CallStack stk'
  ctrlStk ?= cs'
  return v



-- @getMem@ yields the memory model of the current path if any.
getMem :: (Functor m, Monad m) =>  Simulator sbe m (Maybe (SBEMemory sbe))
getMem = preuse currentPathMem

-- | Run simulator in given context.
runSimulator :: forall sbe a m.
  ( Functor sbe
  , Ord (SBETerm sbe)
  , Functor m
  , MonadIO m
  , MonadException m
  )
  => Codebase sbe          -- ^ Post-transform LLVM code, memory alignment, and
                           -- type aliasing info
  -> SBE sbe               -- ^ A symbolic backend
  -> SBEMemory sbe         -- ^ The SBE's LLVM memory model
  -> Maybe LSSOpts         -- ^ Simulation options
  -> Simulator sbe m a     -- ^ Simulator action to perform
  -> m a
runSimulator cb sbe mem mopts m = do
  let newSt = State { codebase     = cb
                    , symBE        = sbe
                    , liftSymBE    = liftIO . sbeRunIO sbe
                    , verbosity    = 6
                    , lssOpts      = fromMaybe defaultLSSOpts mopts
                    , _ctrlStk     = Just $ initialCtrlStk sbe mem
                    , _globalTerms = M.empty
                    , _blockPtrs   = M.empty
                    , _fnOverrides = M.empty
                    , _errorPaths  = []
                    , _pathCounter = 1
                    , _aigOutputs  = []
                    , _breakpoints = M.empty
                    , _onPathPosChange = return ()
                    , _onSimError = killPathOnError
                    , _onUserInterrupt = throwIO UserInterrupt
                    }
  ea <- flip evalStateT newSt $ runExceptT $ runSM $ do
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
  cb <- gets codebase
  let nms = cb^.cbGlobalNameMap
  -- Register all function definitions.
  do let defines = [ d | (_,Right d) <- M.toList nms]
     forM_ defines $ \d -> do
       let idl       = nub $ mapMaybe symBlockLabel $ M.keys (sdBody d)
       insertGlobalFn (sdName d) idl
  -- Add symbol for declarations.
  do let declares = cbUndefinedFns cb
     forM_ declares $ \(sym,_) -> do
       insertGlobalFn sym []
  -- Initialize global data
  do let globals = [ g | (_,Left g) <- M.toList nms]
         -- Topologically sort globals so that they're initialized in
         -- dependency order.
         edges = [ (g, globalSym g, cbGlobalDeps cb g) | g <- globals ]
         (gr, vertexFn) = G.graphFromEdges' edges
         sortedGlobals = map ((\(g, _, _) -> g) . vertexFn) $
                         G.topSort (G.transposeG gr)
     forM_ sortedGlobals $ \g -> do
       cdata <- evalExprInCC "addGlobal" (globalValue g)
       let noDataSpc = "Not enough space in data segment to allocate new global."
       insertGlobalTerm noDataSpc (globalSym g) (MemType (globalType g)) $
          \s m -> memInitGlobal s m (globalType g) cdata

--------------------------------------------------------------------------------
-- Misc utility functions

entryRsltReg :: Ident
entryRsltReg = Ident "__galois_final_rslt"

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
  callDefine' False calleeSym retReg args
  signalPathPosChangeEvent
  run

callDefine' ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => Bool                    -- ^ Is this a redirected call?
  -> Symbol                  -- ^ Callee symbol
  -> Maybe (MemType, Ident)  -- ^ Callee return type and result register
  -> [(MemType,SBETerm sbe)]           -- ^ Callee arguments
  -> Simulator sbe m ()
callDefine' isRedirected calleeSym@(Symbol calleeName) mreg args = do
  -- NB: Check overrides before anything else so we catch overriden intrinsics
  symOver <- use (fnOverrides . at calleeSym)
  case symOver of
    Nothing -> normal
    Just (Redirect calleeSym', _)
      -- NB: We break transitive redirection to avoid cycles
      | isRedirected -> normal
      | otherwise    -> callDefine' True calleeSym' mreg args
    Just (Override f, _) -> do
      r <- f calleeSym mreg args
      withCurrentCallStack "callDefine'" $ do
        -- Set return value
        topCallFrame . cfRegs %= setReturnValue mreg r
        -- Increment PC
        incPC
  where
    normal
      | isPrefixOf "llvm." calleeName = do
          whenVerbosity (>= 1) $ do
            --TODO: Give option of stopping on warnings like this.
            tellUser $ "Warning: skipping unsupported LLVM intrinsic "
                         ++ show calleeName
          withCurrentCallStack "callDefine" incPC
      | otherwise = do
          runNormalSymbol calleeSym mreg (snd <$> args)

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
  => Symbol                 -- ^ Callee symbol
  -> Maybe (MemType, Ident) -- ^ Callee return type and result register
  -> [SBETerm sbe]          -- ^ Callee arguments
  -> Simulator sbe m ()
runNormalSymbol calleeSym mreg args = do
  def <- lookupSymbolDef calleeSym
  dbugM' 5 $ "callDefine': callee " ++ show (ppSymbol calleeSym)
  Just cs <- use ctrlStk
  sbe <- gets symBE
  let m = cs^.currentPath^.pathMem
  (c,m') <- liftSBE $ stackPushFrame sbe m
  let cf = newCallFrame def args
  let cs' = cs & currentPath . pathStack %~ pushCallFrame cf mreg
               & currentPath . pathMem  .~ m'
  ctrlStk ?= cs'
  -- Push stack frame in current process memory.
  let fr = "Stack push frame failure: insufficient stack space"
  processMemCond fr c

getProgramReturnValue :: (Monad m, Functor m)
                      => Simulator sbe m (Maybe (SBETerm sbe))
getProgramReturnValue = 
  preuse $ currentPathOfState . pathStack . pathStackReturnValue

getProgramFinalMem :: (Monad m, Functor m)
                   => Simulator sbe m (Maybe (SBEMemory sbe))
getProgramFinalMem = preuse currentPathMem

-- | Check to see if simulator hit breakpoint, and enter debugger if so.
signalPathPosChangeEvent :: Monad m => Simulator sbe m ()
signalPathPosChangeEvent = do
  mp <- preuse currentPathOfState
  case mp of
    Nothing -> return ()
    Just{} -> join (use onPathPosChange)


killPathOnError :: (Functor sbe, Functor m, MonadException m)
                => ErrorHandler sbe m
killPathOnError cs rsn = do
  -- Reset state
  ctrlStk ?= cs
  -- Get current path before last step.
  let p = cs^.currentPath
  sbe <- gets symBE
  -- Log error path  
  whenVerbosity (>=1) $ do
    dbugM $ show $ text "Simulation error:" <+> ppFailRsn rsn
  -- Just print location at verbosity 1 and 2.
  whenVerbosity (`elem` [1,2]) $ do
    dbugM $ show $ indent 2 (text "at" <+> ppPathNameAndLoc p)
  -- Print full path at verbosity 3+.
  whenVerbosity (>=3) $ do
    dbugM $ show $ ppPath sbe p
  -- Kill the current path.
  killCurrentPath rsn
  -- Check to see if there is a new active path that may have hit
  -- a breakpoint.
  signalPathPosChangeEvent
  -- Resume execution
  run
  
-- | Run execution until completion or a breakpoint is encountered.
run :: forall sbe m .
  ( Functor sbe
  , Functor m
  , MonadException m
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

    Just cs ->
      case cs^.currentPathStack of
        CallStack stk -> do
          let onError rsn = do
                h <- use onSimError
                h cs rsn
                run
          let assertionFailedHandler (AssertionFailed msg) = do
                onError (FailRsn msg)                
          handle assertionFailedHandler $ do
            let userIntHandler :: AsyncException -> Simulator sbe m ()
                userIntHandler UserInterrupt = do
                  ctrlStk ?= cs -- Reset control stack.
                  join $ use onUserInterrupt
                  run
                userIntHandler e = throwIO e
            handle userIntHandler $ do
              flip catchSM onError $ do
               let cf =  stk^.topCallFrame 
               -- Get statement to execute next.
               let stmt = cfStmt cf
               -- Log what we're about to execute
               whenVerbosity (>=2) $ do
                 let p = cs^.currentPath
                 dbugM $ show $
                   text "Executing" <+> parens (ppPathNameAndLoc p) <> colon 
                     <+> ppStmt stmt
               -- Execute statement
               step stmt
               whenVerbosity (>=5) dumpCtrlStk
               -- Check to see if we have hit a breakpoint from previous step.
               signalPathPosChangeEvent
               run
        FinStack mr -> assert (csHasSinglePath cs) $ do      
           -- Normal program termination on at least one path.
           -- Report termination info at appropriate verbosity levels; also,
           -- inform user about error paths when present and optionally dump
           -- them.
           whenVerbosity (>=5) $ dumpCtrlStk
           whenVerbosity (>=2) $ do
             dbugM "run terminating normally: found valid exit frame"        
             case mr of
               Nothing -> dbugM "Program had no return value."
               Just (rv,_) -> dbugTerm "Program returned value" rv
             numErrs <- length <$> use errorPaths
             showEPs <- optsErrorPathDetails <$> gets lssOpts
             when (numErrs > 0 && not showEPs) $
               tellUser "Warning: Some paths yielded errors. To see details, use --errpaths."
             when (numErrs > 0 && showEPs) $ do
               dbugM $ showErrCnt numErrs
               dumpErrorPaths
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

incPC :: MTL.State (CallStack sbe) ()
incPC = topCallFrame . cfPC += 1

assignReg :: Ident -> MemType -> SBETerm sbe -> MTL.State (CallStack sbe) ()
assignReg reg tp v = topCallFrame . cfRegs . at reg ?= (v,tp)

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
  mr <- preuse (currentPathOfState . pathCallFrames . cfRegs)
  mmem <- preuse currentPathMem
  return EvalContext { evalContextName = nm
                     , evalGlobals = gm
                     , evalRegs = mr
                     , evalMemory = mmem
                     }

evalExprInCC :: (Functor m, MonadIO m, Functor sbe) 
         => String -> SymValue (SBETerm sbe) -> Simulator sbe m (SBETerm sbe)
evalExprInCC nm sv = runEvaluator nm $ evalExpr sv

type Evaluator sbe = ExceptT FailRsn (ReaderT (EvalContext sbe) IO)

mkIEqPred :: Monad m => SBETerm sbe -> BitWidth -> Integer -> Simulator sbe m (SBEPred sbe)
mkIEqPred v w expected = do
  sbe <- gets symBE
  iv <- liftSBE $ termInt sbe w expected
  liftSBE $ applyIEq sbe w v iv

negatePred :: Monad m => SBEPred sbe -> Simulator sbe m (SBEPred sbe)
negatePred p = do
  sbe <- gets symBE
  liftSBE $ applyBNot sbe p

andAll :: Monad m => [SBEPred sbe] -> Simulator sbe m (SBEPred sbe)
andAll [] = gets (sbeTruePred . symBE)
andAll (h:l) = do
  sbe <- gets symBE
  foldM (\x y -> liftSBE (applyAnd sbe x y)) h l

checkPathUnsat :: Monad m => SBEPred sbe -> Simulator sbe m Bool
checkPathUnsat c = do
  cparents <- assumptionsForActivePath
  sbe <- gets symBE
  a <- liftSBE $ applyAnd sbe c cparents
  fsat <- liftSBE $ termSAT sbe a
  return (fsat == Unsat)

runEvaluator ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => String
  -> Evaluator sbe a -> Simulator sbe m a
runEvaluator nm m = do
  ec <- getCurrentEvalContext nm
  mr <- liftIO $ runReaderT (runExceptT m) ec
  either throwSM return mr

evalExpr :: SymValue (SBETerm sbe)
         -> Evaluator sbe (SBETerm sbe)
evalExpr sv = do
  ec <- lift ask
  case sv of
    SValIdent i -> 
      case evalRegs ec of
        Just regs ->
          case regs^.at i of
            Just (x,_)  -> return x
            Nothing -> throwE $ FailRsn $
               "Could not find register: "
                ++ show (ppIdent i) ++ " in " 
                ++ show (M.keys regs)
                ++ " when attempting to evaluate expression."
        Nothing -> error $ "evalExpr called by " ++ evalContextName ec ++ " with missing frame."
    SValSymbol sym -> do
      case evalGlobals ec ^. at sym of
        Just t -> return t
        Nothing ->
          error $ "evalExp' called with missing global symbol: " ++ show sym
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
insertGlobalTerm errMsg sym _ f = do
  Just m <- preuse currentPathMem
  mr <- withSBE $ \s -> f s m
  case mr of
    Nothing -> errorPath errMsg
    Just (r,m')  -> do
      currentPathMem .= m'
      globalTerms . at sym ?= r

--------------------------------------------------------------------------------
-- Instruction stepper and related functions


splitBranches :: (Functor m, MonadIO m)
              => [(SBEPred sbe, SymBlockID)] -> MergeLocation -> Simulator sbe m ()
splitBranches allPairs ml = do
  runSat <- gets (optsSatAtBranches . lssOpts)
  branches <-
    if runSat then
      filterM (\(c,_) -> not <$> checkPathUnsat c) allPairs
    else
      return allPairs
  case branches of
    [] -> errorPath "Unsatisfiable path detected at switch."
    ((_,b):r) -> do
      let pushBranch (c,cb) nm = addCtrlBranch c cb nm ml

      nm <- use pathCounter
      pathCounter += toInteger (length r)

      mapM_ (uncurry pushBranch) (reverse (r `zip` [nm..]))
      setCurrentBlock b

-- | Execute a single LLVM-Sym AST instruction
-- Returns true if execution should continue, and false if
-- we should enter the debugger.
step ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => SymStmt (SBETerm sbe) -> Simulator sbe m ()
step (Call callee args mres) = do
  sbe <- gets symBE
  join $ runEvaluator "PushCallFrame" $ do
    calleeSym <- 
      case callee of
        SValSymbol sym -> return sym
        _ -> do
          fp <- evalExpr callee
          Just m <- lift $ asks evalMemory
          r <- liftIO $ sbeRunIO sbe $ codeLookupSymbol sbe m fp
          case r of
            Left e -> do
              throwE $ FailRsn $
                "PushCallFrame: Failed to resolve callee function pointer: "
                ++ show (ppSymValue callee) ++ "\n"
                ++ show e ++ "\n"
                ++ show (prettyTermD sbe fp)
            Right sym -> return sym
    argTerms <- (traverse . (\k (a,b) -> k b <&> \b' -> (a,b'))) evalExpr args
    return $ callDefine' False calleeSym mres argTerms 

step (Ret mtv) = do
  mrv <- traverse (evalExprInCC "return") mtv
  returnCurrentPath mrv

step (Br cond trueBlock falseBlock ml) = do
  sbe <- gets symBE
  v <- evalExprInCC "branch" cond
  case asUnsignedInteger sbe 1 v of
    Just 1 -> setCurrentBlock trueBlock
    Just w -> assert (w == 0) $ setCurrentBlock falseBlock
    Nothing -> do
      (pt, pf) <- both (mkIEqPred v 1) (1,0)
      splitBranches [(pt,trueBlock), (pf,falseBlock)] ml

step (Switch w sv defBlock cases ml) = do
  sbe <- gets symBE
  v <- evalExprInCC "switch" sv
  case asUnsignedInteger sbe w v of
    Just v' ->
      case M.lookup v' cases of
        Just cb -> setCurrentBlock cb
        Nothing -> setCurrentBlock defBlock
    Nothing -> do
      preds <- traverse (mkIEqPred v w) (M.keys cases)
      notPred <- andAll =<< traverse negatePred preds
      let casePairs = preds `zip` M.elems cases
          defPair = (notPred, defBlock)
      splitBranches (casePairs ++ [defPair]) ml
step (Jump bid) = setCurrentBlock bid

step (Assign r tp tv) = do
  v <- runEvaluator "assign" $ evalExpr tv
  withCurrentCallStack "assign" $ do
    assignReg r tp v
    incPC

step (Alloca reg ty msztv a) = do
  -- Get number of elements and with of number of elements.
  (aw,sizeTm) <- 
    case msztv of
      Just (w,v) -> (w,) <$> evalExprInCC "alloca" v
      Nothing -> do
        aw <- ptrBitwidth <$> getDL
        sbe <- gets symBE
        (aw,) <$> liftSBE (termInt sbe aw 1)
  ptr <- alloca ty aw sizeTm a
  withCurrentCallStack "alloca" $ do
    assignReg reg (PtrType (MemType ty)) ptr
    incPC

step (Load reg v ty a) = do
  addrTerm <- evalExprInCC "load" v
  dumpMem 6 "lo/ad pre"
  val <- load ty addrTerm a
  dumpMem 6 "load post"
  withCurrentCallStack "load" $ do
    assignReg reg ty val
    incPC

step (Store valType val addr a) = do
  whenVerbosity (<=6) $ dumpMem 6 "store pre"
  join $ runEvaluator "store" $ do
    valTerm <- evalExpr val
    addrTerm <- evalExpr addr
    return $ store valType valTerm addrTerm a
  withCurrentCallStack "store" $ do
    incPC
  whenVerbosity (<=6) $ dumpMem 6 "store post"

step Unreachable
  = error "step: Encountered 'unreachable' instruction"

step s@BadSymStmt{} = do
  errorPath $ "Path execution encountered unsupported statement:\n"
            ++ show (ppStmt s)


--------------------------------------------------------------------------------
-- Debugging

dbugTerm :: (MonadIO m, Functor m) => String -> SBETerm sbe -> Simulator sbe m ()
dbugTerm desc t = do
  d <- withSBE' $ \s -> prettyTermD s t
  dbugM $ desc ++ ": " ++ show d

_nowarn_unused :: a
_nowarn_unused = undefined
  (dbugTerm undefined undefined :: Simulator IO IO ())

--------------------------------------------------------------------------------
-- Standard overrides

warning :: (Functor m, MonadIO m) => String -> Simulator sbe m ()
warning msg = do
  mp <- getPath
  -- Get location information
  let prefix = maybe (text "Warning") 
                     (\p -> text "Warning at" <+> ppPathNameAndLoc p)
                     mp
  liftIO $ putStrLn $ show prefix ++ ". " ++ msg
