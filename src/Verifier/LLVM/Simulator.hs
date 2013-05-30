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
  , callDefine_
  , defaultSEH
  , lookupSymbolDef
  , getProgramReturnValue
  , getProgramFinalMem
  , evalExpr
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
  , setSEH
  , withDL
  , warning
  , ErrorPath
  , errorPaths
  , LSSOpts(..)
  ) where

import           Control.Applicative
import           Control.Lens hiding (act,from)
import           Control.Monad.Error
import           Control.Monad.State.Class
import           Control.Monad.Trans.State.Strict (evalStateT)
import           Data.List                 (isPrefixOf, nub)
import qualified Data.Map                  as M
import           Data.Maybe
import qualified Data.Set                  as S
import qualified Data.Vector               as V
import           Numeric                   (showHex)
import           System.Exit
import           System.IO
import Text.PrettyPrint.Leijen hiding ((<$>), align, line)

import           Verifier.LLVM.AST
import           Verifier.LLVM.Backend
import           Verifier.LLVM.Codebase
import           Verifier.LLVM.LLVMIntrinsics
import           Verifier.LLVM.LSSOverrides
import           Verifier.LLVM.Simulator.Internals
import           Verifier.LLVM.Simulator.SimUtils

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

-- @setMem@ sets the memory model in the current path, which must exist.
setMem :: (Functor m, Monad m) => SBEMemory sbe -> Simulator sbe m ()
setMem mem = currentPathMem .= mem

-- | Run simulator in given context.
runSimulator :: forall sbe a .
  ( Functor sbe
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
                    , _trBreakpoints = S.empty
                    }
  ea <- flip evalStateT newSt $ runErrorT $ runSM $ do
    initGlobals
    registerLLVMIntrinsicOverrides
    registerLibcOverrides
    registerLSSOverrides
    m
  -- TODO: call exception handlers given by to-be-written SEH fields
  case ea of
    Left ErrorPathExc{}   -> error "internal: uncaught error path exception"
    Left (UnknownExc mfr) -> error $ "internal: uncaught unknown exception: "
                                     ++ maybe "(no details)" (show . ppFailRsn) mfr
    Right x               -> return x

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
  do declares <- cbUndefinedFns . codebase <$> get
     forM_ declares $ \(sym,_) -> do
       insertGlobalFn sym []
  -- Initialize global data
  do let globals = [ g | (_,Left g) <- M.toList nms]
     forM_ globals $ \g -> do
       cb1 onMkGlobTerm g
       cdata <- evalExpr "addGlobal" (globalValue g)
       cb2 onPreGlobInit g cdata
       let noDataSpc = "Not enough space in data segment to allocate new global."
       insertGlobalTerm noDataSpc (globalSym g) (MemType (globalType g)) $ \s m ->
              memInitGlobal s m (globalType g) cdata
       cb2 onPostGlobInit g cdata

callDefine_ ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => Symbol     -- ^ Callee symbol
  -> RetType      -- ^ Callee return type
  -> [(MemType, SBETerm sbe)] -- ^ Callee arguments
  -> Simulator sbe m ()
callDefine_ c t ag = void $ callDefine c t ag

-- | External entry point for a function call.  The argument generator is used
-- to create actuals passed to the function, and the return value is those
-- arguments.  In the case when no arguments created or invoking
-- intrinsics/overrides, the return value will always be the empty list.
callDefine ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => Symbol     -- ^ Callee symbol
  -> RetType       -- ^ Callee return type
  -> [(MemType,SBETerm sbe)] -- ^ Callee argument generator
  -> Simulator sbe m [SBETerm sbe]
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
  r <- callDefine' False entryRetNormalID calleeSym retReg args
  r <$ run

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
  -> Simulator sbe m [SBETerm sbe]
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
      return []
  where
    normal
      | isPrefixOf "llvm." calleeName = do
          whenVerbosity (>= 1) $ do --TODO: Give option of stopping on warnings like this.
            tellUser $ "Warning: skipping unsupported LLVM intrinsic " ++ show calleeName
          setCurrentBlock normalRetID
          return []
      | otherwise = do
          runNormalSymbol normalRetID calleeSym mreg (snd <$> args)

-- | Return symbol definition with given name or fail.
lookupSymbolDef :: (Functor m, MonadIO m, Functor sbe)
                => Symbol
                -> Simulator sbe m (SymDefine (SBETerm sbe))
lookupSymbolDef sym = do
  mdef <- lookupDefine sym <$> gets codebase
  case mdef of
    Just def -> return def
    Nothing  -> do
      errorPath $ "Failed to find definition for symbol "
                     ++ show (ppSymbol sym) ++ " in codebase."

runNormalSymbol ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => SymBlockID            -- ^ Normal call return block id
  -> Symbol              -- ^ Callee symbol
  -> Maybe (MemType, Ident)     -- ^ Callee return type and result register
  -> [SBETerm sbe] -- ^ Callee arguments
  -> Simulator sbe m [SBETerm sbe]
runNormalSymbol normalRetID calleeSym mreg args = do
  def <- lookupSymbolDef calleeSym
  sbe <- gets symBE
  dbugM' 5 $ "callDefine': callee " ++ show (ppSymbol calleeSym)
  do Just cs <- use ctrlStk
     let m = cs^.currentPath^.pathMem
     (c,m') <- withSBE $ \s -> stackPushFrame s m
     let cs' = cs & pushCallFrame sbe calleeSym normalRetID mreg
                  & activePath . pathRegs .~ bindArgs (sdArgs def) args
                  & activePath . pathMem  .~ m'
     ctrlStk ?= ActiveCS cs'
     -- Push stack frame in current process memory.
     let fr = "Stack push frame failure: insufficient stack space"
     processMemCond fr c
  return args
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

-- | Return true if the path has asserted false to be true, and therefore we
-- can call errorPath on it.
pathAssertedFalse :: SBE sbe -> Path sbe -> Bool
pathAssertedFalse sbe p = asBool sbe (p^.pathAssertions) == Just False

run ::
  ( Functor m
  , MonadIO m
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
      flip catchError handleError $ do
        let p = cs^.activePath
        let Just pcb = pathCB p
        sbe <- gets symBE
        when (pathAssertedFalse sbe p) $
          errorPath $ "This path is infeasible"
        let sym = pathFuncSym p
        Just def <- lookupDefine sym <$> gets codebase
        let sb = lookupSymBlock def pcb
        use (evHandlers.onBlockEntry) >>= ($sb)
        runStmts . sbStmts $ sb
      run
  where
    handleError (ErrorPathExc _rsn s) = do
      -- errorPath ensures that the simulator state provided in the
      -- exception data is correct for the next invocation of run,
      -- so overwrite the current state here.
      put s
    handleError e = throwError e
    showErrCnt x
      | x == 1    = "Encountered errors on exactly one path. Details below."
      | otherwise = "Encountered errors on " ++ show x ++ " paths.  Details below."
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
  v <- evalExpr "evalCond" t
  sbe <- gets symBE
  iv <- liftSBE $ termInt sbe w i
  liftSBE $ applyIEq sbe w v iv

data EvalContext sbe = EvalContext {
       evalContextName :: String
     , evalGlobals :: GlobalMap sbe
     , evalRegs :: Maybe (RegMap (SBETerm sbe))
     }

getCurrentEvalContext :: (Functor m, Monad m) => String -> Simulator sbe m (EvalContext sbe)
getCurrentEvalContext nm =do
  gm <- use globalTerms
  mr <- preuse (currentPathOfState . pathRegs)
  return EvalContext { evalContextName = nm
                     , evalGlobals = gm
                     , evalRegs = mr
                     }           

evalExpr :: (Functor m, MonadIO m, Functor sbe) 
         => String -> SymValue (SBETerm sbe) -> Simulator sbe m (SBETerm sbe)
evalExpr nm sv = do
  ec <- getCurrentEvalContext nm
  evalExpr' ec sv

evalExpr' ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => EvalContext sbe -> SymValue (SBETerm sbe) -> Simulator sbe m (SBETerm sbe)
evalExpr' ec sv = do
  mr <- liftIO $ runErrorT $ evalExprImpl ec sv
  case mr of
    Left (FailRsn fr) -> errorPath fr
    Right v -> return v

evalExprImpl :: EvalContext sbe -> SymValue (SBETerm sbe) -> ErrorT FailRsn IO (SBETerm sbe)
evalExprImpl ec sv =
  case sv of
    SValIdent i -> 
      case evalRegs ec of
        Just regs ->
          case regs^.at i of
            Just (x,_)  -> return x
            Nothing -> throwError $ FailRsn $
               "ILLEGAL: evalExpr' could not find register: "
                ++ show (ppIdent i) ++ " in " ++ show (M.keys regs)
        Nothing -> error $ "evalExpr' called by " ++ evalContextName ec ++ " with missing frame."
    SValSymbol sym -> do
      case evalGlobals ec ^. at sym of
        Just t -> return t
        Nothing -> error "evalExp' called with missing global symbol."
    SValExpr _ (ExprEvalFn fn) -> fn (evalExprImpl ec)

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

-- | Execute a single LLVM-Sym AST instruction
step ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => SymStmt (SBETerm sbe) -> Simulator sbe m ()

step (PushCallFrame callee args mres retTgt) = do
  ec <- getCurrentEvalContext "PushCallFrame"
  calleeSym <- 
    case callee of
      SValSymbol sym -> return sym
      _ -> do
        fp <- evalExpr' ec callee
        r <- resolveFunPtrTerm fp
        case r of
          LookupResult sym -> return sym
          _ -> do
            sbe <- gets symBE
            errorPath $ "PushCallFrame: Failed to resolve callee function pointer: "
                        ++ show (ppSymValue callee) ++ "\n"
                        ++ show r ++ "\n"
                        ++ show (prettyTermD sbe fp)
  argTerms <- (traverse. _2) (evalExpr' ec) args
  void $ callDefine' False retTgt calleeSym mres argTerms

step (Return mtv) = do
  sbe <- gets symBE
  mrv <- traverse (evalExpr "mergeReturn") mtv
  withActiveCS (returnCurrentPath sbe mrv)

step (PushPendingExecution bid cond ml elseStmts) = do
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
   Just True  -> setCurrentBlock bid
   -- Don't bother with elseStmts if negated condition is unsat
   Nothing | fsat == UnSat -> setCurrentBlock bid
   -- Don't bother with pending path as condition is false.
   Just False -> runStmts elseStmts
   -- Don't bother with pending path as condition is unsat
   Nothing | tsat == UnSat -> runStmts elseStmts
   Nothing -> do
     nm <- use pathCounter
     pathCounter += 1
     withActiveCS $ \cs ->
       ActiveCS <$> addCtrlBranch sbe c bid nm ml cs
     runStmts elseStmts

step (SetCurrentBlock bid) = setCurrentBlock bid

step (Assign reg tp (Val tv)) = do
  assignReg reg tp =<< evalExpr "eval@Val" tv

step (Assign reg tp (Alloca ty msztv a)) = do
  assignReg reg tp =<<
    case msztv of
      Just (w,v) -> do
        sizeTm <- evalExpr "alloca" v
        alloca ty w sizeTm a
      Nothing -> do
        aw <- ptrBitwidth <$> getDL
        sbe <- gets symBE
        sizeTm <- liftSBE $ termInt sbe aw 1
        alloca ty aw sizeTm a

step (Assign reg tp (Load v ty a)) = do
  addrTerm <- evalExpr "load" v
  dumpMem 6 "load pre"
  val <- load ty addrTerm a
  dumpMem 6 "load post"
  assignReg reg tp val

step (Store valType val addr a) = do
  whenVerbosity (<=6) $ dumpMem 6 "store pre"
  ec <- getCurrentEvalContext "store"
  valTerm  <- evalExpr' ec val
  addrTerm <- evalExpr' ec addr
  store valType valTerm addrTerm a
  whenVerbosity (<=6) $ dumpMem 6 "store post"

step Unreachable
  = error "step: Encountered 'unreachable' instruction"

step s@BadSymStmt{} = do
  errorPath $ "Path execution encountered unsupported statement:\n"
            ++ show (ppStmt s)

--------------------------------------------------------------------------------
-- Callbacks and event handlers

cb1 :: (Functor m, Monad m)
  => (SEH sbe m -> a -> Simulator sbe m ()) -> a -> Simulator sbe m ()
cb1 f x   = join $ gets (f . view evHandlers) <*> pure x

cb2 :: (Functor m, Monad m)
  => (SEH sbe m -> a -> b -> Simulator sbe m ()) -> a -> b -> Simulator sbe m ()
cb2 f x y = join $ gets (f . view evHandlers) <*> pure x <*> pure y

defaultSEH :: Monad m => SEH sbe m
defaultSEH = SEH
               (return ())
               (\_   -> return ())
               (\_   -> return ())
               (\_   -> return ())
               (\_   -> return ())
               (\_   -> return ())
               (\_ _ -> return ())
               (\_ _ -> return ())

--------------------------------------------------------------------------------
-- Memory operation helpers

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
      setMem m'
      let fr =  memFailRsn sbe ("Failed malloc allocation of type " ++ show (ppMemType ty)) []
      t <$ processMemCond fr c

--------------------------------------------------------------------------------
-- Misc utility functions

setSEH :: Monad m => SEH sbe m -> Simulator sbe m ()
setSEH seh = evHandlers .= seh

runStmts ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => [SymStmt (SBETerm sbe)] -> Simulator sbe m ()
runStmts = mapM_ dbugStep

entryRsltReg :: Ident
entryRsltReg = Ident "__galois_final_rslt"

--------------------------------------------------------------------------------
-- Debugging

dbugStep ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => SymStmt (SBETerm sbe) -> Simulator sbe m ()
dbugStep stmt = do
  mp <- getPath
  case mp of
    Nothing -> dbugM' 2 $ "Executing: (no current path): " ++ show (ppStmt stmt)
    Just p  -> do
      dbugM' 2 $ "Executing ("
                 ++ "#" ++ show (pathName p) ++ "): "
                 ++ show (ppSymbol (pathFuncSym p))
                 ++ maybe "" (show . parens . ppSymBlockID) (pathCB p)
                 ++ ": " ++
                 case stmt of
                   PushPendingExecution{} -> "\n"
                   _ -> ""
                 ++ show (ppStmt stmt)
--  repl
  cb1 (view onPreStep) stmt
  step stmt
  cb1 onPostStep stmt
  whenVerbosity (>=5) dumpCtrlStk

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
  let prefix =
        case mp of
          Nothing -> ""
          Just p -> case pathCB p of
                      Nothing -> " at " ++ fn
                      Just cb -> " at " ++ fn ++ ":" ++ cbid
                        where cbid = show (ppSymBlockID cb)
            where fn = show $ ppSymbol $ pathFuncSym p
  liftIO $ putStrLn $ "Warning" ++ prefix ++ ". " ++ msg

data PrintfFlags = PrintfFlags {
    zeroPad :: Bool
  }

printfToString :: forall sbe m . (Functor sbe, Functor m, MonadIO m)
               => String -> [(MemType,SBETerm sbe)] -> Simulator sbe m String
printfToString fmt args = do
    let vargs = V.fromList args

    let valueAt :: Int -> Simulator sbe m (MemType,SBETerm sbe)
        valueAt p = maybe (errorPath msg) return (vargs V.!? p)
          where msg = "Could not get argument at position " ++ show p
    sbe <- gets symBE
    let badArg p = errorPath $ "printf given bad argument at position " ++ show p
    let pr :: ((MemType, SBETerm sbe) -> Maybe String)
           -> Int -> Simulator sbe m String
        pr f p = maybe (badArg p) return . f =<< valueAt p
    let fmtSigned (IntType w, v) = Just $
          case asSignedInteger sbe w v of
            Just cv  -> show cv
            Nothing -> show (prettyTermD sbe v)
        fmtSigned _ = Nothing
    let fmtUnsigned (IntType w, v) = Just $
          case asUnsignedInteger sbe w v of
            Just cv -> show cv
            Nothing -> show (prettyTermD sbe v)
        fmtUnsigned _ = Nothing
    let fmtPointer (PtrType{}, v) = Just $
          case asConcretePtr sbe v of
            Just cv  -> "0x" ++ showHex cv ""
            Nothing -> show (prettyTermD sbe v)
        fmtPointer _ = Nothing
    let printString p = do
          mv <- valueAt p
          case mv of
            (PtrType{}, v) -> loadString "printToString" v
            _ -> badArg p
    let procString ('%':r) p rs = procArg r defaultFlags p rs
          where defaultFlags = PrintfFlags { zeroPad = False }
        procString (c:r) p rs = procString r p (c:rs)
        procString [] _ rs = return (reverse rs)

        procArg ('d':r) _ p rs = procRest r (p+1) rs =<< pr fmtSigned p
        procArg ('i':r) _ p rs = procRest r (p+1) rs =<< pr fmtSigned p
        procArg ('p':r) _ p rs = procRest r (p+1) rs =<< pr fmtPointer p
        procArg ('u':r) _ p rs = procRest r (p+1) rs =<< pr fmtUnsigned p
        procArg ('s':r) _ p rs = procRest r (p+1) rs =<< printString p
        procArg r       _ _ _  = errorPath $ "Unsupported format string " ++ show r

        procRest r p rs s = procString r p (reverse s ++ rs)
    procString fmt 0 []

printfHandler :: StdOvd sbe m
printfHandler = override $ \args ->
  case args of
    ((_,fmtPtr) : rest) -> do
      fmtStr <- loadString "printf format string" fmtPtr
      --isSym <- withSBE' isSymbolic
      --ptrWidth <- withLC llvmAddrWidthBits
      --let fmtStr' = fixFormat (ptrWidth `div` 4) (map isSym rest) fmtStr
      --resString <- symPrintf fmtStr' <$> mapM termToArg rest
      resString <- printfToString fmtStr rest
      unlessQuiet $ liftIO $ putStr resString
      withSBE $ \s -> termInt s 32 (toInteger (length resString))
    _ -> wrongArguments "printf"

mallocHandler :: BitWidth -> StdOvd m sbe
mallocHandler aw = override $ \args ->
  case args of
    [(_,sizeTm)] -> malloc i8 aw sizeTm
    _ -> wrongArguments "malloc"

allocaHandler :: BitWidth -> StdOvd m sbe
allocaHandler aw = override $ \args ->
  case args of
    [(_,sizeTm)] -> alloca i8 aw sizeTm 0
    _ -> wrongArguments "alloca"

assertHandler__assert_rtn :: StdOvd sbe m
assertHandler__assert_rtn = voidOverride $ \args -> do
  case args of
    [(_,v1), (_,v2), (_,v3), (_,v4)] -> do
          fname     <- loadString "assert function" v1
          file      <- loadString "assert filename" v2
          sbe <- gets symBE
          let Just line = asSignedInteger sbe undefined v3
          err       <- loadString "assert error message" v4
          errorPath $ unwords [ "__assert_rtn:"
                      , file ++ ":" ++ show line ++ ":" ++ fname ++ ":"
                      , err
                      ]
    _ -> errorPath "Incorrect number of parameters passed to __assert_rtn()"

registerLibcOverrides :: (Functor m, MonadIO m, Functor sbe) => Simulator sbe m ()
registerLibcOverrides = do
  aw <- ptrBitwidth <$> getDL
  -- Register malloc
  tryRegisterOverride "malloc" $ \d -> (\_ -> mallocHandler aw) <$> fdRetType d
  let sizeT = IntType aw
  registerOverrides
    [ ("__assert_rtn", voidFunDecl [i8p, i8p, i32, i8p], assertHandler__assert_rtn)
    --, ("exit", voidTy, [i32], False, exitHandler)
    , ("alloca", funDecl i8p [sizeT], allocaHandler aw)
    , ("free", voidFunDecl [i8p], voidOverride $ \_ -> return ())
    , ("printf", FunDecl (Just i32) [strTy] True, printfHandler)
    ]
