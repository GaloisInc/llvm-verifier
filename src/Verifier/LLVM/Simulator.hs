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
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE OverloadedStrings     #-}

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
  , EvalContext(..)
  , evalExpr'
  , prettyTermSBE
  , runSimulator
  , liftSBE
  , withSBE
  , withSBE'
  -- * Memory operations
  , alloca
  , load
  , store
  , sizeof
  , processMemCond
  -- for testing
  , dbugM
  , dbugTerm
  , dumpMem
  , getMem
  , setSEH
  , withDL
  , warning
  , i8
  , i32
  , i64
  , i8p
  , i32p
  )
where

import           Control.Applicative
import qualified Control.Exception         as CE
import           Control.Lens hiding (act,from)
import           Control.Monad.Error
import           Control.Monad.State       hiding (State)
import           Data.List                 (isPrefixOf, nub)
import qualified Data.Map                  as M
import           Data.Maybe
import           Data.String
import qualified Data.Vector               as V
import           Numeric                   (showHex)
import           System.Exit
import           System.IO
import           Text.PrettyPrint

import           Verifier.LLVM.AST
import           Verifier.LLVM.Backend
import           Verifier.LLVM.Codebase
import           Verifier.LLVM.Simulator.Common
import           Verifier.LLVM.Simulator.SimUtils
import           Verifier.LLVM.Utils

-- Utility functions

withActiveCS :: (MonadIO m)
             => (ActiveCS sbe -> IO (CS sbe))
             -> Simulator sbe m ()
withActiveCS f = do
  Just (ActiveCS cs) <- use ctrlStk
  cs' <- liftIO (f cs)
  ctrlStk ?= cs'

-- | Traversal for current path of simulator state.
currentPathOfState :: Simple Traversal (State sbe m) (Path sbe)
currentPathOfState = ctrlStk . _Just . currentPath

-- | Obtain the first pending path in the topmost merge frame; @Nothing@ means
-- that the control stack is empty or the top entry of the control stack has no
-- pending paths recorded.
getPath :: (Functor m, Monad m)
        => Simulator sbe m (Maybe (Path sbe))
getPath = preuse currentPathOfState

modifyPathRegs :: (Functor m, Monad m)
                => (RegMap (SBETerm sbe) -> RegMap (SBETerm sbe)) -> Simulator sbe m ()
modifyPathRegs f = do
  Just p <- preuse currentPathOfState
  currentPathOfState .= over pathRegs f p

-- | Traversal for current path memory if any.
currentPathMem :: Simple Traversal (State sbe m) (SBEMemory sbe)
currentPathMem = currentPathOfState . pathMem

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
  => Codebase              -- ^ Post-transform LLVM code, memory alignment, and
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
                    , _fnOverrides = M.empty
                    , _evHandlers  = seh
                    , _errorPaths  = []
                    , _pathCounter = 1
                    , _aigOutputs  = []
                    }
  ea <- flip evalStateT newSt $ runErrorT $ runSM $ do
    initGlobals
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
       let argTys    = snd <$> sdArgs d
           sym       = sdName d
           fty       = FunType $ FunDecl (sdRetType d) argTys False
           idl       = nub $ mapMaybe symBlockLabel $ M.keys (sdBody d)
           noCodeSpc = "Not enough space in code memory to allocate new definition."
       insertGlobalTerm noCodeSpc sym fty $ \s m -> memAddDefine s m sym idl
  -- Add symbol for declarations.
  do declares <- cbUndefinedFns . codebase <$> get
     forM_ declares $ \(sym,d) -> do
       let errMsg = "Insufficient space for new declaration."
       insertGlobalTerm errMsg sym (FunType d) $ \s m -> memAddDefine s m sym []
  -- Initialize global data
  do let globals = [ g | (_,Left g) <- M.toList nms]
     let ec = EvalContext "addGlobal" Nothing
     forM_ globals $ \g -> do
       cb1 onMkGlobTerm g
       cdata <- evalExpr' ec (globalValue g)
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
  unless (t == sdRetType def) $
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
  override <- use (fnOverrides . at calleeSym)
  case override of
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
          intrinsic calleeName mreg (snd <$> args)
          setCurrentBlock normalRetID
          return []
      | otherwise = do
          runNormalSymbol normalRetID calleeSym mreg (snd <$> args)

-- | Return symbol definition with given name or fail.
lookupSymbolDef :: (Functor m, MonadIO m, Functor sbe)
                => Symbol -> Simulator sbe m SymDefine
lookupSymbolDef sym = do
  mdef <- lookupDefine sym <$> gets codebase
  case mdef of
    Just def -> return def
    Nothing  -> do
      errorPath $ FailRsn $ "Failed to find definition for symbol "
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
     let fr = FailRsn "Stack push frame failure: insufficient stack space"
     processMemCond fr c
  return args
  where
    err doc = error $ "callDefine/bindArgs: " ++ render doc

    bindArgs formals actuals
      | length formals /= length actuals =
          err $ text "incorrect number of actual parameters"
            $+$ text "formals: " <> text (show (fst <$> formals))
      | otherwise =
          foldr (uncurry bindArg) M.empty (formals `zip` actuals)
    bindArg (r,tp) v = M.insert r (v,tp)

intrinsic ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => String -> Maybe (MemType, Ident) -> [SBETerm sbe]
  -> Simulator sbe m ()
intrinsic intr mreg args0 =
  case (intr, mreg, args0) of
    ("llvm.memcpy.p0i8.p0i8.i64", Nothing, [dst, src, len, align, _isvol]) ->
      memcpy dst src len align
    ("llvm.memset.p0i8.i64", Nothing, [dst, val, len, align, _]) ->
      memSet dst val 64 len align
    ("llvm.uadd.with.overflow.i64", Just reg, [x, y]) ->
      uaddWithOverflow 64 reg x y
    ("llvm.objectsize.i32", Just reg, _)         -> objSz 32 reg
    ("llvm.objectsize.i64", Just reg, _)         -> objSz 64 reg
    -- Do nothing.
    ("llvm.lifetime.start", Nothing,_) -> return ()
    ("llvm.lifetime.end", Nothing,_) -> return ()
    _ -> whenVerbosity (>= 1) $ do --TODO: Give option of stopping on warnings like this.
      tellUser $ "Warning: skipping unsupported LLVM intrinsic " ++ show intr     
  where
    memcpy dst src len align = do
      Just m <- preuse currentPathMem
      (c,m') <- withSBE $ \sbe -> memCopy sbe m dst src len align
      currentPathMem .= m'
      sbe <- gets symBE
      let pts = map (prettyTermD sbe) [dst,src,len]
      let fr = FailRsn $
                 "memcopy operation was not valid: (dst,src,len) = "
                   ++ show (parens $ hcat $ punctuate comma $ pts)
      processMemCond fr c
    uaddWithOverflow w (tp,reg) x y= assignReg reg tp =<< withSBE fn
      where fn sbe = applyTypedExpr sbe (UAddWithOverflow w x y)
    objSz w (tp,reg) = do
      let [_ptr, maxOrMin] = args0
      sbe <- gets symBE
      let mval = asUnsignedInteger sbe 1 maxOrMin
      case mval of
        Nothing -> errorPath $ FailRsn $ "llvm.objectsize.i{32,64} expects concrete 2nd parameter"
        Just v  -> assignReg reg tp =<< withSBE (\s -> termInt s w tv)
          where tv = if v == 0 then -1 else 0

memSet :: ( Functor m, MonadIO m
          , Functor sbe
          ) =>
          SBETerm sbe -- Destination pointer.
       -> SBETerm sbe -- Value (i8)
       -> BitWidth    -- Width of length
       -> SBETerm sbe -- Length
       -> SBETerm sbe -- Alignment (i32)
       -> Simulator sbe m ()
memSet dst val lenWidth len align = do
  sbe <- gets symBE
  let lenVal = asUnsignedInteger sbe undefined len
  case lenVal of
    Just 0 -> return ()
    _ -> do
      store i8 val dst
      negone   <- liftSBE  $ termInt sbe lenWidth (-1)
      dst'     <- ptrInc dst
      len'     <- termAdd lenWidth len negone
      memSet dst' val lenWidth len' align

finalRetValOfPath :: Simple Traversal (Path sbe) (SBETerm sbe)
finalRetValOfPath = pathRegs . at entryRsltReg . _Just . _1

getProgramReturnValue :: (Monad m, Functor m)
                      => Simulator sbe m (Maybe (SBETerm sbe))
getProgramReturnValue = preuse $ currentPathOfState . finalRetValOfPath

getProgramFinalMem :: (Monad m, Functor m)
                   => Simulator sbe m (Maybe (SBEMemory sbe))
getProgramFinalMem = preuse currentPathMem

-- Handle a condition returned by the memory model
processMemCond ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => -- Maybe PMCInfo ->
    FailRsn -> SBEPred sbe -> Simulator sbe m ()
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
          errorPath $ FailRsn $ "This path is infeasible"
        let sym = pathFuncSym p
        Just def <- lookupDefine sym <$> gets codebase
        runStmts $ sbStmts $ lookupSymBlock def pcb
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
evalCond :: (Functor sbe, Functor m, MonadIO m) => SymCond -> Simulator sbe m (SBEPred sbe)
evalCond (HasConstValue t w i) = do
  v <- evalExpr "evalCond" t
  sbe <- gets symBE
  iv <- liftSBE $ termInt sbe w i
  liftSBE $ applyIEq sbe w v iv

data EvalContext sbe = EvalContext {
       evalContextName :: String
     , evalRegs :: Maybe (RegMap (SBETerm sbe))
     }

getCurrentEvalContext :: (Functor m, Monad m) => String -> Simulator sbe m (EvalContext sbe)
getCurrentEvalContext nm = EvalContext nm . fmap (view pathRegs) <$> getPath

evalExpr :: (Functor m, MonadIO m, Functor sbe) 
         => String -> TypedSymValue -> Simulator sbe m (SBETerm sbe)
evalExpr nm symValue = do
  ec <- getCurrentEvalContext nm
  evalExpr' ec symValue

evalExpr' ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => EvalContext sbe -> TypedSymValue -> Simulator sbe m (SBETerm sbe)
evalExpr' ec tsv =
  case tsv of
    SValIdent i -> 
      case evalRegs ec of
        Just regs ->
          case regs^.at i of
            Just (x,_)  -> return x
            Nothing -> illegal $ "evalExpr' could not find register: "
                         ++ show (ppIdent i) ++ " in " ++ show (M.keys regs)
        Nothing -> error $ "evalExpr' called by " ++ evalContextName ec
                      ++ " with missing frame."
    SValSymbol sym -> do
      Just t <- use (globalTerms . at sym)
      return t
    SValExpr te -> do
      tv <- traverse (evalExpr' ec) te
      sbe <- gets symBE
      liftSBE $ applyTypedExpr sbe tv

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
    Nothing -> errorPath (FailRsn errMsg)
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
  => SymStmt -> Simulator sbe m ()

step (PushCallFrame callee args mres retTgt) = do
  ec <- getCurrentEvalContext "PushCallFrame"
  argTerms <- (traverse. _2) (evalExpr' ec) args
  calleeSym <- 
    case callee of
      SValSymbol sym -> return sym
      _ -> do
        fp <- evalExpr' ec callee
        r <- resolveFunPtrTerm fp
        case r of
          Result sym -> return sym
          _ -> do
            sbe <- gets symBE
            errorPath $ FailRsn $ "PushCallFrame: Failed to resolve callee function pointer: "
                        ++ show (ppTypedSymValue callee) ++ "\n"
                        ++ show r ++ "\n"
                        ++ show (prettyTermD sbe fp)
  void $ callDefine' False retTgt calleeSym mres argTerms

step (Return mtv) = do
  sbe <- gets symBE
  mrv <- traverse (evalExpr "mergeReturn") mtv
  withActiveCS (returnCurrentPath sbe mrv)

step (PushPendingExecution bid cond ml elseStmts) = do
  sbe <- gets symBE
  c <- evalCond cond
  case asBool sbe c of
   -- Don't bother with elseStmts as condition is true. q
   Just True  -> setCurrentBlock bid
   -- Don't bother with pending path as condition is false.
   Just False -> runStmts elseStmts
   Nothing -> do
     nm <- use pathCounter
     pathCounter += 1
     withActiveCS $ \cs ->
       ActiveCS <$> addCtrlBranch sbe c bid nm ml cs
     runStmts elseStmts

step (SetCurrentBlock bid) = setCurrentBlock bid

step (Assign reg tp (Val tv)) = do
  assignReg reg tp =<< evalExpr "eval@Val" tv

step (Assign reg tp (Alloca ty msztv malign)) = do
  assignReg reg tp =<<
    case msztv of
      Just (w,v) -> do
        sizeTm <- evalExpr "alloca" v
        alloca ty w sizeTm malign
      Nothing -> do
        aw <- withDL ptrBitwidth
        sbe <- gets symBE
        sizeTm <- liftSBE $ termInt sbe aw 1
        alloca ty aw sizeTm malign

step (Assign reg tp (Load v ty _malign)) = do
  addrTerm <- evalExpr "load" v
  dumpMem 6 "load pre"
  val <- load ty addrTerm
  dumpMem 6 "load post"
  assignReg reg tp val

step (Store valType val addr _malign) = do
  whenVerbosity (<=6) $ dumpMem 6 "store pre"
  ec <- getCurrentEvalContext "store"
  valTerm  <- evalExpr' ec val
  addrTerm <- evalExpr' ec addr
  store valType valTerm addrTerm
  whenVerbosity (<=6) $ dumpMem 6 "store post"

step Unreachable
  = error "step: Encountered 'unreachable' instruction"

step s@BadSymStmt{} = unimpl (show (ppStmt s))

--------------------------------------------------------------------------------
-- Symbolic expression evaluation

-----------------------------------------------------------------------------------------
-- Term operations and helpers

ptrInc :: (Functor m, MonadIO m)
        => SBETerm sbe -> Simulator sbe m (SBETerm sbe)
ptrInc x = do
  sbe <- gets symBE
  w <- withDL ptrBitwidth
  y <- liftSBE $ termInt sbe w 1
  liftSBE $ applyTypedExpr sbe (PtrAdd x y)

termAdd :: (Functor m, Monad m)
        => BitWidth -> SBETerm sbe -> SBETerm sbe -> Simulator sbe m (SBETerm sbe)
termAdd w x y = withSBE $ \sbe -> applyTypedExpr sbe (IntArith (Add False False) Nothing w x y)

--------------------------------------------------------------------------------
-- SBE lifters and helpers

liftSBE :: Monad m => sbe a -> Simulator sbe m a
liftSBE sa = gets liftSymBE >>= \f -> f sa

withSBE :: (Functor m, Monad m) => (SBE sbe -> sbe a) -> Simulator sbe m a
withSBE f = gets symBE >>= \sbe -> liftSBE (f sbe)

withSBE' :: (Functor m, Monad m) => (SBE sbe -> a) -> Simulator sbe m a
withSBE' f = gets symBE >>= \sbe -> return (f sbe)

getDL :: Monad m => Simulator sbe m DataLayout
getDL = gets (cbDataLayout . codebase)

withDL :: (Functor m, MonadIO m) => (DataLayout -> a) -> Simulator sbe m a
withDL f = f <$> getDL

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
               (\_ _ -> return ())
               (\_ _ -> return ())

--------------------------------------------------------------------------------
-- Memory operation helpers

alloca ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => MemType
  -> BitWidth
  -> SBETerm sbe
  -> Maybe Int
  -> Simulator sbe m (SBETerm sbe)
alloca ty szw sztm malign = do
  Just m <- preuse currentPathMem
  sbe <- gets symBE
  rslt <- liftSBE $ stackAlloc sbe m ty szw sztm (maybe 0 lg malign)
  case rslt of
    ASymbolicCountUnsupported  -> errorPath $ FailRsn $
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
  -> Maybe Int -- ^ Alignment
  -> Simulator sbe m (SBETerm sbe)
malloc ty szw sztm malign  = do
  sbe <- gets symBE
  Just m <- preuse currentPathMem
  rslt <- liftSBE $ heapAlloc sbe m ty szw sztm (maybe 0 lg malign)
  case rslt of
    ASymbolicCountUnsupported -> errorPath $ FailRsn $
      "malloc only supports concrete element count "
        ++ "(try a different memory model?)"
    AResult c t m' -> do
      setMem m'
      let fr =  memFailRsn sbe ("Failed malloc allocation of type " ++ show (ppMemType ty)) []
      t <$ processMemCond fr c

-- | Load value at addr in current path.
load ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => MemType -> SBETerm sbe -> Simulator sbe m (SBETerm sbe)
load tp addr = do
  sbe <- gets symBE
  Just mem <- preuse currentPathMem
  (cond, v) <- liftSBE $ memLoad sbe mem tp addr
  let fr = memFailRsn sbe "Invalid load address" [addr]
  processMemCond fr cond
  return v

store ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => MemType -> SBETerm sbe -> SBETerm sbe -> Simulator sbe m ()
store tp val dst = do
  sbe <- gets symBE

  Just m <- preuse currentPathMem
  (c, m') <- liftSBE $ memStore sbe m dst tp val
  currentPathMem .= m'

  let fr = memFailRsn sbe "Invalid store address: " [dst]
  processMemCond fr c

memFailRsn :: SBE sbe -> String -> [SBETerm sbe] -> FailRsn
memFailRsn sbe desc terms = FailRsn $ show $ text desc <+> ppTuple pts
  --TODO: See if we can get a reasonable location for the failure.
  where pts = map (prettyTermD sbe) terms

--------------------------------------------------------------------------------
-- Misc utility functions

setSEH :: Monad m => SEH sbe m -> Simulator sbe m ()
setSEH seh = evHandlers .= seh

unlessQuiet :: MonadIO m => Simulator sbe m () -> Simulator sbe m ()
unlessQuiet act = getVerbosity >>= \v -> unless (v == 0) act

-- For user feedback that gets silenced when verbosity = 0.
tellUser :: (MonadIO m) => String -> Simulator sbe m ()
tellUser msg = unlessQuiet $ dbugM msg

-- | Returns the a term representing the target-specific number of bytes
-- required to store a value of the given type.
-- The result has width llvmAddrWidthBits
sizeof :: (MonadIO m, Functor m) => MemType -> Simulator sbe m TypedSymValue
sizeof ty = withDL $ \dl ->
  SValExpr $ SValInteger (ptrBitwidth dl) (toInteger (memTypeSize dl ty))

resolveFunPtrTerm ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => SBETerm sbe -> Simulator sbe m LookupSymbolResult
resolveFunPtrTerm fp = do
  Just m <- preuse currentPathMem
  withSBE $ \s -> codeLookupSymbol s m fp

runStmts ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => [SymStmt] -> Simulator sbe m ()
runStmts = mapM_ dbugStep

entryRsltReg :: Ident
entryRsltReg = Ident "__galois_final_rslt"

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
  unless (fargs == targs) $ e $ "has different argument types.\n" 
    ++ "  Argument types of " ++ nm fnm ++ ": " ++ ppTypes fargs ++ "\n"
    ++ "  Argument types of " ++ tnm ++ ": " ++ ppTypes targs ++ "\n"
  unless (frtn == trtn) $ e $ "has a different return type.\n"
    ++ "  Return type of " ++ nm fnm ++ ": " ++ show (ppRetType frtn) ++ "\n"
    ++ "  Return type of " ++ tnm ++ ": " ++ show (ppRetType trtn) ++ "\n"
  when (fva && not tva) $ e $ "does not accept varargs.\n"
  when (not fva && tva) $ e $ "allows varargs.\n"

registerOverride ::
  ( Functor m
  , Functor sbe
  , MonadIO m
  )
  => Symbol
  -> FunDecl 
  -> Override sbe m
  -> Simulator sbe m ()
registerOverride sym decl handler = do
  cb <- gets codebase
  case cb^.cbFunctionType sym of
    Nothing -> return ()
    Just fd -> do
      checkTypeCompat sym fd "override" decl -- (FunDecl retTy argTys)
      fnOverrides . at sym ?= (handler, False)

--------------------------------------------------------------------------------
-- Error handling

unimpl :: (MonadIO m, Functor m, Functor sbe) => String -> Simulator sbe m a
unimpl msg  = errorPath $ FailRsn $ "UN{SUPPORTED,IMPLEMENTED}: " ++ msg

illegal :: (MonadIO m, Functor m, Functor sbe) => String -> Simulator sbe m a
illegal msg = errorPath $ FailRsn $ "ILLEGAL: " ++ msg

errorPath ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => FailRsn -> Simulator sbe m a
errorPath rsn = do
  s <- get
  let sbe = symBE s
  let Just (ActiveCS cs) = s^.ctrlStk
  let p = cs^.activePath
  -- Log error path  
  whenVerbosity (>=3) $ do
    dbugM $ "Error path encountered: " ++ show (ppFailRsn rsn)
    dbugM $ show $ ppPath sbe p
  mcs <- liftIO $ markCurrentPathAsError sbe cs
  let s' = s & ctrlStk .~ mcs
             & errorPaths %~ (EP rsn p:)
  -- Merge negation of assumptions in current path into conditions on merge frame.
  -- NB: Since we've set up the control stack for the next invocation of
  -- run, and explicitly captured the error path, we need to be sure to
  -- ship that modified state back to the catch site so it execution can
  -- continue correctly.
  throwError $ ErrorPathExc rsn s'

--------------------------------------------------------------------------------
-- Debugging

prettyTermSBE :: (Functor m, Monad m) => SBETerm sbe -> Simulator sbe m Doc
prettyTermSBE t = withSBE' $ \s -> prettyTermD s t

dumpMem :: (Functor m, MonadIO m) => Int -> String -> Simulator sbe m ()
dumpMem v msg =
  whenVerbosity (>=v) $ do
    dbugM $ msg ++ ":"
    Just m <- preuse currentPathMem
    withSBE (\s -> memDump s m Nothing)

dbugStep ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => SymStmt -> Simulator sbe m ()
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
  cb1 onPreStep stmt
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
dbugTerm desc t = dbugM =<< ((++) (desc ++ ": ")) . render <$> prettyTermSBE t

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
          t <- load i8 addr
          sbe <- gets symBE
          addr' <- ptrInc addr
          case asUnsignedInteger sbe undefined t of
            Nothing -> do
              errorPath $ FailRsn $
                 "Encountered a symbolic byte in " ++ nm ++ "."
            Just 0 -> return []
            Just v -> (v:) <$> go addr'

termIntS :: (Functor m, Monad m, Integral a) =>
            Int -> a -> Simulator sbe m (SBETerm sbe)
termIntS w n = withSBE $ \s -> termInt s w (fromIntegral n)

data PrintfFlags = PrintfFlags {
    zeroPad :: Bool
  }

printfToString :: forall sbe m . (Functor sbe, Functor m, MonadIO m)
               => String -> [(MemType,SBETerm sbe)] -> Simulator sbe m String
printfToString fmt args = do
    let vargs = V.fromList args

    let e = errorPath . FailRsn
    let valueAt :: Int -> Simulator sbe m (MemType,SBETerm sbe)
        valueAt p = maybe (e msg) return (vargs V.!? p)
          where msg = "Could not get argument at position " ++ show p
    sbe <- gets symBE
    let badArg p = e $ "printf given bad argument at position " ++ show p
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
        procArg r       _ _ _  = e $ "Unsupported format string " ++ show r

        procRest r p rs s = procString r p (reverse s ++ rs)
    procString fmt 0 []





printfHandler :: StdOvd sbe m
printfHandler = Override $ \_sym _rty args ->
  case args of
    ((_,fmtPtr) : rest) -> do
      fmtStr <- loadString "printf format string" fmtPtr
      --isSym <- withSBE' isSymbolic
      --ptrWidth <- withLC llvmAddrWidthBits
      --let fmtStr' = fixFormat (ptrWidth `div` 4) (map isSym rest) fmtStr
      --resString <- symPrintf fmtStr' <$> mapM termToArg rest
      resString <- printfToString fmtStr rest
      unlessQuiet $ liftIO $ putStr resString
      Just <$> termIntS 32 (length resString)
    _ -> errorPath $ FailRsn "printf called with no arguments"

printSymbolic :: StdOvd sbe m
printSymbolic = Override $ \_sym _rty args ->
  case args of
    [(_,ptr)] -> do
      v <- load i8 ptr
      d <- withSBE' $ \sbe -> prettyTermD sbe v
      liftIO $ print d
      return Nothing
    _ -> errorPath $ FailRsn "lss_print_symbolic: wrong number of arguments"

allocHandler :: (Functor m, Monad m, MonadIO m, Functor sbe)
             => BitWidth
             -> (MemType
                   -> BitWidth
                   -> SBETerm sbe
                   -> Maybe Int
                   -> Simulator sbe m (SBETerm sbe))
             -> Override sbe m
allocHandler aw fn = Override $ \_sym _rty args ->
  case args of
    [(_,sizeTm)] -> Just <$> fn i8 aw sizeTm Nothing
    _ -> e "alloca: wrong number of arguments"
  where
    e = errorPath . FailRsn

abortHandler :: StdOvd sbe m
abortHandler = Override $ \_sym _rty args -> do
  case args of
    [(_,tv)] -> do
      msg <- loadString "abort message" tv
      e $ "lss_abort(): " ++ msg
    _ -> e "Incorrect number of parameters passed to lss_abort()"
  where
    e = errorPath . FailRsn

showPathOverride :: StdOvd sbe m
showPathOverride = Override $ \_sym _rty _args -> do
  Just p   <- getPath
  sbe <- gets symBE
  unlessQuiet $ dbugM $ show $ nest 2 $ ppPath sbe p
  return Nothing

showMemOverride :: StdOvd sbe m
showMemOverride = Override $ \_sym _rty _args -> do
  unlessQuiet $ dumpMem 1 "lss_show_mem()"
  return Nothing

userSetVebosityOverride :: StdOvd sbe m
userSetVebosityOverride = Override $ \_sym _rty args -> do
  case args of
    [(_,v)] -> do
      sbe <- gets symBE
      case asUnsignedInteger sbe 32 v of
        Nothing  -> e "symbolic verbosity is illegal"
        Just v'' -> Nothing <$ setVerbosity (fromIntegral v'')
    _ -> e "Incorrect number of parameters passed to lss_set_verbosity"
  where
    e = errorPath . FailRsn

assertHandler__assert_rtn :: StdOvd sbe m
assertHandler__assert_rtn = Override $ \_sym _rty args -> do
  case args of
    [(_,v1), (_,v2), (_,v3), (_,v4)] -> do
          fname     <- loadString "assert function" v1
          file      <- loadString "assert filename" v2
          sbe <- gets symBE
          let Just line = asSignedInteger sbe undefined v3
          err       <- loadString "assert error message" v4
          e $ unwords [ "__assert_rtn:"
                      , file ++ ":" ++ show line ++ ":" ++ fname ++ ":"
                      , err
                      ]
    _ -> e "Incorrect number of parameters passed to __assert_rtn()"
  where
    e = errorPath . FailRsn

freshInt' :: Int -> StdOvd sbe m
freshInt' n = Override $ \_ _ _ -> Just <$> withSBE (flip freshInt n)

-- | @freshIntArray n@ returns an override that yields an array of integers,
-- each with with @n@ bits.
freshIntArray :: Int -> StdOvd sbe m
freshIntArray n = Override $ \_sym _rty args ->
  case args of
    [(_, sizeTm), _, _] -> do
      sbe <- gets symBE
      case asUnsignedInteger sbe 32 sizeTm of
        Just size -> do
          let sz = fromIntegral size
              ety = IntType n
              ty = ArrayType (fromIntegral size) ety
          arrPtr <- alloca ety 32 sizeTm Nothing
          elts <- replicateM sz (withSBE $ flip freshInt n)
          arrTm <- liftSBE $ termArray sbe ety elts
          store ty arrTm arrPtr
          return (Just arrPtr)
        Nothing -> e "lss_fresh_array_uint called with symbolic size"
    _ -> e "lss_fresh_array_uint: wrong number of arguments"
  where
    e = errorPath . FailRsn

checkAigFile ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => String -> Simulator sbe m ()
checkAigFile filename = do
  eab <- liftIO $ CE.try (openFile filename WriteMode)
  case eab of
    Left (e :: CE.SomeException) -> errorPath $ FailRsn $ "checkAigFile: " ++ show e
    Right h                       -> liftIO $ hClose h

writeIntAiger :: StdOvd sbe m
writeIntAiger = Override $ \_sym _rty args ->
  case args of
    [(_,t), (_,fptr)] -> do
      file <- loadString "lss_write_aiger_uint file" fptr
      checkAigFile file
      withSBE $ \s -> writeAiger s file [t]
      return Nothing
    _ -> errorPath
         $ FailRsn "lss_write_aiger_uint: wrong number of arguments"

addAigOutput :: StdOvd sbe m
addAigOutput = Override $ \_sym _rty args ->
  case args of
    [(_,t)] -> Nothing <$ (aigOutputs %= (t:))
    _   -> errorPath $ FailRsn "lss_aiger_add_output: wrong number of arguments"

addAigArrayOutput :: MemType -- ^ Type of value target points to.
                  -> StdOvd sbe m
addAigArrayOutput tgtTy = Override $ \_sym _rty args ->
  case args of
    [(_,tptr), (_, sizeTm)] -> do
      sbe <- gets symBE
      case asUnsignedInteger sbe 32 sizeTm of
        Just size -> do
          elems <- loadArray tptr tgtTy size
          arrTm <- liftSBE $ termArray sbe tgtTy elems
          Nothing <$ (aigOutputs %= (arrTm:))
        Nothing -> e "lss_aiger_add_output_array called with symbolic size"
    _ -> e "lss_aiger_add_output_array: wrong number of arguments"
  where
    e = errorPath . FailRsn

writeCollectedAigerOutputs :: StdOvd sbe m
writeCollectedAigerOutputs = Override $ \_sym _rty args ->
  case args of
    [(_,fptr)] -> do
      outputTerms <- reverse <$> use aigOutputs
      if null outputTerms then
        e "lss_write_aiger: no AIG outputs have been collected"
      else do
        file <- loadString "lss_write_aiger file" fptr
        withSBE $ \s -> writeAiger s file outputTerms
        Nothing <$ (aigOutputs .= [])
    _ -> e "lss_write_aiger: wrong number of arguments"
  where
    e = errorPath . FailRsn

writeIntArrayAiger :: MemType -> StdOvd sbe m
writeIntArrayAiger ety = Override $ \_sym _rty args ->
  case args of
    [(PtrType{}, tptr), (IntType sizeW, sizeTm), (PtrType{},fptr)] -> do
      sbe <- gets symBE
      case asUnsignedInteger sbe sizeW sizeTm of
        Just size -> do
          elems <- loadArray tptr ety size
          arrTm <- liftSBE $ termArray sbe ety elems
          file <- loadString "lss_write_aiger_array_uint" fptr
          checkAigFile file
          withSBE $ \s -> writeAiger s file [arrTm]
          return Nothing
        Nothing ->
          e "lss_write_aiger_array_uint called with symbolic size"
    _ -> e "lss_write_aiger_array_uint: wrong number of arguments"
  where
    e = errorPath . FailRsn

writeCNF :: StdOvd sbe m
writeCNF = Override $ \_sym _rty args ->
  case args of
    [(IntType{}, t), (PtrType{},fptr)] -> do
      file <- loadString "lss_write_cnf" fptr
      _ <- withSBE $ \s -> writeCnf s file t
      return Nothing
    _ -> e "lss_write_cnf: wrong number of arguments"
  where
    e = errorPath . FailRsn

loadArray ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => SBETerm sbe -- Term
  -> MemType -- Element type
  -> Integer -- Count
  -> Simulator sbe m [SBETerm sbe]
loadArray ptr tp count = do
    sbe <- gets symBE
    let go i r a 
          | i == 0 = return r
          | otherwise = do
             v <- liftSBE $ applyTypedExpr sbe (GetConstArrayElt c tp a (i-1))
             go (i-1) (v:r) a
    go c [] =<< load (ArrayType (fromInteger count) tp) ptr
  where c = fromInteger count

-- | Attempts to read an array of boolean values from a pointer with the given number
-- of elements.
getEvalInputs :: (Functor m, MonadIO m, Functor sbe)
               => String -- ^ Name of function calling this for error purposes.
              -> SBETerm sbe -- ^ Pointer to input values (should be an i8p).
              -> SBETerm sbe -- ^ Size of array of inputs (should be an int32).
              -> Simulator sbe m [Bool]
getEvalInputs nm p sz = do
  sbe <- gets symBE
  let e = errorPath . FailRsn
  case asUnsignedInteger sbe 32 sz of
    Just csz -> do
      elems <- loadArray p i8 csz
      case traverse (asUnsignedInteger sbe 8) elems of
        Just ints -> return $ (/= 0) <$> ints
        Nothing -> e $ nm ++ ": symbolc inputs not supported."
    Nothing -> e $ nm ++ ": symbolic size not supported."

evalAigerOverride :: StdOvd m sbe
evalAigerOverride =
  Override $ \_sym _rty args ->
    case args of
      [(_,tm), (PtrType{}, p), (IntType 32,szTm)] -> do
        sbe <- gets symBE
        bools <- getEvalInputs "lss_eval_aiger" p szTm
        Just <$> liftSBE (evalAiger sbe bools tm)
      _ -> e "lss_eval_aiger: wrong number of arguments"
  where
    e = errorPath . FailRsn

evalAigerArray :: MemType -- Type of first argument pointer.
               -> StdOvd sbe m
evalAigerArray ty =
  Override $ \_sym _rty args ->
    case args of
      [ (PtrType{}, sym)
       ,(PtrType{}, dst)
       ,(IntType szw, szTm)
       ,(PtrType{},  input)
       ,(IntType 32, inputSz)
       ] -> do
        sbe <- gets symBE
        case asUnsignedInteger sbe szw szTm of
          Just sz -> do
            bools <- getEvalInputs "lss_eval_aiger_array" input inputSz
            tm <- load (ArrayType (fromInteger sz) ty) sym
            res <- liftSBE $ evalAiger sbe bools tm
            store (ArrayType (fromInteger sz) ty) dst res
            return Nothing
          _ -> e "lss_eval_aiger_array: symbolic sizes not supported"
      _ -> e "lss_eval_aiger_array: wrong number of arguments"
  where
    e = errorPath . FailRsn

overrideByName :: StdOvd sbe m
overrideByName = Override $ \_sym _rty args ->
  case args of
    [(PtrType{}, fromNamePtr), (PtrType{}, toNamePtr)] -> do
      src <- fromString <$> loadString "lss_override_function_by_name from" fromNamePtr
      tgt <- fromString <$> loadString "lss_override_function_by_name to" toNamePtr
      src `userRedirectTo` tgt
    _ -> errorPath
         $ FailRsn "lss_override_function_by_name: wrong number of arguments"

overrideByAddr :: StdOvd sbe m
overrideByAddr = Override $ \_sym _rty args ->
  case args of
    [(PtrType{}, fromPtr), (PtrType{}, toPtr)] -> do
      syms <- both resolveFunPtrTerm (fromPtr, toPtr)
      case syms of
        (Result src, Result tgt) -> src `userRedirectTo` tgt
        _                    -> resolveErr
    _ -> argsErr
  where
    e          = errorPath . FailRsn
    resolveErr = e "overrideByAddr: Failed to resolve function pointer"
    argsErr    = e "lss_override_function_by_addr: wrong number of arguments"

overrideIntrinsic :: StdOvd sbe m
overrideIntrinsic = Override $ \_sym _rty args ->
  case args of
    [(PtrType{}, nmPtr), (PtrType{}, fp)] -> do
      nm  <- fromString <$> loadString "lss_override_llvm_intrinsic" nmPtr
      msym <- resolveFunPtrTerm fp
      case msym of
        Result sym -> nm `userRedirectTo` sym
        _ -> e "overrideIntrinsic: Failed to resolve function pointer"
    _ -> e "lss_override_llvm_intrinsic: wrong number of arguments"
  where
    e = errorPath . FailRsn

userRedirectTo :: MonadIO m
  => Symbol -> Symbol -> Simulator sbe m (Maybe (SBETerm sbe))
userRedirectTo src tgt = do
  cb <- gets codebase
  let nameOf = show . ppSymbol 
  --TODO: Add better error messages.
  case (cb^.cbFunctionType src, cb^.cbFunctionType tgt) of
    (Nothing,_) -> error $ "Could not find symbol " ++ nameOf src ++ "."
    (_,Nothing) -> error $ "Could not find symbol " ++ nameOf tgt ++ "."  
    (Just fd, Just td) -> do
      checkTypeCompat src fd (show (ppSymbol tgt)) td
      fnOverrides . at src ?= (Redirect tgt, True)
      return Nothing

overrideResetByName :: StdOvd sbe m
overrideResetByName = Override $ \_sym _rty args ->
  case args of
    [(PtrType{}, fnNamePtr)] -> do
      fnSym <- fromString <$> loadString "lss_override_reset_by_name" fnNamePtr
      fnSym `userRedirectTo` fnSym
    _ -> errorPath $ FailRsn "lss_override_reset_by_name: wrong number of arguments"

overrideResetByAddr :: StdOvd sbe m
overrideResetByAddr = Override $ \_sym _rty args ->
  case args of
    [(PtrType{},fp)] -> do
      msym <- resolveFunPtrTerm fp
      case msym of
        Result sym -> sym `userRedirectTo` sym
        _        -> e "overrideResetByAddr: Failed to resolve function pointer"
    _ -> e "lss_override_reset_by_addr: wrong number of arguments"
  where
    e = errorPath . FailRsn

-- TODO (?): We may want to avoid retraction of overridden intrinsics, since
-- presumably the user always wants the overridden version.
overrideResetAll :: StdOvd sbe m
overrideResetAll = Override $ \_sym _rty args ->
  case args of
    [] -> do ovds <- use fnOverrides
             forM_ (M.assocs ovds) $ \(sym, (_, userOvd)) ->
               when userOvd $ do
                 fnOverrides . at sym .= Nothing
             return Nothing
    _ -> e "lss_override_reset_all: wrong number of arguments"
  where
    e = errorPath . FailRsn

type OverrideEntry sbe m = (Symbol, FunDecl, Override sbe m)

registerLibcOverrides :: (Functor m, MonadIO m, Functor sbe) => Simulator sbe m ()
registerLibcOverrides = do
  aw <- withDL ptrBitwidth
  let sizeT = IntType aw
  cb <- gets codebase
  -- Register malloc
  case cb^.cbFunctionType "malloc" of
    Nothing -> return ()
    Just d -> do
      case fdRetType d of
        Just _ ->
          registerOverride "malloc" d $
            allocHandler (fromIntegral aw) malloc
        _ -> error "malloc has unexpected return type."
  registerOverrides
    [ ("__assert_rtn", voidFunDecl [i8p, i8p, i32, i8p], assertHandler__assert_rtn)
    --, ("exit", voidTy, [i32], False, exitHandler)
    , ("alloca", funDecl voidPtr [sizeT], allocHandler (fromIntegral aw) alloca)
    , ("free", voidFunDecl [voidPtr],
       -- TODO: stub! Does this need to be implemented?
       Override $ \_sym _rty _args -> return Nothing)
    , ("printf", FunDecl (Just i32) [strTy] True, printfHandler)
    ]

i8, i16, i32, i64 :: MemType
i8     = IntType 8
i16    = IntType 16
i32    = IntType 32
i64    = IntType 64

i8p, i16p, i32p, i64p :: MemType
i8p    = PtrType (MemType i8)
i16p   = PtrType (MemType i16)
i32p   = PtrType (MemType i32)
i64p   = PtrType (MemType i64)

strTy :: MemType
strTy = i8p

voidPtr :: MemType
voidPtr = PtrType VoidType

registerLSSOverrides :: (Functor m, MonadIO m, Functor sbe) => Simulator sbe m ()
registerLSSOverrides = registerOverrides
  [ ("lss_abort",          voidFunDecl [strTy], abortHandler)
  , ("lss_print_symbolic", voidFunDecl [i8p],   printSymbolic)
  , ("lss_fresh_uint8",    funDecl  i8 [ i8],   freshInt'  8)
  , ("lss_fresh_uint16",   funDecl i16 [i16],   freshInt' 16)
  , ("lss_fresh_uint32",   funDecl i32 [i32],   freshInt' 32)
  , ("lss_fresh_uint64",   funDecl i64 [i64],   freshInt' 64)
  , ("lss_fresh_array_uint8",  funDecl  i8p [i32,  i8,  i8p], freshIntArray 8)
  , ("lss_fresh_array_uint16", funDecl i16p [i32, i16, i16p], freshIntArray 16)
  , ("lss_fresh_array_uint32", funDecl i32p [i32, i32, i32p], freshIntArray 32)
  , ("lss_fresh_array_uint64", funDecl i64p [i32, i64, i64p], freshIntArray 64)
  , ("lss_aiger_add_output_uint8",  voidFunDecl [ i8], addAigOutput)
  , ("lss_aiger_add_output_uint16", voidFunDecl [i16], addAigOutput)
  , ("lss_aiger_add_output_uint32", voidFunDecl [i32], addAigOutput)
  , ("lss_aiger_add_output_uint64", voidFunDecl [i64], addAigOutput)
  , ("lss_aiger_add_output_array_uint8" , voidFunDecl [ i8p, i32], addAigArrayOutput i8)
  , ("lss_aiger_add_output_array_uint16", voidFunDecl [i16p, i32], addAigArrayOutput i16)
  , ("lss_aiger_add_output_array_uint32", voidFunDecl [i32p, i32], addAigArrayOutput i32)
  , ("lss_aiger_add_output_array_uint64", voidFunDecl [i64p, i32], addAigArrayOutput i64)
  , ("lss_write_aiger",        voidFunDecl [strTy], writeCollectedAigerOutputs)
  , ("lss_write_aiger_uint8",  voidFunDecl [ i8, strTy], writeIntAiger)
  , ("lss_write_aiger_uint16", voidFunDecl [i16, strTy], writeIntAiger)
  , ("lss_write_aiger_uint32", voidFunDecl [i32, strTy], writeIntAiger)
  , ("lss_write_aiger_uint64", voidFunDecl [i64, strTy], writeIntAiger)
  , ("lss_write_aiger_array_uint8",  voidFunDecl [i8p,  i32, strTy], writeIntArrayAiger i8)
  , ("lss_write_aiger_array_uint16", voidFunDecl [i16p, i32, strTy], writeIntArrayAiger i16)
  , ("lss_write_aiger_array_uint32", voidFunDecl [i32p, i32, strTy], writeIntArrayAiger i32)
  , ("lss_write_aiger_array_uint64", voidFunDecl [i64p, i32, strTy], writeIntArrayAiger i64)
  , ("lss_eval_aiger_uint8",  funDecl  i8 [ i8, i8p, i32], evalAigerOverride)
  , ("lss_eval_aiger_uint16", funDecl i16 [i16, i8p, i32], evalAigerOverride)
  , ("lss_eval_aiger_uint32", funDecl i32 [i32, i8p, i32], evalAigerOverride)
  , ("lss_eval_aiger_uint64", funDecl i64 [i64, i8p, i32], evalAigerOverride)
  , ("lss_eval_aiger_array_uint8",  voidFunDecl [i8p,  i8p,  i32, i8p, i32], evalAigerArray i8)
  , ("lss_eval_aiger_array_uint16", voidFunDecl [i16p, i16p, i32, i8p, i32], evalAigerArray i16)
  , ("lss_eval_aiger_array_uint32", voidFunDecl [i32p, i32p, i32, i8p, i32], evalAigerArray i32)
  , ("lss_eval_aiger_array_uint64", voidFunDecl [i64p, i64p, i32, i8p, i32], evalAigerArray i64)
  , ("lss_write_cnf", voidFunDecl [i32, strTy], writeCNF)
  , ("lss_override_function_by_name", voidFunDecl [strTy, strTy], overrideByName)
  , ("lss_override_function_by_addr", voidFunDecl [strTy, strTy], overrideByAddr)
  , ("lss_override_llvm_intrinsic",   voidFunDecl [strTy, strTy], overrideIntrinsic)
  , ("lss_override_reset_by_name",    voidFunDecl [strTy], overrideResetByName)
  , ("lss_override_reset_by_addr",    voidFunDecl [strTy], overrideResetByAddr)
  , ("lss_override_reset_all", voidFunDecl [], overrideResetAll)
  , ("lss_show_path", voidFunDecl [], showPathOverride)
  , ("lss_show_mem",  voidFunDecl [], showMemOverride)
  , ("lss_set_verbosity", voidFunDecl [i32], userSetVebosityOverride)
  ]

registerOverrides ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => [OverrideEntry sbe m] -> Simulator sbe m ()
registerOverrides = mapM_ fn
  where fn (sym, fd, handler) = registerOverride sym fd handler