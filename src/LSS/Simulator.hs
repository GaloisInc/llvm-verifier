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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE OverloadedStrings     #-}

module LSS.Simulator
  ( module LSS.Execution.Codebase
  , Simulator (SM)
  , State(..)
  , SEH(..)
  , callDefine
  , callDefine_
  , defaultSEH
  , lookupSymbolDef
  , getProgramReturnValue
  , getProgramFinalMem
  , EvalContext
  , getEvalContext
  , getTypedTerm'
  , prettyTermSBE
  , runSimulator
  , withSBE
  , withSBE'
  , getSizeT
  -- * Memory operations
  , alloca
  , load
  , store
  , sizeof
  , processMemCond
  -- for testing
  , dbugM
  , dbugTerm
  , dbugTypedTerm
  , dumpMem
  , getMem
  , getTypedTerm
  , setSEH
  , withLC
  , warning
  )
where

import           Control.Applicative
import           Control.Monad.Error hiding (mapM, sequence)
import           Control.Monad.State       hiding (State, mapM, sequence)
import           Data.Int
import           Data.LLVM.TargetData
import           Data.LLVM.Symbolic.AST
import           Data.List                 hiding (union)
import           Data.Maybe
import           Data.String
import           LSS.Execution.Codebase
import           LSS.Execution.Common
import           LSS.Execution.MergeFrame
import           LSS.Execution.Utils
import           LSS.LLVMUtils
import           Numeric                   (showHex)
import           System.Exit
import           System.IO
import           Text.LLVM                 (Typed(..), (=:))
import           Text.PrettyPrint.HughesPJ
import Prelude   hiding (mapM, sequence)
import Data.Traversable
import           Verifier.LLVM.Backend


import qualified Control.Exception         as CE
import qualified Data.Map                  as M
import qualified Text.LLVM                 as L
import qualified Data.Vector               as V

runSimulator :: forall sbe a .
  ( Functor sbe
  )
  => Codebase              -- ^ Post-transform LLVM code, memory alignment, and
                           -- type aliasing info
  -> SBE sbe               -- ^ A symbolic backend
  -> SBEMemory sbe         -- ^ The SBE's LLVM memory model
  -> SEH sbe IO            -- ^ Simulation event handlers (use defaultSEH if no
                           -- event handling is needed)
  -> Maybe LSSOpts         -- Simulation options
  -> Simulator sbe IO a     -- ^ Simulator action to perform
  -> IO a
runSimulator cb sbe mem seh mopts m = do
  ea <- runErrorT go `evalStateT` newSt
  -- TODO: call exception handlers given by to-be-written SEH fields
  case ea of
    Left ErrorPathExc{}   -> error "internal: uncaught error path exception"
    Left (UnknownExc mfr) -> error $ "internal: uncaught unknown exception: "
                                     ++ maybe "(no details)" (show . ppFailRsn) mfr
    Right x               -> return x
  where
    lifter :: forall v . sbe v -> Simulator sbe IO v
    lifter = SM . lift . lift . sbeRunIO sbe
    newSt = newSimState cb sbe mem lifter seh mopts
    go    = runSM $ do
      true <- liftSBE $ termBool sbe True
      name <- newPathName
      let p = Path { pathFuncSym = entrySymbol
                   , pathRegs = M.empty
                   , pathException = Nothing
                   , pathCB = Nothing
                   , pathMem = mem
                   , pathName = name
                   , pathAssumptions = true
                   , pathAssertions = true
                   }
      let ef = ExitFrame {
                   efPending = [p]
                 }
      pushMergeFrame (ExitMergeFrame ef)
      initGlobals
      registerLibcOverrides
      registerLSSOverrides
      m

newSimState :: Codebase
            -> SBE sbe
            -> SBEMemory sbe
            -> LiftSBE sbe m
            -> SEH sbe m
            -> Maybe LSSOpts
            -> State sbe m
newSimState cb sbe _mem lifter seh mopts =
  State
  { codebase     = cb
  , symBE        = sbe
  , liftSymBE    = lifter
  , ctrlStk      = emptyCtrlStk
  , globalTerms  = M.empty
  , fnOverrides  = M.empty
  , verbosity    = 6
  , evHandlers   = seh
  , errorPaths   = []
  , lssOpts      = maybe defaultLSSOpts id mopts
  , pathCounter  = 0
  , aigOutputs   = []
  }

-- | Initialize all global data and register all defines.
initGlobals ::
  ( MonadIO m
  , Functor sbe
  , Functor m
  )
  => Simulator sbe m ()
initGlobals = do
  nms <- cbGlobalNameMap <$> gets codebase
  -- Register all function definitions.
  do let defines = [ d | (_,Right d) <- M.toList nms]
     forM_ defines $ \d -> do
       let argTys    = map typedType $ sdArgs d
           sym       = sdName d
           key       = (sym, Just argTys)
           fty       = L.FunTy (sdRetType d) argTys (sdVarArgs d)
           idl       = nub $ mapMaybe symBlockLabel $ M.keys (sdBody d)
           noCodeSpc = "Not enough space in code memory to allocate new definition."
       insertGlobalTerm noCodeSpc key fty $ \s m -> memAddDefine s m sym idl
  -- Add symbol for declarations.
  do declares <- L.modDeclares <$> origModule <$> gets codebase
     forM_ declares $ \d -> do
       let sym = L.decName d
       let errMsg = "Insufficient space for new declaration."
       let key = (sym, Just (L.decArgs d))
       let ty = L.FunTy (L.decRetType d) (L.decArgs d) (L.decVarArgs d)
       insertGlobalTerm errMsg key ty $ \s m -> memAddDefine s m sym []
  -- Initialize global data
  do let globals = [ g | (_,Left g) <- M.toList nms]
     forM_ globals $ \g -> do
       let key = (L.globalSym g, Nothing)
       cb1 onMkGlobTerm g
       ec <- getEvalContext "adGlobal" Nothing
       cdata <- getTypedTerm' ec (L.globalType g =: L.globalValue g)
       cb2 onPreGlobInit g cdata
       let noDataSpc = "Not enough space in data segment to allocate new global."
       _ <- insertGlobalTerm noDataSpc key (L.globalType g) $ \s m ->
              memInitGlobal s m cdata
       cb2 onPostGlobInit g cdata

callDefine_ ::
  ( LogMonad m
  , MonadIO m
  , Functor m
  , Functor sbe
  )
  => L.Symbol     -- ^ Callee symbol
  -> L.Type       -- ^ Callee return type
  -> [Typed (SBETerm sbe)] -- ^ Callee arguments
  -> Simulator sbe m ()
callDefine_ c t ag = callDefine c t ag >> return ()

-- | External entry point for a function call.  The argument generator is used
-- to create actuals passed to the function, and the return value is those
-- arguments.  In the case when no arguments created or invoking
-- intrinsics/overrides, the return value will always be the empty list.
callDefine ::
  ( LogMonad m
  , MonadIO m
  , Functor m
  , Functor sbe
  )
  => L.Symbol     -- ^ Callee symbol
  -> L.Type       -- ^ Callee return type
  -> [Typed (SBETerm sbe)] -- ^ Callee argument generator
  -> Simulator sbe m [Typed (SBETerm sbe)]
callDefine calleeSym t args = do
  def <- lookupSymbolDef calleeSym
  let retReg = case sdRetType def of
                 L.PrimType L.Void -> Nothing
                 retType -> Just (retType =: entryRsltReg)
  unless (t == sdRetType def) $
    dbugM $ show $
      text "Warning: callDefine given incorrect return type of"
              <+> L.ppType t
              <+>  text "for" <+> L.ppSymbol calleeSym <+> text "when actual type is"
              <+> L.ppType (sdRetType def) <> text "."
  r <- callDefine' False entryRetNormalID calleeSym retReg args
  run
  return r

setReturnValue :: String -> Maybe (Typed Reg) -> Maybe t
               ->  RegMap t -> RegMap t
setReturnValue _n (Just tr) (Just rv) rm = M.insert (typedValue tr) (typedAs tr rv) rm
setReturnValue _n Nothing   Nothing   rm = rm
setReturnValue nm Nothing   (Just _) _  =
  error $ nm ++ ": Return value where non expected"
setReturnValue nm (Just tr) Nothing   _  =
  error $ nm ++ ": Missing return value for " ++ show (typedType tr)
          ++ " " ++ show (L.ppIdent (typedValue tr))

callDefine' ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => Bool                                         -- ^ Is this a redirected call?
  -> SymBlockID                                   -- ^ Normal call return block id
  -> L.Symbol                                     -- ^ Callee symbol
  -> Maybe (Typed Reg)                            -- ^ Callee return type and result register
  -> [Typed (SBETerm sbe)] -- ^ Callee arguments
  -> Simulator sbe m [Typed (SBETerm sbe)]
callDefine' isRedirected normalRetID calleeSym@(L.Symbol calleeName) mreg args = do
  -- NB: Check overrides before anything else so we catch overriden intrinsics
  override <- M.lookup calleeSym <$> gets fnOverrides
  case override of
    Nothing -> normal
    Just (Redirect calleeSym', _)
      -- NB: We break transitive redirection to avoid cycles
      | isRedirected -> normal
      | otherwise    -> callDefine' True normalRetID calleeSym' mreg args
    Just (Override f, _) -> do
      r <- f calleeSym mreg args
      modifyPathRegsM $ setReturnValue "callDefine'" mreg r
      modifyPath $ \p -> p { pathCB = Just normalRetID }
      return []
  where
    normal
      | isPrefixOf "llvm." calleeName = do
          intrinsic calleeName mreg args
          modifyPath $ \p -> p { pathCB = Just normalRetID }
          return []
      | otherwise = do
          runNormalSymbol normalRetID calleeSym mreg args

-- | Return symbol definition with given name or fail.
lookupSymbolDef :: (Functor m, MonadIO m, Functor sbe)
                => L.Symbol -> Simulator sbe m SymDefine
lookupSymbolDef sym = do
  mdef <- lookupDefine sym <$> gets codebase
  case mdef of
    Just def -> return def
    Nothing  -> do
      errorPath $ FailRsn $ "Failed to find definition for symbol "
                        ++ show (L.ppSymbol sym) ++ " in codebase."

runNormalSymbol ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => SymBlockID            -- ^ Normal call return block id
  -> L.Symbol              -- ^ Callee symbol
  -> Maybe (Typed Reg)     -- ^ Callee return type and result register
  -> [Typed (SBETerm sbe)] -- ^ Callee arguments
  -> Simulator sbe m [Typed (SBETerm sbe)]
runNormalSymbol normalRetID calleeSym mreg args = do
  def <- lookupSymbolDef calleeSym
  Just (p,mf) <- popPending <$> popMergeFrame "runNormalSymbol"
  pushMergeFrame mf
  let name = pathName p
  let mem = pathMem p
  true <- withSBE $ \s -> termBool s True
  let path =  Path { pathFuncSym = calleeSym
                   , pathRegs = M.empty
                   , pathException = Nothing
                   , pathCB = Just initSymBlockID
                   , pathMem = mem
                   , pathName = name
                   , pathAssumptions = true
                   , pathAssertions = true
                   }
  let ms = pathMergedState p
  let rf = ReturnFrame {
              rfFuncSym     = pathFuncSym p
            , rfRegs        = pathRegs p
            , rfRetReg      = mreg
            , rfNormalLabel = normalRetID
            , rfExceptLabel = Nothing
            , rfNormalState = ms
            , rfExceptState = ms
            , rfPending = [path]
            }
  pushMergeFrame (ReturnMergeFrame rf)
  dbugM' 5 $ "callDefine': callee " ++ show (L.ppSymbol calleeSym)
  lc <- gets (cbLLVMCtx . codebase)
  modifyPathRegsM $ \_ -> bindArgs lc (sdArgs def) args
  -- Push stack frame in current process memory.
  do Just m <- getMem
     (c,m') <- withSBE $ \s -> stackPushFrame s m
     setMem m'
     let fr = FailRsn "Stack push frame failure: insufficient stack space"
     processMemCond fr c
  return args
  where
    err doc = error $ "callDefine/bindArgs: " ++ render doc

    bindArgs lc formals actuals
      | length formals /= length actuals =
          err $ text "incorrect number of actual parameters"
            $+$ text "formals: " <> text (show formals)

      | otherwise =
          foldr (bindArg lc) M.empty (formals `zip` actuals)

    bindArg lc (Typed (L.PtrTo (L.Alias a)) reg, v) mp =
      bindArg lc (Typed (L.PtrTo (llvmLookupAlias lc a)) reg, v) mp

    bindArg lc (reg, Typed (L.PtrTo (L.Alias a)) v) mp =
      bindArg lc (reg, Typed (L.PtrTo (llvmLookupAlias lc a)) v) mp

    bindArg lc (Typed (L.Alias a) reg, v) mp =
      bindArg lc (Typed (llvmLookupAlias lc a) reg, v) mp

    bindArg lc (reg, Typed (L.Alias a) v) mp =
      bindArg lc (reg, Typed (llvmLookupAlias lc a) v) mp

    bindArg _ (Typed ft reg, (Typed at v)) mp =
      -- WARNING: This code now does an implcit conversion from ft to at.
          let ok = M.insert reg (Typed ft v) mp
          in
            -- It's doubtful that anything will remain excluded here, but this
            -- makes it explicit when we've not handled particular argument
            -- types.
            case at of
            L.PrimType L.Integer{} -> ok
            L.PtrTo{}              -> ok
            _ -> err $ text "unsupported arg type:" <+> L.ppType at

--      | otherwise = err
--          $ text "formal/actual type mismatch:"
--            <+> L.ppType ft <+> text "vs." <+> L.ppType at
--            $+$ text (show ft) <+> text "vs." <+> text (show at)

intrinsic ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => String -> Maybe (Typed Reg) -> [Typed (SBETerm sbe)]
  -> Simulator sbe m ()
intrinsic intr mreg args0 =
  case (intr, mreg) of
    ("llvm.memcpy.p0i8.p0i8.i64", Nothing)    -> memcpy
    ("llvm.memset.p0i8.i64", Nothing)         -> memset
    ("llvm.uadd.with.overflow.i64", Just reg) -> uaddWithOverflow reg
    ("llvm.objectsize.i32", Just reg)         -> objSz True reg
    ("llvm.objectsize.i64", Just reg)         -> objSz False reg
    -- Do nothing.
    ("llvm.lifetime.start", Nothing) -> return ()
    ("llvm.lifetime.end", Nothing) -> return ()
    _ -> whenVerbosity (>= 1) $ do --TODO: Give option of stopping on warnings like this.
      tellUser $ "Warning: skipping unsupported LLVM intrinsic " ++ show intr     
  where
    memcpy = do
      let [dst, src, len, align, _isvol] = map typedValue args0
      Just m <- getMem
      (c,m') <- withSBE $ \sbe -> memCopy sbe m dst src len align
      setMem m'
      sbe <- gets symBE
      let pts = map (prettyTermD sbe) [dst,src,len]
      let fr = FailRsn $
                 "memcopy operation was not valid: (dst,src,len) = "
                   ++ show (parens $ hcat $ punctuate comma $ pts)
      processMemCond fr c
    memset = do
      let [dst, val, len, align, _isvol] = args0
      memSet (typedValue dst) val (typedValue len) (typedValue align)
    uaddWithOverflow reg = do
      let [x, y] = map typedValue args0
      b0 <- withSBE $ \sbe -> termBool sbe False
      x' <- withSBE $ \sbe -> termArray sbe [b0, x]
      y' <- withSBE $ \sbe -> termArray sbe [b0, y]
      z <- termAdd x' y'
      [ov, z'] <- withSBE $ \sbe -> termDecomp sbe [i1, i64] z
      res <- withSBE $ \sbe -> termArray sbe [typedValue z', typedValue ov]
      assign (typedValue reg) (Typed (L.Struct [i64, i1]) res)
    objSz is32 reg = do
      let [_ptr, maxOrMin] = map typedValue args0
      mval <- withSBE' $ \s -> snd <$> asUnsignedInteger s maxOrMin
      case mval of
        Nothing -> errorPath $ FailRsn $ "llvm.objectsize.i{32,64} expects concrete 2nd parameter"
        Just v  -> let tv = if is32
                              then int32const $ if v == 0 then -1 else 0
                              else int64const $ if v == 0 then -1 else 0
                   in
                     assign (typedValue reg) =<< getTypedTerm "objSz" tv

memSet :: ( Functor m, MonadIO m
          , Functor sbe
          ) =>
          SBETerm sbe
       -> Typed (SBETerm sbe)
       -> SBETerm sbe
       -> SBETerm sbe
       -> Simulator sbe m ()
memSet dst val len align = do
  lenVal <- withSBE' $ \s -> snd <$> asUnsignedInteger s len
  case lenVal of
    Just 0 -> return ()
    _ -> do
      store val dst
      ptrWidth <- withLC llvmAddrWidthBits
      lenWidth <- withSBE' $ \s -> termWidth s len
      one      <- withSBE  $ \s -> termInt s ptrWidth 1
      negone   <- withSBE  $ \s -> termInt s (fromIntegral lenWidth) (-1)
      dst'     <- termAdd dst one
      len'     <- termAdd len negone
      memSet dst' val len' align

exitFramePath :: ExitFrame term mem -> Maybe (Path' term mem)
exitFramePath ef = safeHead (efPending ef)

-- | Return value of this path.
pathRetVal :: Path' term mem -> Maybe term
pathRetVal p = typedValue <$> M.lookup entryRsltReg (pathRegs p)

getProgramReturnValue :: (Monad m, Functor m)
  => Simulator sbe m (Maybe (SBETerm sbe))
getProgramReturnValue = do
  sbe <- gets symBE
  Just (top, _) <- popMF <$> gets ctrlStk
  return $
    case top of
      ExitMergeFrame ef -> pathRetVal =<< safeHead (efPending ef)
      _ -> error $ "getProgramReturnValue: program not yet terminated "
                     ++ show (ppMergeFrame sbe top)

getProgramFinalMem :: (Monad m, Functor m)
  => Simulator sbe m (Maybe (SBEMemory sbe))
getProgramFinalMem = do
  Just (top, _) <- popMF <$> gets ctrlStk
  return $
    case top of
      ExitMergeFrame (exitFramePath -> Just p) -> Just (pathMem p)
      _                -> error "getProgramFinalMem: program not yet terminated"

-- data PMCInfo = PMCIExpr SymExpr
--              | PMCIStmt SymStmt
--              | PMCIIntrinsic String
--              | PMCIPushMemFrame

-- Handle a condition returned by the memory model
processMemCond ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => -- Maybe PMCInfo ->
    FailRsn -> SBETerm sbe -> Simulator sbe m ()
processMemCond rsn cond = do
  sbe <- gets symBE
  case asBool sbe cond of
    Just True  -> return ()
    Just False -> errorPath rsn
    _ -> do
      Just (p, mf) <- popPending <$> popMergeFrame "processMemCond"
      -- TODO: provide more detail here?
      whenVerbosity (>= 6) $ do
        tellUser $ "Warning: Obtained symbolic validity result from memory model."
        tellUser $ "This means that certain memory accesses were valid only on some paths."
        tellUser $ "In this case, the symbolic validity result was encountered at:"
        tellUser $ show $ ppPathLoc sbe p
        tellUser ""
      p' <- liftSBE $ addPathAssertion sbe cond p
      pushMergeFrame $ pushPending p' mf

-- | Return true if the path has asserted false to be true, and therefore we
-- can call errorPath on it.
pathAssertedFalse :: SBE sbe -> Path sbe -> Bool
pathAssertedFalse sbe p = asBool sbe (pathAssertions p) == Just False

-- Eat the control stack up to the exit frame, and then finalize
-- it so we'll report termination when queried via
-- getProgramReturnValue, etc.
{-
(ef:[]) <- dropWhile (not . isExitFrame) . mergeFrames <$> gets ctrlStk
modify $ \s -> s{ ctrlStk = CtrlStk [finalizeExit ef] }
numErrs <- length <$> gets errorPaths
CE.assert (numErrs > 0) $ return ()
showEPs <- optsErrorPathDetails <$> gets lssOpts
              if showEPs
                then tellUser "All paths yielded errors!" >> dumpErrorPaths
                else tellUser "All paths yielded errors! To see details, use --errpaths."
-}

run ::
  ( LogMonad m
  , Functor m
  , MonadIO m
  , Functor sbe
  )
  => Simulator sbe m ()
run = do
  top <- popMergeFrame "run"
  case top of
    ExitMergeFrame{} -> do
      -- Normal program termination on at least one path.
      pushMergeFrame top
      -- Report termination info at appropriate verbosity levels; also,
      -- inform user about error paths when present and optionally dump
      -- them.
      dumpCtrlStk' 5
      whenVerbosity (>=2) $ do
        dbugM "run terminating normally: found valid exit frame"
        mrv <- getProgramReturnValue
        case mrv of
          Nothing -> dbugM "Program had no return value."
          Just rv -> dbugTerm "Program returned value" rv
        numErrs <- length <$> gets errorPaths
        showEPs <- optsErrorPathDetails <$> gets lssOpts
        when (numErrs > 0 && not showEPs) $
          tellUser "Warning: Some paths yielded errors. To see details, use --errpaths."
        when (numErrs > 0 && showEPs) $ do
          dbugM $ showErrCnt numErrs
          dumpErrorPaths
    _ -> do
      case pendingPaths top of
        p:_  -> do
          pushMergeFrame top
          flip catchError handleError $ do
            let Just pcb = pathCB p
            sbe <- gets symBE
            when (pathAssertedFalse sbe p) $
              errorPath $ FailRsn $ "This path is infeasible"
            let sym = pathFuncSym p
            Just def <- lookupDefine sym <$> gets codebase
            -- TODO: Figure out how to make sure we get a valid path.
            runStmts $ sbStmts $ lookupSymBlock def pcb
          run
        [] -> do  -- Need to pop frame and get merge path.
          next <- popMergeFrame "run@next"
          case getMergedState "run@next" top of
            EmptyState assumptions _ -> -- All paths lead to errors.
              case next of
                ExitMergeFrame _ -> do
                  pushMergeFrame next
                  showEPs <- optsErrorPathDetails <$> gets lssOpts
                  if showEPs then
                    tellUser "All paths yielded errors!" >> dumpErrorPaths
                  else
                    tellUser "All paths yielded errors! To see details, use --errpaths."
                _ -> pushErrorPath "run@error" next assumptions >> run
            PathState p a -> do
              p' <- withSBE $ \sbe -> addPathAssertion sbe a p
              pushMergeFrame $ pushPending p' next
              run
  where
    handleError (ErrorPathExc _rsn s) = do
      -- errorPath ensures that the simulator state provided in the
      -- exception data is correct for the next invocation of run,
      -- so overwrite the current state here.
      modify (const s)
    handleError e = throwError e
    showErrCnt x
      | x == 1    = "Encountered errors on exactly one path. Details below."
      | otherwise = "Encountered errors on " ++ show x ++ " paths.  Details below."
    dumpErrorPaths = do
        dbugM $ replicate 80 '-'
        eps <- gets errorPaths
        forM_ eps $ \ep -> do
          let p = epPath ep
          sbe <- gets symBE
          dbugM $ "Error reason        : " ++ show (ppFailRsn (epRsn ep))
          dbugM $ "Error path state    :\n" ++ show (nest 2 $ ppPath sbe p)
          whenVerbosity (>= 3) $ do
            dbugM "Error path memory: "
          withSBE (\s -> memDump s (pathMem p) Nothing)
          when (length eps > 1) $ dbugM "--"
        dbugM $ replicate 80 '-'

--------------------------------------------------------------------------------
-- LLVM-Sym operations

-- | @popMergeFrame@ removes the top entry of the control stack; assumes
-- that the control stack is nonempty.
popMergeFrame :: MonadIO m => String -> Simulator sbe m (MF sbe)
popMergeFrame ctx = do
  s <- get
  case popMF (ctrlStk s) of
    Nothing -> error $
      "popMergeFrame called with empty control stack by " ++ show ctx
    Just (mf, cs) -> do
      whenVerbosity (>= 6) $ do
        when (isExitFrame mf) $ do
          dbugM $ "Exit frame was removed by " ++ ctx
      modify $ \_ -> s { ctrlStk = cs }
      return mf

-- | @pushMergeFrame mf@ pushes mf to the control stack
pushMergeFrame :: Monad m => MF sbe -> Simulator sbe m ()
pushMergeFrame = modifyCS . pushMF

assign :: (Functor m, MonadIO m)
  => Reg -> Typed (SBETerm sbe) -> Simulator sbe m ()
assign reg v = modifyPathRegsM $ M.insert reg v

-- | Evaluate condition in current path.
evalCond :: (Functor sbe, Functor m, MonadIO m) => SymCond -> Simulator sbe m (SBETerm sbe)
evalCond TrueSymCond = withSBE $ \sbe -> termBool sbe True
evalCond (HasConstValue typedTerm i) = do
  Typed (L.PrimType (L.Integer w)) v <- getTypedTerm "evalCond" typedTerm
  sbe <- gets symBE
  iv <- liftSBE $ termInt sbe (fromIntegral w) i
  liftSBE $ applyIEq sbe v iv
evalCond (NotConstValues typedTerm is) = do
  Typed (L.PrimType (L.Integer w)) t <- getTypedTerm "evalCond" typedTerm
  sbe <- gets symBE
  true <- liftSBE $ termBool sbe True
  il <- mapM (liftSBE . termInt sbe (fromIntegral w)) is
  ir <- mapM (liftSBE . applyICmp sbe L.Ine t) il
  let fn r v = liftSBE $ applyAnd sbe r v
  foldM fn true ir

mergeReturn ::
  ( LogMonad m
  , Functor m
  , MonadIO m
  , Functor sbe
  )
  => Maybe (Typed SymValue)
  -> Simulator sbe m ()
mergeReturn mtv = do
  mrv <- mapM (getTypedTerm "mergeReturn") mtv
  -- Pop the current path and rest of the merge frame.
  Just (p, mf) <- popPending <$> popMergeFrame "mergeReturn"
  sbe <- gets symBE
  let rf = case mf of
             ReturnMergeFrame rf' -> rf'
             _ -> error $ show $ text "Unexpected frame when return expected"
                    <+> ppMergeFrame sbe mf
  -- Pop stack frame from memory.
  m' <- liftSBE $ stackPopFrame sbe (pathMem p)
  -- Get the path after updating return value and memory.
  let rm' = setReturnValue "mergeReturn" (rfRetReg rf) (typedValue <$> mrv)
              (rfRegs rf)
  let p' = p { pathFuncSym = rfFuncSym rf
             , pathRegs = rm'
             , pathCB = Just (rfNormalLabel rf)
             , pathMem = m' }
  -- Merge updated path with top merge state.
  mmerged <- mergePaths p' (rfNormalState rf)
  -- Merge the current path into the merged state for the current merge frame.
  pushMergeFrame (ReturnMergeFrame rf { rfNormalState = mmerged })

-- | @mergePaths p1 p2@ merges path p1 into path p2, which may be empty; when p2
-- is empty, this function merely p1 as the merged path. Yields Nothing if
-- merging fails.
mergePaths ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => Path sbe
  -> MergedState (SBETerm sbe) (SBEMemory sbe)
  -> Simulator sbe m (MergedState (SBETerm sbe) (SBEMemory sbe))
mergePaths cp (EmptyState assumptions assertions) = do
  a <- withSBE $ \sbe ->
   applyAnd sbe (pathAssumptions cp) (pathAssertions cp)
  let p' = cp { pathAssumptions = assumptions
              , pathAssertions = assertions
              }
  return (PathState p' a)
mergePaths cp (PathState p a) = do
  CE.assert (pathCB cp == pathCB p) $ do
    let c = pathAssumptions cp
    sbe <- gets symBE
    whenVerbosity (>= 4) $ do
      dbugM $ "Merging paths. "
      whenVerbosity (>= 6) $ do
        ppPathM "from" cp
        ppPathM "to" p
    let mergeTerm x y = liftSBE $ applyIte sbe c x y
    let mergeTyped (Typed t1 v1) (Typed t2 v2) =
          CE.assert (t1 == t2) $
            Typed t1 <$> mergeTerm v1 v2
    -- Merge call frame
    merged <- sequence $
      M.intersectionWith mergeTyped (pathRegs cp) (pathRegs p)
    -- Get merge memory
    mem' <- liftSBE $ memMerge sbe c (pathMem cp) (pathMem p)
    let p' = p { pathRegs = merged
               , pathMem = mem'
               }
    a' <- mergeTerm (pathAssertions cp) a
    whenVerbosity (>=6) $ ppPathM "mergedPath" p'
    return (PathState p' a')

data EvalContext sbe = EvalContext {
       evalContextName :: String
     , evalLLVMContext :: LLVMContext
     , evalGlobalTerms :: GlobalMap sbe
     , evalRegs :: Maybe (RegMap (SBETerm sbe))
     , evalSBE :: SBE sbe
     }

getEvalContext :: Monad m => String -> Maybe (RegMap (SBETerm sbe)) -> Simulator sbe m (EvalContext sbe)
getEvalContext nm mrm = do
  lc <- gets (cbLLVMCtx . codebase)
  gt <- gets globalTerms
  sbe <- gets symBE
  return EvalContext { evalContextName = nm
                     , evalLLVMContext = lc
                     , evalGlobalTerms = gt
                     , evalRegs = mrm
                     , evalSBE = sbe
                     }

getGlobalPtrTerm :: EvalContext sbe
                 -> (L.Symbol, Maybe [L.Type])
                 -> Typed (SBETerm sbe)
getGlobalPtrTerm ec key@(sym, tys) =
  case M.lookup key (evalGlobalTerms ec) of
    Just t  -> t
    Nothing ->
      error $ "getGlobalPtrTerm: symbol resolution failed: "
              ++ show (L.ppSymbol sym) ++ " (" ++ show tys ++ ")"

-- | getTypedTerm' in the context of the current call frame
getTypedTerm ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => String -> Typed L.Value -> Simulator sbe m (Typed (SBETerm sbe))
getTypedTerm nm tv = do
  mp <- getPath
  ec <- getEvalContext nm (pathRegs <$> mp)
  getTypedTerm' ec tv

-- | Obtain the typed SBE term representation for the given LLVM value; performs
-- identifier lookup in the regmap of the given call frame as needed.
getTypedTerm' ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => EvalContext sbe -> Typed L.Value -> Simulator sbe m (Typed (SBETerm sbe))

getTypedTerm' ec (Typed (L.Alias i) v) = getTypedTerm' ec (Typed tp v)
  where tp = llvmLookupAlias (evalLLVMContext ec) i

getTypedTerm' ec (Typed t@(L.PrimType (L.Integer (fromIntegral -> w))) (L.ValInteger x))
  = liftSBE $ Typed t <$> termInt (evalSBE ec) w x

getTypedTerm' ec (Typed t@(L.PrimType (L.Integer 1)) (L.ValBool b))
  = liftSBE $ Typed t <$> termInt (evalSBE ec) 1 (fromIntegral (fromEnum b))

getTypedTerm' ec (Typed t@(L.PtrTo _) L.ValNull)
    = liftSBE $ Typed t <$> termInt (evalSBE ec) ptrWidth 0
  where ptrWidth = llvmAddrWidthBits (evalLLVMContext ec)

getTypedTerm' ec (Typed _ ce@L.ValConstExpr{}) = evalCE ec ce

getTypedTerm' ec (Typed ty@(L.Array len ety) (L.ValArray ety' es))
  = do
  CE.assert (ety == ety') $ return ()
  CE.assert (fromIntegral len == length es) $ return ()
  valTerms <- mapM (getTypedTerm' ec) (Typed ety <$> es)
  liftSBE $ Typed ty <$> termArray (evalSBE ec) (map typedValue valTerms)

getTypedTerm' ec (Typed ty@(L.Array _len ety@(L.PrimType L.Integer{})) (L.ValString str))
  = do
  lc <- gets (cbLLVMCtx . codebase)
  CE.assert (llvmStoreSizeOf lc ty == fromIntegral (length str)) $ return ()
  charTerms <- mapM (getTypedTerm' ec) $ map toChar str
  liftSBE $ Typed ty <$> termArray (evalSBE ec) (map typedValue charTerms)
  where
    toChar = Typed ety . L.ValInteger . fromIntegral . fromEnum

getTypedTerm' ec (Typed ty@(L.Struct _fldTys) (L.ValStruct fldTVs))
  = do
  fldTerms <- mapM (getTypedTerm' ec) fldTVs
  liftSBE $ Typed ty <$> termArray (evalSBE ec) (map typedValue fldTerms)

getTypedTerm' ec (Typed (L.PtrTo (L.FunTy _rty argtys _isVarArgs)) (L.ValSymbol sym))
  = return $ getGlobalPtrTerm ec (sym, Just argtys)

getTypedTerm' ec (Typed _ (L.ValSymbol sym))
  = return $ getGlobalPtrTerm ec (sym, Nothing)

getTypedTerm' _ (Typed ty (L.ValDouble v)) =
  Typed ty <$> withSBE (\sbe -> termDouble sbe v)

getTypedTerm' ec (Typed _ (L.ValIdent i)) =
  case evalRegs ec of
    Just rm -> lkupIdent i rm
    Nothing -> error $ "getTypedTerm' called by " ++ evalContextName ec ++ " with missing frame."

getTypedTerm' _ (Typed ty L.ValUndef)
  = zeroInit ty

getTypedTerm' _ (Typed ty L.ValZeroInit)
  = zeroInit ty

getTypedTerm' ec tv@(Typed t v)
  = do
  unimpl $ "getTypedTerm': unsupported value / call frame presence: "
            ++ "\n" ++ show (L.ppType t) ++ " =: " ++ show (L.ppValue v)
            ++ "\n" ++ show (parens $ text $ show tv)
            ++ "\nmrm = " ++ show (ppRegMap (evalSBE ec) <$> evalRegs ec)

zeroInit ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => L.Type -> Simulator sbe m (Typed (SBETerm sbe))
zeroInit ty = do
  szBytes <- fromIntegral <$> withLC (`llvmAllocSizeOf` ty)
  Typed ty <$> withSBE (\sbe -> termInt sbe (szBytes * 8) 0)

insertGlobalTerm ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => String
  -> (L.Symbol, Maybe [L.Type])
  -> L.Type
  -> (SBE sbe -> SBEMemory sbe -> sbe (Maybe (SBETerm sbe, SBEMemory sbe)))
  -> Simulator sbe m (Typed (SBETerm sbe))
insertGlobalTerm errMsg key ty act = do
  Just m <- getMem
  mr <- withSBE $ \s -> act s m
  case mr of
    Nothing -> errorPath (FailRsn errMsg)
    Just (r,m')  -> do
      setMem m'
      let t = Typed (L.PtrTo ty) r
      modify $ \s -> s{ globalTerms = M.insert key t (globalTerms s) }
      return t

--------------------------------------------------------------------------------
-- Instruction stepper and related functions

-- | Execute a single LLVM-Sym AST instruction
step ::
  ( LogMonad m
  , MonadIO m
  , Functor m
  , Functor sbe
  )
  => SymStmt -> Simulator sbe m ()

step (PushCallFrame callee args mres retTgt) = do
  eab <- resolveCallee callee
  case eab of
    Left msg        -> errorPath $ FailRsn $ "PushCallFrame: " ++ msg
    Right calleeSym -> do
      Just p <- getPath
      ec <- getEvalContext "pushCallFrame" (Just (pathRegs p))
      argTerms <- mapM (getTypedTerm' ec) args
      _ <- callDefine' False retTgt calleeSym mres argTerms
      return ()

step (PushInvokeFrame _fn _args _mres _e) = unimpl "PushInvokeFrame"

step (PushPostDominatorFrame pdid) = do
  Just (p,mf) <- popPending <$> popMergeFrame "pushPostDominatorFrame"
  pushMergeFrame mf
  sbe <- gets symBE
  newm <- liftSBE $ memPushMergeFrame sbe (pathMem p)
  true <- liftSBE $ termBool sbe True
  let p' = p { pathMem = newm
             , pathAssumptions = true
             , pathAssertions = true
             }
  let pf = PostdomFrame {
               pdfMergedState = pathMergedState p
             , pdfPending = [p']
             , pdfLabel = pdid
             }
  pushMergeFrame (PostdomMergeFrame pf)
step (MergePostDominator pdid) = do
    -- Pop the current path and rest of the merge frame.
  Just (p,mf) <- popPending <$> popMergeFrame "mergePostDominator"
  -- Sanity check merge frame.
  case mf of
    PostdomMergeFrame pdf
      | pdfLabel pdf == pdid -> return ()
      | otherwise   -> error "merge postdom: top pdom frame has unexpected block ID"
    _ -> error "merge postdom: expected postdom merge frame"
  --- Pop merge frame in path
  newm <- withSBE (\s -> memPopMergeFrame s (pathMem p))
  let p' = p { pathMem = newm }
  -- Get merge state.
  mmerged <- mergePaths p' (getMergedState "mergePostDominator" mf)
  pushMergeFrame (setMergedState mmerged mf)

step (MergeReturn mrslt) = mergeReturn mrslt

step (PushPendingExecution cond) = do
  c <- evalCond cond
  Just (p, mf) <- popPending <$> popMergeFrame "pushPendingExecution"
  name <- newPathName
  pd <- withSBE $ \sbe -> addPathAssumption sbe c (p { pathName = name })
  pushMergeFrame $ pushPending p $ pushPending pd mf

step (SetCurrentBlock bid) =
  modifyPath $ \p ->
    p { pathCB = Just bid
      }

step (Assign reg expr) = assign reg =<< eval expr

step (Store val addr _malign) = do
  whenVerbosity (<=6) $ dumpMem 6 "store pre"
  valTerm  <- getTypedTerm "store@1" val
  addrTerm <- getTypedTerm "store@2" addr
  store valTerm (typedValue addrTerm)
  whenVerbosity (<=6) $ dumpMem 6 "store post"

step (IfThenElse cond thenStmts elseStmts) = do
  c <- evalCond cond
  sbe <- gets symBE
  case asBool sbe c of
    Just True -> runStmts thenStmts
    _ -> runStmts elseStmts

step Unreachable
  = error "step: Encountered 'unreachable' instruction"

step Unwind = unimpl "unwind"

-- | Return value one as an integer with the address width bits.
getSizeT :: (Functor m, MonadIO m) => Integer -> Simulator sbe m (Typed (SBETerm sbe))
getSizeT v = do
  aw <- withLC llvmAddrWidthBits
  Typed (L.iT (fromIntegral aw)) <$> withSBE (\sbe -> termInt sbe aw v)

--------------------------------------------------------------------------------
-- Symbolic expression evaluation

-- | @eval expr@ evaluates @expr@ via the symbolic backend
eval ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => SymExpr -> Simulator sbe m (Typed (SBETerm sbe))
eval (Arith op (Typed (L.Alias a) v1) v2) = do
  ty <- withLC (`llvmLookupAlias` a)
  eval (Arith op (Typed ty v1) v2)
eval (Arith op (Typed t@(L.PrimType L.Integer{}) v1) v2) = do
  Typed _t1 x <- getTypedTerm "arith@1" (Typed t v1)
  Typed _t2 y <- getTypedTerm "arith@2" (Typed t v2)
  --CE.assert (t == t1 && t == t2) $ return ()
  r <- withSBE (\sbe -> applyArith sbe op x y)  
  return $ Typed t r 
  
eval e@Arith{} = unimpl $ "Arithmetic expr type: " ++ show (ppSymExpr e)
eval (Bit op (Typed (L.Alias a) v1) v2) = do
  t1 <- withLC (`llvmLookupAlias` a)
  eval (Bit op (Typed t1 v1) v2)
eval (Bit op tv1@(Typed t _) v2) = do
  [x, y] <- map typedValue <$> mapM (getTypedTerm "bit") [tv1, typedType tv1 =: v2]
  Typed t <$> withSBE (\sbe -> applyBitwise sbe op x y)
eval (Conv op (Typed (L.Alias a) v) t2) = do
  t1 <- withLC (`llvmLookupAlias` a)
  eval (Conv op (Typed t1 v) t2)
eval (Conv op tv (L.Alias a)) = do
  t2 <- withLC (`llvmLookupAlias` a)
  eval (Conv op tv t2)
eval (Conv op tv@(Typed (L.PrimType L.Integer{}) _) t2@(L.PrimType L.Integer{})) = do
  Typed _t x <- getTypedTerm "prim" tv
  --CE.assert (t == t1) $ return ()
  Typed t2 <$> withSBE (\sbe -> applyConv sbe op x t2)
eval (Conv L.PtrToInt tv t2@(L.PrimType L.Integer{})) = do
  mp <- getPath
  ec <- getEvalContext "eval@PtrToInt" (pathRegs <$> mp)
  evalPtrToInt ec tv t2
eval (Conv L.IntToPtr tv@(Typed (L.PrimType L.Integer{}) _) t2) = do
  mp <- getPath
  ec <- getEvalContext "eval@IntToPtr" (pathRegs <$> mp)
  evalIntToPtr ec tv t2
eval (Conv L.BitCast tv ty) = Typed ty . typedValue <$> getTypedTerm "bitCast" tv
eval e@Conv{} = unimpl $ "Conv expr type: " ++ show (ppSymExpr e)
eval (Alloca (L.Alias a) msztv malign) = do
  ty <- withLC (`llvmLookupAlias` a)
  eval (Alloca ty msztv malign)
eval (Alloca ty msztv malign ) = do
  sizeTm <-
    case msztv of
      Just tv -> getTypedTerm "alloca" tv
      Nothing -> getSizeT 1
  alloca ty sizeTm malign
eval (Load (Typed (L.Alias a) v) malign) = do
  ty <- withLC (`llvmLookupAlias` a)
  eval (Load (Typed ty v) malign)
eval (Load tv@(Typed (L.PtrTo ty) _) _malign) = do
  addrTerm <- getTypedTerm "load" tv
  dumpMem 6 "load pre"
  v <- load addrTerm
  return (Typed ty v) <* dumpMem 6 "load post"
eval e@(Load _ _) = illegal $ "Load operand: " ++ show (ppSymExpr e)
eval (ICmp op (Typed (L.Alias a) v1) v2) = do
  t <- withLC (`llvmLookupAlias` a)
  eval (ICmp op (Typed t v1) v2)
eval (ICmp op (Typed t v1) v2) = do
  x <- typedValue <$> getTypedTerm "icmp@1" (Typed t v1)
  y <- typedValue <$> getTypedTerm "icmp@2" (Typed t v2)
  -- Removed checks because type may be alias.
  --CE.assert (t == t1 && t == t2 && (isIntegerType t || L.isPointer t)) $ return ()
  Typed i1 <$> withSBE (\sbe -> applyICmp sbe op x y)
eval (FCmp _op _tv1 _v2      ) = unimpl "eval FCmp"
eval (Val tv)                  = getTypedTerm "eval@Val" tv
eval e@GEP{}                   = evalGEP e
eval (Select tc (Typed (L.Alias a) v1) v2) = do
  t <- withLC (`llvmLookupAlias` a)
  eval (Select tc (Typed t v1) v2)
eval (Select tc tv1 v2)        = do
  [Typed _ c, Typed t x, Typed _ y] <- mapM (getTypedTerm "eval@Select") [tc, tv1, typedAs tv1 v2]
  sbe <- gets symBE
  r <- case asBool sbe c of
         Just True  -> return x
         Just False -> return y
         Nothing    -> liftSBE (applyIte sbe c x y)
  return (Typed t r)
eval (ExtractValue tv i      ) = evalExtractValue tv i
eval (InsertValue _tv _ta _i ) = unimpl "eval InsertValue"

evalPtrToInt, evalIntToPtr ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => EvalContext sbe
  -> Typed L.Value
  -> L.Type
  -> Simulator sbe m (Typed (SBETerm sbe))
evalPtrToInt ec (Typed (L.Alias a) v) t2 = do
  let t1 = llvmLookupAlias (evalLLVMContext ec) a
   in evalPtrToInt ec (Typed t1 v) t2
evalPtrToInt ec tv (L.Alias a) = do
  evalPtrToInt ec tv (llvmLookupAlias (evalLLVMContext ec) a)
evalPtrToInt ec tv@(Typed t1 _) t2@(L.PrimType (L.Integer tgtWidth)) = do
  Typed t v <- getTypedTerm' ec tv
  CE.assert(t == t1) $ return ()
  addrWidth <- fromIntegral <$> withLC llvmAddrWidthBits
  Typed t2 <$>
    if tgtWidth == addrWidth
      then return v
      else let op = if addrWidth > tgtWidth then L.Trunc else L.ZExt
           in withSBE $ \s -> applyConv s op v t2
evalPtrToInt _ _ _ = errorPath $ FailRsn "Invalid parameters to evalPtrToInt"

evalIntToPtr ec (Typed (L.Alias a) v) t2 =
  let t1 = llvmLookupAlias (evalLLVMContext ec) a
   in evalIntToPtr ec (Typed t1 v) t2
evalIntToPtr ec tv (L.Alias a) = do
  evalIntToPtr ec tv (llvmLookupAlias (evalLLVMContext ec) a)
evalIntToPtr ec tv@(Typed t1@(L.PrimType (L.Integer srcWidth)) _) t2 = do
  Typed t v <- getTypedTerm' ec tv
  CE.assert (t == t1) $ return ()
  addrWidth <- fromIntegral <$> withLC llvmAddrWidthBits
  Typed t2 <$>
    if srcWidth == addrWidth
      then return v
      else let op = if srcWidth > addrWidth then L.Trunc else L.ZExt
           in withSBE $ \s -> applyConv s op v t2
evalIntToPtr _ _ _ = errorPath $ FailRsn "Invalid parameters to evalIntToPtr"

evalExtractValue ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => Typed SymValue -> [Int32] -> Simulator sbe m (Typed (SBETerm sbe))
evalExtractValue (Typed (L.Alias a) v) idxs = do
  t <- withLC (`llvmLookupAlias` a)
  evalExtractValue (Typed t v) idxs
evalExtractValue tv idxs = do
  sv <- getTypedTerm "evalExtractValue" tv
  go sv idxs
    where go v [] = return v
          go (Typed (L.Struct ftys) v) (i : is) = impl v ftys i is
          go (Typed (L.PackedStruct ftys) v) (i : is) = impl v ftys i is
          go (Typed (L.Array n ty) v) (i : is) =
            impl v (replicate (fromIntegral n) ty) i is
          go _ _ = error "non-composite type in extractvalue"
          impl v tys i is =
            CE.assert (fromIntegral i <= length tys) $ do
              vs <- withSBE $ \sbe -> termDecomp sbe tys v
              go (vs !! fromIntegral i) is

evalGEP ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => SymExpr -> Simulator sbe m (Typed (SBETerm sbe))
evalGEP (GEP ib (Typed (L.Alias a) v) idxs) = do
  t <- withLC (`llvmLookupAlias` a)
  evalGEP (GEP ib (Typed t v) idxs)
-- TODO: check bounds when 'ib' is True
evalGEP (GEP _ib tv0 idxs0) = impl idxs0 =<< getTypedTerm "evalGEP" tv0
  where
    impl [] (Typed referentTy ptrVal) = do
      return $ Typed (L.PtrTo referentTy) ptrVal

    impl (idx:idxs) (Typed (L.PtrTo referentTy) ptrVal) = do
      impl idxs =<< baseOffset idx referentTy ptrVal

    impl (idx:idxs) (Typed (L.Array _len elemTy) ptrVal) = do
      impl idxs =<< baseOffset idx elemTy ptrVal

    impl (idx : idxs) (Typed (L.Struct fldTys) ptrVal) = do
      Typed _ idxTerm <- getTypedTerm "evalGep@2" idx
      (skipFlds, head -> fldTy) <- do
        midxVal <- withSBE' (\sbe -> snd <$> asSignedInteger sbe idxTerm)
        case midxVal of
          Nothing -> illegal "Failed to obtain concrete value for GEP index"
          Just n  -> return $ splitAt (fromIntegral n) fldTys
      newPtrVal <- Typed fldTy <$> foldM addSz ptrVal skipFlds
      impl idxs newPtrVal

    impl idxs (Typed (L.Alias ident) v) = do
      impl idxs =<< (`Typed` v) <$> withLC (`llvmLookupAlias` ident)

    impl _ tv = do
      unimpl $ "GEP: support for aggregate type NYI: "
               ++ show (L.ppType (typedType tv))
               ++ " : " ++ show (typedType tv)

    -- @addSz p ty@ computes @p + sizeof(ty)
    addSz p ty = termAdd p . typedValue
                   =<< promote =<< getTypedTerm "addSz" =<< sizeof ty

    -- @baseOffset i ty p@ computes @p + i * sizeof(ty)@
    baseOffset ::
      ( MonadIO m
      , Functor m
      , Functor sbe
      )
      => Typed SymValue -> L.Type -> SBETerm sbe
      -> Simulator sbe m (Typed (SBETerm sbe))
    baseOffset idx referentTy ptrVal = do
      Typed _ idxTerm <- promote =<< getTypedTerm "baseOffset" idx
      Typed _ szTerm  <- promote =<< getTypedTerm "baseOffset" =<< sizeof referentTy
      r <- termAdd ptrVal =<< termMul idxTerm szTerm
      return (Typed referentTy r)

    -- @promote x@ promotes integer value x to the target's pointer width
    promote :: (MonadIO m, Functor m, Functor sbe)
      => Typed (SBETerm sbe) -> Simulator sbe m (Typed (SBETerm sbe))
    promote x@(Typed (L.PrimType (L.Integer iw)) v1) = do
      aw <- fromIntegral <$> withLC llvmAddrWidthBits
      if aw > iw
        then Typed (intn aw) <$> termConv L.SExt v1 (intn aw)
        else return x
    promote _ = illegal "promotion of non-integer value"
evalGEP e = illegal $ "evalGEP: expression is not a GEP: " ++ show (ppSymExpr e)

evalCE ::
  ( MonadIO m
  , Functor sbe
  , Functor m
  )
  => EvalContext sbe -> L.Value -> Simulator sbe m (Typed (SBETerm sbe))
evalCE ec (L.ValConstExpr ce)
  = case ce of
      -- TODO: check bounds when 'ib' is True
      L.ConstGEP inbounds (splitAt 1 -> ((head -> ptr), idxs)) ->
        evalGEP (GEP inbounds ptr idxs)
      L.ConstConv L.BitCast tv t ->
        Typed t . typedValue <$> getTypedTerm' ec tv
      L.ConstConv L.PtrToInt tv t ->
        evalPtrToInt ec tv t
      L.ConstConv L.IntToPtr tv t ->
        evalIntToPtr ec tv t
      _ -> unimpl $ "evalCE: " ++ show ce
evalCE _ e = illegal $ "evalCE: value expression is not const" ++ show (L.ppValue e)

-----------------------------------------------------------------------------------------
-- Term operations and helpers

termAdd, termMul :: (Functor m, Monad m)
  => SBETerm sbe -> SBETerm sbe -> Simulator sbe m (SBETerm sbe)
termAdd x y = withSBE $ \sbe -> applyArith sbe (L.Add False False) x y
termMul x y = withSBE $ \sbe -> applyArith sbe (L.Mul False False) x y

termConv :: (Functor m, Monad m)
  => L.ConvOp -> SBETerm sbe -> L.Type -> Simulator sbe m (SBETerm sbe)
termConv op x ty = withSBE $ \sbe -> applyConv sbe op x ty

--------------------------------------------------------------------------------
-- SBE lifters and helpers

liftSBE :: Monad m => sbe a -> Simulator sbe m a
liftSBE sa = gets liftSymBE >>= \f -> f sa

withSBE :: (Functor m, Monad m) => (SBE sbe -> sbe a) -> Simulator sbe m a
withSBE f = gets symBE >>= \sbe -> liftSBE (f sbe)

withSBE' :: (Functor m, Monad m) => (SBE sbe -> a) -> Simulator sbe m a
withSBE' f = gets symBE >>= \sbe -> return (f sbe)

-- @getMem@ yields the memory model of the current path, which must exist.
getMem :: (Functor m, Monad m) =>  Simulator sbe m (Maybe (SBEMemory sbe))
getMem = fmap (pathMem <$>) getPath

-- @setMem@ sets the memory model in the current path, which must exist.
setMem :: (Functor m, Monad m) => SBEMemory sbe -> Simulator sbe m ()
setMem mem = modifyPath $ \p -> p { pathMem = mem }

withLC :: (Functor m, MonadIO m) => (LLVMContext -> a) -> Simulator sbe m a
withLC f = f <$> gets (cbLLVMCtx . codebase)

--------------------------------------------------------------------------------
-- Callbacks and event handlers

cb1 :: (Functor m, Monad m)
  => (SEH sbe m -> a -> Simulator sbe m ()) -> a -> Simulator sbe m ()
cb1 f x   = join $ gets (f . evHandlers) <*> pure x

cb2 :: (Functor m, Monad m)
  => (SEH sbe m -> a -> b -> Simulator sbe m ()) -> a -> b -> Simulator sbe m ()
cb2 f x y = join $ gets (f . evHandlers) <*> pure x <*> pure y

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

type AllocRslt sbe =
  Either (StackAllocaResult (SBETerm sbe) (SBEMemory sbe))
         (HeapAllocResult   (SBETerm sbe) (SBEMemory sbe))
type AllocAct sbe =
  SBE sbe -> SBEMemory sbe -> Typed (SBETerm sbe) -> sbe (AllocRslt sbe)

alloca, malloc ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => L.Type
  -> Typed (SBETerm sbe)
  -> Maybe Int
  -> Simulator sbe m (Typed (SBETerm sbe))
alloca ty sztm malign = do
  Just m <- getMem
  sbe <- gets symBE
  rslt <- liftSBE $ stackAlloca sbe m ty sztm (maybe 0 lg malign)
  case rslt of
    SASymbolicCountUnsupported  -> errorPath $ FailRsn $
      "Stack allocation only supports a concrete element count "
        ++ "(try a different memory model?)"
    SAResult c t m' -> do
      setMem m'
      let fr = memFailRsn sbe ("Failed alloca allocation of type " ++ show (L.ppType ty)) []
      processMemCond fr c
      return (Typed (L.PtrTo ty) t)

malloc ty sztm malign = doAlloc ty sztm $ \s m nt ->
  Right <$> heapAlloc s m ty nt (maybe 0 lg malign)

doAlloc ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => L.Type -> Typed (SBETerm sbe) -> AllocAct sbe
  -> Simulator sbe m (Typed (SBETerm sbe))
doAlloc ty sztm allocActFn = do
  Just m        <- getMem
  rslt          <- withSBE $ \s -> allocActFn s m sztm
  sbe <- gets symBE
  (c, t, m', s) <- case rslt of
    Left SASymbolicCountUnsupported  -> errorPath $ err "alloca"
    Left (SAResult c t m')           -> do
      liftSBE $ memDump sbe m' Nothing
      return (c, t, m', "alloca")
    Right HASymbolicCountUnsupported -> errorPath $ err "malloc"
    Right (HAResult c t m')          -> return (c, t, m', "malloc")
  setMem m'
  let fr =  memFailRsn sbe ("Failed " ++ s ++ " allocation of type " ++ show (L.ppType ty)) []
  processMemCond fr c
  return (Typed (L.PtrTo ty) t)
  where
    err s = FailRsn
            $ s ++ " only support concrete element count "
                ++ "(try a different memory model?)"

-- | Load value at addr in current path.
load ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => Typed (SBETerm sbe) -> Simulator sbe m (SBETerm sbe)
load addr = do
  sbe <- gets symBE
  Just mem <- getMem
  (cond, v) <- liftSBE $ memLoad sbe mem addr
  let fr = memFailRsn sbe "Invalid load address" [typedValue addr]
  processMemCond fr cond
  return v

store ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => Typed (SBETerm sbe) -> SBETerm sbe -> Simulator sbe m ()
store val dst = do
  Just m <- getMem
  sbe <- gets symBE
  (c, m') <- liftSBE $ memStore sbe m val dst
  setMem m'
  let fr = memFailRsn sbe "Invalid store address: " [dst]
  processMemCond fr c

memFailRsn :: SBE sbe -> String -> [SBETerm sbe] -> FailRsn
memFailRsn sbe desc terms = do
  --TODO: See if we can get a reasonable location for the failure.
  let pts = map (prettyTermD sbe) terms
   in FailRsn $ show $ text desc <+> ppTuple pts

--------------------------------------------------------------------------------
-- Misc utility functions

setSEH :: Monad m => SEH sbe m -> Simulator sbe m ()
setSEH seh = modify $ \s -> s{ evHandlers = seh }

newPathName :: Monad m => Simulator sbe m Integer
newPathName = do
  cnt <- gets pathCounter
  modify $ \s -> s{ pathCounter = cnt + 1 }
  return cnt

unlessQuiet :: MonadIO m => Simulator sbe m () -> Simulator sbe m ()
unlessQuiet act = getVerbosity >>= \v -> unless (v == 0) act

-- For user feedback that gets silenced when verbosity = 0.
tellUser :: (MonadIO m) => String -> Simulator sbe m ()
tellUser msg = unlessQuiet $ dbugM msg

-- | Returns the a term representing the target-specific number of bytes
-- required to store a value of the given type.
sizeof :: (MonadIO m, Functor m) => L.Type -> Simulator sbe m (Typed L.Value)
sizeof ty = Typed (L.PrimType (L.Integer 32))
              <$> L.ValInteger <$> withLC (`llvmAllocSizeOf` ty)

resolveCallee ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => SymValue -> Simulator sbe m (Either String L.Symbol)
resolveCallee callee = case callee of
 L.ValSymbol sym   -> ok sym
 L.ValIdent i      -> resolveIdent i
 L.ValConstExpr{}  -> do
   ec <- getEvalContext "resolveCallee" Nothing
   findDefineByPtr =<< evalCE ec callee
 L.ValAsm{}        -> err $ "Inline assembly is not supported: " ++ show (L.ppValue callee)
 _                 -> err $ "Unexpected callee value: " ++ show (L.ppValue callee) ++ ":" ++ show callee
 where
   resolveIdent i = do
     Just p <- getPath
     findDefineByPtr =<< lkupIdent i (pathRegs p)
   findDefineByPtr (Typed ty fp) = case L.elimFunPtr ty of
     Nothing -> err "Callee is not a function pointer"
     _       -> do
       r <- resolveFunPtrTerm fp
       case r of
         Result sym -> ok sym
         _        -> do
           sbe <- gets symBE
           err $ "Failed to resolve callee function pointer: "
                 ++ show (L.ppValue callee) ++ "\n"
                 ++ show r ++ "\n"
                 ++ show (prettyTermD sbe fp)
   ok sym  = return $ Right $ sym
   err msg = return $ Left $ "resolveCallee: " ++ msg

resolveFunPtrTerm ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => SBETerm sbe -> Simulator sbe m LookupSymbolResult
resolveFunPtrTerm fp = do
  Just m <- getMem
  withSBE $ \s -> codeLookupSymbol s m fp

lkupIdent ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => L.Ident -> RegMap (SBETerm sbe) -> Simulator sbe m (Typed (SBETerm sbe))
lkupIdent i regs = do
  case M.lookup i regs of
    Just x  -> return x
    Nothing -> illegal
               $ "lkupIdent failure: "
                 ++ show (L.ppIdent i)
                 ++ " is not in regmap of given call frame."

runStmts ::
  ( LogMonad m
  , Functor m
  , MonadIO m
  , Functor sbe
  )
  => [SymStmt] -> Simulator sbe m ()
runStmts = mapM_ dbugStep

entryRsltReg :: Reg
entryRsltReg = L.Ident "__galois_final_rslt"

-- | Obtain the first pending path in the topmost merge frame; @Nothing@ means
-- that the control stack is empty or the top entry of the control stack has no
-- pending paths recorded.
getPath :: (Functor m, Monad m)
  => Simulator sbe m (Maybe (Path sbe))
getPath = do
  mcs <- popMF <$> gets ctrlStk
  return $
    case mcs of
      Nothing -> Nothing
      Just (mf,_) -> safeHead (pendingPaths mf)


-- | Manipulate the control stack
modifyCS :: Monad m => (CS sbe -> CS sbe) -> Simulator sbe m ()
modifyCS f = modify $ \s -> s{ ctrlStk = f (ctrlStk s) }

-- | Manipulate the current path (i.e., the first pending path in topmost
-- control stack entry)
modifyPath :: (Functor m , Monad m)
  => (Path sbe -> Path sbe) -> Simulator sbe m ()
modifyPath f = modifyCS $ \cs ->
  let Just (mf,cs') = popMF cs
      Just (p,mf') = popPending mf
   in pushMF (pushPending (f p) mf') cs'

modifyPathRegsM :: (Functor m, Monad m) => (RegMap (SBETerm sbe) -> RegMap (SBETerm sbe)) -> Simulator sbe m ()
modifyPathRegsM = modifyPath . modifyPathRegs

type StdOvd m sbe =
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => Override sbe m

checkTypeCompat :: Monad m => L.Declare -> String -> L.Declare -> Simulator sbe m ()      
checkTypeCompat fd tnm td = do
  let nm = show . L.ppSymbol . L.decName
  let e rsn = error $ "Attempt to replace " ++ nm fd
                     ++ " with function " ++ tnm ++ " that " ++ rsn
  let ppTypes :: [L.Type] -> String
      ppTypes tys = '(' : intercalate ", " (map (show . L.ppType) tys) ++ ")" 
  unless (L.decArgs fd == L.decArgs td) $ e $ "has different argument types.\n" 
    ++ "  Argument types of " ++ nm fd ++ ": " ++ ppTypes (L.decArgs fd) ++ "\n"
    ++ "  Argument types of " ++ tnm ++ ": " ++ ppTypes (L.decArgs td) ++ "\n"
  when (L.decVarArgs fd && not (L.decVarArgs td)) $
    e "is a non-variable argument function."
  when (not (L.decVarArgs fd) && L.decVarArgs td) $
    e "is a variable argument function."
  unless (L.decRetType fd == L.decRetType td) $ e $ "has a different return type.\n"
    ++ "  Return type of " ++ nm fd ++ ": " ++ show (L.ppType (L.decRetType fd)) ++ "\n"
    ++ "  Return type of " ++ tnm ++ ": " ++ show (L.ppType (L.decRetType td)) ++ "\n"

registerOverride ::
  ( Functor m
  , Functor sbe
  , MonadIO m
  )
  => L.Symbol -> L.Type -> [L.Type] -> Bool -> Override sbe m
  -> Simulator sbe m ()
registerOverride sym retTy argTys va handler = do
  -- TODO: Verify function exists and argument types match types of override.
  dm <- gets (cbDeclareMap . codebase)
  case M.lookup sym dm of
    Nothing -> return ()
    Just fd -> do
      let td = L.Declare { L.decName = L.Symbol "override"   
                         , L.decArgs = argTys
                         , L.decVarArgs = va
                         , L.decRetType = retTy
                         }
      checkTypeCompat fd "override" td
      modify $ \s ->
        s { fnOverrides = M.insert sym (handler, False) (fnOverrides s)
          }

--------------------------------------------------------------------------------
-- Error handling

unimpl, illegal ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => String -> Simulator sbe m a
unimpl msg  = errorPath $ FailRsn $ "UN{SUPPORTED,IMPLEMENTED}: " ++ msg
illegal msg = errorPath $ FailRsn $ "ILLEGAL: " ++ msg

-- | Push an error path to the
pushErrorPath :: (Functor m, Monad m) => String -> MF sbe -> SBETerm sbe -> Simulator sbe m ()
pushErrorPath nm mf pa =
  case getMergedState nm mf of
    EmptyState _ _ -> pushMergeFrame mf
    PathState mergedPath assertions -> do
      -- Update assertions with negation.
      na <- withSBE $ \sbe -> applyBNot sbe pa
      a' <- withSBE $ \sbe -> applyAnd sbe na assertions
      let ps' = PathState mergedPath a'
      pushMergeFrame $ setMergedState ps' mf

errorPath ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => FailRsn -> Simulator sbe m a
errorPath rsn = do
  -- Pop the control stack and move the current path to the error paths list
  mmf <- popMergeFrame $ "errorPath: " ++ show rsn
  let (p,mf) = maybe err id (popPending mmf)
        where err = error $ "errorPath has empty path with " ++ show rsn
  pushErrorPath "errorPath" mf (pathAssumptions p)
  sbe <- gets symBE
  whenVerbosity (>=3) $ do
    dbugM $ "Error path encountered: " ++ show (ppFailRsn rsn)
    dbugM $ show $ ppPath sbe p
  modify $ \s -> s{ errorPaths = EP rsn p : errorPaths s }
  -- Merge negation of assumptions in current path into conditions on merge frame.

  -- NB: Since we've set up the control stack for the next invocation of
  -- run, and explicitly captured the error path, we need to be sure to
  -- ship that modified state back to the catch site so it execution can
  -- continue correctly.
  throwError =<< ErrorPathExc rsn <$> get

--------------------------------------------------------------------------------
-- Debugging

ppPathM :: (MonadIO m, Functor m) => String -> Path sbe -> Simulator sbe m ()
ppPathM desc p = do
  sbe <- gets symBE
  dbugM $ desc ++ "\n" ++ show (ppPath sbe p)
  withSBE (\s -> memDump s (pathMem p) Nothing)

prettyTermSBE :: (Functor m, Monad m) => SBETerm sbe -> Simulator sbe m Doc
prettyTermSBE t = withSBE' $ \s -> prettyTermD s t

dumpMem :: (Functor m, MonadIO m) => Int -> String -> Simulator sbe m ()
dumpMem v msg =
  whenVerbosity (>=v) $ do
    dbugM $ msg ++ ":"
    Just m <- getMem
    withSBE (\s -> memDump s m Nothing)

dbugStep ::
  ( LogMonad m
  , MonadIO m
  , Functor m
  , Functor sbe
  )
  => SymStmt -> Simulator sbe m ()
dbugStep stmt = do
  mp <- getPath
  case mp of
    Nothing -> dbugM' 2 $ "Executing: (no current path): " ++ show (ppSymStmt stmt)
    Just p  -> do
      dbugM' 2 $ "Executing ("
                 ++ "#" ++ show (pathName p) ++ "): "
                 ++ show (L.ppSymbol (pathFuncSym p))
                 ++ maybe "" (show . parens . ppSymBlockID) (pathCB p)
                 ++ ": " ++
                 case stmt of
                   IfThenElse{} -> "\n"
                   _ -> ""
                 ++ show (ppSymStmt stmt)
--  repl
  cb1 onPreStep stmt
  step stmt
  cb1 onPostStep stmt
  dumpCtrlStk' 5

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

dbugTypedTerm :: (MonadIO m, Functor m) => String -> Typed (SBETerm sbe) -> Simulator sbe m ()
dbugTypedTerm desc (Typed ty t) =
  dbugTerm (desc ++ "(" ++ show (L.ppType ty) ++ ")") t

_nowarn_unused :: a
_nowarn_unused = undefined
  (dbugTerm undefined undefined :: Simulator IO IO ())
  (dbugTypedTerm undefined undefined :: Simulator IO IO ())
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
            where fn = show $ L.ppSymbol $ pathFuncSym p
  liftIO $ putStrLn $ "Warning" ++ prefix ++ ". " ++ msg

-- | Load a null-termianted string at given address.
-- May fail if a symbolic byte is found.
loadString ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => String -> L.Typed (SBETerm sbe) -> Simulator sbe m String
loadString nm ptr = do
  case typedType ptr of
    L.PtrTo (L.PrimType (L.Integer 8)) -> do
      -- Load ptr, ptr+1, until zero byte, convert each into char,
      -- assemble into list
      cs <- go ptr
      return $ map (toEnum . fromEnum) $ cs
      where go addr = do
              t <- load addr
              c <- withSBE' $ \s -> snd <$> asUnsignedInteger s t
              ptrWidth <- withLC llvmAddrWidthBits
              one <- withSBE $ \s -> termInt s ptrWidth 1
              addr' <- termAdd (typedValue addr) one
              case c of
                Nothing -> do
                  errorPath $ FailRsn $
                    "Encountered a symbolic byte in " ++ nm ++ "."
                Just 0 -> return []
                Just v -> (v:) <$> go (typedAs addr addr')
    ty -> errorPath $ FailRsn
          $ "loading string with invalid type: "
            ++ show (L.ppType ty)

termIntS :: (Functor m, Monad m, Integral a) =>
            Int -> a -> Simulator sbe m (SBETerm sbe)
termIntS w n = withSBE $ \s -> termInt s w (fromIntegral n)

data PrintfFlags = PrintfFlags {
    zeroPad :: Bool
  }

printfToString :: (Functor sbe, Functor m, MonadIO m)
               => String -> [L.Typed (SBETerm sbe)] -> Simulator sbe m String
printfToString fmt args = procString fmt 0 []
  where vargs = V.fromList args
        valueAt p | 0 <= p && p < V.length vargs = typedValue $ vargs V.! p
                  | otherwise = error $ "Could not get argument at position " ++ show p
        defaultFlags = PrintfFlags { zeroPad = False }
        procString ('%':r) p rs = procArg r defaultFlags p rs
        procString (c:r) p rs = procString r p (c:rs)
        procString [] _ rs = return (reverse rs)
        procArg ('d':r) _ p rs = procRest r (p+1) rs =<< printSigned (valueAt p)
        procArg ('i':r) _ p rs = procRest r (p+1) rs =<< printSigned (valueAt p)
        procArg ('p':r) _ p rs = procRest r (p+1) rs =<< printPointer (valueAt p)
        procArg ('u':r) _ p rs = procRest r (p+1) rs =<< printUnsigned (valueAt p)
        procArg ('s':r) _ p rs = procRest r (p+1) rs =<< printString (valueAt p)
        procArg r       _ _ _  = error $ "Unsupported format string " ++ show r
        procRest r p rs s = procString r p (reverse s ++ rs)
        printSigned val = withSBE' $ \sbe ->
          case asSignedInteger sbe val of
            Just (_,v) -> show v
            Nothing -> show (prettyTermD sbe val)
        printUnsigned val = withSBE' $ \sbe ->
          case asSignedInteger sbe val of
            Just (_,v) -> show v
            Nothing -> show (prettyTermD sbe val)
        printPointer val = withSBE' $ \sbe ->
          case asUnsignedInteger sbe val of
            Just (_,v) -> "0x" ++ showHex v ""
            Nothing -> show (prettyTermD sbe val)
        printString val = loadString "printToString" (i8p =: val)


printfHandler :: StdOvd sbe m
printfHandler = Override $ \_sym _rty args ->
  case args of
    (fmtPtr : rest) -> do
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
    [ptr] -> do
      v <- load ptr
      d <- withSBE' $ \sbe -> prettyTermD sbe v
      liftIO $ print d
      return Nothing
    _ -> errorPath $ FailRsn "lss_print_symbolic: wrong number of arguments"

allocHandler :: (Functor m, Monad m, MonadIO m, Functor sbe)
             => (L.Type
                   -> Typed (SBETerm sbe)
                   -> Maybe Int
                   -> Simulator sbe m (Typed (SBETerm sbe)))
             -> Override sbe m
allocHandler fn = Override $ \_sym _rty args ->
  case args of
    [sizeTm] -> (Just . typedValue) <$> fn i8 sizeTm Nothing
    _ -> e "alloca: wrong number of arguments"
  where
    e = errorPath . FailRsn

abortHandler :: StdOvd sbe m
abortHandler = Override $ \_sym _rty args -> do
  case args of
    [tv@(Typed t _)]
      | t == strTy -> do
          msg <- loadString "abort message" tv
          e $ "lss_abort(): " ++ msg
      | otherwise -> e "Incorrect type passed to lss_abort()"
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
    [(Typed _t v)] -> do
      v' <- withSBE' $ \s -> snd <$> asUnsignedInteger s v
      case v' of
        Nothing  ->
          e "symbolic verbosity is illegal"
        Just v'' -> do
          setVerbosity (fromIntegral v'')
          return Nothing
    _ -> e "Incorrect number of parameters passed to lss_set_verbosity"
  where
    e = errorPath . FailRsn

{-
--TODO: Fix exit handler
exitHandler :: StdOvd sbe m
exitHandler = Override $ \_sym _rty args -> do
  case args of
    [Typed t v]
      | not (isIntegerType t) -> e "Non-integer type passed to exit()"
      | otherwise             -> do
          rvt <- prettyTermSBE v
          e $ "exit() invoked with argument " ++ show rvt
    _ -> e "Incorrect number of parameters passed to exit()"
  where
    e = errorPath . FailRsn
-}

assertHandler__assert_rtn :: StdOvd sbe m
assertHandler__assert_rtn = Override $ \_sym _rty args -> do
  case args of
    [v1@(Typed t1 _), Typed t2 v2, Typed t3 v3, Typed t4 v4] ->
      if t1 == i8p && t2 == i8p && t3 == i32 && t4 == i8p
        then do
          fname     <- loadString "assert function" v1
          file      <- loadString "assert filename" (i8p =: v2)
          Just line <- withSBE' (\s -> snd <$> asSignedInteger s v3)
          err       <- loadString "assert error message" (i8p =: v4)
          e $ unwords [ "__assert_rtn:"
                      , file ++ ":" ++ show line ++ ":" ++ fname ++ ":"
                      , err
                      ]
        else e "Argument(s) of wrong type passed to __assert_rtn"
    _ -> e "Incorrect number of parameters passed to __assert_rtn()"
  where
    e = errorPath . FailRsn

freshInt' :: Int -> StdOvd sbe m
freshInt' n = Override $ \_ _ _ -> Just <$> withSBE (flip freshInt n)

freshIntArray :: Int -> StdOvd sbe m
freshIntArray n = Override $ \_sym _rty args ->
  case args of
    [sizeTm, _, _] -> do
      msize <- withSBE' $ \s -> snd <$> asUnsignedInteger s (typedValue sizeTm)
      case msize of
        Just size -> do
          let sz = fromIntegral size
              sz32 = fromIntegral size
              ety = intn . toEnum . fromEnum $ n
              ty = L.Array sz32 ety
          arrPtr <- typedValue <$> alloca ety sizeTm Nothing
          elts <- replicateM sz (withSBE $ flip freshInt n)
          arrTm <- withSBE $ flip termArray elts
          let typedArrTm = Typed ty arrTm
          store typedArrTm arrPtr
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
    [t, fptr] -> do
      file <- loadString "lss_write_aiger_uint file" fptr
      checkAigFile file
      withSBE $ \s -> writeAiger s file [typedValue t]
      return Nothing
    _ -> errorPath
         $ FailRsn "lss_write_aiger_uint: wrong number of arguments"

addAigOutput :: StdOvd sbe m
addAigOutput = Override $ \_sym _rty args ->
  case args of
    [t] -> do
      modify $ \s -> s{ aigOutputs = typedValue t : aigOutputs s }
      return Nothing
    _   -> errorPath
           $ FailRsn "lss_aiger_add_output: wrong number of arguments"

addAigArrayOutput :: StdOvd sbe m
addAigArrayOutput = Override $ \_sym _rty args ->
  case args of
    [tptr, sizeTm] -> do
      msize <- withSBE' $ \s -> snd <$> asUnsignedInteger s (typedValue sizeTm)
      case (msize, typedType tptr) of
        (Just size, L.PtrTo tgtTy) -> do
          elems <- loadArray tptr tgtTy size
          arrTm <- withSBE $ flip termArray elems
          modify $ \s -> s{ aigOutputs = arrTm : aigOutputs s }
          return Nothing
        (Nothing, _) ->
          e "lss_aiger_add_output_array called with symbolic size"
        _ -> e "lss_aiger_add_output_array: invalid argument type"
    _ -> e "lss_aiger_add_output_array: wrong number of arguments"
  where
    e = errorPath . FailRsn

writeCollectedAigerOutputs :: StdOvd sbe m
writeCollectedAigerOutputs = Override $ \_sym _rty args ->
  case args of
    [fptr] -> do
      outputTerms <- reverse <$> gets aigOutputs
      if null outputTerms
        then e "lss_write_aiger: no AIG outputs have been collected"
        else do
          file <- loadString "lss_write_aiger file" fptr
          withSBE $ \s -> writeAiger s file outputTerms
          modify $ \s -> s{ aigOutputs = [] }
          return Nothing
    _ -> e "lss_write_aiger: wrong number of arguments"
  where
    e = errorPath . FailRsn

writeIntArrayAiger :: L.Type -> StdOvd sbe m
writeIntArrayAiger _ety = Override $ \_sym _rty args ->
  case args of
    [tptr, sizeTm, fptr] -> do
      msize <- withSBE' $ \s -> snd <$> asUnsignedInteger s (typedValue sizeTm)
      case (msize, typedType tptr) of
        (Just size, L.PtrTo tgtTy) -> do
          elems <- loadArray tptr tgtTy size
          arrTm <- withSBE $ flip termArray elems
          file <- loadString "lss_write_aiger_array_uint" fptr
          checkAigFile file
          withSBE $ \s -> writeAiger s file [arrTm]
          return Nothing
        (Nothing, _) ->
          e "lss_write_aiger_array_uint called with symbolic size"
        _ -> e "lss_write_aiger_array_uint: invalid argument type"
    _ -> e "lss_write_aiger_array_uint: wrong number of arguments"
  where
    e = errorPath . FailRsn

writeCNF :: StdOvd sbe m
writeCNF = Override $ \_sym _rty args ->
  case args of
    [t, fptr] -> do
      file <- loadString "lss_write_cnf" fptr
      _ <- withSBE $ \s -> writeCnf s file (typedValue t)
      return Nothing
    _ -> e "lss_write_cnf: wrong number of arguments"
  where
    e = errorPath . FailRsn

loadArray ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => Typed (SBETerm sbe)
  -> L.Type
  -> Integer
  -> Simulator sbe m [SBETerm sbe]
loadArray ptr ety count = do
  ptrWidth <- withLC llvmAddrWidthBits
  elemWidth <- withLC (`llvmStoreSizeOf` ety)
  inc <- withSBE $ \s -> termInt s ptrWidth elemWidth
  go inc ptr count
    where go _one _addr 0 = return []
          go one addr size = do
            t     <- load addr
            addr' <- termAdd (typedValue addr) one
            (t:) <$> go one (typedAs addr addr') (size - 1)

storeArray ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => SBETerm sbe
  -> L.Type
  -> [SBETerm sbe]
  -> Simulator sbe m ()
storeArray ptr ety elems = do
  ptrWidth <- withLC llvmAddrWidthBits
  elemWidth <- withLC (`llvmStoreSizeOf` ety)
  inc <- withSBE $ \s -> termInt s ptrWidth elemWidth
  go inc ptr elems
    where go _one _addr [] = return ()
          go one addr (e:es) = do
            store (Typed ety e) addr
            addr' <- termAdd addr one
            go one addr' es

evalAigerOverride :: StdOvd m sbe
evalAigerOverride =
  Override $ \_sym _rty args ->
    case args of
      [Typed _ tm, p@(Typed (L.PtrTo ety) _), Typed _ szTm] -> do
        msz <- withSBE' $ \s -> snd <$> asUnsignedInteger s szTm
        case msz of
          Just sz -> do
            elems <- loadArray p ety sz
            ints <- mapM
                    (\t -> withSBE' $ \s -> snd <$> asUnsignedInteger s t)
                    elems
            let bools = map (not . (== 0)) $ catMaybes ints
            Just <$> (withSBE $ \s -> evalAiger s bools tm)
          Nothing -> e "lss_eval_aiger: symbolic size not supported"
      _ -> e "lss_eval_aiger: wrong number of arguments"
  where
    e = errorPath . FailRsn

evalAigerArray :: L.Type -> StdOvd sbe m
evalAigerArray ty =
  Override $ \_sym _rty args ->
    case args of
      [sym, dst, szTm, input@(Typed (L.PtrTo ety) _), inputSz] -> do
        msz <- withSBE' $ \s -> snd <$> asUnsignedInteger s (typedValue szTm)
        misz <- withSBE' $ \s -> snd <$> asUnsignedInteger s (typedValue inputSz)
        case (msz, misz) of
          (Just sz, Just isz) -> do
            inputs <- loadArray input ety isz
            ints <- mapM
                    (\t -> withSBE' $ \s -> snd <$> asUnsignedInteger s t)
                    inputs
            let bools = map (not . (== 0)) $ catMaybes ints
            tm <- loadArray sym ty sz
            tm' <- withSBE $ \s -> termArray s tm
            res <- withSBE $ \s -> evalAiger s bools tm'
            let tys = replicate (fromIntegral sz) ety
            res' <- withSBE $ \s -> termDecomp s tys res
            storeArray (typedValue dst) ety (map typedValue res')
            return Nothing
          _ -> e "lss_eval_aiger_array: symbolic sizes not supported"
      _ -> e "lss_eval_aiger_array: wrong number of arguments"
  where
    e = errorPath . FailRsn

overrideByName :: StdOvd sbe m
overrideByName = Override $ \_sym _rty args ->
  case args of
    [fromNamePtr, toNamePtr] -> do
      from <- fromString <$> loadString "lss_override_function_by_name from" fromNamePtr
      to   <- fromString <$> loadString "lss_override_function_by_name to" toNamePtr
      from `userRedirectTo` to
    _ -> errorPath
         $ FailRsn "lss_override_function_by_name: wrong number of arguments"

overrideByAddr :: StdOvd sbe m
overrideByAddr = Override $ \_sym _rty args ->
  case args of
    [_, _] -> do
      [mfromSym, mtoSym] <- mapM (resolveFunPtrTerm . typedValue) args
      case (mfromSym, mtoSym) of
        (Result from, Result to) -> from `userRedirectTo` to
        _                    -> resolveErr
    _ -> argsErr
  where
    e          = errorPath . FailRsn
    resolveErr = e "overrideByAddr: Failed to resolve function pointer"
    argsErr    = e "lss_override_function_by_addr: wrong number of arguments"

overrideIntrinsic :: StdOvd sbe m
overrideIntrinsic = Override $ \_sym _rty args ->
  case args of
    [nmPtr, fp] -> do
      nm  <- fromString <$> loadString "lss_override_llvm_intrinsic" nmPtr
      msym <- resolveFunPtrTerm (typedValue fp)
      case msym of
        Result sym -> nm `userRedirectTo` sym
        _ -> e "overrideIntrinsic: Failed to resolve function pointer"
    _ -> e "lss_override_llvm_intrinsic: wrong number of arguments"
  where
    e = errorPath . FailRsn

userRedirectTo :: MonadIO m
  => L.Symbol -> L.Symbol -> Simulator sbe m (Maybe (SBETerm sbe))
userRedirectTo from to = do
  dm <- gets (cbDeclareMap . codebase)
  let nameOf = show . L.ppSymbol 
  --TODO: Add better error messages.
  case (M.lookup from dm, M.lookup to dm) of
    (Nothing,_) -> error $ "Could not find symbol " ++ nameOf from ++ "."
    (_,Nothing) -> error $ "Could not find symbol " ++ nameOf to ++ "."  
    (Just fd,Just td) -> do
      checkTypeCompat fd (show (L.ppSymbol (L.decName td))) td
      modify $ \s ->
        s{ fnOverrides = M.insert from (Redirect to, True) (fnOverrides s) }
      return Nothing

overrideResetByName :: StdOvd sbe m
overrideResetByName = Override $ \_sym _rty args ->
  case args of
    [fnNamePtr] -> do
      fnSym <- fromString <$> loadString "lss_override_reset_by_name" fnNamePtr
      fnSym `userRedirectTo` fnSym
    _ -> errorPath
         $ FailRsn "lss_override_reset_by_name: wrong number of arguments"

overrideResetByAddr :: StdOvd sbe m
overrideResetByAddr = Override $ \_sym _rty args ->
  case args of
    [fp] -> do
      msym <- resolveFunPtrTerm (typedValue fp)
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
    [] -> do ovds <- gets fnOverrides
             forM_ (M.assocs ovds) $ \(sym, (_, userOvd)) ->
               when userOvd $ modify $ \s ->
                 s{ fnOverrides = M.delete sym (fnOverrides s)}
             return Nothing
    _ -> e "lss_override_reset_all: wrong number of arguments"
  where
    e = errorPath . FailRsn

type OverrideEntry sbe m = (L.Symbol, L.Type, [L.Type], Bool, Override sbe m)

registerLibcOverrides :: (Functor m, MonadIO m, Functor sbe) => Simulator sbe m ()
registerLibcOverrides = do
  aw <- withLC llvmAddrWidthBits
  let sizeT = intn (fromIntegral aw)
  dm <- gets (cbDeclareMap . codebase)
  -- Register malloc
  case M.lookup "malloc" dm of
    Nothing -> return ()
    Just d -> do
      unless (L.decArgs d == [sizeT] && not (L.decVarArgs d)) $ do
        error "malloc has unexpected arguments."
      case L.decRetType d of
        L.PtrTo _ -> return ()
        _ -> error "malloc has unexpected return type."
      registerOverride "malloc" (L.decRetType d) [sizeT] False $
        allocHandler malloc
  registerOverrides
    [ ("__assert_rtn", voidTy, [i8p, i8p, i32, i8p], False, assertHandler__assert_rtn)
    --, ("exit", voidTy, [i32], False, exitHandler)
    , ("alloca", voidPtr, [sizeT], False, allocHandler alloca)
    , ("free", voidTy, [voidPtr], False,
       -- TODO: stub! Does this need to be implemented?
       Override $ \_sym _rty _args -> return Nothing)
    , ("printf", i32, [strTy], True, printfHandler)
    ]

registerLSSOverrides :: (Functor m, MonadIO m, Functor sbe) => Simulator sbe m ()
registerLSSOverrides = registerOverrides
  [ ("lss_abort", voidTy, [strTy], False, abortHandler)
  , ("lss_print_symbolic", voidTy, [i8p], False, printSymbolic)
  , ("lss_fresh_uint8",   i8,  [i8], False, freshInt'  8)
  , ("lss_fresh_uint16", i16, [i16], False, freshInt' 16)
  , ("lss_fresh_uint32", i32, [i32], False, freshInt' 32)
  , ("lss_fresh_uint64", i64, [i64], False, freshInt' 64)
  , ("lss_fresh_array_uint8",   i8p, [i32,  i8, i8p], False, freshIntArray 8)
  , ("lss_fresh_array_uint16", i16p, [i32, i16, i16p], False, freshIntArray 16)
  , ("lss_fresh_array_uint32", i32p, [i32, i32, i32p], False, freshIntArray 32)
  , ("lss_fresh_array_uint64", i64p, [i32, i64, i64p], False, freshIntArray 64)
  , ("lss_aiger_add_output_uint8",  voidTy,  [i8], False, addAigOutput)
  , ("lss_aiger_add_output_uint16", voidTy, [i16], False, addAigOutput)
  , ("lss_aiger_add_output_uint32", voidTy, [i32], False, addAigOutput)
  , ("lss_aiger_add_output_uint64", voidTy, [i64], False, addAigOutput)
  , ("lss_aiger_add_output_array_uint8" , voidTy, [ i8p, i32], False, addAigArrayOutput)
  , ("lss_aiger_add_output_array_uint16", voidTy, [i16p, i32], False, addAigArrayOutput)
  , ("lss_aiger_add_output_array_uint32", voidTy, [i32p, i32], False, addAigArrayOutput)
  , ("lss_aiger_add_output_array_uint64", voidTy, [i64p, i32], False, addAigArrayOutput)
  , ("lss_write_aiger", voidTy, [strTy], False, writeCollectedAigerOutputs)
  , ("lss_write_aiger_uint8",  voidTy, [i8,  strTy], False, writeIntAiger)
  , ("lss_write_aiger_uint16", voidTy, [i16, strTy], False, writeIntAiger)
  , ("lss_write_aiger_uint32", voidTy, [i32, strTy], False, writeIntAiger)
  , ("lss_write_aiger_uint64", voidTy, [i64, strTy], False, writeIntAiger)
  , ("lss_write_aiger_array_uint8", voidTy, [i8p, i32, strTy], False,
     writeIntArrayAiger i8)
  , ("lss_write_aiger_array_uint16", voidTy, [i16p, i32, strTy], False,
     writeIntArrayAiger i16)
  , ("lss_write_aiger_array_uint32", voidTy, [i32p, i32, strTy], False,
     writeIntArrayAiger i32)
  , ("lss_write_aiger_array_uint64", voidTy, [i64p, i32, strTy], False,
     writeIntArrayAiger i64)
  , ("lss_eval_aiger_uint8",   i8, [i8,  i8p, i32], False, evalAigerOverride)
  , ("lss_eval_aiger_uint16", i16, [i16, i8p, i32], False, evalAigerOverride)
  , ("lss_eval_aiger_uint32", i32, [i32, i8p, i32], False, evalAigerOverride)
  , ("lss_eval_aiger_uint64", i64, [i64, i8p, i32], False, evalAigerOverride)
  , ("lss_eval_aiger_array_uint8",  voidTy, [i8p,  i8p,  i32, i8p, i32], False, evalAigerArray i8)
  , ("lss_eval_aiger_array_uint16", voidTy, [i16p, i16p, i32, i8p, i32], False, evalAigerArray i16)
  , ("lss_eval_aiger_array_uint32", voidTy, [i32p, i32p, i32, i8p, i32], False, evalAigerArray i32)
  , ("lss_eval_aiger_array_uint64", voidTy, [i64p, i64p, i32, i8p, i32], False, evalAigerArray i64)
  , ("lss_write_cnf", voidTy, [i32, strTy], False, writeCNF)
  , ("lss_override_function_by_name", voidTy, [strTy, strTy], False, overrideByName)
  , ("lss_override_function_by_addr", voidTy, [strTy, strTy], False, overrideByAddr)
  , ("lss_override_llvm_intrinsic", voidTy, [strTy, strTy], False, overrideIntrinsic)
  , ("lss_override_reset_by_name", voidTy, [strTy], False, overrideResetByName)
  , ("lss_override_reset_by_addr", voidTy, [strTy], False, overrideResetByAddr)
  , ("lss_override_reset_all", voidTy, [], False, overrideResetAll)
  , ("lss_show_path", voidTy, [], False, showPathOverride)
  , ("lss_show_mem", voidTy, [], False, showMemOverride)
  , ("lss_set_verbosity", voidTy, [i32], False, userSetVebosityOverride)
  ]

registerOverrides ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => [OverrideEntry sbe m] -> Simulator sbe m ()
registerOverrides = mapM_ fn
  where fn (sym, rty, atys, va, handler) = registerOverride sym rty atys va handler
