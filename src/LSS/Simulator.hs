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
  , getLC
  , mkStructInfo
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
--  , getTypedTerm
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

import Data.LLVM.Symbolic.Translation

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
  let lifter :: forall v . sbe v -> Simulator sbe IO v
      lifter = SM . lift . lift . sbeRunIO sbe
  let newSt = State { codebase     = cb
                    , symBE        = sbe
                    , liftSymBE    = lifter
                    , ctrlStk      = initialCtrlStk sbe mem
                    , globalTerms  = M.empty
                    , fnOverrides  = M.empty
                    , verbosity    = 6
                    , evHandlers   = seh
                    , errorPaths   = []
                    , lssOpts      = fromMaybe defaultLSSOpts mopts
                    , pathCounter  = 1
                    , aigOutputs   = []
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
     ec <- getEvalContext "adGlobal" Nothing
     forM_ globals $ \g -> do
       let key = (L.globalSym g, Nothing)
       cb1 onMkGlobTerm g
       Just symValue <- withLLVMContext $
         return $ liftValue (L.globalType g) (L.globalValue g)
       cdata <- getTypedTerm' ec symValue
       let tpcdata = L.Typed (L.globalType g) cdata
       cb2 onPreGlobInit g cdata
       let noDataSpc = "Not enough space in data segment to allocate new global."
       _ <- insertGlobalTerm noDataSpc key (L.globalType g) $ \s m ->
              memInitGlobal s m tpcdata
       cb2 onPostGlobInit g tpcdata

callDefine_ ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => L.Symbol     -- ^ Callee symbol
  -> L.Type       -- ^ Callee return type
  -> [SBETerm sbe] -- ^ Callee arguments
  -> Simulator sbe m ()
callDefine_ c t ag = callDefine c t ag >> return ()

-- | External entry point for a function call.  The argument generator is used
-- to create actuals passed to the function, and the return value is those
-- arguments.  In the case when no arguments created or invoking
-- intrinsics/overrides, the return value will always be the empty list.
callDefine ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => L.Symbol     -- ^ Callee symbol
  -> L.Type       -- ^ Callee return type
  -> [SBETerm sbe] -- ^ Callee argument generator
  -> Simulator sbe m [SBETerm sbe]
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

tryModifyCS :: Monad m => String -> (CS sbe -> Maybe (CS sbe)) -> Simulator sbe m ()
tryModifyCS ctx f = modify $ modifyCS fn
  where fn = fromMaybe (error err) . f
          where err = "internal: tryModifyCS " ++ show ctx

tryModifyCSIO :: MonadIO m => String -> (CS sbe -> Maybe (IO (CS sbe))) -> Simulator sbe m ()
tryModifyCSIO ctx f = do
  let err = "internal: tryModifyCSIO " ++ show ctx
  s <- get
  cs <- liftIO $ fromMaybe (error err) $ f (ctrlStk s)
  put s { ctrlStk = cs }

setCurrentBlock :: MonadIO m => SymBlockID -> Simulator sbe m ()
setCurrentBlock b = do
  sbe <- gets symBE
  tryModifyCSIO "setCurrentBlock" $ jumpCurrentPath sbe b

callDefine' ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => Bool                                         -- ^ Is this a redirected call?
  -> SymBlockID                                   -- ^ Normal call return block id
  -> L.Symbol                                     -- ^ Callee symbol
  -> Maybe (Typed Reg)                            -- ^ Callee return type and result register
  -> [SBETerm sbe] -- ^ Callee arguments
  -> Simulator sbe m [SBETerm sbe]
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
      modifyPathRegs $ setReturnValue "callDefine'" mreg r
      setCurrentBlock normalRetID
      return []
  where
    normal
      | isPrefixOf "llvm." calleeName = do
          intrinsic calleeName mreg args
          setCurrentBlock normalRetID
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
  -> [SBETerm sbe] -- ^ Callee arguments
  -> Simulator sbe m [SBETerm sbe]
runNormalSymbol normalRetID calleeSym mreg args = do
  def <- lookupSymbolDef calleeSym
  tryModifyCS "runNormalSymbol" $
    pushCallFrame calleeSym normalRetID mreg
  dbugM' 5 $ "callDefine': callee " ++ show (L.ppSymbol calleeSym)
  modifyPathRegs $ \_ -> bindArgs (sdArgs def) args
  -- Push stack frame in current process memory.
  do Just m <- getMem
     (c,m') <- withSBE $ \s -> stackPushFrame s m
     setMem m'
     let fr = FailRsn "Stack push frame failure: insufficient stack space"
     processMemCond fr c
  return args
  where
    err doc = error $ "callDefine/bindArgs: " ++ render doc

    bindArgs formals actuals
      | length formals /= length actuals =
          err $ text "incorrect number of actual parameters"
            $+$ text "formals: " <> text (show formals)
      | otherwise =
          foldr bindArg M.empty (formals `zip` actuals)
    bindArg (k,v) = M.insert (typedValue k) (L.Typed (typedType k) v) 

intrinsic ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => String -> Maybe (Typed Reg) -> [SBETerm sbe]
  -> Simulator sbe m ()
intrinsic intr mreg args0 =
  case (intr, mreg) of
    ("llvm.memcpy.p0i8.p0i8.i64", Nothing)    -> memcpy
    ("llvm.memset.p0i8.i64", Nothing)         -> memset 64
    ("llvm.uadd.with.overflow.i64", Just reg) -> uaddWithOverflow 64 reg
    ("llvm.objectsize.i32", Just reg)         -> objSz 32 reg
    ("llvm.objectsize.i64", Just reg)         -> objSz 64 reg
    -- Do nothing.
    ("llvm.lifetime.start", Nothing) -> return ()
    ("llvm.lifetime.end", Nothing) -> return ()
    _ -> whenVerbosity (>= 1) $ do --TODO: Give option of stopping on warnings like this.
      tellUser $ "Warning: skipping unsupported LLVM intrinsic " ++ show intr     
  where
    memcpy = do
      let [dst, src, len, align, _isvol] = args0
      Just m <- getMem
      (c,m') <- withSBE $ \sbe -> memCopy sbe m dst src len align
      setMem m'
      sbe <- gets symBE
      let pts = map (prettyTermD sbe) [dst,src,len]
      let fr = FailRsn $
                 "memcopy operation was not valid: (dst,src,len) = "
                   ++ show (parens $ hcat $ punctuate comma $ pts)
      processMemCond fr c
    memset lenWidth = do
      let [dst, val, len, align, _isvol] = args0
      memSet dst val lenWidth len align
    uaddWithOverflow w reg = do
      let [x, y] = args0
      (ov,z') <- withSBE $ \sbe -> applyUAddWithOverflow sbe w x y
      res <- withSBE $ \sbe -> termStruct sbe [ L.Typed (intn (fromIntegral w)) z'
                                              , L.Typed i1 ov
                                              ]
      assign reg res
    objSz w reg = do
      let [_ptr, maxOrMin] = args0
      mval <- withSBE' $ \s -> snd <$> asUnsignedInteger s maxOrMin
      case mval of
        Nothing -> errorPath $ FailRsn $ "llvm.objectsize.i{32,64} expects concrete 2nd parameter"
        Just v  -> assign reg =<< withSBE (\s -> termInt s w tv)
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
  lenVal <- withSBE' $ \s -> snd <$> asUnsignedInteger s len
  case lenVal of
    Just 0 -> return ()
    _ -> do
      store (L.Typed i8 val) dst
      lc <- getLC
      let ptrWidth = llvmAddrWidthBits lc
      one      <- withSBE  $ \s -> termInt s ptrWidth 1
      negone   <- withSBE  $ \s -> termInt s lenWidth (-1)
      dst'     <- termAdd ptrWidth dst one
      len'     <- termAdd lenWidth len negone
      memSet dst' val lenWidth len' align


-- | Return value of this path.
pathRetVal :: Path sbe -> Maybe (SBETerm sbe)
pathRetVal p = typedValue <$> M.lookup entryRsltReg (pathRegs p)

getProgramReturnValue :: (Monad m, Functor m)
  => Simulator sbe m (Maybe (SBETerm sbe))
getProgramReturnValue =
 (pathRetVal <=< getCurrentPath) <$> gets ctrlStk

getProgramFinalMem :: (Monad m, Functor m)
  => Simulator sbe m (Maybe (SBEMemory sbe))
getProgramFinalMem =
  (fmap pathMem . getCurrentPath) <$> gets ctrlStk

withCurrentPath :: (Functor m, Monad m)
                => String
                -> (Path sbe -> Simulator sbe m (a, Path sbe))
                -> Simulator sbe m a
withCurrentPath nm fn = do
  s <- get
  case modifyCurrentPathM (ctrlStk s) fn of
    Nothing -> error $ "internal: withCurrentPath had no path " ++ show nm 
    Just mr -> do
      (r,cs) <- mr
      put s { ctrlStk = cs }
      return r
  
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
    _ ->
      withCurrentPath "processMemCond" $ \p -> do
        -- TODO: provide more detail here?
        whenVerbosity (>= 6) $ do
          tellUser $ "Warning: Obtained symbolic validity result from memory model."
          tellUser $ "This means that certain memory accesses were valid only on some paths."
          tellUser $ "In this case, the symbolic validity result was encountered at:"
          tellUser $ show $ ppPathLoc sbe p
          tellUser ""
        p' <- liftSBE $ addPathAssertion sbe cond p
        return ((), p')

-- | Return true if the path has asserted false to be true, and therefore we
-- can call errorPath on it.
pathAssertedFalse :: SBE sbe -> Path sbe -> Bool
pathAssertedFalse sbe p = asBool sbe (pathAssertions p) == Just False

run ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => Simulator sbe m ()
run = do
  cs <- gets ctrlStk
  if isFinished cs then
    case getCurrentPath cs of
      Just p -> do
        -- Normal program termination on at least one path.
        -- Report termination info at appropriate verbosity levels; also,
        -- inform user about error paths when present and optionally dump
        -- them.
        whenVerbosity (>=5) $ dumpCtrlStk
        whenVerbosity (>=2) $ do
          dbugM "run terminating normally: found valid exit frame"
          case pathRetVal p of
            Nothing -> dbugM "Program had no return value."
            Just rv -> dbugTerm "Program returned value" rv
          numErrs <- length <$> gets errorPaths
          showEPs <- optsErrorPathDetails <$> gets lssOpts
          when (numErrs > 0 && not showEPs) $
            tellUser "Warning: Some paths yielded errors. To see details, use --errpaths."
          when (numErrs > 0 && showEPs) $ do
            dbugM $ showErrCnt numErrs
            dumpErrorPaths
      Nothing -> do
        -- All paths ended in errors.
        showEPs <- gets (optsErrorPathDetails . lssOpts)
        if showEPs then
          tellUser "All paths yielded errors!" >> dumpErrorPaths
        else
          tellUser "All paths yielded errors! To see details, use --errpaths."
  else do
    let Just p = getCurrentPath cs
    flip catchError handleError $ do
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

assign :: (Functor m, MonadIO m)
       => L.Typed Reg -> SBETerm sbe -> Simulator sbe m ()
assign (L.Typed tp reg) v = modifyPathRegs $ M.insert reg (L.Typed tp v)

-- | Evaluate condition in current path.
evalCond :: (Functor sbe, Functor m, MonadIO m) => SymCond -> Simulator sbe m (SBEPred sbe)
evalCond TrueSymCond = withSBE' sbeTruePred
evalCond (HasConstValue t w i) = do
  v <- getBackendValue "evalCond" t
  sbe <- gets symBE
  iv <- liftSBE $ termInt sbe w i
  liftSBE $ applyIEq sbe w v iv

data EvalContext sbe = EvalContext {
       evalContextName :: String
     , evalLLVMContext :: LLVMContext
     , evalGlobalTerms :: GlobalMap sbe
     , evalRegs :: Maybe (RegMap (SBETerm sbe))
     , evalSBE :: SBE sbe
     }

withLLVMContext :: Monad m
                 => ((?lc :: LLVMContext) => Simulator sbe m a)
                 -> Simulator sbe m a
withLLVMContext m = do
  lc <- gets (cbLLVMCtx . codebase)
  let ?lc = lc in m

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

getCurrentEvalContext :: (Functor m, Monad m) => String -> Simulator sbe m (EvalContext sbe)
getCurrentEvalContext nm = do
  mp <- getPath
  getEvalContext nm (pathRegs <$> mp)

getBackendValue :: (Functor m, MonadIO m, Functor sbe) 
                => String -> TypedSymValue -> Simulator sbe m (SBETerm sbe)
getBackendValue nm symValue = do
  ec <- getCurrentEvalContext nm
  getTypedTerm' ec symValue

getTypedTerm' ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => EvalContext sbe -> TypedSymValue -> Simulator sbe m (SBETerm sbe)
getTypedTerm' ec tsv = do
  let sbe = evalSBE ec
  let zeroInit ty = do
        szBytes <- fromIntegral <$> withLC (`llvmAllocSizeOf` ty)
        liftSBE $ termInt sbe (szBytes * 8) 0
  case tsv of
    SValInteger w x -> liftSBE $ termInt sbe w x
    SValDouble d -> liftSBE $ termDouble sbe d
    SValIdent i -> 
      case evalRegs ec of
        Just rm -> typedValue <$> lkupIdent i rm
        Nothing -> error $ "getTypedTerm' called by " ++ evalContextName ec
                      ++ " with missing frame."
    SValSymbol sym maybeTypes ->
      return $ typedValue $ getGlobalPtrTerm ec (sym, maybeTypes)
    SValNull{} -> liftSBE $ termInt sbe ptrWidth 0
      where ptrWidth = llvmAddrWidthBits (evalLLVMContext ec)
    SValArray etp el -> do
      valTerms <- mapM (getTypedTerm' ec) el
      liftSBE $ termArray sbe etp valTerms
    SValStruct flds -> do
      let convertFld (Typed tp v) = Typed tp <$> getTypedTerm' ec v
      fldv <- mapM convertFld flds
      liftSBE $ termStruct sbe fldv
    SValExpr te -> do
      tv <- mapM (getTypedTerm' ec) te
      liftSBE $ applyTypedExpr sbe tv
    SValUndef tp -> zeroInit tp
    SValZeroInit tp -> zeroInit tp

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
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => SymStmt -> Simulator sbe m ()

step (PushCallFrame callee args mres retTgt) = do
  ec <- getCurrentEvalContext "PushCallFrame"
  argTerms <- mapM (getTypedTerm' ec) args
  calleeSym <- 
    case callee of
      SValSymbol sym _ -> return sym
      _ -> do
        fp <- getTypedTerm' ec callee
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
  mrv <- mapM (getBackendValue "mergeReturn") mtv
  tryModifyCSIO "Return" $ returnCurrentPath sbe mrv

step (PushPendingExecution bid cond ml elseStmts) = do
  sbe <- gets symBE
  c <- evalCond cond
  case asBool sbe c of
   -- Don't bother with elseStmts as condition is true. 
   Just True  -> setCurrentBlock bid
   -- Don't bother with pending path as condition is false.
   Just False -> runStmts elseStmts
   Nothing -> do
     s <- get
     let nm = pathCounter s
     put s { pathCounter = nm + 1 }
     tryModifyCS "PushPendingExecution" $ addCtrlBranch c bid nm ml
     runStmts elseStmts

step (SetCurrentBlock bid) = setCurrentBlock bid

step (Assign reg expr) = assign reg =<< eval expr

step (Store (Typed valType val) addr _malign) = do
  whenVerbosity (<=6) $ dumpMem 6 "store pre"
  ec <- getCurrentEvalContext "store"
  valTerm  <- getTypedTerm' ec val
  addrTerm <- getTypedTerm' ec addr
  store (Typed valType valTerm) addrTerm
  whenVerbosity (<=6) $ dumpMem 6 "store post"

step Unreachable
  = error "step: Encountered 'unreachable' instruction"

step Unwind = unimpl "unwind"
step (BadSymStmt s) = unimpl (show (L.ppStmt s))

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
  => SymExpr -> Simulator sbe m (SBETerm sbe)
eval (Val tv) = getBackendValue "eval@Val" tv
eval (Alloca ty msztv malign) = do
  sizeTm <-
    case msztv of
      Just (w,v) -> L.Typed (L.PrimType (L.Integer (fromIntegral w))) <$>
                      getBackendValue "alloca" v
      Nothing -> getSizeT 1
  typedValue <$> alloca ty sizeTm malign
eval (Load v ty _malign) = do
  addrTerm <- getBackendValue "load" v
  dumpMem 6 "load pre"
  load (L.Typed (L.PtrTo ty) addrTerm) <* dumpMem 6 "load post"

-----------------------------------------------------------------------------------------
-- Term operations and helpers

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

-- @getMem@ yields the memory model of the current path, which must exist.
getMem :: (Functor m, Monad m) =>  Simulator sbe m (Maybe (SBEMemory sbe))
getMem = fmap (pathMem <$>) getPath

-- @setMem@ sets the memory model in the current path, which must exist.
setMem :: (Functor m, Monad m) => SBEMemory sbe -> Simulator sbe m ()
setMem mem = tryModifyCS "setMem" $ modifyPath $ \p -> p { pathMem = mem }

getLC :: Monad m => Simulator sbe m LLVMContext
getLC = gets (cbLLVMCtx . codebase)

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
  Either (StackAllocaResult sbe)
         (HeapAllocResult   sbe)
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

unlessQuiet :: MonadIO m => Simulator sbe m () -> Simulator sbe m ()
unlessQuiet act = getVerbosity >>= \v -> unless (v == 0) act

-- For user feedback that gets silenced when verbosity = 0.
tellUser :: (MonadIO m) => String -> Simulator sbe m ()
tellUser msg = unlessQuiet $ dbugM msg

-- | Returns the a term representing the target-specific number of bytes
-- required to store a value of the given type.
-- The result has width llvmAddrWidthBits
sizeof :: (MonadIO m, Functor m) => L.Type -> Simulator sbe m TypedSymValue
sizeof ty = withLC $ \lc ->
  SValInteger (llvmAddrWidthBits lc) (llvmAllocSizeOf lc ty)

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
  ( Functor m
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
getPath = gets (getCurrentPath . ctrlStk)

modifyPathRegs :: (Functor m, Monad m)
                => (RegMap (SBETerm sbe) -> RegMap (SBETerm sbe)) -> Simulator sbe m ()
modifyPathRegs rmf = tryModifyCS "modifyPathRegs" $ modifyPath fn
  where fn p = p { pathRegs = rmf (pathRegs p) }

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

errorPath ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => FailRsn -> Simulator sbe m a
errorPath rsn = do
  sbe <- gets symBE
  -- Update control stack.
  Just p <- gets (getCurrentPath . ctrlStk)
  -- Log error path  
  whenVerbosity (>=3) $ do
    dbugM $ "Error path encountered: " ++ show (ppFailRsn rsn)
    dbugM $ show $ ppPath sbe p
  s <- get
  let Just mkCS = markCurrentPathAsError sbe (ctrlStk s)
  cs <- liftIO mkCS
  let s' = s { ctrlStk = cs
             , errorPaths = EP rsn p : errorPaths s 
             }
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
    Just m <- getMem
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
    Nothing -> dbugM' 2 $ "Executing: (no current path): " ++ show (ppSymStmt stmt)
    Just p  -> do
      dbugM' 2 $ "Executing ("
                 ++ "#" ++ show (pathName p) ++ "): "
                 ++ show (L.ppSymbol (pathFuncSym p))
                 ++ maybe "" (show . parens . ppSymBlockID) (pathCB p)
                 ++ ": " ++
                 case stmt of
                   PushPendingExecution{} -> "\n"
                   _ -> ""
                 ++ show (ppSymStmt stmt)
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
  => String -> SBETerm sbe -> Simulator sbe m String
loadString nm ptr = do
    -- Load ptr, ptr+1, until zero byte, convert each into char,
    -- assemble into list
    cs <- go ptr
    return $ map (toEnum . fromEnum) $ cs
  where go addr = do
          t <- load (L.Typed i8p addr)
          c <- withSBE' $ \s -> snd <$> asUnsignedInteger s t
          ptrWidth <- withLC llvmAddrWidthBits
          one <- withSBE $ \s -> termInt s ptrWidth 1
          addr' <- termAdd ptrWidth addr one
          case c of
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

printfToString :: (Functor sbe, Functor m, MonadIO m)
               => String -> [SBETerm sbe] -> Simulator sbe m String
printfToString fmt args = procString fmt 0 []
  where vargs = V.fromList args
        valueAt p | 0 <= p && p < V.length vargs = vargs V.! p
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
        printString val = loadString "printToString" val


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

printSymbolic :: L.Type -> StdOvd sbe m
printSymbolic tp = Override $ \_sym _rty args ->
  case args of
    [ptr] -> do
      v <- load (L.Typed tp ptr)
      d <- withSBE' $ \sbe -> prettyTermD sbe v
      liftIO $ print d
      return Nothing
    _ -> errorPath $ FailRsn "lss_print_symbolic: wrong number of arguments"

allocHandler :: (Functor m, Monad m, MonadIO m, Functor sbe)
             => Int32
             -> (L.Type
                   -> Typed (SBETerm sbe)
                   -> Maybe Int
                   -> Simulator sbe m (Typed (SBETerm sbe)))
             -> Override sbe m
allocHandler aw fn = Override $ \_sym _rty args ->
  case args of
    [sizeTm] -> (Just . typedValue) <$> fn i8 (L.Typed (intn aw) sizeTm) Nothing
    _ -> e "alloca: wrong number of arguments"
  where
    e = errorPath . FailRsn

abortHandler :: StdOvd sbe m
abortHandler = Override $ \_sym _rty args -> do
  case args of
    [tv] -> do
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
    [v] -> do
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
    [v1, v2, v3, v4] -> do
          fname     <- loadString "assert function" v1
          file      <- loadString "assert filename" v2
          Just line <- withSBE' (\s -> snd <$> asSignedInteger s v3)
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
    [sizeTm, _, _] -> do
      msize <- withSBE' $ \s -> snd <$> asUnsignedInteger s sizeTm
      case msize of
        Just size -> do
          let sz = fromIntegral size
              ety = intn . toEnum . fromEnum $ n
              ty = L.Array (fromIntegral size) ety
          arrPtr <- typedValue <$> alloca ety (L.Typed i32 sizeTm) Nothing
          elts <- replicateM sz (withSBE $ flip freshInt n)
          arrTm <- withSBE $ \sbe -> termArray sbe ety elts
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
      withSBE $ \s -> writeAiger s file [t]
      return Nothing
    _ -> errorPath
         $ FailRsn "lss_write_aiger_uint: wrong number of arguments"

addAigOutput :: StdOvd sbe m
addAigOutput = Override $ \_sym _rty args ->
  case args of
    [t] -> do
      modify $ \s -> s{ aigOutputs = t : aigOutputs s }
      return Nothing
    _   -> errorPath
           $ FailRsn "lss_aiger_add_output: wrong number of arguments"

addAigArrayOutput :: L.Type -- ^ Type of value target points to.
                  -> StdOvd sbe m
addAigArrayOutput tgtTy = Override $ \_sym _rty args ->
  case args of
    [tptr, sizeTm] -> do
      msize <- withSBE' $ \s -> snd <$> asUnsignedInteger s sizeTm
      case msize of
        Just size -> do
          elems <- loadArray (L.Typed (L.PtrTo tgtTy) tptr) tgtTy size
          arrTm <- withSBE $ \sbe -> termArray sbe tgtTy elems
          modify $ \s -> s{ aigOutputs = arrTm : aigOutputs s }
          return Nothing
        Nothing ->
          e "lss_aiger_add_output_array called with symbolic size"
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
writeIntArrayAiger ety = Override $ \_sym _rty args ->
  case args of
    [tptr, sizeTm, fptr] -> do
      msize <- withSBE' $ \s -> snd <$> asUnsignedInteger s sizeTm
      case msize of
        Just size -> do
          elems <- loadArray (L.Typed (L.PtrTo ety) tptr) ety size
          arrTm <- withSBE $ \sbe -> termArray sbe ety elems
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
    [t, fptr] -> do
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
  => Typed (SBETerm sbe) -- Term
  -> L.Type -- Element type
  -> Integer -- Count
  -> Simulator sbe m [SBETerm sbe]
loadArray ptr ety count = do
  ptrWidth <- withLC llvmAddrWidthBits
  elemWidth <- withLC (`llvmStoreSizeOf` ety)
  one <- withSBE $ \s -> termInt s ptrWidth elemWidth
  let go _addr 0 = return []
      go addr size = do
        t     <- load addr
        addr' <- termAdd ptrWidth (typedValue addr) one
        (t:) <$> go (typedAs addr addr') (size - 1)
  go ptr count

evalAigerOverride :: L.Type -> StdOvd m sbe
evalAigerOverride ety =
  Override $ \_sym _rty args ->
    case args of
      [tm, p, szTm] -> do
        msz <- withSBE' $ \s -> snd <$> asUnsignedInteger s szTm
        case msz of
          Just sz -> do
            elems <- loadArray (L.Typed (L.PtrTo ety) p) ety sz
            ints <- mapM
                    (\t -> withSBE' $ \s -> snd <$> asUnsignedInteger s t)
                    elems
            let bools = map (not . (== 0)) $ catMaybes ints
            Just <$> (withSBE $ \s -> evalAiger s bools tm)
          Nothing -> e "lss_eval_aiger: symbolic size not supported"
      _ -> e "lss_eval_aiger: wrong number of arguments"
  where
    e = errorPath . FailRsn

evalAigerArray :: L.Type -- Type of first argument pointer.
               -> StdOvd sbe m
evalAigerArray ty =
  Override $ \_sym _rty args ->
    case args of
      [sym, dst, szTm, input, inputSz] -> do
        msz <- withSBE' $ \s -> snd <$> asUnsignedInteger s szTm
        misz <- withSBE' $ \s -> snd <$> asUnsignedInteger s inputSz
        case (msz, misz) of
          (Just sz, Just isz) -> do
            inputs <- loadArray (L.Typed (L.PtrTo i8) input) i8 isz
            ints <- mapM
                    (\t -> withSBE' $ \s -> snd <$> asUnsignedInteger s t)
                    inputs
            let bools = map (not . (== 0)) $ catMaybes ints
            tm <- load (L.Typed (L.Array (fromInteger sz) ty) sym)
            res <- withSBE $ \s -> evalAiger s bools tm
            store (L.Typed (L.Array (fromInteger sz) ty) dst) res
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
      [mfromSym, mtoSym] <- mapM resolveFunPtrTerm args
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
      msym <- resolveFunPtrTerm fp
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
        allocHandler (fromIntegral aw) malloc
  registerOverrides
    [ ("__assert_rtn", voidTy, [i8p, i8p, i32, i8p], False, assertHandler__assert_rtn)
    --, ("exit", voidTy, [i32], False, exitHandler)
    , ("alloca", voidPtr, [sizeT], False, allocHandler (fromIntegral aw) alloca)
    , ("free", voidTy, [voidPtr], False,
       -- TODO: stub! Does this need to be implemented?
       Override $ \_sym _rty _args -> return Nothing)
    , ("printf", i32, [strTy], True, printfHandler)
    ]

registerLSSOverrides :: (Functor m, MonadIO m, Functor sbe) => Simulator sbe m ()
registerLSSOverrides = registerOverrides
  [ ("lss_abort", voidTy, [strTy], False, abortHandler)
  , ("lss_print_symbolic", voidTy, [i8p], False, printSymbolic i8)
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
  , ("lss_aiger_add_output_array_uint8" , voidTy, [ i8p, i32], False, addAigArrayOutput i8)
  , ("lss_aiger_add_output_array_uint16", voidTy, [i16p, i32], False, addAigArrayOutput i16)
  , ("lss_aiger_add_output_array_uint32", voidTy, [i32p, i32], False, addAigArrayOutput i32)
  , ("lss_aiger_add_output_array_uint64", voidTy, [i64p, i32], False, addAigArrayOutput i64)
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
  , ("lss_eval_aiger_uint8",   i8, [i8,  i8p, i32], False, evalAigerOverride i8)
  , ("lss_eval_aiger_uint16", i16, [i16, i8p, i32], False, evalAigerOverride i8)
  , ("lss_eval_aiger_uint32", i32, [i32, i8p, i32], False, evalAigerOverride i8)
  , ("lss_eval_aiger_uint64", i64, [i64, i8p, i32], False, evalAigerOverride i8)
  , ("lss_eval_aiger_array_uint8",  voidTy, [i8p,  i8p,  i32, i8p, i32], False
    , evalAigerArray i8)
  , ("lss_eval_aiger_array_uint16", voidTy, [i16p, i16p, i32, i8p, i32], False
    , evalAigerArray i16)
  , ("lss_eval_aiger_array_uint32", voidTy, [i32p, i32p, i32, i8p, i32], False
    , evalAigerArray i32)
  , ("lss_eval_aiger_array_uint64", voidTy, [i64p, i64p, i32, i8p, i32], False
    , evalAigerArray i64)
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
