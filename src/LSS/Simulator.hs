{- |
Module           : $Header$
Description      : The symbolic simulator for LLVM-Symbolic programs
Stability        : provisional
Point-of-contact : jstanley
-}

-- Debugging output at various verbosity levels:
-- 1: No output, the lowest verbosity level
-- 2: Instruction trace only
-- 3: (open)
-- 4: (open)
-- 5: Simulator internal state (control stack dump per instruction)
-- 6: Memory model dump on load/store operations only (for nontrivial codes,
--    this generates a /lot/ of output)
-- 7: Memory model dump pre/post every operation (for nontrivial codes, this
--    generates a /lot/ of output -- now with more output than level 6!)

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module LSS.Simulator
  ( module LSS.Execution.Codebase
  , callDefine
  , getProgramReturnValue
  , runSimulator
  , withSBE -- Exported so we can construct argument values.
  )
where

import           Control.Applicative
import           Control.Monad.State       hiding (State)
import           Data.Bits
import           Data.Int
import           Data.LLVM.Memory
import           Data.LLVM.Symbolic.AST
import           Data.List
import           Data.Maybe
import           LSS.Execution.Codebase
import           LSS.Execution.Common
import           LSS.Execution.MergeFrame
import           LSS.Execution.Utils
import           LSS.SBEInterface
import           Text.LLVM                 (Typed(..), (=:))
import           Text.PrettyPrint.HughesPJ
import           Verinf.Symbolic.Common    (ConstantProjection(..))
import qualified Control.Exception         as CE
import qualified Data.Map                  as M
import qualified Text.LLVM                 as L

runSimulator :: (Functor m, MonadIO m)
  => Codebase              -- ^ Post-transform LLVM code
  -> LLVMContext           -- ^ Memory alignment and type aliasing info
  -> SBE sbe               -- ^ A symbolic backend
  -> SBEMemory sbe         -- ^ The SBE's LLVM memory model
  -> LiftSBE sbe m         -- ^ Lift from symbolic backend to base monad
  -> Simulator sbe m a     -- ^ Simulator action to perform
  -> m a
runSimulator cb lc sbe mem lifter m =
  evalStateT (runSM (setup >> m)) (newSimState cb lc sbe mem lifter)
  where
    setup = do
      modifyCS $ pushMF emptyExitFrame

newSimState :: Codebase -> LLVMContext -> SBE sbe -> SBEMemory sbe -> LiftSBE sbe m -> State sbe m
newSimState cb lc sbe mem lifter =
  State
  { codebase    = cb
  , llvmCtx     = lc
  , symBE       = sbe
  , memModel    = mem
  , liftSymBE   = lifter
  , ctrlStk     = emptyCtrlStk
  , globalTerms = M.empty
  , verbosity   = 1
  }

callDefine ::
  ( LogMonad m
  , MonadIO m
  , Functor m
  , Functor sbe
  , ConstantProjection (SBEClosedTerm sbe)
  )
  => L.Symbol              -- ^ Callee symbol
  -> L.Type                -- ^ Callee return type
  -> [Typed (SBETerm sbe)] -- ^ Callee argumenxblitts
  -> Simulator sbe m ()
callDefine calleeSym t args = do
  callDefine' entryRetNormalID calleeSym (Just $ t =: entryRsltReg) args
  run

callDefine' ::(MonadIO m, Functor m, Functor sbe)
  => SymBlockID            -- ^ Normal call return block id
  -> L.Symbol              -- ^ Callee symbol
  -> Maybe (Typed Reg)     -- ^ Callee return type and result register
  -> [Typed (SBETerm sbe)] -- ^ Callee arguments
  -> Simulator sbe m ()
callDefine' normalRetID calleeSym@(L.Symbol calleeName) mreg args
  | isPrefixOf "llvm." calleeName
  = do
  CE.assert (isNothing mreg) $ return ()
  intrinsic calleeName args
  | otherwise
  = do
  def  <- lookupDefine calleeSym <$> gets codebase
  dbugM' 5 $ "callDefine': callee " ++ show (L.ppSymbol calleeSym)
  banners' 5 $ show $ ppSymDefine def
  path <- newPath $ CallFrame calleeSym $ bindArgs (sdArgs def) args
  modifyCS $ pushPendingPath path
           . pushMF (ReturnFrame mreg normalRetID
                       Nothing Nothing Nothing [])
  pushMemFrame
  where
    err doc = error $ "callDefine/bindArgs: " ++ render doc

    bindArgs formals actuals
      | length formals /= length actuals =
          err $ text "incorrect number of actual parameters"
      | otherwise =
          foldr bindArg M.empty (formals `zip` actuals)

    bindArg (Typed ft reg, v@(Typed at _)) mp
      | ft == at =
          let ok = M.insert reg v mp
          in
            -- It's doubtful that anything will remain excluded here, but this
            -- makes it explicit when we've not handled particular argument
            -- types.
            case at of
            L.PrimType L.Integer{} -> ok
            L.PtrTo{}              -> ok
            _ -> err $ text "unsupported arg type:" <+> L.ppType at
      | otherwise = err
          $ text "formal/actual type mismatch:"
            <+> L.ppType ft <+> text "vs." <+> L.ppType at

intrinsic :: (MonadIO m, Functor m, Functor sbe)
  => String -> [Typed (SBETerm sbe)] -> Simulator sbe m ()
intrinsic intr args0 =
  case intr of
    -- TODO: Handle intrinsic overrides
    "llvm.memcpy.p0i8.p0i8.i64" -> memcpy
    _ -> error $ "Unsupported LLVM intrinsic: " ++ intr
  where
    memcpy = do
      let [dst, src, len, align, _isvol] = map typedValue args0
      mutateMem_ $ \sbe mem -> memCopy sbe mem dst src len align

getProgramReturnValue :: (Monad m, Functor m)
  => Simulator sbe m (Maybe (SBETerm sbe))
getProgramReturnValue = do
  (top, _) <- popMF <$> gets ctrlStk
  case top of
    ExitFrame _ mrv -> return mrv
    _               -> error "getProgramReturnValue: program not yet terminated"

run ::
  ( LogMonad m
  , Functor m
  , MonadIO m
  , Functor sbe
  , ConstantProjection (SBEClosedTerm sbe)
  )
  => Simulator sbe m ()
run = do
  mtop <- topMF <$> gets ctrlStk
  case mtop of
    Nothing  -> error "run: empty control stack"
    Just top
      | isExitFrame top -> do
          -- Set the exit merge frame return value (if any) and clear merged
          -- state.
          modifyCS $ \(popMF -> (_, cs)) -> pushMF (finalizeExit top) cs
          dbugM' 2 $ "run terminating normally: found valid exit frame"
          dumpCtrlStk' 2
      | otherwise -> do
          case topPending top of
            Nothing -> error $ "internal: run: no path to execute"
            Just p  -> runPath p
  where
    runPath (pathCB -> Nothing)    = error "runPath: no current block"
    runPath p@(pathCB -> Just pcb) = withCallFrame p $ \frm -> do
      def <- lookupDefine (frmFuncSym frm) <$> gets codebase
      runStmts $ sbStmts $ lookupSymBlock def pcb
      run
    runPath _ = error "unreachable"

--------------------------------------------------------------------------------
-- LLVM-Sym operations

-- | @pushMemFrame@ tells the memory model to push a new stack frame to the
-- stack region.
pushMemFrame :: (MonadIO m, Functor m, Functor sbe) => Simulator sbe m ()
pushMemFrame = do
  dbugM' 6 "Memory model: pushing stack frame"
  mutateMem_ stackPushFrame

-- | @pushMemFrame@ tells the memory model to pop a stack frame from the stack
-- region.
popMemFrame :: (MonadIO m, Functor m, Functor sbe) => Simulator sbe m ()
popMemFrame = do
  dbugM' 6 "Memory model: popping stack frame"
  mutateMem_ stackPopFrame

-- | @popMergeFrame@ removes the top entry of the control stack; assumes
-- that the control stack is nonempty.
popMergeFrame :: Monad m => Simulator sbe m (MergeFrame (SBETerm sbe))
popMergeFrame = do
  s <- get
  let (mf, cs) = popMF (ctrlStk s)
  modifyCS $ const cs
  return mf

-- | @pushMergeFrame mf@ pushes mf to the control stack
pushMergeFrame :: Monad m => MergeFrame (SBETerm sbe) -> Simulator sbe m ()
pushMergeFrame = modifyCS . pushMF

assign :: (Functor m, Monad m)
  => Reg -> Typed (SBETerm sbe) -> Simulator sbe m ()
assign reg v = modifyCallFrameM $ \frm ->
  frm{ frmRegs = M.insert reg v (frmRegs frm) }

setCurrentBlockM :: (Functor m, Monad m) => SymBlockID -> Simulator sbe m ()
setCurrentBlockM bid = modifyPath (setCurrentBlock bid)

getCurrentBlockM :: (Functor m, Monad m) => Simulator sbe m SymBlockID
getCurrentBlockM = do
  mp <- getPath
  case mp of
    Nothing -> error "getCurrentBlock: no current path"
    Just p  -> maybe (error "getCurrentBlock: no current block") return (pathCB p)

-- | @clearCurrentExecution@ clears the current pending path from the top merge
-- frame; then, if no pending paths remain, it merges the top merge frame with
-- the merge frame beneath it on the control stack.
clearCurrentExecution ::
  (Functor m, MonadIO m)
  => Simulator sbe m ()
clearCurrentExecution = do
  top <- popMergeFrame
  if (1 == length (pendingPaths top))
    then do
      -- We just executed the last remaining path, so merge the current merge
      -- frame into the caller's merge frame.
      pushMergeFrame =<< mergeMFs top =<< popMergeFrame
    else do
      -- We still have pending paths, so only remove the current path.
      pushMergeFrame $ snd $ popPending top


mergeReturn :: (LogMonad m, Functor m, MonadIO m, Functor sbe)
  => Maybe (Typed SymValue)
  -> Simulator sbe m ()
mergeReturn mtv = do
  mtop <- topMF <$> gets ctrlStk
  case mtop of
    Just ReturnFrame{} -> return ()
    Just _             -> error "mergeReturn: expected return merge frame"
    Nothing            -> error "mergeReturn: empty control stack"

  case mtv of
    Nothing -> return ()
    Just tv -> do
      -- Set the return value in the current path
      rv <- getTypedTerm tv
      modifyPath $ setReturnValue (Just $ typedValue rv)

  -- Merge the current path into the merged state for the current merge frame.
  Just p <- getPath
  modifyMF $ modifyMergedState $ mergePaths p

  popMemFrame

  dbugM' 5 $ "After mergeReturn, but before clearCurrentExecution:"
  dumpCtrlStk' 5

-- | @mergeMFs src dst@ merges the @src@ merge frame into @dst@
mergeMFs :: (MonadIO m)
  => MergeFrame (SBETerm sbe)
  -> MergeFrame (SBETerm sbe)
  -> Simulator sbe m (MergeFrame (SBETerm sbe))

-- Source frame is a return frame
mergeMFs src@ReturnFrame{} dst = do
  let Just p = getMergedState src -- NB: src /must/ have a merged state.

  case dst of
    ExitFrame{} -> do
      -- Merge src's merged state with dst's merged state
      let f Nothing = Just p
          f ms      = mergePaths p ms
      return $ modifyMergedState f dst
    ReturnFrame{} -> do
      case pathRetVal p of
        Nothing -> return dst
        Just rv -> do
          -- Extract the return value from src's merged state and associate it
          -- with src's return register name in the call frame of dst's current
          -- path.
          case rfRetReg src of
            Nothing   -> error "mergeMFs: src return frame has RV but no return register"
            Just rreg -> do
              -- Associate the return value in src's merged state with src's
              -- return register name in the caller's call frame.
              return $ (`modifyPending` dst) $ \dstPath ->
                let trv = typedAs rreg rv
                    reg = typedValue rreg
                in
                  modifyCallFrame (setReg reg trv) dstPath
    PostdomFrame{} -> do
      error "mergeMFs: postdom dst frame nyi"

-- Source frame is a postdom frame
mergeMFs src@PostdomFrame{} dst = do
  let Just p = getMergedState src -- NB: src /must/ have a merged state.
  if isExitFrame dst
    then error "mergeMFs: postdom MF => exit MF is not allowed"
    else
      return $ (`modifyPending` dst) $ const p

mergeMFs _ _ = error "mergeMFs: unsupported source merge frame type"

-- | @mergePaths p1 p2@ merges path p1 into path p2, which may be empty; when p2
-- does is empty, this function produces p1 as the merged path. Yields Nothing
-- if merging fails.
mergePaths :: Path term -> Maybe (Path term) -> Maybe (Path term)
-- TODO: We'll need a better explanation for merge failure than "Nothing"; see
-- precondition violation explanation datatype in JSS, for example.
mergePaths _p1 (Just _p2) = error "real path merging nyi"
mergePaths p1 Nothing     = Just p1

-- | getTypedTerm' in the context of the current call frame
getTypedTerm :: (Functor m, MonadIO m)
  => Typed L.Value -> Simulator sbe m (Typed (SBETerm sbe))
getTypedTerm tv = (`getTypedTerm'` tv) =<< Just <$> getCallFrame

-- | Obtain the typed SBE term representation for the given LLVM value; performs
-- identifier lookup in the regmap of the given call frame as needed.  Note that
-- a call frame is only required for identifier lookup, and may be omitted for
-- other types of values.
getTypedTerm' :: (Functor m, MonadIO m, term ~ SBETerm sbe)
  => Maybe (CallFrame (SBETerm sbe)) -> Typed L.Value -> Simulator sbe m (Typed (SBETerm sbe))

getTypedTerm' _ (Typed t@(L.PrimType (L.Integer (fromIntegral -> w))) (L.ValInteger x))
  = Typed t <$> withSBE (\sbe -> termInt sbe w x)

getTypedTerm' _ (Typed (L.PtrTo (L.FunTy _rty argtys _isVarArgs)) (L.ValSymbol sym))
  = getGlobalPtrTerm (sym, Just argtys)

getTypedTerm' _ tv@(Typed (L.PtrTo L.FunTy{}) _)
  = error $ "getTypedTerm': Non-symbol ptr-to-fun nyi: " ++ show tv

getTypedTerm' mfrm (Typed ty@(L.Array len ety) (L.ValArray ety' es))
  = do
    CE.assert (ety == ety') $ return ()
    CE.assert (fromIntegral len == length es) $ return ()
    valTerms <- mapM (getTypedTerm' mfrm) (Typed ety <$> es)
    Typed ty <$> withSBE (\sbe -> termArray sbe (map typedValue valTerms))

getTypedTerm' _ (Typed _ (L.ValSymbol sym))
  = getGlobalPtrTerm (sym, Nothing)

getTypedTerm' mfrm (Typed _ (L.ValConstExpr ce))
  = case ce of
      L.ConstConv L.BitCast tv t ->
        Typed t . typedValue <$> getTypedTerm' mfrm tv
      _ -> error $ "getTypedTerm: ConstExpr eval nyi : " ++ show ce

getTypedTerm' (Just frm) (Typed _ (L.ValIdent i))
  = return $ lkupIdent i frm
getTypedTerm' mfrm tv@(Typed t v)
  = do
  sbe <- gets symBE
  error $ "getTypedTerm': unsupported value / call frame presence: "
          ++ show (L.ppType t) ++ " =: " ++ show (L.ppValue v)
          ++ show (parens $ text $ show tv)
          ++ show ("mfrm = " ++ show (ppCallFrame sbe <$> mfrm))

getGlobalPtrTerm :: (Functor m, MonadIO m)
  => (L.Symbol, Maybe [L.Type]) -> Simulator sbe m (Typed (SBETerm sbe))
getGlobalPtrTerm key@(sym, _) = do
  mt <- M.lookup key <$> gets globalTerms
  case mt of
    Just t  -> return t
    Nothing -> do
      cb <- gets codebase
      maybe err (either addGlobal addDef) (lookupSym sym cb)
  where
    err = error $ "getGlobalPtrTerm: symbol resolution failed: "
                  ++ show (L.ppSymbol sym)

    addDef def = do
      let argTys = map typedType $ sdArgs def
          fty    = L.FunTy (sdRetType def) argTys (sdVarArgs def)
          idl    = nub $ mapMaybe symBlockIdent $ M.keys (sdBody def)
      ins fty $ \sbe mem -> memAddDefine sbe mem sym idl

    addGlobal g = do
      cdata <- getTypedTerm' Nothing (L.globalType g =: L.globalValue g)
      ins (L.globalType g) $ \sbe mem -> memInitGlobal sbe mem cdata

    ins ty act = do
      t <- Typed (L.PtrTo ty) <$> mutateMem act
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
  , ConstantProjection (SBEClosedTerm sbe)
  )
  => SymStmt -> Simulator sbe m ()

step ClearCurrentExecution =
  clearCurrentExecution

step (PushCallFrame callee args mres) = do
  cb  <- getCurrentBlockM
  eab <- resolveCallee callee
  case eab of
    Left msg        -> error $ "PushCallFrame: " ++ msg
    Right calleeSym -> callDefine' cb calleeSym mres
                         =<< mapM getTypedTerm args

step (PushInvokeFrame _fn _args _mres _e) =
  error "PushInvokeFrame nyi"

step (PushPostDominatorFrame pdid) = do
  Just p <- getPath
  pushMergeFrame $ pushPending p $ emptyPdomFrame pdid

step (MergePostDominator pdid cond) = do
  mtop <- topMF <$> gets ctrlStk
  case mtop of
    Just (PostdomFrame _ _ lab)
      | lab == pdid -> return ()
      | otherwise   -> error "merge postdom: top pdom frame has unexpected block ID"
    Just _          -> error "merge postdom: expected postdom merge frame"
    Nothing         -> error "merge postdom: empty control stack"

  Just p <- getPath

  -- Construct the new path constraint for the current path
  newPC  <- case cond of
    TrueSymCond -> return (pathConstraints p)
                     &&& boolTerm True {- tmp: typecheck &&& -}
    HasConstValue{} -> error "path constraint addition: HasConstValue nyi"

  pretty <- withSBE' $ \sbe -> prettyTermD sbe newPC
  dbugM' 5 $ "New path constraint is: " ++ render pretty

  -- Merge the current path into the merged state for the current merge frame
  modifyMF $ modifyMergedState $ mergePaths p{ pathConstraints = newPC }

step MergeReturnVoidAndClear = mergeReturn Nothing >> clearCurrentExecution

step (MergeReturnAndClear rslt) = mergeReturn (Just rslt) >> clearCurrentExecution

step (PushPendingExecution _cond) =
  error "PushPendingExecution nyi"

step (SetCurrentBlock bid) = setCurrentBlockM bid

step (AddPathConstraint _cond) =
  error "AddPathConstraint nyi"

step (Assign reg expr) = assign reg =<< eval expr

step (Store val addr _malign) = do
  dumpMem 6 "store pre"
  valTerm          <- getTypedTerm val
  Typed _ addrTerm <- getTypedTerm addr
  mutateMem_ (\sbe mem -> memStore sbe mem valTerm addrTerm)
  dumpMem 6 "store post"

step (IfThenElse cond thenStmts elseStmts) = do
  b <- evalCond cond
  runStmts $ if b then thenStmts else elseStmts
  where
    evalCond TrueSymCond = return True
    evalCond (HasConstValue v i) = do
      Typed t v' <- getTypedTerm (Typed i1 v)
      CE.assert (t == i1) $ return ()

      mb <- fmap (fromIntegral . fromEnum)
            <$> withSBE' (\sbe -> getBool $ closeTerm sbe v')
      case mb of
        Nothing -> error "non-bool or symbolic bool SymCond HasConstValue terms nyi"
        Just b  -> return (i == b)

step Unreachable
  = error "step: Encountered 'unreachable' instruction"

step Unwind
  = error "Unwind nyi"

--------------------------------------------------------------------------------
-- Symbolic expression evaluation

-- | @eval expr@ evaluates @expr@ via the symbolic backend
eval :: (Functor m, MonadIO m)
  => SymExpr -> Simulator sbe m (Typed (SBETerm sbe))

eval (Arith op (Typed t@(L.PrimType L.Integer{}) v1) v2) = do
  Typed t1 x <- getTypedTerm (Typed t v1)
  Typed t2 y <- getTypedTerm (Typed t v2)
  CE.assert (t == t1 && t == t2) $ return ()
  Typed t <$> withSBE (\sbe -> applyArith sbe op x y)
eval s@Arith{} = error $ "Unsupported arith expr type: " ++ show (ppSymExpr s)

eval (Bit _op _tv1 _v2) = error "eval Bit nyi"

eval (Conv op tv@(Typed t1@(L.PrimType L.Integer{}) _) t2@(L.PrimType L.Integer{})) = do
  Typed t x <- getTypedTerm tv
  CE.assert (t == t1) $ return ()
  Typed t2 <$> withSBE (\sbe -> applyConv sbe op x t2)

eval (Conv L.BitCast tv ty) =
  Typed ty . typedValue <$> getTypedTerm tv

eval s@Conv{} = error $ "Unsupported/illegal conv expr type: " ++ show (ppSymExpr s)

eval (Alloca t msztv malign ) = do
  szt <- case msztv of
           Nothing   -> getTypedTerm (i32const 1)
           Just sztv -> getTypedTerm sztv
  let alloca sbe m = stackAlloca sbe m t szt (maybe 0 lg malign)

  Typed (L.PtrTo t) <$> mutateMem alloca

eval (Load tv@(Typed (L.PtrTo ty) _) _malign) = do
  addrTerm <- getTypedTerm tv
  let load sbe mem = memLoad sbe mem addrTerm

  dumpMem 6 "load pre"
  Typed ty <$> withMem load
    <* dumpMem 6 "load post"

eval s@(Load _ _) = error $ "Illegal load operand: " ++ show (ppSymExpr s)

eval (ICmp op (Typed t@(L.PrimType L.Integer{}) v1) v2) = do
  Typed t1 x <- getTypedTerm (Typed t v1)
  Typed t2 y <- getTypedTerm (Typed t v2)
  CE.assert (t == t1 && t == t2) $ return ()
  Typed i1 <$> withSBE (\sbe -> applyICmp sbe op x y)
eval s@ICmp{} = error $ "Unsupported icmp expr type: " ++ show (ppSymExpr s)

eval (FCmp _op _tv1 _v2      ) = error "eval FCmp nyi"
eval (Val _tv                ) = error "eval Val nyi"

eval (GEP tv0 idxs0) = impl idxs0 =<< getTypedTerm tv0
  where
    impl :: (MonadIO m, Functor m)
         => [Typed SymValue]
         -> Typed (SBETerm sbe)
         -> Simulator sbe m (Typed (SBETerm sbe))

    impl [] (Typed referentTy ptrVal) = do
      -- Truncate the final pointer value down to the target's address width
      addrWidth <- fromIntegral <$> withLC llvmAddrWidthBits
      Typed (L.PtrTo referentTy) <$> termConv L.Trunc ptrVal (L.iT addrWidth)

    impl (idx:idxs) (Typed (L.PtrTo referentTy) ptrVal) = do
      impl idxs =<< baseOffset idx referentTy ptrVal

    impl (idx:idxs) (Typed (L.Array _len elemTy) ptrVal) = do
      impl idxs =<< baseOffset idx elemTy ptrVal

    impl _ tv = do
      dbugTypedTerm "tv" tv
      error $ "GEP: support for aggregate type NYI: " ++ show (typedType tv)

    sizeof :: (MonadIO m, Functor m) => L.Type -> Simulator sbe m (Typed L.Value)
    sizeof ty = Typed (L.PrimType (L.Integer 32)) <$> L.ValInteger <$> withLC (`llvmByteSizeOf` ty)

    -- @baseOffset i ty p@ computes @p + i * sizeof(ty)@
    baseOffset :: (MonadIO m, Functor m)
      => Typed SymValue -> L.Type -> SBETerm sbe
      -> Simulator sbe m (Typed (SBETerm sbe))
    baseOffset idx referentTy ptrVal = do
      Typed _ idxTerm <- getTypedTerm idx
      Typed _ szTerm  <- getTypedTerm =<< sizeof referentTy
      Typed referentTy <$> saxpy idxTerm szTerm ptrVal

eval (Select _tc _tv1 _v2    ) = error "eval Select nyi"
eval (ExtractValue _tv _i    ) = error "eval ExtractValue nyi"
eval (InsertValue _tv _ta _i ) = error "eval InsertValue nyi"

-----------------------------------------------------------------------------------------
-- Term operations and helpers

termAdd, termMul :: (Functor m, Monad m)
  => SBETerm sbe -> SBETerm sbe -> Simulator sbe m (SBETerm sbe)
termAdd x y = withSBE $ \sbe -> applyArith sbe L.Add x y
termMul x y = withSBE $ \sbe -> applyArith sbe L.Mul x y

termConv :: (Functor m, Monad m)
  => L.ConvOp -> SBETerm sbe -> L.Type -> Simulator sbe m (SBETerm sbe)
termConv op x ty = withSBE $ \sbe -> applyConv sbe op x ty

-- @saxpy a x y@ computes a * x + y, promoting terms to larger sizes as needed.
saxpy :: (Functor m, MonadIO m)
  => SBETerm sbe
  -> SBETerm sbe
  -> SBETerm sbe
  -> Simulator sbe m (SBETerm sbe)
saxpy a x y = do
  (a', x') <- resizeTerms a x
  t        <- termMul a' x'
  (t', y') <- resizeTerms t y
  termAdd t' y'

-- | @resizeTerms a b@ yields both arguments back after zero-extending the smaller of
-- the two terms.
resizeTerms :: (MonadIO m, Functor m)
  => SBETerm sbe
  -> SBETerm sbe
  -> Simulator sbe m (SBETerm sbe, SBETerm sbe)
resizeTerms a b = do
  wa <- fromIntegral <$> withSBE' (\sbe -> termWidth sbe a)
  wb <- fromIntegral <$> withSBE' (\sbe -> termWidth sbe b)
  if wa > wb
    then conv b (L.iT wa) >>= \b' -> return (a, b')
    else
      if wb > wa
         then conv a (L.iT wb) >>= \a' -> return (a', b)
         else return (a, b)
  where
    conv = termConv L.SExt

(&&&) :: (ConstantProjection (SBEClosedTerm sbe), Functor m, Monad m)
  => Simulator sbe m (SBETerm sbe)
  -> Simulator sbe m (SBETerm sbe)
  -> Simulator sbe m (SBETerm sbe)
mx &&& my = do
   x <- mx
   xb <- withSBE' $ \sbe -> getBool (closeTerm sbe x)
   case xb of
     Just True  -> my
     Just False -> return x
     _          -> do
       y  <- my
       yb <- withSBE' $ \sbe -> getBool (closeTerm sbe y)
       case yb of
         Just True  -> return x
         Just False -> return y
         _          -> withSBE $ \sbe -> applyBitwise sbe L.And x y

--------------------------------------------------------------------------------
-- SBE lifters and helpers

liftSBE :: Monad m => sbe a -> Simulator sbe m a
liftSBE sa = gets liftSymBE >>= \f -> f sa

withSBE :: (Functor m, Monad m) => (SBE sbe -> sbe a) -> Simulator sbe m a
withSBE f = gets symBE >>= \sbe -> liftSBE (f sbe)

withSBE' :: (Functor m, Monad m) => (SBE sbe -> a) -> Simulator sbe m a
withSBE' f = gets symBE >>= \sbe -> return (f sbe)

withMem :: (Functor m, Monad m)
  => (SBE sbe -> SBEMemory sbe -> sbe a) -> Simulator sbe m a
withMem f = do
  gets memModel >>= withSBE . flip f

mutateMem :: (Functor m, MonadIO m)
  => (SBE sbe -> SBEMemory sbe -> sbe (a, SBEMemory sbe)) -> Simulator sbe m a
mutateMem f = do
  dumpMem 7 "mutateMem pre"
  m0 <- gets memModel
  (r, m1) <- withSBE (`f` m0)
  modify $ \s -> s{ memModel = m1 }
  dumpMem 7 "mutateMem post"
  return r

mutateMem_ :: (Functor m, MonadIO m, Functor sbe)
  => (SBE sbe -> SBEMemory sbe -> sbe (SBEMemory sbe)) -> Simulator sbe m ()
mutateMem_ f = mutateMem (\sbe mem -> ((,) ()) <$> f sbe mem) >> return ()

withLC :: (Functor m, MonadIO m) => (LLVMContext -> a) -> Simulator sbe m a
withLC f = f <$> gets llvmCtx

--------------------------------------------------------------------------------
-- Misc utility functions

lg :: (Ord a, Bits a) => a -> a
lg = genericLength . takeWhile (>0) . drop 1 . iterate (`shiftR` 1)

i32const :: Int32 -> Typed L.Value
i32const x = L.iT 32 =: L.ValInteger (fromIntegral x)

resolveCallee :: (MonadIO m, Functor m) => SymValue -> Simulator sbe m (Either String L.Symbol)
resolveCallee callee = case callee of
 L.ValSymbol sym -> ok sym
 L.ValIdent i    -> resolveIdent i
 _               -> err $ "Unexpected callee value: " ++ show (L.ppValue callee)
 where
   resolveIdent i = do
     Typed t fp <- lkupIdent i <$> getCallFrame
     case L.elimFunPtr t of
       Nothing -> err "Callee identifier referent is not a function pointer"
       Just (_rty, _argtys, _isVarArgs) -> do
         pr <- withMem $ \sbe mem -> codeLookupDefine sbe mem fp
         case pr of
           Result sym -> ok sym
           _          -> err "resolveCallee: Failed to resolve callee function pointer"
   ok sym  = return $ Right $ sym
   err msg = return $ Left $ "resolveCallee: " ++ msg

lkupIdent :: L.Ident -> CallFrame term -> Typed term
lkupIdent i = flip (M.!) i . frmRegs

runStmts ::
  ( LogMonad m
  , Functor m
  , MonadIO m
  , Functor sbe
  , ConstantProjection (SBEClosedTerm sbe)
  )
  => [SymStmt] -> Simulator sbe m ()
runStmts = mapM_ dbugStep

i1 :: L.Type
i1 = L.PrimType (L.Integer 1)

typedAs :: Typed a -> b -> Typed b
typedAs tv x = const x <$> tv

setReg :: Reg -> Typed term -> CallFrame term -> CallFrame term
setReg r v frm@(CallFrame _ regMap) = frm{ frmRegs = M.insert r v regMap }

entryRsltReg :: Reg
entryRsltReg = L.Ident "__galois_final_rslt"

newPath :: (Functor m, Monad m)
  => CallFrame (SBETerm sbe) -> Simulator sbe m (Path (SBETerm sbe))
newPath cf = Path cf Nothing Nothing (Just initSymBlockID) <$> boolTerm True

boolTerm :: (Functor m, Monad m) => Bool -> Simulator sbe m (SBETerm sbe)
boolTerm = withSBE . flip termBool

-- | Obtain the first pending path in the topmost merge frame; @Nothing@ means
-- that the control stack is empty or the top entry of the control stack has no
-- pending paths recorded.
getPath :: (Functor m, Monad m)
  => Simulator sbe m (Maybe (Path (SBETerm sbe)))
getPath = topPendingPath <$> gets ctrlStk

-- | Obtain the call frame of the current path; runtime error if the control
-- stack is empty or if there is no current path.
getCallFrame :: (Functor m, Monad m) => Simulator sbe m (CallFrame (SBETerm sbe))
getCallFrame = do
  mp <- getPath
  case mp of
    Nothing -> error "getCallFrame: no current path"
    Just p  -> return $ pathCallFrame p

withCallFrame :: Path term -> (CallFrame term -> a) -> a
withCallFrame (pathCallFrame -> cf) f = f cf

-- | Manipulate the control stack
modifyCS :: Monad m
  => (CtrlStk (SBETerm sbe) -> CtrlStk (SBETerm sbe)) -> Simulator sbe m ()
modifyCS f = modify $ \s -> s{ ctrlStk = f (ctrlStk s) }

-- | Manipulate the current merge frame (i.e., the top control stack entry)
modifyMF :: (Monad m)
  => (MergeFrame (SBETerm sbe) -> MergeFrame (SBETerm sbe))
  -> Simulator sbe m ()
modifyMF f = modifyCS $ \cs -> let (mf, cs') = popMF cs in pushMF (f mf) cs'

-- | Manipulate the current path (i.e., the first pending path in topmost
-- control stack entry)
modifyPath :: (Functor m , Monad m)
  => (Path (SBETerm sbe) -> Path (SBETerm sbe)) -> Simulator sbe m ()
modifyPath f = modifyCS $ \cs ->
  let (p, cs') = popPendingPath cs in pushPendingPath (f p) cs'

modifyCallFrameM :: (Functor m, Monad m)
  => (CallFrame (SBETerm sbe) -> CallFrame (SBETerm sbe)) -> Simulator sbe m ()
modifyCallFrameM = modifyPath . modifyCallFrame

--------------------------------------------------------------------------------
-- Debugging

dumpMem :: (Functor m, MonadIO m) => Int -> String -> Simulator sbe m ()
dumpMem v msg =
  whenVerbosity (>=v) $ do
    dbugM $ msg ++ ":"
    mem <- gets memModel
    withSBE (`memDump` mem)

dbugStep ::
  ( LogMonad m
  , MonadIO m
  , Functor m
  , Functor sbe
  , ConstantProjection (SBEClosedTerm sbe)
  )
  => SymStmt -> Simulator sbe m ()
dbugStep stmt = do
  mp <- getPath
  case mp of
    Nothing -> dbugM' 2 $ "Executing: (no current path): " ++ show (ppSymStmt stmt)
    Just p  -> withCallFrame p $ \frm -> do
      dbugM' 2 $ "Executing: "
                 ++ show (L.ppSymbol (frmFuncSym frm))
                 ++ maybe "" (show . parens . ppSymBlockID) (pathCB p)
                 ++ ": " ++
                 case stmt of
                   IfThenElse{} -> "\n"
                   _ -> ""
                 ++ show (ppSymStmt stmt)
  step stmt
  dumpCtrlStk' 5

dbugTerm :: (MonadIO m, Functor m) => String -> SBETerm sbe -> Simulator sbe m ()
dbugTerm desc t = do
  pretty <- render <$> withSBE' (\sbe -> prettyTermD sbe t)
  dbugM $ desc ++ ": " ++ pretty

dbugTypedTerm :: (MonadIO m, Functor m) => String -> Typed (SBETerm sbe) -> Simulator sbe m ()
dbugTypedTerm desc (Typed ty t) =
  dbugTerm (desc ++ "(" ++ show (L.ppType ty) ++ ")") t

_nowarn_unused :: a
_nowarn_unused = undefined
  (dbugTerm undefined undefined :: Simulator IO IO ())
  (dbugTypedTerm undefined undefined :: Simulator IO IO ())

