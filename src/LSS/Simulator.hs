{- |
Module           : $Header$
Description      : The symbolic simulator for LLVM-Symbolic programs
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE UndecidableInstances       #-}

module LSS.Simulator
  ( module LSS.Execution.Codebase
  , callDefine
  , runSimulator
  , withSBE -- Exported so we can construct argument values.
  )
where

import           Control.Applicative
import           Control.Arrow             hiding ((<+>))
import           Control.Monad.State       hiding (State)
import           Data.LLVM.Symbolic.AST
import           LSS.Execution.Codebase
import           LSS.Execution.Common
import           LSS.Execution.MergeFrame
import           LSS.Execution.Utils
import           LSS.SBEInterface
import           Text.LLVM                 ((=:))
import           Text.PrettyPrint.HughesPJ
import           Text.PrettyPrint.Pretty
import           Verinf.Symbolic.Common    (PrettyTerm(..))
import qualified Control.Exception         as CE
import qualified Data.Map                  as M
import qualified Text.LLVM                 as L

-- BEGIN TESTING
import qualified LSS.SBESymbolic as TestSBE
-- END TESTING

liftSBE :: Monad m => sbe a -> Simulator sbe m a
liftSBE sa = gets liftSymBE >>= \f -> f sa

withSBE :: (Functor m, Monad m) => (SBE sbe -> sbe a) -> Simulator sbe m a
withSBE f = gets symBE >>= \sbe -> liftSBE (f sbe)

runSimulator :: (Functor m, MonadIO m)
  => Codebase              -- ^ Post-transform LLVM code
  -> SBE sbe               -- ^ A symbolic backend
  -> LiftSBE sbe m         -- ^ Lift from symbolic backend to base monad
  -> Simulator sbe m a     -- ^ Simulator action to perform
  -> m a
runSimulator cb sbe lifter m =
  evalStateT (runSM (setup >> m)) (newSimState cb sbe lifter)
  where
    setup = do
      modifyCS $ pushMF emptyExitFrame

newSimState :: Codebase -> SBE sbe -> LiftSBE sbe m -> State sbe m
newSimState cb sbe liftSBE' = State cb sbe liftSBE' emptyCtrlStk

callDefine ::(MonadIO m, Functor m, PrettyTerm (SBETerm sbe))
  => L.Symbol                              -- ^ Callee symbol
  -> L.Type                                -- ^ Callee return type
  -> [L.Typed (AtomicValue (SBETerm sbe))] -- ^ Callee arguments
  -> Simulator sbe m ()
callDefine calleeSym t args = do
  callDefine' entryRetNormalID calleeSym (Just $ t =: entryRsltReg) args
  run

callDefine' ::(MonadIO m, Functor m, PrettyTerm (SBETerm sbe))
  => SymBlockID                            -- ^ Normal call return block id
  -> L.Symbol                              -- ^ Callee symbol
  -> Maybe (L.Typed Reg)                   -- ^ Callee return type and result register
  -> [L.Typed (AtomicValue (SBETerm sbe))] -- ^ Callee arguments
  -> Simulator sbe m ()
callDefine' normalRetID calleeSym mreg args = do
  mcp  <- getPath
  def  <- lookupDefine calleeSym <$> gets codebase
  path <- pushCallFrame (CallFrame calleeSym (bindArgs (sdArgs def) args))
          <$> setCurrentBlock initSymBlockID
          <$> newPath

  modifyCS $ pushPendingPath path
           . pushMF (ReturnFrame mreg normalRetID
                       Nothing Nothing Nothing [])

--   dbugM $ show $ ppSymDefine def
--   dumpCtrlStk
  where
    err doc = error $ "callDefine/bindArgs: " ++ render doc

    bindArgs formals actuals
      | length formals /= length actuals =
          err $ text "incorrect number of actual parameters"
      | otherwise =
          foldr bindArg M.empty (formals `zip` actuals)

    bindArg (L.Typed ft reg, L.Typed at val) mp
      | ft == at =
          case val of
            v@(IValue w _) -> case ft of
              L.PrimType (L.Integer w')
                | fromIntegral w == w' -> M.insert reg v mp
                | otherwise            -> err $ text "int width mismatch"
              ty -> err $ text "unsupported type:" <+> L.ppType ty
      | otherwise = err
          $ text "formal/actual type mismatch:"
            <+> L.ppType ft <+> text "vs." <+> L.ppType at

getProgramReturnValue :: (Monad m, Functor m)
  => Simulator sbe m (Maybe (SBETerm sbe))
getProgramReturnValue = do
  (top, _) <- popMF <$> gets ctrlStk
  case top of
    ExitFrame _ mrv -> return mrv
    _               -> error "getProgramReturnValue: program not yet terminated"

run :: (Functor m, MonadIO m, PrettyTerm (SBETerm sbe)) => Simulator sbe m ()
run = do
  mtop <- topMF <$> gets ctrlStk
  case mtop of
    Nothing  -> error "run: empty control stack"
    Just top
      | isExitFrame top -> do
          -- Set the exit frame return value (if any) and clear merged state.
          modifyCS $ \(popMF -> (_, cs)) -> pushMF (finalizeExit top) cs
          dbugM $ "run terminating normally: found valid exit frame"
          dumpCtrlStk
      | otherwise -> do
          case topPending top of
            Nothing -> error $ "internal: run: no path to execute"
            Just p  -> runPath p
  where
    runPath (pathCB -> Nothing)    = error "runPath: no current block"
    runPath p@(pathCB -> Just pcb) = withCallFrame p $ \frm -> do
      def <- lookupDefine (frmFuncSym frm) <$> gets codebase
      mapM_ dbugStep (sbStmts $ lookupSymBlock def pcb)
      run
    runPath _ = error "unreachable"

--------------------------------------------------------------------------------
-- LLVM-Sym operations

-- | @popCallFrameM@ removes the top entry of the call frame stack in the current
-- path; assumes that the call frame stack is nonempty and that there is a
-- current path defined.
popCallFrameM :: (Functor m, Monad m) => Simulator sbe m (CallFrame (SBETerm sbe))
popCallFrameM = do
  mp <- getPath
  case mp of
    Nothing -> error "popCallFrameM: no current path"
    Just p  -> do
      let (frm, p') = popCallFrame p
      modifyPath $ const p'
      return frm

-- | @pushCallFrameM frm@ pushes @frm@ to the top of the call frame stack in the
-- current path.  Assumes that there is a current path defined.
pushCallFrameM :: (Functor m, Monad m)
  => CallFrame (SBETerm sbe) -> Simulator sbe m ()
pushCallFrameM frm = do
  mp <- getPath
  case mp of
    Nothing -> error "pushCallFrameM: no current path"
    Just p  -> modifyPath $ pushCallFrame frm

-- | @popMergeFrame@ removes the top entry of the control stack; assumes
-- that the control stack is nonempty.
popMergeFrameM :: Monad m => Simulator sbe m (MergeFrame (SBETerm sbe))
popMergeFrameM = do
  s <- get
  let (mf, cs) = popMF (ctrlStk s)
  modifyCS $ const cs
  return mf

-- | @pushMergeFrame mf@ pushes mf to the control stack
pushMergeFrameM :: Monad m => MergeFrame (SBETerm sbe) -> Simulator sbe m ()
pushMergeFrameM = modifyCS . pushMF

assign :: (Functor m, Monad m)
  => Reg -> AtomicValue (SBETerm sbe) -> Simulator sbe m ()
assign reg v = modifyCallFrame $ \frm ->
  frm{ frmRegs = M.insert reg v (frmRegs frm) }

setCurrentBlockM :: (Functor m, Monad m) => SymBlockID -> Simulator sbe m ()
setCurrentBlockM bid = modifyPath (setCurrentBlock bid)

getCurrentBlock :: (Functor m, Monad m) => Simulator sbe m SymBlockID
getCurrentBlock = do
  mp <- getPath
  case mp of
    Nothing -> error "getCurrentBlock: no current path"
    Just p  -> maybe (error "getCurrentBlock: no current block") return (pathCB p)

mergeReturn :: (Functor m, MonadIO m, PrettyTerm (SBETerm sbe))
  => Maybe (L.Typed SymValue)
  -> Simulator sbe m ()
mergeReturn Nothing   = return ()
mergeReturn (Just tv) = do
  mtop <- topMF <$> gets ctrlStk
  case mtop of
    Just ReturnFrame{} -> return ()
    Just _             -> error "mergeReturn: expected return merge frame"
    Nothing            -> error "mergeReturn: empty control stack"

  -- Set the return value in the current path
  callFrm <- getCallFrame
  rslt    <- fromAV <$> getTermAV' callFrm tv
  modifyPath $ setReturnValue (Just rslt)

  -- Merge the current path into the merged state for the current merge frame.
  Just p <- getPath
  modifyMF $ \mf -> setMergedState (mergePaths p $ getMergedState mf) mf

  dbugM $ "After mergeReturn, but before clearCurrentExecution:"
  dumpCtrlStk

-- | @clearCurrentExecution@ clears the current pending path from the top merge
-- frame; then, if no pending paths remain, it merges the top merge frame with
-- the merge frame beneath it on the control stack.
clearCurrentExecution ::
  (Functor m, MonadIO m, PrettyTerm (SBETerm sbe))
  => Simulator sbe m ()
clearCurrentExecution = do
  top <- popMergeFrameM
  if (1 == length (pendingPaths top))
    then do
      -- We just executed the last remaining path, so merge the current merge
      -- frame into the caller's merge frame.
      pushMergeFrameM =<< mergeMFs top =<< popMergeFrameM
    else do
      -- We still have pending paths, so only remove the current path.
      pushMergeFrameM $ snd $ popPending top
  dbugM $ "After clearCurrentExecution:"

-- | @mergeMFs src dst@ merges the @src@ merge frame into @dst@
mergeMFs :: (MonadIO m, PrettyTerm (SBETerm sbe))
  => MergeFrame (SBETerm sbe)
  -> MergeFrame (SBETerm sbe)
  -> Simulator sbe m (MergeFrame (SBETerm sbe))

mergeMFs src@ReturnFrame{} dst = do
  let Just p = getMergedState src -- NB: src /must/ have a merged state.
  case pathRetVal p of
    Nothing -> error "mergeMFs w/ void retty nyi"
    Just rv
      | isExitFrame dst -> do
          -- Merge src's merged state with dst's merged state
          let f Nothing = Just p
              f ms      = mergePaths p ms
          return $ modifyMergedState f dst
      | isReturnFrame dst -> do
          -- Extract the return value from src's merged state and associate it
          -- with src's return register name in the top call frame of dst's
          -- current path.
          case rfRetReg src of
            Nothing   -> error "mergeMFs: src return frame has RV but no return register"
            Just rreg -> do
              -- Associate the return value in src's merged state with src's
              -- return register name in the caller's call frame.
              return $ (`modifyPending` dst) $ \dstPath ->
                let rvAsAV                = toAV $ typedAs rreg rv
                    reg                   = L.typedValue rreg
                    (callerCF, dstPath') = popCallFrame dstPath
                in
                  pushCallFrame (setReg reg rvAsAV callerCF) dstPath'
      | otherwise -> do
          error "mergeMFs: postdom dst frame nyi"

mergeMFs _ _ = error "mergeMFs: non-retrun src frame nyi"

-- | @mergePaths p1 p2@ merges path p1 into path p2, which may be empty; when p2
-- does is empty, this function produces p1 as the merged path. Yields Nothing
-- if merging fails.
mergePaths :: Path term -> Maybe (Path term) -> Maybe (Path term)
-- TODO: We'll need a better explanation for merge failure than "Nothing"; see
-- precondition violation explanation datatype in JSS, for example.
mergePaths _p1 (Just _p2) = error "real path merging nyi"
mergePaths p1 Nothing     = Just p1

-- | Looks up the given identifier in the register map of the current frame.
-- Assumes the identifier is present in the map and that the current path and
-- current frame exist.  Raises runtime errors otherwise.
lkupIdent :: (Functor m, Monad m)
  => L.Ident -> Simulator sbe m (AtomicValue (SBETerm sbe))
lkupIdent i = lkupIdent' i <$> getCallFrame

lkupIdent' :: L.Ident -> CallFrame term -> AtomicValue term
lkupIdent' i = flip (M.!) i . frmRegs

-- | getTermAV' in the context of the current call frame
getTermAV :: (Functor m, Monad m)
  => L.Typed L.Value -> Simulator sbe m (AtomicValue (SBETerm sbe))
getTermAV tv = (`getTermAV'` tv) =<< getCallFrame

-- | Obtain the AtomicValue-wrapped SBE term representation for the given LLVM
-- value; performs identifier lookup in the regmap of the given call frame as
-- needed.
getTermAV' :: (Functor m, Monad m, term ~ SBETerm sbe)
  => CallFrame term -> L.Typed L.Value -> Simulator sbe m (AtomicValue (SBETerm sbe))
getTermAV' _ (L.Typed (L.PrimType (L.Integer (fromIntegral -> w))) (L.ValInteger x))
  = IValue w <$> withSBE (\sbe -> termInt sbe w x)
getTermAV' frm (L.Typed _ (L.ValIdent i))
  = return $ lkupIdent' i frm
getTermAV' _ (L.Typed t v)
  = error $ "getTermAV': unsupported value: "
          ++ show (L.ppType t) ++ " =: " ++ show (L.ppValue v)

--------------------------------------------------------------------------------
-- Instruction stepper

-- | Execute a single LLVM-Sym AST instruction
step :: (MonadIO m, Functor m, Monad m, PrettyTerm (SBETerm sbe))
  => SymStmt -> Simulator sbe m ()

step ClearCurrentExecution =
  error "ClearCurrentExecution nyi"

-- PushCallFrame SymValue [Typed SymValue] (Maybe (Typed Reg))
step (PushCallFrame (L.ValSymbol calleeSym) args mres) = do
  cb <- getCurrentBlock
  callDefine' cb calleeSym mres
    =<< mapM (\tv -> typedAs tv <$> getTermAV tv) args

step (PushCallFrame _ _ _) =
  error "PushCallFrame with non-Symbol callees nyi"

step (PushInvokeFrame _fn _args _mres _e) =
  error "PushInvokeFrame nyi"

step (PushPostDominatorFrame _pdid) =
  error "PushPostDominatorFrame nyi"

step (MergePostDominator _pdid _cond) =
  error "MergePostDominator nyi"

step MergeReturnVoidAndClear =
  error "MergeReturnVoidAndClear nyi"

step (MergeReturnAndClear rslt) = do
  mergeReturn (Just rslt)
  clearCurrentExecution

step (PushPendingExecution _cond) =
  error "PushPendingExecution nyi"

step (SetCurrentBlock bid) = setCurrentBlockM bid

step (AddPathConstraint _cond) =
  error "AddPathConstraint nyi"

step (Assign reg expr) = assign reg =<< eval expr

step (Store _addr _val) =
  error "Store nyi"

step (IfThenElse _c _thenStms _elseStms)
  = error "IfThenElse nyi"

step Unreachable
  = error "step: Encountered 'unreachable' instruction"

step Unwind
  = error "Unwind nyi"

--------------------------------------------------------------------------------
-- Symbolic expression evaluation

-- | @eval expr@ evaluates @expr@ via the symbolic backend
eval :: (Functor m, MonadIO m, PrettyTerm (SBETerm sbe))
  => SymExpr -> Simulator sbe m (AtomicValue (SBETerm sbe))

eval (Arith op (L.Typed t@(L.PrimType L.Integer{}) v1) v2) = do
  IValue w1 x <- getTermAV (L.Typed t v1)
  IValue w2 y <- getTermAV (L.Typed t v2)
  IValue (CE.assert (w1 == w2) w1) <$> withSBE (\sbe -> applyArith sbe op x y)

eval s@Arith{} = error $ "Unsupported arith expr: " ++ show (ppSymExpr s)

eval (Bit _op _tv1 _v2       ) = error "eval Bit nyi"
eval (Conv _op _tv1 _t       ) = error "eval Conv nyi"
eval (Alloca _t _mtv _malign ) = error "eval Alloca nyi"
eval (Load _tv               ) = error "eval Load nyi"
eval (ICmp _op _tv1 _v2      ) = error "eval ICmp nyi"
eval (FCmp _op _tv1 _v2      ) = error "eval FCmp nyi"
eval (Val _tv                ) = error "eval Val nyi"
eval (GEP _tv _idxs          ) = error "eval GEP nyi"
eval (Select _tc _tv1 _v2    ) = error "eval Select nyi"
eval (ExtractValue _tv _i    ) = error "eval ExtractValue nyi"
eval (InsertValue _tv _ta _i ) = error "eval InsertValue nyi"

--------------------------------------------------------------------------------
-- Misc utility functions

typedAs :: L.Typed a -> b -> L.Typed b
typedAs tv x = const x <$> tv

setReg :: Reg -> AtomicValue term -> CallFrame term -> CallFrame term
setReg r v frm@(CallFrame _ regMap) = frm{ frmRegs = M.insert r v regMap }

entryRsltReg :: Reg
entryRsltReg = L.Ident "__galois_final_rslt"

newPath :: (Functor m, Monad m) => Simulator sbe m (Path (SBETerm sbe))
newPath = Path [] Nothing Nothing Nothing <$> withSBE (`termBool` True)

-- | Manipulate the control stack
modifyCS :: Monad m
  => (CtrlStk (SBETerm sbe) -> CtrlStk (SBETerm sbe)) -> Simulator sbe m ()
modifyCS f = modify $ \s -> s{ ctrlStk = f (ctrlStk s) }

-- | Obtain the first pending path in the topmost merge frame; @Nothing@ means
-- that the control stack is empty or the top entry of the control stack has no
-- pending paths recorded.
getPath :: (Functor m, Monad m)
  => Simulator sbe m (Maybe (Path (SBETerm sbe)))
getPath = topPendingPath <$> gets ctrlStk

-- | Obtain the active frame in the current path; runtime error if the control
-- stack is empty or if there is no current path.
getCallFrame :: (Functor m, Monad m) => Simulator sbe m (CallFrame (SBETerm sbe))
getCallFrame = do
  mpath <- getPath
  case mpath of
    Nothing                          -> error "getCallFrame: no current path"
    Just (pathCallFrames -> [])      -> error "getCallFrame: call frame stack is empty"
    Just (pathCallFrames -> (frm:_)) -> return frm
    _                                -> error "unreachable"

-- | Manipulate the current path (i.e., the first pending path in topmost
-- control stack entry)
modifyPath :: (Functor m , Monad m)
  => (Path (SBETerm sbe) -> Path (SBETerm sbe)) -> Simulator sbe m ()
modifyPath f = modifyCS $ \cs ->
  let (p, cs') = popPendingPath cs in pushPendingPath (f p) cs'

withCallFrame :: Path term -> (CallFrame term -> a) -> a
withCallFrame (pathCallFrames -> pfs) f
  | null pfs  = error "withCallFrame: empty frame list"
  | otherwise = f (head pfs)

-- | Manipulate the current frame (i.e., the top frame stack entry of the
-- current path)
modifyCallFrame :: (Functor m, Monad m)
  => (CallFrame (SBETerm sbe) -> CallFrame (SBETerm sbe)) -> Simulator sbe m ()
modifyCallFrame f = modifyPath $ \p ->
  let (frm, p') = popCallFrame p in pushCallFrame (f frm) p'

-- | Manipulate the current merge frame (i.e., the top call stack entry)
modifyMF :: (Monad m)
  => (MergeFrame (SBETerm sbe) -> MergeFrame (SBETerm sbe))
  -> Simulator sbe m ()
modifyMF f = modifyCS $ \cs -> let (mf, cs') = popMF cs in pushMF (f mf) cs'

dbugStep :: (MonadIO m, PrettyTerm (SBETerm sbe), Functor m)
  => SymStmt -> Simulator sbe m ()
dbugStep stmt = do
  Just p <- getPath
  withCallFrame p $ \frm -> do
    dbugM $ "Executing: "
            ++ show (L.ppSymbol (frmFuncSym frm))
            ++ maybe "" (show . parens . ppSymBlockID) (pathCB p)
            ++ ": " ++ show (ppSymStmt stmt)
    step stmt
    dumpCtrlStk

--------------------------------------------------------------------------------
-- Testing

main :: IO ()
main = do
  let i32 = L.iT 32
  cb <- loadCodebase "foo.bc"

--   runSimulator cb TestSBE.sbeSymbolic (SM . lift . TestSBE.liftSBESymbolic) $ do
--     i1 <- withSBE $ \sbe -> termInt sbe 32 2
--     i2 <- withSBE $ \sbe -> termInt sbe 32 3
--     callDefine (L.Symbol "int32_add") i32
--       [ i32 =: IValue 32 i1 , i32 =: IValue 32 i2 ]
--     mrv <- getProgramReturnValue
--     dbugM $ "Driver: program return value is: " ++ maybe ("none") prettyTerm mrv

--   runSimulator cb TestSBE.sbeSymbolic (SM . lift . TestSBE.liftSBESymbolic) $ do
--     i1 <- withSBE $ \sbe -> termInt sbe 32 2
--     callDefine (L.Symbol "int32_square") i32 [ i32 =: IValue 32 i1 ]
--     mrv <- getProgramReturnValue
--     dbugM $ "Driver: program return value is: " ++ maybe ("none") prettyTerm mrv

  runSimulator cb TestSBE.sbeSymbolic (SM . lift . TestSBE.liftSBESymbolic) $ do
    i1 <- withSBE $ \sbe -> termInt sbe 32 2
    i2 <- withSBE $ \sbe -> termInt sbe 32 3
    callDefine (L.Symbol "int32_muladd") i32
      [ i32 =: IValue 32 i1 , i32 =: IValue 32 i2 ]
    mrv <- getProgramReturnValue
    dbugM $ "Driver: program return value is: " ++ maybe ("none") prettyTerm mrv
