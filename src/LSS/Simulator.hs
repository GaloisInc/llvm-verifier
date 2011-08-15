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
  , getProgramReturnValue
  , runSimulator
  , withSBE -- Exported so we can construct argument values.
  )
where

import           Control.Applicative
import           Control.Monad.State       hiding (State)
import           Data.LLVM.Symbolic.AST
import           LSS.Execution.Codebase
import           LSS.Execution.Common
import           LSS.Execution.MergeFrame
import           LSS.Execution.Utils
import           LSS.SBEInterface
import           Text.LLVM                 (Typed(..), (=:))
import           Text.PrettyPrint.HughesPJ
import           Text.PrettyPrint.Pretty
import           Verinf.Symbolic.Common    (ConstantProjection, PrettyTerm(..))
import qualified Control.Exception         as CE
import qualified Data.Map                  as M
import qualified Text.LLVM                 as L

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
newSimState cb sbe lifter =
  State
  { codebase  = cb
  , symBE     = sbe
  , liftSymBE = lifter
  , ctrlStk   = emptyCtrlStk
  , verbosity = 1
  }

callDefine ::
  ( LogMonad m
  , MonadIO m
  , Functor m
  , PrettyTerm (SBETerm sbe)
  , ConstantProjection (SBETerm sbe)
  )
  => L.Symbol              -- ^ Callee symbol
  -> L.Type                -- ^ Callee return type
  -> [Typed (SBETerm sbe)] -- ^ Callee arguments
  -> Simulator sbe m ()
callDefine calleeSym t args = do
  callDefine' entryRetNormalID calleeSym (Just $ t =: entryRsltReg) args
  run

callDefine' ::(MonadIO m, Functor m, PrettyTerm (SBETerm sbe))
  => SymBlockID            -- ^ Normal call return block id
  -> L.Symbol              -- ^ Callee symbol
  -> Maybe (Typed Reg)     -- ^ Callee return type and result register
  -> [Typed (SBETerm sbe)] -- ^ Callee arguments
  -> Simulator sbe m ()
callDefine' normalRetID calleeSym mreg args = do
  def  <- lookupDefine calleeSym <$> gets codebase
  whenVerbosity (>=5) $ do
    dbugM $ "callDefine': callee " ++ show (L.ppSymbol calleeSym)
    banners $ show $ ppSymDefine def
  path <- newPath $ CallFrame calleeSym $ bindArgs (sdArgs def) args
  modifyCS $ pushPendingPath path
           . pushMF (ReturnFrame mreg normalRetID
                       Nothing Nothing Nothing [])
  where
    err doc = error $ "callDefine/bindArgs: " ++ render doc

    bindArgs formals actuals
      | length formals /= length actuals =
          err $ text "incorrect number of actual parameters"
      | otherwise =
          foldr bindArg M.empty (formals `zip` actuals)

    bindArg (Typed ft reg, v@(Typed at _)) mp
      | ft == at = case at of
          L.PrimType (L.Integer{}) -> M.insert reg v mp
          _                        -> err $ text "unsupported arg type:" <+> L.ppType at
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

run ::
  ( LogMonad m
  , Functor m
  , MonadIO m
  , PrettyTerm (SBETerm sbe)
  , ConstantProjection (SBETerm sbe)
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

mergeReturn :: (LogMonad m, Functor m, MonadIO m, PrettyTerm (SBETerm sbe))
  => Maybe (Typed SymValue)
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
  rv      <- getTypedTerm' callFrm tv
  modifyPath $ setReturnValue (Just $ typedValue rv)

  -- Merge the current path into the merged state for the current merge frame.
  Just p <- getPath
  modifyMF $ modifyMergedState $ mergePaths p

  dbugM' 5 $ "After mergeReturn, but before clearCurrentExecution:"
  dumpCtrlStk' 5

-- | @mergeMFs src dst@ merges the @src@ merge frame into @dst@
mergeMFs :: (MonadIO m, PrettyTerm (SBETerm sbe))
  => MergeFrame (SBETerm sbe)
  -> MergeFrame (SBETerm sbe)
  -> Simulator sbe m (MergeFrame (SBETerm sbe))

-- Source frame is a return frame
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
      | otherwise -> do
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

-- | @clearCurrentExecution@ clears the current pending path from the top merge
-- frame; then, if no pending paths remain, it merges the top merge frame with
-- the merge frame beneath it on the control stack.
clearCurrentExecution ::
  (Functor m, MonadIO m, PrettyTerm (SBETerm sbe))
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

lkupIdent' :: L.Ident -> CallFrame term -> Typed term
lkupIdent' i = flip (M.!) i . frmRegs

-- | getTypedTerm' in the context of the current call frame
getTypedTerm :: (Functor m, Monad m)
  => Typed L.Value -> Simulator sbe m (Typed (SBETerm sbe))
getTypedTerm tv = (`getTypedTerm'` tv) =<< getCallFrame

-- | Obtain the typed SBE term representation for the given LLVM value; performs
-- identifier lookup in the regmap of the given call frame as needed.
getTypedTerm' :: (Functor m, Monad m, term ~ SBETerm sbe)
  => CallFrame term -> Typed L.Value -> Simulator sbe m (Typed (SBETerm sbe))

getTypedTerm' _ (Typed t@(L.PrimType (L.Integer (fromIntegral -> w))) (L.ValInteger x))
  = Typed t <$> withSBE (\sbe -> termInt sbe w x)

getTypedTerm' _ (Typed (L.PtrTo L.FunTy{}) _)
  = error "getTypedTerm': TODO: Support for funptr args (requires mem interaction)"

getTypedTerm' frm (Typed _ (L.ValIdent i))
  = return $ lkupIdent' i frm

getTypedTerm' _ tv@(Typed t v)
  = error $ "getTypedTerm': unsupported value: "
          ++ show (L.ppType t) ++ " =: " ++ show (L.ppValue v)
          ++ show (parens $ text $ show tv)

--------------------------------------------------------------------------------
-- Instruction stepper

-- | Execute a single LLVM-Sym AST instruction
step ::
  ( LogMonad m
  , MonadIO m
  , Functor m
  , Monad m
  , PrettyTerm (SBETerm sbe)
  , ConstantProjection (SBETerm sbe)
  )
  => SymStmt -> Simulator sbe m ()

step ClearCurrentExecution =
  clearCurrentExecution

step (PushCallFrame (L.ValSymbol calleeSym) args mres) = do
  cb <- getCurrentBlockM
  callDefine' cb calleeSym mres =<< mapM getTypedTerm args

step (PushCallFrame _ _ _) =
  error "PushCallFrame with non-Symbol callees nyi"

step (PushInvokeFrame _fn _args _mres _e) =
  error "PushInvokeFrame nyi"

step (PushPostDominatorFrame pdid) = do
  Just p <- getPath
  pushMergeFrame $ pushPending p $ emptyPdomFrame pdid

step (MergePostDominator pdid cond) = do
  mtop <- topMF <$> gets ctrlStk
  case mtop of
    Just PostdomFrame{} -> return ()
    Just _              -> error "merge postdom: expected postdom merge frame"
    Nothing             -> error "merge postdom: empty control stack"

  Just p <- getPath

  -- Construct the new path constraint for the current path
  newPC  <- case cond of
    TrueSymCond -> return (pathConstraints p) &&& boolTerm True
    HasConstValue{} -> error "path constraint addition: HasConstValue nyi"

  dbugM' 5 $ "New path constraint is: " ++ prettyTerm newPC

  -- Merge the current path into the merged state for the current merge frame
  modifyMF $ modifyMergedState $ mergePaths p{ pathConstraints = newPC }

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

step (IfThenElse cond thenStmts elseStmts) = do
  b <- evalCond cond
  runStmts $ if b then thenStmts else elseStmts
  where
    evalCond TrueSymCond = return True
    evalCond (HasConstValue v i) = do
      Typed t v' <- getTypedTerm (Typed i1 v)
      CE.assert (t == i1) $ return ()
      mb <- fmap (fromIntegral . fromEnum) <$> withSBE (`getBool` v')
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
eval :: (Functor m, MonadIO m, PrettyTerm (SBETerm sbe))
  => SymExpr -> Simulator sbe m (Typed (SBETerm sbe))

eval (Arith op (Typed t@(L.PrimType L.Integer{}) v1) v2) = do
  Typed t1 x <- getTypedTerm (Typed t v1)
  Typed t2 y <- getTypedTerm (Typed t v2)
  CE.assert (t == t1 && t == t2) $ return ()
  Typed t <$> withSBE (\sbe -> applyArith sbe op x y)

eval s@Arith{} = error $ "Unsupported arith expr type: " ++ show (ppSymExpr s)

eval (Bit _op _tv1 _v2       ) = error "eval Bit nyi"
eval (Conv _op _tv1 _t       ) = error "eval Conv nyi"
eval (Alloca _t _mtv _malign ) = error "eval Alloca nyi"
eval (Load _tv               ) = error "eval Load nyi"

eval (ICmp op (Typed t@(L.PrimType L.Integer{}) v1) v2) = do
  Typed t1 x <- getTypedTerm (Typed t v1)
  Typed t2 y <- getTypedTerm (Typed t v2)
  CE.assert (t == t1 && t == t2) $ return ()
  Typed i1 <$> withSBE (\sbe -> applyICmp sbe op x y)

eval s@ICmp{} = error $ "Unsupported icmp expr type: " ++ show (ppSymExpr s)

eval (FCmp _op _tv1 _v2      ) = error "eval FCmp nyi"
eval (Val _tv                ) = error "eval Val nyi"
eval (GEP _tv _idxs          ) = error "eval GEP nyi"
eval (Select _tc _tv1 _v2    ) = error "eval Select nyi"
eval (ExtractValue _tv _i    ) = error "eval ExtractValue nyi"
eval (InsertValue _tv _ta _i ) = error "eval InsertValue nyi"

(&&&) :: (ConstantProjection (SBETerm sbe), Functor m, Monad m)
  => Simulator sbe m (SBETerm sbe)
  -> Simulator sbe m (SBETerm sbe)
  -> Simulator sbe m (SBETerm sbe)
mx &&& my = do
   x <- mx
   xb <- withSBE $ \sbe -> getBool sbe x
   case xb of
     Just True  -> my
     Just False -> return x
     _          -> do
       y  <- my
       yb <- withSBE $ \sbe -> getBool sbe y
       case yb of
         Just True  -> return x
         Just False -> return y
         _          -> withSBE $ \sbe -> applyBAnd sbe x y

--------------------------------------------------------------------------------
-- Misc utility functions

runStmts ::
  ( LogMonad m
  , Functor m
  , MonadIO m
  , PrettyTerm (SBETerm sbe)
  , ConstantProjection (SBETerm sbe)
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

dbugStep ::
  ( LogMonad m
  , MonadIO m
  , PrettyTerm (SBETerm sbe)
  , Functor m
  , ConstantProjection (SBETerm sbe)
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
