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
  )
where

import           Control.Applicative
import           Control.Monad.State       hiding (State)
import           Data.Int
import           Data.LLVM.Symbolic.AST
import           LSS.Execution.Codebase
import           LSS.Execution.Common
import           LSS.Execution.MergeFrame
import           LSS.Execution.Utils
import           LSS.SBEInterface
import           Text.PrettyPrint.HughesPJ
import           Text.PrettyPrint.Pretty

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
      -- Push the merge frame corresponding to program exit
      modifyCS =<< pushMF . emptyExitFrame <$> emptyPath

newSimState :: Codebase -> SBE sbe -> LiftSBE sbe m -> State sbe m
newSimState cb sbe liftSBE' = State cb sbe liftSBE' emptyCtrlStk

callDefine ::(MonadIO m, Functor m, Show (SBETerm sbe))
  => L.Symbol                            -- ^ Callee symbol
  -> L.Type                              -- ^ Callee return type
  -> [Typed (AtomicValue (SBETerm sbe))] -- ^ Callee arguments
  -> Simulator sbe m ()
callDefine callee retTy args = do
  def  <- lookupDefine callee <$> gets codebase
  path <- pushCallFrame (CallFrame callee (bindArgs (sdArgs def) args))
          <$> setCurrentBlock' initSymBlockID
          <$> emptyPath
  modifyCS $ pushPendingPath path . pushMF emptyReturnFrame

  dbugM $ show $ ppSymDefine def
  dumpCtrlStk

  run
  where
    err doc = error $ "callDefine/bindArgs: " ++ render doc

    bindArgs formals actuals
      | length formals /= length actuals =
          err $ text "incorrect number of actual parameters"
      | otherwise =
          foldr bindArg M.empty (formals `zip` actuals)

    bindArg (Typed ft reg, Typed at val) mp
      | ft == at =
          case val of
            v@(IValue w _) -> case ft of
              L.PrimType (L.Integer w')
                | w == w'   -> M.insert reg v mp
                | otherwise -> err $ text "int width mismatch"
              ty -> err $ text "unsupported type:" <+> L.ppType ty
      | otherwise = err
          $ text "formal/actual type mismatch:"
            <+> L.ppType ft <+> text "vs." <+> L.ppType at

-----------------------------------------------------------------------------------------
-- The Semantics instance & related functions

run :: (Functor m, MonadIO m, Show (SBETerm sbe)) => Simulator sbe m ()
run = do
  mpath <- getPath
  case mpath of
    Nothing -> dbugM $ "run terminating: no path to execute (ok)"
    Just p  -> runPath p
  where
    runPath (pathCB -> Nothing)    = error "runPath: no current block"
    runPath p@(pathCB -> Just pcb) = withCallFrame p $ \frm -> do
      def <- lookupDefine (frmFuncSym frm) <$> gets codebase
      let blk = lookupSymBlock def pcb
      mapM_ dbugStep (sbStmts blk)
      run
    runPath _ = error "unreachable"

--------------------------------------------------------------------------------
-- LLVM-Sym operations

-- | @popCallFrame@ removes the top entry of the call frame stack in the current
-- path; assumes that the call frame stack is nonempty and that there is a
-- current path defined.
popCallFrame :: (Functor m, Monad m) => Simulator sbe m (CallFrame (SBETerm sbe))
popCallFrame = do
  mp <- getPath
  case mp of
    Nothing -> error "popCallFrame: no current path"
    Just p  -> do
      let (frm, p') = popCallFrame' p
      modifyPath $ const p'
      return frm

-- | @popMergeFrame@ removes the top entry of the merge frame stack; assumes
-- that the control stack is nonempty.
popMergeFrame :: Monad m => Simulator sbe m (MergeFrame (SBETerm sbe))
popMergeFrame = do
  s <- get
  let (mf, cs) = popMF (ctrlStk s)
  modifyCS $ const cs
  return mf

assign :: (Functor m, Monad m)
  => Reg -> AtomicValue (SBETerm sbe) -> Simulator sbe m ()
assign reg v = modifyCallFrame $ \frm ->
  frm{ frmRegs = M.insert reg v (frmRegs frm) }

setCurrentBlock :: (Functor m, Monad m) => SymBlockID -> Simulator sbe m ()
setCurrentBlock bid = modifyPath (setCurrentBlock' bid)

mergeReturn :: (MonadIO m, Show (SBETerm sbe))
  => CallFrame (SBETerm sbe)
  -> MergeFrame (SBETerm sbe)
  -> Maybe (L.Typed SymValue)
  -> Simulator sbe m ()
mergeReturn frm mf (Just (L.Typed t rslt)) = do
  dbugM $ "MergeReturnAndClear: \npopped frame is:\n" ++ show (pp frm)
  dbugM $ "return value is: " ++ show (L.ppValue rslt)
  dumpCtrlStk
  dbugM $ "popped mf is: " ++ show (pp mf)
  error "mergeReturn early term"

-- | Looks up the given identifier in the register map of the current frame.
-- Assumes the identifier is present in the map and that the current path and
-- current frame exist.  Raises runtime errors otherwise.
lkupIdent :: (Functor m, Monad m)
  => L.Ident -> Simulator sbe m (AtomicValue (SBETerm sbe))
lkupIdent i = flip (M.!) i . frmRegs <$> getCallFrame

getTerm :: (Functor m, Monad m)
  => Maybe Int32 -> L.Value -> Simulator sbe m (AtomicValue (SBETerm sbe))
getTerm (Just w) (L.ValInteger x) = IValue w <$> withSBE (\sbe -> termInteger sbe x)
getTerm _       (L.ValIdent i)   = lkupIdent i
getTerm _ v = error $ "getTerm: unsupported value: " ++ show (L.ppValue v)

--------------------------------------------------------------------------------
-- Instruction stepper

-- | Execute a single LLVM-Sym AST instruction
step :: (MonadIO m, Functor m, Monad m, Show (SBETerm sbe))
  => SymStmt -> Simulator sbe m ()

step ClearCurrentExecution =
  error "ClearCurrentExecution nyi"

step (PushCallFrame _fn _args _mres) =
  error "PushCallFrame nyi"

step (PushInvokeFrame _fn _args _mres _e) =
  error "PushInvokeFrame nyi"

step (PushPostDominatorFrame _pdid) =
  error "PushPostDominatorFrame nyi"

step (MergePostDominator _pdid _cond) =
  error "MergePostDominator nyi"

step MergeReturnVoidAndClear =
  error "MergeReturnVoidAndClear nyi"

step (MergeReturnAndClear rslt) = do
  frm <- popCallFrame
  mf  <- popMergeFrame
  mergeReturn frm mf (Just rslt)
  -- TODO: clearCurrentExecution
  error "MergeReturnAndClear nyi"

step (PushPendingExecution _cond) =
  error "PushPendingExecution nyi"

step (SetCurrentBlock bid) = setCurrentBlock bid

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
eval :: (Functor m, MonadIO m, Show (SBETerm sbe))
  => SymExpr -> Simulator sbe m (AtomicValue (SBETerm sbe))
eval (Arith op (L.Typed (L.PrimType (L.Integer w)) v1) v2) = do
  IValue _ x <- getTerm (Just w) v1
  IValue _ y <- getTerm (Just w) v2
  IValue w <$> case op of
                 L.Add -> withSBE $ \sbe -> applyAdd sbe x y
                 _     -> error "Unsupported integer arith op"

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

emptyPath :: (Functor m, Monad m) => Simulator sbe m (Path (SBETerm sbe))
emptyPath = Path [] Nothing Nothing Nothing <$> withSBE falseTerm

setCurrentBlock' :: SymBlockID -> Path term -> Path term
setCurrentBlock' blk p = p{ pathCB = Just blk }

-- | @pushCallFrame f p@ pushes frame f onto path p's frame stack
pushCallFrame :: CallFrame term -> Path term -> Path term
pushCallFrame f p@Path{ pathCallFrames = frms } = p{ pathCallFrames = f : frms}

-- | @popCallFrame' p@ pops the top frame of path p's frame stack; runtime error if
-- the frame stack is empty
popCallFrame' :: Path term -> (CallFrame term, Path term)
popCallFrame' Path{ pathCallFrames = [] }       = error "popCallFrame': empty frame stack"
popCallFrame' p@Path{ pathCallFrames = (f:fs) } = (f, p{ pathCallFrames = fs })

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
  let (frm, p') = popCallFrame' p in pushCallFrame (f frm) p'

dbugStep :: (MonadIO m, Show (SBETerm sbe), Functor m)
  => SymStmt -> Simulator sbe m ()
dbugStep stmt = do
  dbugM ("Executing: " ++ show (ppSymStmt stmt))
  step stmt
  dumpCtrlStk
