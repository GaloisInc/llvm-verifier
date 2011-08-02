{- |
Module           : $Header$
Description      : The symbolic simulator for LLVM-Symbolic programs
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module LSS.Simulator
  ( module LSS.Execution.Codebase
  , AtomicValue(..)
  , Simulator(..)
  , Value'(..)
  , callDefine
  , runSimulator
  )

where

import           Control.Applicative
import           Control.Arrow             hiding ((<+>))
import           Control.Monad
import           Control.Monad.State       hiding (State)
import           Data.Int
import           Data.Maybe
import           Data.LLVM.Symbolic.AST
import           LSS.Execution.Codebase
import           LSS.Execution.MergeFrame
import           LSS.Execution.Semantics
import           LSS.SBEInterface
import           Text.PrettyPrint.HughesPJ
import           Text.PrettyPrint.Pretty

import qualified Data.Map                  as M
import qualified Text.LLVM                 as LLVM
import qualified Text.PrettyPrint.HughesPJ as PP

type CtrlStk term    = CtrlStk' (Path term) term
type MergeFrame term = MergeFrame' (Path term) term

type Value' term = AtomicValue term
type LiftSBE sym m = forall a. sym a -> m a

-- | Symbolic simulator state
data State sbe term sym m = State
  { codebase :: Codebase              -- ^ LLVM code, post-transformation to sym ast
  , symBE    :: sbe                   -- ^ Symbolic backend interface
  , liftSBE  :: LiftSBE sym m         -- ^ Lift SBE operations into the Simulator base monad
  , ctrlStk  :: CtrlStk term          -- ^ Control stack for tracking merge points
  }

-- | Captures all symbolic execution state for a unique control-flow path (as
-- specified by the recorded path constraints)
data Path term = Path
  { pathFrames      :: [Frame term]       -- ^ The dynamic call stack for this
                                          -- path
  , pathException   :: Maybe term         -- ^ When handling an exception along
                                          -- this path, a pointer to the
                                          -- exception structure; Nothing
                                          -- otherwise
  , pathReturnValue :: Maybe term         -- ^ The return value along this path,
                                          -- if any.
  , pathCB          :: Maybe SymBlockID   -- ^ The currently-executing basic
                                          -- block along this path, if any.
  , pathConstraints :: term               -- ^ The constraints necessary for
                                          -- execution of this path
  }

-- | A frame (activation record) in the program being simulated
data Frame term = Frame
  { frmFuncSym :: LLVM.Symbol
  , frmRegs    :: M.Map Reg (Value' term)
  }
  deriving Show

newtype Simulator term sym m a = SM { runSM :: StateT (State (SBE sym term) term sym m) m a }
  deriving ( Functor
           , Monad
           , MonadIO
           , MonadState (State (SBE sym term) term sym m)
           )

runSimulator ::
  (Sim term sym m i)
  => Codebase                 -- ^ Post-transform LLVM code
  -> SBE sym term             -- ^ symbolic backend
  -> LiftSBE sym m            -- ^ lift op from symbolic backend
  -> Simulator term sym m a   -- ^ Simulator action to perform
  -> m a
runSimulator cb sbe liftSBE' m =
  evalStateT (runSM (setup >> m)) (newSimState cb sbe liftSBE')
  where
    setup = do
      -- Push the merge frame corresponding to program exit
      modifyCS =<< pushMF . emptyExitFrame <$> emptyPath

newSimState ::
     Codebase
  -> SBE sym term
  -> LiftSBE sym m
  -> State (SBE sym term) term sym m
newSimState cb sbe liftSBE' = State cb sbe liftSBE' emptyCtrlStk

callDefine ::
  Sim term sym m i
  => LLVM.Symbol           -- ^ Callee symbol
  -> LLVM.Type             -- ^ Callee return type
  -> [Typed (Value' term)] -- ^ Calee arguments
  -> Simulator term sym m ()
callDefine callee retTy args = do
  def  <- lookupDefine callee <$> gets codebase
  path <- pushFrame (Frame callee (bindArgs (sdArgs def) args))
          <$> setCurrentBlock' initSymBlockID
          <$> emptyPath
  modifyCS $ pushPendingPath path . pushMF emptyReturnFrame

  liftIO $ putStrLn $ show $ ppSymDefine def

  cs <- gets ctrlStk
  liftIO $ putStrLn $ show $ pp cs

  run
  where
    err doc = error $ "callDefine/bindArgs: " ++ render doc

    bindArgs :: [Typed Reg] -> [Typed (Value' term)] -> M.Map Reg (Value' term)
    bindArgs formals actuals
      | length formals /= length actuals =
          err $ text "incorrect number of actual parameters"
      | otherwise =
          foldr bindArg M.empty (formals `zip` actuals)

    bindArg (Typed ft reg, Typed at val) mp
      | ft == at =
          case val of
            v@(IValue w _) -> case ft of
              LLVM.PrimType (LLVM.Integer w')
                | w == w'   -> M.insert reg v mp
                | otherwise -> err $ text "int width mismatch"
              ty -> err $ text "unsupported type:" <+> LLVM.ppType ty
      | otherwise = err
          $ text "formal/actual type mismatch:"
            <+> LLVM.ppType ft <+> text "vs." <+> LLVM.ppType at

-- TODO abstract over term type as specified by SBE record-of-fns
type Term = Int

-----------------------------------------------------------------------------------------
-- The Semantics instance & related functions

instance
  (Functor m, MonadIO m)
  => Semantics (Simulator Term sym m) Term where
  iAdd x y = return $ x + y

  setCurrentBlock = undefined

  run      = do
    mpath <- getCurrentPath
    case mpath of
      Nothing -> liftIO $ putStrLn $ "run terminating: no path to execute (ok)"
      Just p  -> runPath p
    where
      runPath (pathCB -> Nothing)    = error "runPath: no current block"
      runPath p@(pathCB -> Just pcb) = withCurrentFrame p $ \frm -> do
        def <- lookupDefine (frmFuncSym frm) <$> gets codebase
        let blk = lookupSymBlock def pcb

        liftIO $ putStrLn $ replicate 80 '-'
        liftIO $ putStrLn $ show $ ppSymBlock blk
        liftIO $ putStrLn $ replicate 80 '-'

        mapM_ step (sbStmts blk)
        run
      runPath _ = error "unreachable"

-- TODO: Pull this out into its own module
-- step :: Semantics m int => SymStmt -> m ()
step :: (Functor m, MonadIO m, Show term) => SymStmt -> Simulator term sym m ()
step ClearCurrentExecution                = error "ClearCurrentExecution nyi"

step (PushCallFrame _fn _args _mres)      = error "PushCallFrame nyi"

step (PushInvokeFrame _fn _args _mres _e) = error "PushInvokeFrame nyi"

step (PushPostDominatorFrame _pdid)       = error "PushPostDominatorFrame nyi"

step (MergePostDominator _pdid _cond)     = error "MergePostDominator nyi"

step MergeReturnVoidAndClear              = error "MergeReturnVoidAndClear nyi"

step (MergeReturnAndClear _resx)          = error "MergeReturnAndClear nyi"

step (PushPendingExecution _cond)         = error "PushPendingExecution nyi"

step (SetCurrentBlock bid)                = do
  liftIO $ putStrLn $ "SetCurrentBlock: setting current block in current path to " ++ show (ppSymBlockID bid)
  modifyCurrentPath $ \p -> p{ pathCB = Just bid }
  error "early term"

step (AddPathConstraint _cond)            = error "AddPathConstraint nyi"

step stmt@(Assign reg expr)               = do
  return undefined
--   liftIO $ putStrLn $ "Doing: " ++ show (ppSymStmt stmt)
--   -- v <- evalSymExpr frm expr
--   v <- undefined
--   assign r v

--   modifyCurrentFrame $ \frm@Frame{ frmRegs = regMap } ->
--     frm{ frmRegs = M.insert reg v regMap }

step (Store _addr _val)                   = error "Store nyi"

step (IfThenElse _c _thenStms _elseStms)  = error "IfThenElse nyi"

step Unreachable                          = error "step: Encountered 'unreachable' instruction"

step Unwind                               = error "Unwind nyi"

{-
evalSymExpr ::
  (Show term, Functor m, MonadIO m)
  => Frame term -> SymExpr -> Simulator sbe term m (Value' term)
evalSymExpr frm expr = do
  case expr of
    Arith op (LLVM.Typed t1 v1) v2 -> case op of
      LLVM.Add -> case t1 of
        LLVM.PrimType LLVM.Integer{} -> undefined
        _ -> error "evalSymExpr: type-specific Add nyi"
      _   -> error $ "evalsymexpr: arith/" ++ show op ++ " nyi"
    Bit op tv1 v2       -> error "evalSymExpr Bit nyi"
    Conv op tv1 t       -> error "evalSymExpr Conv nyi"
    Alloca t mtv malign -> error "evalSymExpr Alloca nyi"
    Load tv             -> error "evalSymExpr Load nyi"
    ICmp op tv1 v2      -> error "evalSymExpr ICmp nyi"
    FCmp op tv1 v2      -> error "evalSymExpr FCmp nyi"
    Val tv              -> error "evalSymExpr Val nyi"
    GEP tv idxs         -> error "evalSymExpr GEP nyi"
    Select tc tv1 v2    -> error "evalSymExpr Select nyi"
    ExtractValue tv i   -> error "evalSymExpr ExtractValue nyi"
    InsertValue tv ta i -> error "evalSymExpr InsertValue nyi"
-}

--------------------------------------------------------------------------------
-- Misc utility functions

-- FIXME

-- liftSBE :: Monad m => m a -> Simulator term sym m a
-- liftSBE = SM . lift

emptyPath :: Sim term sym m i => Simulator term sym m (Path term)
emptyPath = undefined
-- emptyPath = gets symBE >>= \sbe ->
--   Path [] Nothing Nothing Nothing <$> liftSBE (falseTerm sbe)


setCurrentBlock' :: SymBlockID -> Path term -> Path term
setCurrentBlock' blk p = p{ pathCB = Just blk }

-- | @pushFrame f p@ pushes frame f onto path p's frame stack
pushFrame :: Frame term -> Path term -> Path term
pushFrame f p@Path{ pathFrames = frms } = p{ pathFrames = f : frms}

-- | @popFrame p@ pops the top frame of path p's frame stack; runtime error if
-- the frame stack is empty
popFrame :: Path term -> (Frame term, Path term)
popFrame Path{ pathFrames = [] }       = error "popFrame: empty frame stack"
popFrame p@Path{ pathFrames = (f:fs) } = (f, p{ pathFrames = fs })

-- | Manipulate the control stack
modifyCS :: Monad m => (CtrlStk term -> CtrlStk term) -> Simulator term sym m ()
modifyCS f = modify $ \s -> s{ ctrlStk = f (ctrlStk s) }

-- | Obtain the first pending path in the topmost merge frame; @Nothing@ means
-- that the control stack is empty or the top entry of the control stack has no
-- pending paths recorded.
getCurrentPath ::
  (Functor m, Monad m)
  => Simulator term sym m (Maybe (Path term))
getCurrentPath = topPendingPath <$> gets ctrlStk

-- | Obtain the top frame from the frame stack of the current path; runtime
-- error if the control stack is empty or if there is no current path.
getCurrentFrame ::
  (Functor m, Monad m)
  => Simulator term sym m (Frame term)
getCurrentFrame = do
  mpath <- getCurrentPath
  case mpath of
    Nothing                      -> error "getCurrentFrame: no current path"
    Just (pathFrames -> [])      -> error "getCurrentFrame: frame stack is empty"
    Just (pathFrames -> (frm:_)) -> return frm
    _                            -> error "unreachable"

-- | Manipulate the current path (i.e., the first pending path in topmost
-- control stack entry)
modifyCurrentPath ::
  (Functor m , Monad m)
  => (Path term -> Path term) -> Simulator term sym m ()
modifyCurrentPath f = modifyCS $ \cs ->
  let (p, cs') = popPendingPath cs in pushPendingPath (f p) cs'

withCurrentFrame :: Path term -> (Frame term -> a) -> a
withCurrentFrame (pathFrames -> pfs) f
  | null pfs  = error "withCurrentFrame: empty frame list"
  | otherwise = f (head pfs)

-- | Manipulate the current frame (i.e., the top frame stack entry of the
-- current path)
modifyCurrentFrame ::
  (Functor m, Monad m)
  => (Frame term -> Frame term) -> Simulator term sym m ()
modifyCurrentFrame f = modifyCurrentPath $ \p ->
  let (frm, p') = popFrame p in pushFrame (f frm) p'

--------------------------------------------------------------------------------
-- Pretty printing

instance Show a => Pretty (M.Map Reg a) where
  pp mp = vcat [ text (show r ++ " => " ++ show v) | (r,v) <- M.toList mp]

instance Show term => Pretty (Frame term) where
  pp (Frame sym regMap) =
    text "Frm" <> parens (LLVM.ppSymbol sym) <> colon $+$ nest 2 (pp regMap)

instance Show term => Pretty (Path term) where
  pp (Path frms _mexc _mrv mcb pc) =
    text "Path"
    <>  brackets (maybe (text "none") ppSymBlockID mcb)
    <>  colon <> text (show pc)
    $+$ nest 2 (vcat $ map pp frms)

--------------------------------------------------------------------------------
-- Typeclass goop

instance Sim Term IO IO Term

-- Hacky context aliasing
class
  ( Functor m
  , MonadIO m
  , MonadState (State (SBE sym term) term sym m) (Simulator term sym m)
  , Semantics (Simulator term sym m) i
  , Show term
  )
  => Sim term sym m i | m -> term i
