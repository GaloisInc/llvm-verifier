{- |
Module           : $Header$
Description      : The symbolic simulator for LLVM-Symbolic programs
Stability        : provisional
Point-of-contact : jstanley
-}

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

-- | Atomic values for simulation, parameterized over types used by the symbolic
-- simulator to represent primitive types.
data Value' int = IValue { _w :: Int32, unIValue :: int }
  deriving Show

-- | Symbolic simulator state
data State sbe term = State
  { codebase :: Codebase      -- ^ LLVM code, post-transformation to sym ast
  , symBE    :: sbe           -- ^ Symbolic backend interface
  , ctrlStk  :: CtrlStk term  -- ^ Control stack for tracking merge points
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

newtype Simulator sbe term m a = SM { runSM :: StateT (State sbe term) m a }
  deriving (Functor, Monad, MonadIO, MonadState (State sbe term))

runSimulator ::
  (Sim sbe term m i r)
  => Codebase             -- ^ Post-transform LLVM code
  -> sbe                  -- ^ symbolic backend
  -> Simulator sbe term m a   -- ^ Simulator action to perform
  -> m a
runSimulator cb sbe m =
  evalStateT (runSM (setup >> m)) (newSimState cb sbe)
  where
    setup = do
      -- Push the merge frame corresponding to program exit
      modifyCS =<< pushMF . emptyExitFrame <$> emptyPath

newSimState :: Codebase -> sbe -> State sbe term
newSimState cb sbe = State cb sbe emptyCtrlStk

callDefine ::
  Sim sbe term m i r
  => LLVM.Symbol           -- ^ Callee symbol
  -> LLVM.Type             -- ^ Callee return type
  -> [Typed (Value' term)] -- ^ Calee arguments
  -> Simulator sbe term m ()
callDefine callee retTy args = do
  def  <- lookupDefine callee <$> gets codebase
  path <- pushFrame (Frame callee (bindArgs (sdArgs def) args))
          <$> setCurrentBlock initSymBlockID
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
  => Semantics (Simulator sbe Term m) Term Term where
  iAdd x y = return $ x + y
  run      = do
    mpath <- getCurrentPath
    case mpath of
      Nothing -> liftIO $ putStrLn $ "run terminating: no path to execute (ok)"
      Just p  -> runPath p
    where
      runPath (pathCB -> Nothing)    = error "runPath: no current block"
      runPath p@(pathCB -> Just pcb) = withActiveFrame p $ \frm -> do
        def <- lookupDefine (frmFuncSym frm) <$> gets codebase
        let blk = lookupSymBlock def pcb

        liftIO $ putStrLn $ replicate 80 '-'
        liftIO $ putStrLn $ show $ ppSymBlock blk
        liftIO $ putStrLn $ replicate 80 '-'

        mapM_ step (sbStmts blk)
        run
      runPath _ = error "unreachable"

-- TODO: Pull this out into its own module
step :: (Functor m, MonadIO m) => SymStmt -> Simulator sbe term m ()
step ClearCurrentExecution                = error "ClearCurrentExecution nyi"
step (PushCallFrame _fn _args _mres)      = error "PushCallFrame nyi"
step (PushInvokeFrame _fn _args _mres _e) = error "PushInvokeFrame nyi"
step (PushPostDominatorFrame _pdid)       = error "PushPostDominatorFrame nyi"
step (MergePostDominator _pdid _cond)     = error "MergePostDominator nyi"
step MergeReturnVoidAndClear              = error "MergeReturnVoidAndClear nyi"
step (MergeReturnAndClear _resx)          = error "MergeReturnAndClear nyi"
step (PushPendingExecution _cond)         = error "PushPendingExecution nyi"
step (SetCurrentBlock bid)               = do
  liftIO $ putStrLn $ "SetCurrentBlock: setting current block in current path to " ++ show (ppSymBlockID bid)
  modifyCurrentPath $ \p -> p{ pathCB = Just bid }

step (AddPathConstraint _cond)            = error "AddPathConstraint nyi"
step (Assign _reg _expr)                  = error "Assign nyi"
step (Store _addr _val)                   = error "Store nyi"
step (IfThenElse _c _thenStms _elseStms)  = error "IfThenElse nyi"
step Unreachable                          = error "Unreachable nyi"
step Unwind                               = error "Unwind nyi"

--------------------------------------------------------------------------------
-- Misc utility functions

withActiveFrame :: Path term -> (Frame term -> a) -> a
withActiveFrame (pathFrames -> pfs) f
  | null pfs  = error "withActiveFrame: empty frame list"
  | otherwise = f (head pfs)

-- | Obtain the first pending path in the topmost merge frame; @Nothing@ means
-- that the control stack is empty or the top entry of the control stack has no
-- pending paths recorded.
getCurrentPath ::
  (Functor m, Monad m)
  => Simulator sbe term m (Maybe (Path term))
getCurrentPath = topPendingPath <$> gets ctrlStk

emptyPath :: Sim sbe term m i r => Simulator sbe term m (Path term)
emptyPath = Path [] Nothing Nothing Nothing . falseTerm <$> gets symBE

setCurrentBlock :: SymBlockID -> Path term -> Path term
setCurrentBlock blk p = p{ pathCB = Just blk }

-- | @pushFrame f p@ pushes frame f onto path p's frame stack
pushFrame :: Frame term -> Path term -> Path term
pushFrame f p@Path{ pathFrames = frms } = p{ pathFrames = f : frms}

-- | Manipulate the control stack
modifyCS :: Monad m => (CtrlStk term -> CtrlStk term) -> Simulator sbe term m ()
modifyCS f = modify $ \s -> s{ ctrlStk = f (ctrlStk s) }

-- | Manipulate the current path (i.e., the first pending path in topmost
-- control stack entry)
modifyCurrentPath ::
  (Functor m , MonadIO m)
  => (Path term -> Path term) -> Simulator sbe term m ()
modifyCurrentPath f = modifyCS $ \cs ->
  let (p, cs') = popPendingPath cs in pushPendingPath (f p) cs'

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

instance Sim SBEStub Term IO Term Term

-- Hacky context aliasing
class
  ( Functor m
  , MonadIO m
  , MonadState (State sbe term) (Simulator sbe term m)
  , Semantics (Simulator sbe term m) i r
  , SupportedSBE sbe term
  , Show term
  )
  => Sim sbe term m i r | m -> sbe term i r
