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

module LSS.Simulator
  ( module LSS.Execution.Codebase
  , Value'(..)
  , callDefine
  , runSimulator
  )

where

import           Control.Applicative
import           Control.Monad.State       hiding (State)
import           Data.Int
import           Data.LLVM.Symbolic.AST
import           LSS.Execution.Codebase
import           LSS.Execution.MergeFrame
import           LSS.Execution.Semantics
import           LSS.SBEInterface
import           Text.PrettyPrint.HughesPJ

import qualified Data.Map                  as M
import qualified Text.LLVM                 as LLVM
import qualified Text.PrettyPrint.HughesPJ as PP

type MergeFrame term = MergeFrame' (Path term) term
type CSEntry term    = CSEnt' (Path term) term
type CtrlStk term    = CtrlStk' (Path term) term

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
      modifyCS =<< pushMF . ExitFrame <$> emptyPath

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

  cs <- gets ctrlStk
  liftIO $ putStrLn $ show $ ppCS cs

  fail "early term"

  run_
  return undefined
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

instance MonadIO m => Semantics (Simulator sbe Term m) Term Term where
  iAdd x y = return $ x + y
  doStep   = undefined
  run      = undefined

--------------------------------------------------------------------------------
-- Misc utility functions

emptyPath :: Sim sbe term m i r => Simulator sbe term m (Path term)
emptyPath = Path [] Nothing Nothing Nothing . falseTerm <$> gets symBE

setCurrentBlock :: SymBlockID -> Path term -> Path term
setCurrentBlock blk p = p{ pathCB = Just blk }

pushFrame :: Frame term -> Path term -> Path term
pushFrame f p@Path{ pathFrames = frms } = p{ pathFrames = f : frms}

modifyCS ::
  Sim sbe term m i r
  => (CtrlStk term -> CtrlStk term)
  -> Simulator sbe term m ()
modifyCS f = modify $ \s -> s{ ctrlStk = f (ctrlStk s) }

--------------------------------------------------------------------------------
-- Misc pretty-printing

ppRegMap :: Show a => M.Map Reg a -> Doc
ppRegMap mp =
  vcat [ text (show r ++ " => " ++ show v) | (r,v) <- M.toList mp]

ppFrame :: Show term => Frame term -> Doc
ppFrame (Frame sym regMap) =
  text "Frm" <> parens (LLVM.ppSymbol sym) <> colon $+$ nest 2 (ppRegMap regMap)

ppPath :: Show term => Path term -> Doc
ppPath (Path frms _mexc _mrv mcb pc) =
  text "Path"
  <>  brackets (maybe (text "none") ppSymBlockID mcb)
  <>  colon <> text (show pc)
  $+$ nest 2 (vcat $ map ppFrame frms)

ppMF :: Show term => MergeFrame term -> Doc
ppMF (ExitFrame p)                    = text "MF(Exit):" $+$ nest 2 (ppPath p)
ppMF (PostdomFrame p)                 = text "MF(Pdom):" $+$ nest 2 (ppPath p)
ppMF (ReturnFrame _mr nl mns mel mes) = text "MF(Retn):" $+$ nest 2 rest
  where
    mpath str = nest 2 . maybe (parens $ text str) ppPath
    rest      = text "Normal@" <> ppSymBlockID nl <> colon
                $+$ mpath "no normal path set" mns
                $+$ maybe PP.empty
                      ( \el ->
                          text "Exc@" <> ppSymBlockID el
                          $+$ mpath "no exception path set" mes
                      )
                      mel

ppCSE :: Show term => CSEntry term -> Doc
ppCSE (CSEnt mf pps) =
  hang (text "CSE" <+> lbrace) 2
    ( ppMF mf
      $+$ text "Pending paths:"
      $+$ nest 2 (if null pps then text "(none)" else vcat (map ppPath pps))
    )
  $+$ rbrace

ppCS :: Show term => CtrlStk term -> Doc
ppCS (CtrlStk cses) =
  hang (text "CS" <+> lbrace) 2 (vcat (map ppCSE cses))
  $+$ rbrace

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
