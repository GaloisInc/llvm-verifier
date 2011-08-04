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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module LSS.Simulator
  ( module LSS.Execution.Codebase
  , AtomicValue(..)
  , Simulator(..)
  , LiftSBE
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
import           LSS.Execution.Stepper
import           LSS.Execution.Utils
import           LSS.SBEInterface
import           Text.PrettyPrint.HughesPJ
import           Text.PrettyPrint.Pretty

import qualified Control.Exception         as CE
import qualified Data.Map                  as M
import qualified Text.LLVM                 as L
import qualified Text.PrettyPrint.HughesPJ as PP

type CtrlStk term       = CtrlStk' (Path term) term
type MergeFrame term    = MergeFrame' (Path term) term
type LiftSBE sbe m      = forall a. sbe a -> Simulator sbe m a

-- | Symbolic simulator state
data State sbe m = State
  { codebase  :: Codebase              -- ^ LLVM code, post-transformation to sym ast
  , symBE     :: SBE sbe               -- ^ Symbolic backend interface
  , liftSymBE :: LiftSBE sbe m         -- ^ Lift SBE operations into the Simulator monad
  , ctrlStk   :: CtrlStk (SBETerm sbe) -- ^ Control stack for tracking merge points
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
  { frmFuncSym :: L.Symbol
  , frmRegs    :: M.Map Reg (AtomicValue term)
  }
  deriving Show

newtype Simulator sbe m a = SM { runSM :: StateT (State sbe m) m a }
  deriving ( Functor
           , Monad
           , MonadIO
           , MonadState (State sbe m)
           )

liftSBE :: Monad m => sbe a -> Simulator sbe m a
liftSBE sa = gets liftSymBE >>= \f -> f sa

withSBE :: (Functor m, Monad m) => (SBE sbe -> sbe a) -> Simulator sbe m a
withSBE f = gets symBE >>= \sbe -> liftSBE (f sbe)

runSimulator ::
  ( Functor m, MonadIO m)
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

callDefine ::
  ( MonadIO m
  , Functor m
  , Semantics sbe (Simulator sbe m)
  , Show (SBETerm sbe)
  )
  => L.Symbol                            -- ^ Callee symbol
  -> L.Type                              -- ^ Callee return type
  -> [Typed (AtomicValue (SBETerm sbe))] -- ^ Callee arguments
  -> Simulator sbe m ()
callDefine callee retTy args = do
  def  <- lookupDefine callee <$> gets codebase
  path <- pushFrame (Frame callee (bindArgs (sdArgs def) args))
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

-- data Value
--   = ValInteger Integer
--   | ValFloat Float
--   | ValDouble Double
--   | ValIdent Ident
--   | ValSymbol Symbol
--   | ValNull
--   | ValArray Type [Value]
--   | ValStruct [Typed Value]
--   | ValPackedStruct [Typed Value]
--   | ValString String
--     deriving (Show)

instance
  ( Functor m
  , MonadIO m
  , Show (SBETerm sbe)
  )
  => Semantics sbe (Simulator sbe m)
  where

  type IntTerm sbe      = SBETerm sbe
  type FrameTy sbe      = Frame (SBETerm sbe)
  type MergeFrameTy sbe = MergeFrame (SBETerm sbe)

  iAdd x y = withSBE $ \sbe -> applyAdd sbe x y

  assign reg v = do
    modifyCurrentFrame $ \frm ->
      frm{ frmRegs = M.insert reg v (frmRegs frm) }

  setCurrentBlock bid = modifyCurrentPath (setCurrentBlock' bid)

  eval (Arith op (L.Typed (L.PrimType (L.Integer w)) v1) v2) = do
    IValue _ x <- getTerm (Just w) v1
    IValue _ y <- getTerm (Just w) v2
    IValue w <$> case op of
                   L.Add -> iAdd x y
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

  popFrame = do
    mp <- getCurrentPath
    case mp of
      Nothing -> error "popFrame: no current path"
      Just p  -> do
        let (frm, p') = popFrame' p
        modifyCurrentPath $ const p'
        return frm

  popMergeFrame = do
    s <- get
    let (mf, cs) = popMF (ctrlStk s)
    modifyCS $ const cs
    return mf

  mergeReturn frm mf (Just (L.Typed t rslt)) = do
    dbugM $ "MergeReturnAndClear: \npopped frame is:\n" ++ show (pp frm)
    dbugM $ "return value is: " ++ show (L.ppValue rslt)
    dumpCtrlStk
    dbugM $ "popped mf is: " ++ show (pp mf)
    error "mergeReturn early term"

  run = do
    mpath <- getCurrentPath
    case mpath of
      Nothing -> dbugM $ "run terminating: no path to execute (ok)"
      Just p  -> runPath p
    where
      runPath (pathCB -> Nothing)    = error "runPath: no current block"
      runPath p@(pathCB -> Just pcb) = withCurrentFrame p $ \frm -> do
        def <- lookupDefine (frmFuncSym frm) <$> gets codebase
        let blk = lookupSymBlock def pcb
        mapM_ dbugStep (sbStmts blk)
        run
      runPath _ = error "unreachable"

  dumpCtrlStk = banners . show . pp =<< gets ctrlStk


-- | Looks up the given identifier in the register map of the current frame.
-- Assumes the identifier is present in the map and that the current path and
-- current frame exist.  Runtime errors otherwise.
lkupIdent ::
  (Functor m, Monad m)
  => L.Ident -> Simulator sbe m (AtomicValue (SBETerm sbe))
lkupIdent i = flip (M.!) i . frmRegs <$> getCurrentFrame

getTerm ::
  (Functor m, Monad m)
  => Maybe Int32 -> L.Value -> Simulator sbe m (AtomicValue (SBETerm sbe))
getTerm (Just w) (L.ValInteger x) = IValue w <$> withSBE (\sbe -> termInteger sbe x)
getTerm _       (L.ValIdent i)   = lkupIdent i
getTerm _ v = error $ "getTerm: unsupported value: " ++ show (L.ppValue v)

--------------------------------------------------------------------------------
-- Misc utility functions

emptyPath :: (Functor m, Monad m) => Simulator sbe m (Path (SBETerm sbe))
emptyPath = Path [] Nothing Nothing Nothing <$> withSBE falseTerm

setCurrentBlock' :: SymBlockID -> Path term -> Path term
setCurrentBlock' blk p = p{ pathCB = Just blk }

-- | @pushFrame f p@ pushes frame f onto path p's frame stack
pushFrame :: Frame term -> Path term -> Path term
pushFrame f p@Path{ pathFrames = frms } = p{ pathFrames = f : frms}

-- | @popFrame' p@ pops the top frame of path p's frame stack; runtime error if
-- the frame stack is empty
popFrame' :: Path term -> (Frame term, Path term)
popFrame' Path{ pathFrames = [] }       = error "popFrame': empty frame stack"
popFrame' p@Path{ pathFrames = (f:fs) } = (f, p{ pathFrames = fs })

-- | Manipulate the control stack
modifyCS :: Monad m => (CtrlStk (SBETerm sbe) -> CtrlStk (SBETerm sbe)) -> Simulator sbe m ()
modifyCS f = modify $ \s -> s{ ctrlStk = f (ctrlStk s) }

-- | Obtain the first pending path in the topmost merge frame; @Nothing@ means
-- that the control stack is empty or the top entry of the control stack has no
-- pending paths recorded.
getCurrentPath ::
  (Functor m, Monad m)
  => Simulator sbe m (Maybe (Path (SBETerm sbe)))
getCurrentPath = topPendingPath <$> gets ctrlStk

-- | Obtain the active frame in the current path; runtime error if the control
-- stack is empty or if there is no current path.
getCurrentFrame :: (Functor m, Monad m) => Simulator sbe m (Frame (SBETerm sbe))
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
  => (Path (SBETerm sbe) -> Path (SBETerm sbe)) -> Simulator sbe m ()
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
  => (Frame (SBETerm sbe) -> Frame (SBETerm sbe)) -> Simulator sbe m ()
modifyCurrentFrame f = modifyCurrentPath $ \p ->
  let (frm, p') = popFrame' p in pushFrame (f frm) p'

--------------------------------------------------------------------------------
-- Pretty printing

instance Show a => Pretty (M.Map Reg a) where
  pp mp = vcat [ text (show r ++ " => " ++ show v) | (r,v) <- M.toList mp]

instance Show term => Pretty (Frame term) where
  pp (Frame sym regMap) =
    text "Frm" <> parens (L.ppSymbol sym) <> colon $+$ nest 2 (pp regMap)

instance Show term => Pretty (Path term) where
  pp (Path frms _mexc _mrv mcb pc) =
    text "Path"
    <>  brackets (maybe (text "none") ppSymBlockID mcb)
    <>  colon <> text (show pc)
    $+$ nest 2 (vcat $ map pp frms)

-----------------------------------------------------------------------------------------
-- Debugging

dbugStep :: (MonadIO m, Show (SBETerm sbe), Functor m) => SymStmt -> Simulator sbe m ()
dbugStep stmt = do
  dbugM ("Executing: " ++ show (ppSymStmt stmt))
  step stmt
  dumpCtrlStk

banners :: MonadIO m => String -> m ()
banners msg = do
  dbugM $ replicate 80 '-'
  dbugM msg
  dbugM $ replicate 80 '-'
