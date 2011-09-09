{- |
Module           : $Header$
Description      : The symbolic simulator for LLVM-Symbolic programs
Stability        : provisional
Point-of-contact : jstanley
-}

-- Debugging output at various verbosity levels:
-- 1: No output, the lowest verbosity level
-- 2: Instruction trace only
-- 3: Warnings on symbolic validity results from memory model; show
--    path state details when all paths yield errors.
-- 4: Path constraints on nontrivial path merges
-- 5: Simulator internal state (control stack dump per instruction); show
--    memory model details in addition to path state details when all paths
--    yield errors.
-- 6: Memory model dump on load/store operations only (for nontrivial codes,
--    this generates a /lot/ of output).  Complete path dumps on nontrivial path
--    merges.
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
{-# LANGUAGE OverloadedStrings     #-}

module LSS.Simulator
  ( module LSS.Execution.Codebase
  , Simulator (SM)
  , State(..)
  , SEH(..)
  , callDefine
  , callDefine_
  , defaultSEH
  , getProgramReturnValue
  , getProgramFinalMem
  , getTypedTerm'
  , prettyTermSBE
  , runSimulator
  , withSBE
  , withSBE'
  -- * Memory operations
  , alloca
  , sizeof
  , mutateMem
  , mutateMem_
  , processMemCond
  -- for testing
  , withLC
  , dbugTerm
  , dbugTypedTerm
  , dbugM
  , dumpMem
  , getMem
  , getPath'
  , getTypedTerm
  )
where

import           Control.Applicative
import           Control.Monad.State       hiding (State)
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
import           LSS.Printf
import           LSS.SBEInterface
import           System.Exit
import           System.IO
import           Text.LLVM                 (Typed(..), (=:))
import           Text.PrettyPrint.HughesPJ
import           Verinf.Symbolic.Common    (ConstantProjection(..),
                                            CValue(..))

import qualified Control.Arrow             as A
import qualified Control.Exception         as CE
import qualified Data.Foldable             as DF
import qualified Data.Map                  as M
import qualified Text.LLVM                 as L

runSimulator :: (Functor m, MonadIO m)
  => Codebase              -- ^ Post-transform LLVM code, memory alignment, and
                           -- type aliasing info
  -> SBE sbe               -- ^ A symbolic backend
  -> SBEMemory sbe         -- ^ The SBE's LLVM memory model
  -> LiftSBE sbe m         -- ^ Lift from symbolic backend to base monad
  -> SEH sbe m             -- ^ Simulation event handlers (use defaultSEH if no
                           -- event handling is needed)
  -> Maybe LSSOpts         -- Simulation options
  -> Simulator sbe m a     -- ^ Simulator action to perform
  -> m a
runSimulator cb sbe mem lifter seh mopts m =
  evalStateT (runSM (setup >> m)) (newSimState cb sbe mem lifter seh mopts)
  where
    setup = do
      modifyCS $ pushMF emptyExitFrame

newSimState :: Codebase
            -> SBE sbe
            -> SBEMemory sbe
            -> LiftSBE sbe m
            -> SEH sbe m
            -> Maybe LSSOpts
            -> State sbe m
newSimState cb sbe mem lifter seh mopts =
  State
  { codebase     = cb
  , symBE        = sbe
  , initMemModel = mem
  , liftSymBE    = lifter
  , ctrlStk      = emptyCtrlStk
  , globalTerms  = M.empty
  , overrides    = M.empty
  , verbosity    = 1
  , evHandlers   = seh
  , errorPaths   = []
  , lssOpts      = maybe defaultLSSOpts id mopts
  }

type ArgsGen sbe m = Simulator sbe m [Typed (SBETerm sbe)]

-- | External entry point for a function call.  The argument generator is used
-- to create actuals passed to the function, and the return value is those
-- arguments.  In the case when no arguments created or invoking
-- intrinsics/overrides, the return value will always be the empty list.
callDefine ::
  ( LogMonad m
  , MonadIO m
  , Functor m
  , Functor sbe
  , ConstantProjection (SBEClosedTerm sbe)
  )
  => L.Symbol     -- ^ Callee symbol
  -> L.Type       -- ^ Callee return type
  -> ArgsGen sbe m -- ^ Callee argument generator
  -> Simulator sbe m [Typed (SBETerm sbe)]
callDefine calleeSym t argsGen = do
  let gen = do
        registerStandardOverrides
        cb0 onPostOverrideReg
        argsGen
  callDefine' entryRetNormalID calleeSym (Just $ t =: entryRsltReg) (Left gen)
    <* run

callDefine_ ::
  ( LogMonad m
  , MonadIO m
  , Functor m
  , Functor sbe
  , ConstantProjection (SBEClosedTerm sbe)
  )
  => L.Symbol     -- ^ Callee symbol
  -> L.Type       -- ^ Callee return type
  -> ArgsGen sbe m -- ^ Callee argument generator
  -> Simulator sbe m ()
callDefine_ c t ag = callDefine c t ag >> return ()

callDefine' ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  , ConstantProjection (SBEClosedTerm sbe)
  )
  => SymBlockID        -- ^ Normal call return block id
  -> L.Symbol          -- ^ Callee symbol
  -> Maybe (Typed Reg) -- ^ Callee return type and result register
  -> Either (ArgsGen sbe m) [Typed (SBETerm sbe)] -- ^ Callee arguments
  -> Simulator sbe m [Typed (SBETerm sbe)]
callDefine' normalRetID calleeSym@(L.Symbol calleeName) mreg genOrArgs
  | isPrefixOf "llvm." calleeName
  = do
  CE.assert (isNothing mreg) $ return ()
  let args = case genOrArgs of
               Left{}      -> error "internal: ArgsGen use for intrinsic"
               Right args' -> args'
  intrinsic calleeName mreg args
  return []
  | otherwise
  = do
  override <- M.lookup calleeSym <$> gets overrides
  case override of
    Just (Redirect calleeSym') ->
      callDefine' normalRetID calleeSym' mreg genOrArgs
    Just (Override f) -> do
      let args = case genOrArgs of
               Left{}      -> error "internal: ArgsGen use for override"
               Right args' -> args'
      r <- f calleeSym mreg args
      case (mreg, r) of
        (Just reg, Just rv) ->
          modifyPath $ \path ->
            path { pathCallFrame = setReg
                                   (typedValue reg)
                                   (typedAs reg rv)
                                   (pathCallFrame path)
                 }
        (_, _) -> return ()
      return []
    Nothing -> runNormalSymbol normalRetID calleeSym mreg genOrArgs

runNormalSymbol :: (MonadIO m, Functor m, Functor sbe)
  => SymBlockID            -- ^ Normal call return block id
  -> L.Symbol              -- ^ Callee symbol
  -> Maybe (Typed Reg)     -- ^ Callee return type and result register
  -> Either (ArgsGen sbe m) [Typed (SBETerm sbe)] -- ^ Callee arguments
  -> Simulator sbe m [Typed (SBETerm sbe)]
runNormalSymbol normalRetID calleeSym mreg genOrArgs = do
  def  <- lookupDefine calleeSym <$> gets codebase
  dbugM' 5 $ "callDefine': callee " ++ show (L.ppSymbol calleeSym)

  path <- newPath (CallFrame calleeSym M.empty) =<< initMem
  modifyCS $ pushPendingPath path
           . pushMF (ReturnFrame mreg normalRetID
                       Nothing Nothing Nothing [])

  args <- case genOrArgs of
            Left gen   -> gen
            Right args -> return args

  modifyCallFrameM $ \cf -> cf{ frmRegs = bindArgs (sdArgs def) args }
  pushMemFrame
  return args
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

    initMem = do
      Just mf <- topMF <$> gets ctrlStk
      if isExitFrame mf then gets initMemModel else getMem

intrinsic ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  , ConstantProjection (SBEClosedTerm sbe)
  )
  => String -> Maybe (Typed Reg) -> [Typed (SBETerm sbe)]
  -> Simulator sbe m ()
intrinsic intr mreg args0 =
  case (intr, mreg) of
    -- TODO: Handle intrinsic overrides
    ("llvm.memcpy.p0i8.p0i8.i64", Nothing) -> memcpy
    ("llvm.memset.p0i8.i64", Nothing) -> memset
    _ -> error $ "Unsupported LLVM intrinsic: " ++ intr
  where
    memcpy = do
      let [dst, src, len, align, _isvol] = map typedValue args0
      processMemCond =<< mutateMem (\sbe mem -> memCopy sbe mem dst src len align)
    memset = do
      let [dst, val, len, align, _isvol] = args0
      memSet (typedValue dst) val (typedValue len) (typedValue align)

memSet :: ( Monad m, Functor m, MonadIO m
          , Functor sbe
          , ConstantProjection (SBEClosedTerm sbe)
          ) =>
          SBETerm sbe
       -> Typed (SBETerm sbe)
       -> SBETerm sbe
       -> SBETerm sbe
       -> Simulator sbe m ()
memSet dst val len align = do
  lenVal <- withSBE' $ \sbe -> getUVal $ closeTerm sbe len
  case lenVal of
    Just 0 -> return ()
    _ -> do
      processMemCond =<< mutateMem (\sbe mem -> memStore sbe mem val dst)
      ptrWidth <- withLC llvmAddrWidthBits
      lenWidth <- withSBE' $ \sbe -> termWidth sbe len
      one <- withSBE $ \sbe -> termInt sbe ptrWidth 1
      negone <- withSBE $ \sbe -> termInt sbe (fromIntegral lenWidth) (-1)
      dst' <- termAdd dst one
      len' <- termAdd len negone
      memSet dst' val len' align

getProgramReturnValue :: (Monad m, Functor m)
  => Simulator sbe m (Maybe (SBETerm sbe))
getProgramReturnValue = do
  (top, _) <- popMF <$> gets ctrlStk
  case top of
    ExitFrame _ mrv _ -> return mrv
    _                 -> error "getProgramReturnValue: program not yet terminated"

getProgramFinalMem :: (Monad m, Functor m)
  => Simulator sbe m (Maybe (SBEMemory sbe))
getProgramFinalMem = do
  (top, _) <- popMF <$> gets ctrlStk
  case top of
    ExitFrame _ _ mm -> return mm
    _                -> error "getProgramFinalMem: program not yet terminated"

-- Handle a condition returned by the memory model
processMemCond ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  , ConstantProjection (SBEClosedTerm sbe)
  )
  => SBETerm sbe -> Simulator sbe m ()
processMemCond cond = do
  condv <- condTerm cond
  case condv of
    Just True  -> return ()
    Just False -> error "Obtained constant-false memory model result (TODO: replace w/ error path handling)"
    _          -> do
      -- TODO: provide more detail here?
      dbugM' 3 "Warning: Obtained symbolic validity result from memory model."
      p <- getPath' "processMemCond"
      newp <- addPathConstraint p Nothing cond
      modifyPath $ const newp

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
          -- Normal program termination on at least one path. Set the exit merge
          -- frame return value (if any) and clear the merged state.
          modifyCS $ \(popMF -> (_, cs)) -> pushMF (finalizeExit top) cs
          dumpCtrlStk' 5
          whenVerbosity (>=2) $ do
            dbugM "run terminating normally: found valid exit frame"
            mrv <- getProgramReturnValue
            case mrv of
              Nothing -> dbugM "Program had no return value."
              Just rv -> dbugTerm "Program returned value" rv

          -- Let the user know about error paths.
          numErrs <- length <$> gets errorPaths
          showEPs <- optsErrorPathDetails <$> gets lssOpts
          when (numErrs > 0 && not showEPs) $
            dbugM "Warning: Some paths yielded errors. To see details, use --errpaths."
          when (numErrs > 0 && showEPs) $ do
            dbugM $ showErrCnt numErrs
            dumpErrorPaths
      | otherwise -> do
          case topPending top of
            Just p  -> runPath p -- start/continue normal execution
            Nothing -> do        -- No pending path => all paths yielded errors
              numErrs <- length <$> gets errorPaths
              CE.assert (numErrs > 0) $ return ()
              showEPs <- optsErrorPathDetails <$> gets lssOpts
              if showEPs
                then dbugM "All paths yielded errors!" >> dumpErrorPaths
                else dbugM "All paths yielded errors! To see details, use --errpaths."
  where
    runPath (pathCB -> Nothing)    = error "runPath: no current block"
    runPath p@(pathCB -> Just pcb) = withCallFrame p $ \frm -> do
      def <- lookupDefine (frmFuncSym frm) <$> gets codebase
      runStmts $ sbStmts $ lookupSymBlock def pcb
      run
    runPath _ = error "unreachable"

    showErrCnt x
      | x == 1    = "Encountered errors on 1 path; details below."
      | otherwise = "Encountered errors on " ++ show x ++ " paths; details below."

    dumpErrorPaths = do
        dbugM $ replicate 80 '-'
        eps <- gets errorPaths
        forM_ eps $ \ep -> do
          let p = epPath ep
          sbe <- gets symBE
          dbugM $ "Error reason        : " ++ epRsn ep
          dbugM $ "Error path state    :\n" ++ show (nest 2 $ ppPath sbe p)
          dumpMem 5 "Error path memory "
          when (length eps > 1) $ dbugM "--"
        dbugM $ replicate 80 '-'

--------------------------------------------------------------------------------
-- LLVM-Sym operations

-- | @pushMemFrame@ tells the memory model to push a new stack frame to the
-- stack region.
pushMemFrame :: (MonadIO m, Functor m, Functor sbe) => Simulator sbe m ()
pushMemFrame = do
  dbugM' 6 "Memory model: pushing stack frame"
  cond <- mutateMem stackPushFrame
  -- ^ if this fails, ran out of stack
  return ()

-- | @pushMemFrame@ tells the memory model to pop a stack frame from the stack
-- region.
popMemFrame :: (MonadIO m, Functor m, Functor sbe) => Simulator sbe m ()
popMemFrame = do
  dbugM' 6 "Memory model: popping stack frame"
  mutateMem_ stackPopFrame

-- | @popMergeFrame@ removes the top entry of the control stack; assumes
-- that the control stack is nonempty.
popMergeFrame :: Monad m => Simulator sbe m (MF sbe)
popMergeFrame = do
  s <- get
  let (mf, cs) = popMF (ctrlStk s)
  modifyCS $ const cs
  return mf

-- | @pushMergeFrame mf@ pushes mf to the control stack
pushMergeFrame :: Monad m => MF sbe -> Simulator sbe m ()
pushMergeFrame = modifyCS . pushMF

assign :: (Functor m, Monad m)
  => Reg -> Typed (SBETerm sbe) -> Simulator sbe m ()
assign reg v = modifyCallFrameM $ \frm ->
  frm{ frmRegs = M.insert reg v (frmRegs frm) }

setCurrentBlockM :: (Functor m, Monad m) => SymBlockID -> Simulator sbe m ()
setCurrentBlockM bid = modifyPath $ \p ->
  setCurrentBlock bid (setPrevBlock (pathCB p) p)

getCurrentBlockM :: (Functor m, Monad m) => Simulator sbe m SymBlockID
getCurrentBlockM =
  maybe (error "getCurrentBlock: no current block") id
    <$> pathCB
    <$> getPath' "getCurrentBlockM"

-- @addPathConstraintSC p c@ adds the given condition @c@ to the path constraint
-- of @p@. Note that any idents are deref'd in the context of @p@'s call frame.
-- This function does not modify simulator state.
addPathConstraintSC ::
  ( MonadIO m
  , Functor m
  , ConstantProjection (SBEClosedTerm sbe)
  , Functor sbe
  )
  => Path sbe -> SymCond -> Simulator sbe m (Path sbe)
addPathConstraintSC p TrueSymCond           = return p
addPathConstraintSC p c@(HasConstValue v i) = do
  -- Construct the constraint term from the HasConstValue predicate
  Typed _ vt <- getTypedTerm' (Just $ pathCallFrame p) (i1 =: v)
  Typed _ it <- getTypedTerm' Nothing (i1 =: L.ValInteger i)
  ct <- withSBE (\sbe -> applyICmp sbe L.Ieq vt it)
  addPathConstraint p (Just c) ct

-- @addPathConstraint p msc ct@ adds the given condition term @ct@ to the path
-- constraint of @p@.  If msc is Just{}, it is added to the Constraint value
-- associated with the path; otherwise it is ignored.  Note that any idents are
-- deref'd in the context of @p@'s call frame.  This function does not modify
-- simulator state.
addPathConstraint ::
  ( MonadIO m
  , Functor m
  , ConstantProjection (SBEClosedTerm sbe)
  , Functor sbe
  )
  => Path sbe -> Maybe SymCond -> SBETerm sbe -> Simulator sbe m (Path sbe)
addPathConstraint p msc ct = do
  let Constraint conds oldPC = pathConstraint p
  newPC <- return oldPC &&& return ct
  let rslt = Constraint (conds `SCEAnd` maybe (SCTerm ct) SCAtom msc) newPC
  sbe <- gets symBE
  dbugM' 5 $ "addPathConstraint: " ++ show (ppPC sbe rslt)
  return p{ pathConstraint = rslt }

-- | @clearCurrentExecution@ clears the current pending path from the top merge
-- frame; then, if no pending paths remain, it merges the top merge frame with
-- the merge frame beneath it on the control stack.
clearCurrentExecution ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  , ConstantProjection (SBEClosedTerm sbe)
  )
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

mergeReturn ::
  ( LogMonad m
  , Functor m
  , MonadIO m
  , Functor sbe
  , ConstantProjection (SBEClosedTerm sbe)
  )
  => Maybe (Typed SymValue)
  -> Simulator sbe m ()
mergeReturn mtv = do
  mtop <- topMF <$> gets ctrlStk
  top  <- case mtop of
            Just rf@ReturnFrame{} -> return rf
            Just _                -> error "mergeReturn: expected return merge frame"
            Nothing               -> error "mergeReturn: empty control stack"
  case mtv of
    Nothing -> return ()
    Just tv -> do
      rv <- getTypedTerm tv
      modifyPath $ setReturnValue (Just $ typedValue rv)

  -- Merge the current path into the merged state for the current merge frame.
  popMemFrame
  p       <- getPath' "mergeReturn"
  mmerged <- mergePaths p (getMergedState top)
  modifyMF $ setMergedState mmerged

-- | @mergeMFs src dst@ merges the @src@ merge frame into @dst@
mergeMFs ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  , ConstantProjection (SBEClosedTerm sbe)
  )
  => MF sbe -> MF sbe -> Simulator sbe m (MF sbe)
mergeMFs src dst = do
  case src of
    ReturnFrame{} -> do
      case dst of
        ExitFrame{} -> do
          -- Exit frames have no pending paths and represent the program termination
          -- point, so merge src's merged state with dst's merge state.
          (`setMergedState` dst) <$> mergePaths srcMerged (getMergedState dst)
        ReturnFrame{}  -> mergeRetMF
        PostdomFrame{} -> mergeRetMF
    PostdomFrame{}
      |isExitFrame dst -> error "mergeMFs: postdom MF => exit MF is not allowed"
      | otherwise      -> modifyDstPath (const srcMerged)
    ExitFrame{} -> error "mergeMFs: exit frames can not be merge sources"
  where
    modifyDstPath  = return . (`modifyPending` dst)
    Just srcMerged = getMergedState src -- NB: src /must/ have a merged state.
    mergeRetMF     = do
      -- In both cases, we dump up our merged state over dst's current path,
      -- being careful to pick up a few pieces of data from it: execution
      -- context (call frame / current block), and return value if any.
      case pathRetVal srcMerged of
        Nothing -> rfReplace id
        Just rv -> case rfRetReg src of
          Nothing   -> error "mergeMFs: src return frame has RV but no ret reg"
          Just rreg -> rfReplace $ setReg (typedValue rreg) (typedAs rreg rv)
      where
        rfReplace mutCF = modifyDstPath $ \dstPath ->
          srcMerged{ pathCallFrame = mutCF (pathCallFrame dstPath)
                   , pathCB        = pathCB dstPath
                   , pathRetVal    = pathRetVal dstPath
                   }

-- | @mergePaths p1 p2@ merges path p1 into path p2, which may be empty; when p2
-- is empty, this function merely p1 as the merged path. Yields Nothing if
-- merging fails.
mergePaths ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  , ConstantProjection (SBEClosedTerm sbe)
  )
  => Path sbe -> Maybe (Path sbe) -> Simulator sbe m (Maybe (Path sbe))
-- TODO: We'll need a better explanation for merge failure than "Nothing"; see
-- precondition violation explanation datatype in JSS, for example.
mergePaths p1 Nothing     = return $ Just p1
mergePaths from (Just to) = do
  CE.assert (pathCB from == pathCB to) $ return ()

  let [(c1, m1), (c2, m2)] = (pathConstraint A.&&& pathMem) <$> [from, to]

  whenVerbosity (>= 4) $ do
    sbe <- gets symBE
    dbugM $ "Merging paths with constraints: "
            ++ show ( let f = parens . ppSCExpr sbe . symConds in
                      f c1 <+> text "and" <+> f c2
                    )
    whenVerbosity (>= 6) $ do
      ppPathM "from" from
      ppPathM "to" to

  mergedPath <- ( \cf rv mem pc ->
                    to{ pathCallFrame  = cf
                      , pathRetVal     = rv
                      , pathMem        = mem
                      , pathConstraint = pc
                      }
                )
                  <$> mergeCFs
                  <*> mergeRVs
                  <*> mergeMems (pcTerm c1) m1 m2
                  <*> mergePCs c1 c2

  whenVerbosity (>=6) $ ppPathM "mergedPath" mergedPath
  return (Just mergedPath)
  where
    infixl 5 <->
    t <-> f = do
      meq <- withSBE  $ \sbe -> getBool . closeTerm sbe <$> applyICmp sbe L.Ieq t f
      case meq of
        Just True -> return t
        _         -> withSBE $ \sbe ->
                       applyIte sbe (pcTerm $ pathConstraint from) t f

    mergeCFs = do
      merged <- mergeMapsBy (regs from) (regs to) mergeV
      return $ (pathCallFrame to){ frmRegs = merged }
      where
        regs = frmRegs . pathCallFrame

    mergeRVs =
      case (pathRetVal from, pathRetVal to) of
        (Nothing, Nothing)   -> return Nothing
        (Just frv, Just trv) -> Just <$> frv <-> trv
        _                    -> error "merge precond viol: path missing rv"

    mergeMems c a b = do
      {-
      dbugM $ "mergeMems: Memory A"
      withSBE $ \s -> memDump s a Nothing
      dbugM $ "mergeMems: Memory B"
      withSBE $ \s -> memDump s b Nothing
      -}
      withSBE $ \sbe -> memMerge sbe c a b

    mergePCs (Constraint scs1 c1) (Constraint scs2 c2) = do
      Constraint (scs1 `SCEOr` scs2) <$> (return c1 ||| return c2)

    mergeV _a@(Typed t1 v1) _b@(Typed t2 v2) = do
      CE.assert (t1 == t2) $ return ()
      Typed t1 <$> v1 <-> v2

-- @mergeMapsBy from to act@ unions the @from@ and @to@ maps, combing common
-- elements according to the monadic element-merging operation @act@.
mergeMapsBy :: forall k m a. (Ord k, Functor m, Monad m)
  => M.Map k a
  -> M.Map k a
  -> (a -> a -> m a)
  -> m (M.Map k a)
-- TODO: Move this to Verinf and reuse it in the Java path state merging
mergeMapsBy from to act = union <$> merged
  where
    union prefer      = prefer `M.union` from `M.union` to -- left-biased
    merged            = DF.foldrM f M.empty isect
    f (k, v1, v2) acc = flip (M.insert k) acc <$> act v1 v2
    isect             = M.intersectionWithKey (\k v1 v2 -> (k, v1, v2)) from to

-- | getTypedTerm' in the context of the current call frame
getTypedTerm ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  , ConstantProjection (SBEClosedTerm sbe)
  )
  => Typed L.Value -> Simulator sbe m (Typed (SBETerm sbe))
getTypedTerm tv = (`getTypedTerm'` tv) =<< Just <$> getCallFrame

-- | Obtain the typed SBE term representation for the given LLVM value; performs
-- identifier lookup in the regmap of the given call frame as needed.
getTypedTerm' ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  , ConstantProjection (SBEClosedTerm sbe)
  )
  => Maybe (CF sbe) -> Typed L.Value -> Simulator sbe m (Typed (SBETerm sbe))

getTypedTerm' _ (Typed t@(L.PrimType (L.Integer (fromIntegral -> w))) (L.ValInteger x))
  = Typed t <$> withSBE (\sbe -> termInt sbe w x)

getTypedTerm' _ (Typed t@(L.PtrTo _) L.ValNull) = do
  ptrWidth <- withLC llvmAddrWidthBits
  Typed t <$> withSBE (\sbe -> termInt sbe ptrWidth 0)

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

getTypedTerm' mfrm (Typed ty@(L.Array _len ety@(L.PrimType L.Integer{})) (L.ValString str))
  = do
  lc <- gets (cbLLVMCtx . codebase)
  CE.assert (llvmStoreSizeOf lc ty == fromIntegral (length str)) $ return ()
  charTerms <- mapM (getTypedTerm' mfrm) $ map toChar str
  Typed ty <$> withSBE (\sbe -> termArray sbe $ map typedValue charTerms)
  where
    toChar = Typed ety . L.ValInteger . fromIntegral . fromEnum

getTypedTerm' mfrm (Typed ty@(L.Struct _fldTys) (L.ValStruct fldTVs))
  = do
  fldTerms <- mapM (getTypedTerm' mfrm) fldTVs
  Typed ty <$> withSBE (\sbe -> termArray sbe (map typedValue fldTerms))

getTypedTerm' _ (Typed _ (L.ValSymbol sym))
  = getGlobalPtrTerm (sym, Nothing)

getTypedTerm' mfrm (Typed _ (L.ValConstExpr ce))
  = case ce of
      L.ConstGEP _inbounds (splitAt 1 -> ((head -> ptr), idxs)) ->
        evalGEP (GEP ptr idxs)
      L.ConstConv L.BitCast tv t ->
        Typed t . typedValue <$> getTypedTerm' mfrm tv
      _ -> error $ "getTypedTerm: ConstExpr eval nyi : " ++ show ce

getTypedTerm' (Just frm) (Typed _ (L.ValIdent i))
  = return $ lkupIdent i frm

getTypedTerm' _mfrm (Typed ty L.ValUndef)
  = do
  szBytes <- fromIntegral <$> withLC (`llvmAllocSizeOf` ty)
  Typed ty <$> withSBE (\sbe -> termInt sbe (szBytes * 8) 0)

getTypedTerm' mfrm tv@(Typed t v)
  = do
  sbe <- gets symBE
  error $ "getTypedTerm': unsupported value / call frame presence: "
          ++ "\n" ++ show (L.ppType t) ++ " =: " ++ show (L.ppValue v)
          ++ "\n" ++ show (parens $ text $ show tv)
          ++ "\nmfrm = " ++ show (ppCallFrame sbe <$> mfrm)

getGlobalPtrTerm ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  , ConstantProjection (SBEClosedTerm sbe)
  )
  => (L.Symbol, Maybe [L.Type]) -> Simulator sbe m (Typed (SBETerm sbe))
getGlobalPtrTerm key@(sym, tys) = do
  mt <- M.lookup key <$> gets globalTerms
  case mt of
    Just t  -> return t
    Nothing -> do
      cb <- gets codebase
      maybe err (either addGlobal addDef) (lookupSym sym cb)
  where
    err = error $ "getGlobalPtrTerm: symbol resolution failed: "
                  ++ show (L.ppSymbol sym) ++ " (" ++ show tys ++ ")"

    addDef def = do
      let argTys = map typedType $ sdArgs def
          fty    = L.FunTy (sdRetType def) argTys (sdVarArgs def)
          idl    = nub $ mapMaybe symBlockLabel $ M.keys (sdBody def)
      ins fty $ \sbe mem ->
        maybe (error "Not enough space in code memory to allocate new definition.") id
          <$> memAddDefine sbe mem sym idl
    addGlobal g = do
      cb1 onMkGlobTerm g
      cdata <- getTypedTerm' Nothing (L.globalType g =: L.globalValue g)
      cb2 onPreGlobInit g cdata
      r <- ins (L.globalType g) $ \sbe mem -> do
             maybe (error "Not enough space in data segment to allocate new global.") id
               <$> memInitGlobal sbe mem cdata
      cb2 onPostGlobInit g cdata
      return r
    ins :: (Functor m, MonadIO m)
        => L.Type
        -> (SBE sbe -> SBEMemory sbe -> sbe (SBETerm sbe, SBEMemory sbe))
        -> Simulator sbe m (Typed (SBETerm sbe))
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
  _ <- case eab of
         Left msg        -> error $ "PushCallFrame: " ++ msg
         Right calleeSym -> callDefine' cb calleeSym mres
                              =<< Right <$> mapM getTypedTerm args
  return ()

step (PushInvokeFrame _fn _args _mres _e) =
  error "PushInvokeFrame nyi"

step (PushPostDominatorFrame pdid) = do
  p <- getPath' "step PushPostDominatorFrame"
  pushMergeFrame $ pushPending p $ emptyPdomFrame pdid

step (MergePostDominator pdid cond) = do
  mtop <- topMF <$> gets ctrlStk
  top <-
    case mtop of
      (Just t@(PostdomFrame _ _ lab))
        | lab == pdid -> return t
        | otherwise   -> error "merge postdom: top pdom frame has unexpected block ID"
      Just _          -> error "merge postdom: expected postdom merge frame"
      Nothing         -> error "merge postdom: empty control stack"

  p <- getPath' "step MergePostDominator"

  -- Construct the new path constraint for the current path
  newp <- addPathConstraintSC p cond

  -- Merge the current path into the merged state for the current merge frame
  mmerged <- mergePaths newp (getMergedState top)
  modifyMF $ setMergedState mmerged

step MergeReturnVoidAndClear = mergeReturn Nothing >> clearCurrentExecution

step (MergeReturnAndClear rslt) = mergeReturn (Just rslt) >> clearCurrentExecution

step (PushPendingExecution cond) = pushMergeFrame =<< ppe =<< popMergeFrame
  where
    -- Make the current path a pending execution in the top-most merge frame,
    -- with the additional path constraint 'cond'.
    ppe mf = do
      let (p, mf') = popPending mf
      divergent <- addPathConstraintSC p cond
      return (pushPending p . pushPending divergent $ mf')

step (SetCurrentBlock bid) = setCurrentBlockM bid

step (AddPathConstraint cond) = do
  p     <- getPath' "step AddPathConstraint"
  newp  <- addPathConstraintSC p cond
  modifyPath $ const newp

step (Assign reg expr) = assign reg =<< eval expr

step (Store val addr _malign) = do
  whenVerbosity (<=6) $ dumpMem 6 "store pre"
  valTerm          <- getTypedTerm val
  Typed _ addrTerm <- getTypedTerm addr
  processMemCond =<< mutateMem (\sbe mem -> memStore sbe mem valTerm addrTerm)
  whenVerbosity (<=6) $ dumpMem 6 "store post"

step (IfThenElse cond thenStmts elseStmts) = do
  b <- evalCond cond
  runStmts $ if b then thenStmts else elseStmts
  where
    evalCond TrueSymCond = return True
    evalCond (HasConstValue v i) = do
      Typed t v' <- getTypedTerm (Typed i1 v)
      CE.assert (t == i1) $ return ()
      maybe False (==i) . fmap (fromIntegral . fromEnum)
        <$> withSBE' (\sbe -> getBool $ closeTerm sbe v')


step Unreachable
  = error "step: Encountered 'unreachable' instruction"

step Unwind
  = error "Unwind nyi"

--------------------------------------------------------------------------------
-- Symbolic expression evaluation

-- | @eval expr@ evaluates @expr@ via the symbolic backend
eval ::
  ( Functor m
  , MonadIO m
  , ConstantProjection (SBEClosedTerm sbe)
  , Functor sbe
  )
  => SymExpr -> Simulator sbe m (Typed (SBETerm sbe))
eval (Arith op (Typed t@(L.PrimType L.Integer{}) v1) v2) = do
  Typed t1 x <- getTypedTerm (Typed t v1)
  Typed t2 y <- getTypedTerm (Typed t v2)
  CE.assert (t == t1 && t == t2) $ return ()
  Typed t <$> withSBE (\sbe -> applyArith sbe op x y)
eval e@Arith{} = error $ "Unsupported arith expr type: " ++ show (ppSymExpr e)
eval (Bit op tv1@(Typed t _) v2) = do
  [x, y] <- map typedValue <$> mapM getTypedTerm [tv1, typedType tv1 =: v2]
  Typed t <$> withSBE (\sbe -> applyBitwise sbe op x y)
eval (Conv op tv@(Typed t1@(L.PrimType L.Integer{}) _) t2@(L.PrimType L.Integer{})) = do
  Typed t x <- getTypedTerm tv
  CE.assert (t == t1) $ return ()
  Typed t2 <$> withSBE (\sbe -> applyConv sbe op x t2)
eval (Conv L.BitCast tv ty) = Typed ty . typedValue <$> getTypedTerm tv
eval e@Conv{} = error $ "Unsupported/illegal conv expr type: " ++ show (ppSymExpr e)
eval (Alloca ty msztv malign ) = alloca ty msztv malign
eval (Load tv@(Typed (L.PtrTo ty) _) _malign) = do
  addrTerm <- getTypedTerm tv
  dumpMem 6 "load pre"
  (cond,v) <- load addrTerm
  processMemCond cond
  return (Typed ty v) <* dumpMem 6 "load post"
eval e@(Load _ _) = error $ "Illegal load operand: " ++ show (ppSymExpr e)
eval (ICmp op (Typed t v1) v2) = do
  Typed t1 x <- getTypedTerm (Typed t v1)
  Typed t2 y <- getTypedTerm (Typed t v2)
  CE.assert (t == t1 && t == t2 && (isIntegerType t || L.isPointer t)) $ return ()
  Typed i1 <$> withSBE (\sbe -> applyICmp sbe op x y)
eval (FCmp _op _tv1 _v2      ) = error "eval FCmp nyi"
eval (Val tv)                  = getTypedTerm tv
eval e@GEP{}                   = evalGEP e
eval (Select tc tv1 v2)        = do
  [Typed _ c, Typed t x, Typed _ y] <- mapM getTypedTerm [tc, tv1, typedAs tv1 v2]
  mc <- condTerm c
  case mc of
    Just True  -> return (Typed t x)
    Just False -> return (Typed t y)
    Nothing    -> Typed t <$> withSBE (\s -> applyIte s c x y)
eval (ExtractValue _tv _i    ) = error "eval ExtractValue nyi"
eval (InsertValue _tv _ta _i ) = error "eval InsertValue nyi"

evalGEP ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  , ConstantProjection (SBEClosedTerm sbe)
  )
  => SymExpr -> Simulator sbe m (Typed (SBETerm sbe))
evalGEP (GEP tv0 idxs0) = impl idxs0 =<< getTypedTerm tv0
  where
    impl [] (Typed referentTy ptrVal) = do
      return $ Typed (L.PtrTo referentTy) ptrVal

    impl (idx:idxs) (Typed (L.PtrTo referentTy) ptrVal) = do
      impl idxs =<< baseOffset idx referentTy ptrVal

    impl (idx:idxs) (Typed (L.Array _len elemTy) ptrVal) = do
      impl idxs =<< baseOffset idx elemTy ptrVal

    impl (idx : idxs) (Typed (L.Struct fldTys) ptrVal) = do
      Typed _ idxTerm <- getTypedTerm idx
      (skipFlds, head -> fldTy) <- do
        midxVal <- withSBE' (\sbe -> getSVal (closeTerm sbe idxTerm))
        case midxVal of
          Nothing -> error "eval GEP: Failed to obtain value for GEP index"
          Just n  -> return $ splitAt (fromIntegral n) fldTys
      newPtrVal <- Typed fldTy <$> foldM addSz ptrVal skipFlds
      impl idxs newPtrVal

    impl idxs (Typed (L.Alias ident) v) = do
      impl idxs =<< (`Typed` v) <$> withLC (\lc -> llvmLookupAlias lc ident)

    impl _ tv = do
      error $ "GEP: support for aggregate type NYI: "
              ++ show (L.ppType (typedType tv))
              ++ " : " ++ show (typedType tv)

    -- @addSz p ty@ computes @p + sizeof(ty)
    addSz p ty = termAdd p . typedValue
                   =<< promote =<< getTypedTerm =<< sizeof ty

    -- @baseOffset i ty p@ computes @p + i * sizeof(ty)@
    baseOffset ::
      ( MonadIO m
      , Functor m
      , Functor sbe
      , ConstantProjection (SBEClosedTerm sbe)
      )
      => Typed SymValue -> L.Type -> SBETerm sbe
      -> Simulator sbe m (Typed (SBETerm sbe))
    baseOffset idx referentTy ptrVal = do
      Typed _ idxTerm <- promote =<< getTypedTerm idx
      Typed _ szTerm  <- promote =<< getTypedTerm =<< sizeof referentTy
      Typed referentTy <$> (termAdd ptrVal =<< termMul idxTerm szTerm)

    -- @promote x@ promotes integer value x to the target's pointer width
    promote :: (MonadIO m, Functor m)
      => Typed (SBETerm sbe) -> Simulator sbe m (Typed (SBETerm sbe))
    promote x@(Typed (L.PrimType (L.Integer iw)) v1) = do
      aw <- fromIntegral <$> withLC llvmAddrWidthBits
      if aw > iw
        then Typed (intn aw) <$> termConv L.ZExt v1 (intn aw)
        else return x
    promote _ = error "internal: promotion of non-integer value"

evalGEP e = error $ "evalGEP: expression is not a GEP: " ++ show (ppSymExpr e)

-----------------------------------------------------------------------------------------
-- Term operations and helpers

termAdd, termMul :: (Functor m, Monad m)
  => SBETerm sbe -> SBETerm sbe -> Simulator sbe m (SBETerm sbe)
termAdd x y = withSBE $ \sbe -> applyArith sbe L.Add x y
termMul x y = withSBE $ \sbe -> applyArith sbe L.Mul x y

termConv :: (Functor m, Monad m)
  => L.ConvOp -> SBETerm sbe -> L.Type -> Simulator sbe m (SBETerm sbe)
termConv op x ty = withSBE $ \sbe -> applyConv sbe op x ty

(&&&) :: (ConstantProjection (SBEClosedTerm sbe), Functor m, Monad m)
  => Simulator sbe m (SBETerm sbe)
  -> Simulator sbe m (SBETerm sbe)
  -> Simulator sbe m (SBETerm sbe)
infixr 3 &&&
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

(|||) :: (ConstantProjection (SBEClosedTerm sbe), Functor m, Monad m)
  => Simulator sbe m (SBETerm sbe)
  -> Simulator sbe m (SBETerm sbe)
  -> Simulator sbe m (SBETerm sbe)
infixr 2 |||
mx ||| my = do
  let neg t = withSBE $ \sbe -> applyBNot sbe t
  neg =<< ((neg =<< mx) &&& (neg =<< my))

-- ^ FIX above: You need to mux the path constraints together with the
-- condition; it's not simply the OR.

--------------------------------------------------------------------------------
-- SBE lifters and helpers

liftSBE :: Monad m => sbe a -> Simulator sbe m a
liftSBE sa = gets liftSymBE >>= \f -> f sa

withSBE :: (Functor m, Monad m) => (SBE sbe -> sbe a) -> Simulator sbe m a
withSBE f = gets symBE >>= \sbe -> liftSBE (f sbe)

withSBE' :: (Functor m, Monad m) => (SBE sbe -> a) -> Simulator sbe m a
withSBE' f = gets symBE >>= \sbe -> return (f sbe)

-- @getMem@ yields the memory model of the current path, which must exist.
getMem :: (Functor m, Monad m) => Simulator sbe m (SBEMemory sbe)
getMem = pathMem <$> getPath' "getMem"

-- @setMem@ sets the memory model in the current path, which must exist.
setMem :: (Functor m, Monad m) => SBEMemory sbe -> Simulator sbe m ()
setMem mem = do
  mp <- getPath
  case mp of
    Nothing -> error "setMem: no current path"
    _       -> modifyPath $ \p -> p{ pathMem = mem }

-- @withMem@ performs the given action on the memory as provided by @getMem@.
withMem :: (Functor m, Monad m)
  => (SBE sbe -> SBEMemory sbe -> sbe a) -> Simulator sbe m a
withMem f = getMem >>= withSBE . flip f

mutateMem :: (Functor m, MonadIO m)
  => (SBE sbe -> SBEMemory sbe -> sbe (a, SBEMemory sbe)) -> Simulator sbe m a
mutateMem f = do
  dumpMem 7 "mutateMem pre"
  m0 <- getMem
  (r, m1) <- withSBE (`f` m0)
  setMem m1
  dumpMem 7 "mutateMem post"
  return r

mutateMem_ :: (Functor m, MonadIO m, Functor sbe)
  => (SBE sbe -> SBEMemory sbe -> sbe (SBEMemory sbe)) -> Simulator sbe m ()
mutateMem_ f = mutateMem (\sbe mem -> ((,) ()) <$> f sbe mem) >> return ()

withLC :: (Functor m, MonadIO m) => (LLVMContext -> a) -> Simulator sbe m a
withLC f = f <$> gets (cbLLVMCtx . codebase)

load :: (Functor m, Monad m) =>
        Typed (SBETerm sbe) -> Simulator sbe m (SBETerm sbe, SBETerm sbe)
load addr = withMem $ \sbe mem -> memLoad sbe mem addr

--------------------------------------------------------------------------------
-- Callbacks

cb0 :: (Functor m, Monad m)
  => (SEH sbe m -> Simulator sbe m ()) -> Simulator sbe m ()
cb0 f = join $ gets (f . evHandlers)

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

alloca, malloc ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  , ConstantProjection (SBEClosedTerm sbe)
  )
  => L.Type
  -> Maybe (Typed L.Value)
  -> Maybe Int
  -> Simulator sbe m (Typed (SBETerm sbe))
alloca ty msztv malign = do
  nt <- case msztv of
          Nothing  -> getTypedTerm (int32const 1)
          Just ntv -> getTypedTerm ntv
  let parseFn SASymbolicCountUnsupported = error "alloca only supports concrete element count"
      parseFn (SAResult c t m') = ((c,t), m')
  (cond,t) <- mutateMem $ \sbe m ->
                parseFn <$> stackAlloca sbe m ty nt (maybe 0 lg malign)
  processMemCond cond
  return (Typed (L.PtrTo ty) t)

malloc ty msztv malign = do
  nt <- case msztv of
          Nothing  -> getTypedTerm (int32const 1)
          Just ntv -> getTypedTerm ntv
  let parseFn HASymbolicCountUnsupported = error "malloc only supports concrete element count"
      parseFn (HAResult c t m') = ((c,t), m')
  -- TODO: Handle 'size' result
  (cond,t) <- mutateMem $ \sbe m ->
    parseFn <$> heapAlloc sbe m ty nt (maybe 0 lg malign)
  processMemCond cond
  return (Typed (L.PtrTo ty) t)

--------------------------------------------------------------------------------
-- Misc utility functions

-- Evaluate the given term : i1 yields Nothing if the term is symbolic, Just
-- (concrete bool) otherwise.
condTerm :: (Functor m, Monad m, ConstantProjection (SBEClosedTerm sbe))
  => SBETerm sbe -> Simulator sbe m (Maybe Bool)
condTerm c = withSBE' $ \s -> getBool $ closeTerm s c

-- | Returns the a term representing the target-specific number of bytes
-- required to store a value of the given type.
sizeof :: (MonadIO m, Functor m) => L.Type -> Simulator sbe m (Typed L.Value)
sizeof ty = Typed (L.PrimType (L.Integer 32))
              <$> L.ValInteger <$> withLC (`llvmAllocSizeOf` ty)

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
lkupIdent i (frmRegs -> regs) = maybe err id $ M.lookup i regs
  where
    err = error $ "lkupIdent failure: "
                ++ show (L.ppIdent i)
                ++ " is not in regmap of given call frame."
runStmts ::
  ( LogMonad m
  , Functor m
  , MonadIO m
  , Functor sbe
  , ConstantProjection (SBEClosedTerm sbe)
  )
  => [SymStmt] -> Simulator sbe m ()
runStmts = mapM_ dbugStep

setReg :: Reg -> Typed term -> CallFrame term -> CallFrame term
setReg r v frm@(CallFrame _ regMap) = frm{ frmRegs = M.insert r v regMap }

entryRsltReg :: Reg
entryRsltReg = L.Ident "__galois_final_rslt"

newPath :: (Functor m, Monad m)
  => CF sbe -> SBEMemory sbe -> Simulator sbe m (Path sbe)
newPath cf mem = do
  true <- boolTerm True
  return $ Path cf Nothing Nothing (Just initSymBlockID) Nothing mem
             (Constraint (SCAtom TrueSymCond) true)

boolTerm :: (Functor m, Monad m) => Bool -> Simulator sbe m (SBETerm sbe)
boolTerm = withSBE . flip termBool

-- | Obtain the first pending path in the topmost merge frame; @Nothing@ means
-- that the control stack is empty or the top entry of the control stack has no
-- pending paths recorded.
getPath :: (Functor m, Monad m)
  => Simulator sbe m (Maybe (Path sbe))
getPath = do
  mmf <- topMF <$> gets ctrlStk
  return $ case mmf of
             Nothing -> Nothing
             Just mf
               | isExitFrame mf -> Nothing
               | otherwise      -> topPending mf

-- | Obtain the first pending path in the topmost merge frame; runtime error if
-- the control the control stack is empty or the top entry of the control stack
-- has no pending paths recorded.  The string parameter appears in the error if
-- it is raised.
getPath' :: (Functor m, Monad m)
  => String -> Simulator sbe m (Path sbe)
getPath' desc = do
  mp <- getPath
  case mp of
    Nothing -> error $ desc ++ ": no current path (getPath')"
    Just p  -> return p

-- | Obtain the call frame of the current path; runtime error if the control
-- stack is empty or if there is no current path.
getCallFrame :: (Functor m, Monad m) => Simulator sbe m (CF sbe)
getCallFrame = pathCallFrame <$> getPath' "getCallFrame"

-- | Manipulate the control stack
modifyCS :: Monad m => (CS sbe -> CS sbe) -> Simulator sbe m ()
modifyCS f = modify $ \s -> s{ ctrlStk = f (ctrlStk s) }

-- | Manipulate the current merge frame (i.e., the top control stack entry)
modifyMF :: (Monad m)
  => (MF sbe -> MF sbe)
  -> Simulator sbe m ()
modifyMF f = modifyCS $ \cs -> let (mf, cs') = popMF cs in pushMF (f mf) cs'

-- | Manipulate the current path (i.e., the first pending path in topmost
-- control stack entry)
modifyPath :: (Functor m , Monad m)
  => (Path sbe -> Path sbe) -> Simulator sbe m ()
modifyPath f = modifyCS $ \cs ->
  let (p, cs') = popPendingPath cs in pushPendingPath (f p) cs'

modifyCallFrameM :: (Functor m, Monad m) => (CF sbe -> CF sbe) -> Simulator sbe m ()
modifyCallFrameM = modifyPath . modifyCallFrame

registerOverride :: (Functor m, Monad m, Functor sbe, MonadIO m) =>
                    L.Symbol -> L.Type -> [L.Type] -> Bool
                 -> Override sbe m
                 -> Simulator sbe m ()
registerOverride sym retTy argTys va handler = do
  t <- mutateMem $ \sbe mem ->
         maybe (error "Not enough space in code memory to allocate new definition.") id
           <$> memAddDefine sbe mem sym []
  let t' = Typed (L.PtrTo (L.FunTy retTy argTys va)) t
  modify $ \s -> s { overrides = M.insert sym handler (overrides s)
                   , globalTerms =
                       M.insert (sym, Just argTys) t' (globalTerms s)
                   }

--------------------------------------------------------------------------------
-- Error handling

errorPath ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  , ConstantProjection (SBEClosedTerm sbe)
  )
  => String -> Simulator sbe m ()
errorPath rsn = do
  -- Pop the control stack and move the current path to the error paths list
  mmf     <- topMF <$> gets ctrlStk
  (p, mf) <- case mmf of
               Nothing -> error "internal: errorPath invoked with no MF"
               _       -> popPending <$> popMergeFrame
  modify $ \s -> s{ errorPaths = EP rsn p : errorPaths s }

  case getMergedState mf of
    Nothing   -> do
      -- When there's no merged state in the current MF, then either (a) there
      -- are pending paths left to execute that may be valid or (b) there are no
      -- pending paths left to execute, which means that all paths yielded
      -- errors.  In both cases, just replace the modified MF back onto the
      -- control stack and let the run function deal with the next step.
      pushMergeFrame mf
    Just{} -- Merged state in @mf@ => other valid paths have executed
      | not . null $ pendingPaths mf -> do
          -- There are still paths left to execute, so just place @mf@ back onto
          -- the control stack.
          pushMergeFrame mf
      | otherwise -> do
          -- @p@ was the last pending path in @mf@, so manually merge @mf@ with
          -- the merge frame below it on the control stack.
          pushMergeFrame =<< mergeMFs mf =<< popMergeFrame

--------------------------------------------------------------------------------
-- Debugging

ppPathM :: (MonadIO m, Functor m) => String -> Path sbe -> Simulator sbe m ()
ppPathM desc p = do
  sbe <- gets symBE
  dbugM $ desc ++ "\n" ++ show (ppPath sbe p)
  withSBE (\sbe' -> memDump sbe' (pathMem p) Nothing)

prettyTermSBE :: (Functor m, Monad m) => SBETerm sbe -> Simulator sbe m Doc
prettyTermSBE t = withSBE' $ \sbe -> prettyTermD sbe t

dumpMem :: (Functor m, MonadIO m) => Int -> String -> Simulator sbe m ()
dumpMem v msg =
  whenVerbosity (>=v) $ do
    dbugM $ msg ++ ":"
    mem <- getMem
    withSBE (\sbe -> memDump sbe mem Nothing)

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
                 p   <- getPath' "repl"
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

loadString ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  , ConstantProjection (SBEClosedTerm sbe)
  )
  => L.Typed (SBETerm sbe) -> Simulator sbe m String
loadString ptr =
  case typedType ptr of
    L.PtrTo (L.PrimType (L.Integer 8)) -> do
      -- Load ptr, ptr+1, until zero byte, convert each into char,
      -- assemble into list
      cs <- go ptr
      return . map (toEnum . fromEnum) . catMaybes $ cs
      where go addr = do
              (cond,t) <- load addr
              processMemCond cond
              c <- withSBE' $ \sbe -> getUVal $ closeTerm sbe t
              ptrWidth <- withLC llvmAddrWidthBits
              one <- withSBE $ \sbe -> termInt sbe ptrWidth 1
              addr' <- termAdd (typedValue addr) one
              case c of
                Nothing -> return []
                Just 0  -> return []
                _       -> (c:) <$> go (typedAs addr addr')
    ty -> error $ "loading string with invalid type: " ++
                  show (L.ppType ty)

termToArg ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  , ConstantProjection (SBEClosedTerm sbe)
  )
  => L.Typed (SBETerm sbe) -> Simulator sbe m Arg
termToArg term = do
  mc <- withSBE' $ flip closeTerm (typedValue term)
  case (typedType term, termConst mc) of
    (L.PrimType (L.Integer 8), Just (CInt 8 n)) ->
      return $ Arg (fromInteger n :: Int8)
    (L.PrimType (L.Integer 16), Just (CInt 16 n)) ->
      return $ Arg (fromInteger n :: Int16)
    (L.PrimType (L.Integer 32), Just (CInt 32 n)) ->
      return $ Arg (fromInteger n :: Int32)
    (L.PrimType (L.Integer 64), Just (CInt 64 n)) ->
      return $ Arg (fromInteger n :: Int64)
    (L.PtrTo (L.PrimType (L.Integer 8)), _) ->
       Arg <$> loadString term
    _ -> Arg . show <$> prettyTermSBE (typedValue term)

termIntS :: (Functor m, Monad m, Integral a) =>
            Int -> a -> Simulator sbe m (SBETerm sbe)
termIntS w n = withSBE $ \sbe -> termInt sbe w (fromIntegral n)

isSymbolic :: (ConstantProjection (SBEClosedTerm sbe)) =>
              SBE sbe -> L.Typed (SBETerm sbe) -> Bool
isSymbolic sbe = not . isConst . closeTerm sbe . typedValue

printfHandler ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  , ConstantProjection (SBEClosedTerm sbe)
  )
  => Override sbe m
printfHandler = Override $ \_sym _rty args ->
  case args of
    (fmtPtr : rest) -> do
      fmtStr <- loadString fmtPtr
      isSym <- withSBE' isSymbolic
      let fmtStr' = formatAsStrings fmtStr (map isSym rest)
      resString <- symPrintf fmtStr' <$> mapM termToArg rest
      liftIO $ putStr resString
      Just <$> termIntS 32 (length resString)
    _ -> error "printf called with no arguments"

allocHandler :: (Functor m, Monad m, MonadIO m, Functor sbe,
                  ConstantProjection (SBEClosedTerm sbe)) =>
                (L.Type
                   -> Maybe (Typed L.Value)
                   -> Maybe Int
                   -> Simulator sbe m (Typed (SBETerm sbe)))
             -> Override sbe m
allocHandler fn = Override $ \_sym _rty args ->
  case args of
    [sizeTm] -> do
      msize <- withSBE' $ \sbe -> getUVal (closeTerm sbe (typedValue sizeTm))
      case msize of
        Just size -> do
          let sizeVal = Typed i32 (L.ValInteger size)
          (Just . typedValue) <$> fn i8 (Just sizeVal) Nothing
        Nothing -> error "alloca: symbolic size not supported"
    _ -> error "alloca: wrong number of arguments"

abortHandler ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  , ConstantProjection (SBEClosedTerm sbe)
  )
  => Override sbe m
abortHandler = Override $ \_sym _rty args -> do
  case args of
    [tv@(Typed t _)]
      | t == strTy -> do
          msg <- loadString tv
          -- We'll have set the post-call target block already, so set the error
          -- path's current block to the previous so that the correct location
          -- is displayed when we show error paths.
          modifyPath $ \p -> p{ pathCB = prevPathCB p }
          errorPath $ "lss_abort(): " ++ msg
          return Nothing
      | otherwise -> error "Incorrect type passed to lss_abort()."
    _ -> error "Incorrect number of parameters passed to lss_abort()."

freshInt' :: (Functor m, Monad m) => Int -> Override sbe m
freshInt' n = Override $ \_ _ _ -> Just <$> withSBE (flip freshInt n)

freshIntArray :: (Functor m, MonadIO m, Functor sbe,
                  ConstantProjection (SBEClosedTerm sbe))
              => Int -> Override sbe m
freshIntArray n = Override $ \_sym _rty args ->
  case args of
    [sizeTm, _] -> do
      msize <- withSBE' $ \sbe -> getUVal (closeTerm sbe (typedValue sizeTm))
      case msize of
        Just size -> do
          let sz = fromIntegral size
              sz32 = fromIntegral size
              ety = intn . toEnum . fromEnum $ n
              ty = L.Array sz32 ety
              sizeVal = Typed i32 (L.ValInteger size)
          arrPtr <- typedValue <$> alloca ety (Just sizeVal) Nothing
          elts <- replicateM sz (withSBE $ flip freshInt n)
          arrTm <- withSBE $ flip termArray elts
          let typedArrTm = Typed ty arrTm
          processMemCond =<<
            mutateMem (\sbe mem -> memStore sbe mem typedArrTm arrPtr)
          return (Just arrPtr)
        Nothing -> error "fresh_array_uint called with symbolic size"
    _ -> error "fresh_array_uint: wrong number of arguments"

writeIntAiger ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  , ConstantProjection (SBEClosedTerm sbe)
  )
  => Override sbe m
writeIntAiger = Override $ \_sym _rty args ->
  case args of
    [t, fptr] -> do
      file <- loadString fptr
      withSBE $ \sbe -> writeAiger sbe file (typedValue t)
      return Nothing
    _ -> error "write_aiger_uint: wrong number of arguments"

writeIntArrayAiger ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  , ConstantProjection (SBEClosedTerm sbe)
  )
  => L.Type -> Override sbe m
writeIntArrayAiger _ety = Override $ \_sym _rty args ->
  case args of
    [tptr, sizeTm, fptr] -> do
      msize <- withSBE' $ \sbe -> getUVal (closeTerm sbe (typedValue sizeTm))
      case (msize, typedType tptr) of
        (Just size, L.PtrTo tgtTy) -> do
          elems <- loadArray tptr tgtTy size
          arrTm <- withSBE $ flip termArray elems
          file <- loadString fptr
          withSBE $ \sbe -> writeAiger sbe file arrTm
          return Nothing
        (Nothing, _) ->
          error "write_aiger_array_uint called with symbolic size"
        _ -> error "write_aiger_array_uint: invalid argument type"
    _ -> error "write_aiger_array_uint: wrong number of arguments"

loadArray ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  , ConstantProjection (SBEClosedTerm sbe)
  )
  => Typed (SBETerm sbe)
  -> L.Type
  -> Integer
  -> Simulator sbe m [SBETerm sbe]
loadArray ptr ety count = do
  ptrWidth <- withLC llvmAddrWidthBits
  elemWidth <- withLC (`llvmStoreSizeOf` ety)
  inc <- withSBE $ \sbe -> termInt sbe ptrWidth elemWidth
  go inc ptr count
    where go _one _addr 0 = return []
          go one addr size = do
            (cond,t) <- load addr
            processMemCond cond
            addr' <- termAdd (typedValue addr) one
            (t:) <$> go one (typedAs addr addr') (size - 1)

evalAigerOverride :: (Functor m, MonadIO m, Functor sbe,
                      ConstantProjection (SBEClosedTerm sbe)) =>
                     Override sbe m
evalAigerOverride =
  Override $ \_sym _rty args ->
    case args of
      [Typed _ tm, p@(Typed (L.PtrTo ety) _), Typed _ szTm] -> do
        msz <- withSBE' $ \sbe -> getUVal . closeTerm sbe $ szTm
        case msz of
          Just sz -> do
            elems <- loadArray p ety sz
            ints <- mapM
                    (\t -> withSBE' $ \sbe -> getUVal $ closeTerm sbe t)
                    elems
            let bools = map (not . (== 0)) $ catMaybes ints
            Just <$> (withSBE $ \sbe -> evalAiger sbe bools tm)
          Nothing -> error "eval_aiger: symbolic size not supported"
      _ -> error "eval_aiger: wrong number of arguments"

overrideByName :: (Functor m, Monad m, MonadIO m, Functor sbe,
                   ConstantProjection (SBEClosedTerm sbe)) =>
                  Override sbe m
overrideByName = Override $ \_sym _rty args ->
  case args of
    [fromPtr, toPtr] -> do
      from <- loadString fromPtr
      to <- loadString toPtr
      let sym = fromString from
          sym' = fromString to
          handler = Redirect sym'
      modify $ \s -> s { overrides = M.insert sym handler (overrides s) }
      return Nothing
    _ -> error "override_function_by_name: wrong number of arguments"

overrideByAddr :: (Functor m, Monad m, MonadIO m, Functor sbe,
                   ConstantProjection (SBEClosedTerm sbe)) =>
                  Override sbe m
overrideByAddr = Override $ \_sym _rty args ->
  case args of
    [_fromPtr, _toPtr] -> do
      dbugM "overrideByAddr: nyi"
      return Nothing
    _ -> error "override_function_by_addr: wrong number of arguments"

nyiOverride :: Override sbe m
nyiOverride = Override $ \sym _rty _args ->
  error $ "override niy: " ++ show (L.ppSymbol sym)

type OverrideEntry sbe m = (L.Symbol, L.Type, [L.Type], Bool, Override sbe m)
standardOverrides :: (Functor m, Monad m, MonadIO m, Functor sbe,
                      ConstantProjection (SBEClosedTerm sbe)) =>
                     [OverrideEntry sbe m]
standardOverrides =
  [ ("exit", voidTy, [i32], False,
     -- TODO: stub! Should be replaced with something useful.
     Override $ \_sym _rty _args -> dbugM "TODO: Exit!" >> return Nothing)
  , ("alloca", voidPtr, [i32], False, allocHandler alloca)
  , ("malloc", voidPtr, [i32], False, allocHandler malloc)
  , ("printf", i32, [strTy], True, printfHandler)
  , ("lss_abort", voidTy, [strTy], False, abortHandler)
  , ("fresh_uint8",   i8,  [i8], False, freshInt'  8)
  , ("fresh_uint16", i16, [i16], False, freshInt' 16)
  , ("fresh_uint32", i32, [i32], False, freshInt' 32)
  , ("fresh_uint64", i64, [i64], False, freshInt' 64)
  , ("fresh_array_uint8",   i8p, [i32,  i8], False, freshIntArray 8)
  , ("fresh_array_uint16", i16p, [i32, i16], False, freshIntArray 16)
  , ("fresh_array_uint32", i32p, [i32, i32], False, freshIntArray 32)
  , ("fresh_array_uint64", i64p, [i32, i64], False, freshIntArray 64)
  , ("write_aiger_uint8",  voidTy, [i8,  strTy], False, writeIntAiger)
  , ("write_aiger_uint16", voidTy, [i16, strTy], False, writeIntAiger)
  , ("write_aiger_uint32", voidTy, [i32, strTy], False, writeIntAiger)
  , ("write_aiger_uint64", voidTy, [i64, strTy], False, writeIntAiger)
  , ("write_aiger_array_uint8", voidTy, [i8p, i32, strTy], False,
     writeIntArrayAiger i8)
  , ("write_aiger_array_uint16", voidTy, [i16p, i32, strTy], False,
     writeIntArrayAiger i16)
  , ("write_aiger_array_uint32", voidTy, [i32p, i32, strTy], False,
     writeIntArrayAiger i32)
  , ("write_aiger_array_uint64", voidTy, [i64p, i32, strTy], False,
     writeIntArrayAiger i64)
  , ("eval_aiger_uint8",   i8, [i8,  i8p], False, evalAigerOverride)
  , ("eval_aiger_uint16", i16, [i16, i8p], False, evalAigerOverride)
  , ("eval_aiger_uint32", i32, [i32, i8p], False, evalAigerOverride)
  , ("eval_aiger_uint64", i64, [i64, i8p], False, evalAigerOverride)
  , ("eval_aiger_array_uint8",  voidTy, [i8p,  i8p,  i32, i8p, i32], False,
     nyiOverride)
  , ("eval_aiger_array_uint16", voidTy, [i16p, i16p, i32, i8p, i32], False,
     nyiOverride)
  , ("eval_aiger_array_uint32", voidTy, [i32p, i32p, i32, i8p, i32], False,
     nyiOverride)
  , ("eval_aiger_array_uint64", voidTy, [i64p, i64p, i32, i8p, i32], False,
     nyiOverride)
  , ("override_function_by_name", voidTy, [strTy, strTy], False, overrideByName)
  , ("override_function_by_addr", voidTy, [voidPtr, voidPtr], False,
     overrideByAddr)
  ]

registerOverride' :: (MonadIO m, Functor m, Functor sbe) =>
                     OverrideEntry sbe m -> Simulator sbe m ()
registerOverride' (sym, rty, atys, va, handler) =
  registerOverride sym rty atys va handler

registerStandardOverrides :: (Functor m, Monad m, MonadIO m, Functor sbe,
                              ConstantProjection (SBEClosedTerm sbe)) =>
                             Simulator sbe m ()
registerStandardOverrides = do
  mapM_ registerOverride' standardOverrides
