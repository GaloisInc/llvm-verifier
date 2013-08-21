{- |
Module           : $Header$
Description      : Debugger implementation for LSS
Stability        : provisional
Point-of-contact : acfoltzer, jhendrix

Debugger for the LLVM Symbolic Simulator. This module provides
implementations of the 'SEH' event handlers. Commands and their
semantics are loosely based on gdb.

-}

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Verifier.LLVM.Debugger
  ( initializeDebugger
  , addBreakpoint
  , breakOnEntry
  , resetInterrupt
  , checkForBreakpoint
  ) where

import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.Error
import Control.Monad.Identity
import qualified Control.Monad.State as MTL
import Control.Monad.State.Class
import Control.Lens hiding (createInstance)
import Data.Char
import Data.IORef
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.String
import System.Console.Haskeline
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import Text.PrettyPrint.Leijen hiding ((<$>), (</>))
import qualified Text.PrettyPrint.Leijen as PP

-- GHC.IO.Exception is imported so that we can catch
-- the UnsupportedOperation error that may be thrown
-- by getAppUserDataDirectory.
import GHC.IO.Exception

import Verifier.LLVM.Backend
import Verifier.LLVM.Codebase
import Verifier.LLVM.Debugger.Grammar
import Verifier.LLVM.Simulator.Internals
import Verifier.LLVM.Simulator.SimUtils
import Verifier.LLVM.Utils.PrettyPrint

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
import Control.Concurrent (myThreadId)
import System.Posix.Signals
#endif

-- | Break on entry to the function.
breakOnEntry :: (Functor m, Monad m)
             => SymDefine (SBETerm sbe) -> Simulator sbe m ()
breakOnEntry def = addBreakpoint (sdName def) (sdEntry def, 0)

safeGetAppUserDataDirectory :: String -> IO (Maybe FilePath)
safeGetAppUserDataDirectory nm = 
    System.Console.Haskeline.catch (Just <$> getAppUserDataDirectory nm)
          catchErrors
  where -- Catch the UnsupportedOperation and DoesNotExist
        -- exceptions that may be thrown.
        catchErrors :: IOError -> IO (Maybe a)
        catchErrors e =
          case ioeGetErrorType e of
            NoSuchThing -> return Nothing
            UnsupportedOperation -> return Nothing
            _ -> throwIO e

-- | Return location of LSS history file
getLSSHistoryPath :: IO (Maybe FilePath)
getLSSHistoryPath = fmap (</> "history") <$> safeGetAppUserDataDirectory "lss"

------------------------------------------------------------------------
-- User interaction

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
-- @resetInterrupt does nothing on Windows.
resetInterrupt :: IO ()
resetInterrupt = return ()
#else
-- @resetInterrupt@ installs a one-time signal handler so
-- that the next time an interrupt (e.g. Ctrl-C) is given, a
-- @UserInterrupt@ AsyncException will be thrown in the current
-- thread.
-- By default GHC will do this once during program execution, but
-- subsequent uses of Ctrl-C result in immediate termination.
-- This function allows Ctrl-C to be used to interrupt multiple
-- times over program execution.
resetInterrupt :: IO ()
resetInterrupt = do
  tid <- myThreadId
  let handler = throwTo tid UserInterrupt
  let mask = Nothing -- Any other signal handler can block while
  _ <- installHandler sigINT (CatchOnce handler) mask
  return ()
#endif

checkForBreakpoint :: (Functor sbe, Functor m, MonadException m)
                   => DebuggerRef sbe m -> Simulator sbe m ()
checkForBreakpoint r = do
  mcf <- preuse currentCallFrameOfState
  case mcf of
    Just cf -> do
      mbps <- use $ breakpoints . at (sdName (cfFunc cf))
      let atBP = fromMaybe False $ S.member (cf^.cfLocation) <$> mbps
      when atBP (enterDebuggerAtBreakpoint r)
    Nothing -> return ()

setPrevCommand :: MonadIO m
               => DebuggerRef sbe m'
               -> Debugger sbe m' ()
               -> m ()
setPrevCommand dr cmd = do
  liftIO $ modifyIORef dr $! onNoInput .~ cmd

runNextCommand :: (Functor sbe, Functor m, MonadException m)
               => DebuggerCont sbe m
runNextCommand dr = do
  mline <- getInputLine "(lss) "
  ds <- liftIO $ readIORef dr   
  case dropWhile isSpace <$> mline of
    Nothing -> return ()
    Just "" -> do
      runDebugger (ds^.onNoInput) (\() -> runNextCommand) dr
    Just cmdStr -> do
      let pr = runIdentity $ parseString (dsGrammar ds) cmdStr
      case resolveParse pr of
        Left d -> do
          outputStrLn (show d)
          setPrevCommand dr $ return ()
          runNextCommand dr
        Right cmd -> do
          setPrevCommand dr cmd
          runDebugger cmd (\() -> runNextCommand) dr

data DebuggerState sbe m 
   = DebuggerState { -- | Grammar to use for parsing commands
                     dsGrammar :: SimGrammar sbe m
                   , _onNoInput :: Debugger sbe m ()
                   , _resumeThrowsError :: Bool
                   }

-- | Command to run if no input is provided.
onNoInput :: Simple Lens (DebuggerState sbe m) (Debugger sbe m ())
onNoInput = lens _onNoInput (\s v -> s { _onNoInput = v })

-- | Flag that holds if we know that resuming from this point will throw an error.
resumeThrowsError :: Simple Lens (DebuggerState sbe m) Bool
resumeThrowsError = lens _resumeThrowsError (\s v -> s { _resumeThrowsError = v })

type DebuggerRef sbe m = IORef (DebuggerState sbe m)

type DebuggerCont sbe m
   = DebuggerRef sbe m
   -> InputT (Simulator sbe m) ()

newtype Debugger sbe m a
      = Debugger { runDebugger :: (a -> DebuggerCont sbe m)
                               -> DebuggerCont sbe m
                 }


instance Functor (Debugger sbe m) where
  fmap f d = Debugger (\c -> runDebugger d (c . f))

instance Applicative (Debugger sbe m) where
  pure v = Debugger ($v)  
  mf <*> mv = Debugger $ \c -> do
    runDebugger mf (\f -> runDebugger mv (c.f))

instance (Functor sbe, Functor m, MonadException m)
      => Monad (Debugger sbe m) where
  m >>= h = Debugger $ \c -> runDebugger m (\v -> runDebugger (h v) c)
  return v = Debugger ($v)
  fail m = Debugger $ \_c dr -> do
    dbugM $ "Unexpected error: " ++ show m
    setPrevCommand dr $ return ()
    runNextCommand dr

instance (Functor sbe, Functor m, MonadException m)
      => MTL.MonadState (DebuggerState sbe m) (Debugger sbe m) where
  get   = Debugger $ \c r -> liftIO (readIORef r) >>= flip c r
  put v = Debugger $ \c r -> liftIO (writeIORef r v) >> c () r

instance (Functor sbe, Functor m, MonadException m)
      => MonadIO (Debugger sbe m) where
  liftIO m = Debugger (\c r -> liftIO m >>= flip c r)

getDebuggerRef :: Debugger sbe m (DebuggerRef sbe m)
getDebuggerRef = Debugger (\c r -> c r r)

runSim :: Monad m => Simulator sbe m a -> Debugger sbe m a
runSim m = Debugger (\c r -> lift m >>= flip c r)

-- | Resume exectuion of simulator.
resume :: Monad m => Debugger sbe m a
resume = Debugger (\_ _ -> return ())

-- | Setup simulator to run debugger when needed.
initializeDebugger :: (Functor sbe, Functor m, MonadException m)
                   => Simulator sbe m (DebuggerRef sbe m)
initializeDebugger = do
  cb <- gets codebase
  let grammar = allCmds cb
  let ds = DebuggerState { dsGrammar = grammar
                         , _onNoInput = return ()
                         , _resumeThrowsError = False
                         }
  r <- liftIO $ newIORef ds
  onPathPosChange .= checkForBreakpoint r
  onSimError      .= enterDebuggerOnError r
  onUserInterrupt .= enterDebuggerOnInterrupt r
  return r

enterDebuggerOnError :: (Functor sbe, Functor m, MonadException m)
                     => DebuggerRef sbe m
                     -> ErrorHandler sbe m
enterDebuggerOnError r cs rsn = do
  -- Reset state
  ctrlStk ?= cs
  -- Get current path before last step.
  dbugM $ show $
    text "Simulation error:" <+> ppFailRsn rsn
  -- Debugger state
  enterDebugger r True

enterDebuggerOnInterrupt :: (Functor sbe, Functor m, MonadException m)
                         => DebuggerRef sbe m
                         -> Simulator sbe m ()
enterDebuggerOnInterrupt r = do
  dbugM $ show $ 
    text "Simulation interrupted: Entering debugger"
  enterDebugger r False
  dbugM $ "Resuming simulation"
  liftIO $ resetInterrupt

enterDebuggerAtBreakpoint :: (Functor sbe, Functor m, MonadException m)
                          => DebuggerRef sbe m
                          -> Simulator sbe m ()
enterDebuggerAtBreakpoint dr = do
  dbugM "Encountered breakpoint"
  enterDebugger dr False

enterDebugger :: (Functor sbe, Functor m, MonadException m)
              => DebuggerRef sbe m
              -> Bool -- ^ Indicates if debugger was entered due to error in current path.
              -> Simulator sbe m ()
enterDebugger r eoe = do
  liftIO $ modifyIORef r $ resumeThrowsError .~ eoe
  withActivePath () $ \p -> do
    dbugM $ show $ indent 2 (text "at" <+> ppPathNameAndLoc p)
  grammar <- liftIO $ dsGrammar <$> readIORef r
  historyPath <- liftIO getLSSHistoryPath
  let settings = setComplete (matcherCompletions grammar)
               $ defaultSettings { historyFile = historyPath }
  runInputT settings (runNextCommand r)

------------------------------------------------------------------------
-- Completions

matcherCompletions :: (Functor m, Monad m)
                   => Grammar Identity a
                   -> (String, String)
                   -> Simulator sbe m (String, [Completion])
matcherCompletions m (l,_r) = pure (finalize a)
  where rl = reverse l        
        a = runIdentity $ parseString m rl
        finalize pr =
                 (reverse (take p rl), cl)
          where (p,cl) = pr^.parseCompletions

data YesNo = Yes | No
  deriving (Eq)

putAndFlush :: MonadIO m => String -> m ()
putAndFlush msg = liftIO $ putStr msg >> hFlush stdout

-- | Prompts the user for a string matching a choice.
promptChoice :: MonadIO m => String -> [(String,r)] -> m r
promptChoice prompt choices =
    liftIO $ putAndFlush prompt >> loop
  where loop = do
          l <- dropWhile isSpace . fmap toLower <$> getLine
          let resl = [ r | (c,r) <- choices, l `isPrefixOf` c ] 
          case (l,resl) of
            ("",_) -> putAndFlush prompt >> loop
            (_,r:_)  -> return r
            (_,[]) -> do putAndFlush ("Invalid response; " ++ prompt)
                         loop

promptYesNoCancel :: MonadIO m => m (Maybe YesNo)
promptYesNoCancel = promptChoice prompt choices
  where prompt = "Please enter (Y)es, (N)o, or (C)ancel: "
        choices = [("yes", Just Yes), ("no", Just No), ("cancel", Nothing) ]

promptYesNo :: MonadIO m => m YesNo
promptYesNo = promptChoice prompt choices
  where prompt = "Please enter (Y)es or (N)o: "
        choices = [("yes", Yes), ("no", No)]

-- | Check to see if we should warn user before resuming.  Return True
-- if resume should continue
warnIfResumeThrowsError :: (Functor sbe, Functor m, MonadException m)
                        => Debugger sbe m Bool
warnIfResumeThrowsError = do
  rte <- use resumeThrowsError
  mcs <- runSim $ use ctrlStk
  case mcs of
    Just cs | rte -> do
      dbugM "Resuming execution on this path should rethrow the simulation error."
      if csHasSinglePath cs then do
        dbugM "Should lss kill the current path, and stop simulation?"
        ync <- promptYesNoCancel
        case ync of
          Just Yes -> do
            killPathByDebugger
            resume
          Just No -> return True
          Nothing -> return False
      else do
        dbugM "Should lss kill the current path, and resume on a new path?"
        ync <- promptYesNoCancel
        case ync of
          Just Yes -> do
            killPathByDebugger
            return True
          Just No -> return True
          Nothing -> return False
    Just cs -> return (pathIsActive (cs^.currentPath))
    Nothing -> return False

-- | @resumeActivePath m@ runs @m@ with the current path,
-- and resumes execution if there is a current path and @m@
-- returns true.
resumeActivePath :: (Functor sbe, Functor m, MonadException m)
                 => (Path sbe -> Simulator sbe m ())
                 -> Debugger sbe m ()
resumeActivePath action = do
  c <- warnIfResumeThrowsError
  when c $ do
    join $ runSim $ do
      withActivePath (return ()) $ \p -> 
        action p >> return resume

type SimGrammar sbe m = (Functor sbe, Functor m, MonadException m)
                     => Grammar Identity (Debugger sbe m ())

commandHelp :: Grammar m a -> Doc
commandHelp cmds = 
    text "List of commands:" <$$> 
    PP.empty <$$> 
    vcat (ppCmdHelp <$> (help^.helpCmds)) <$$>
    PP.empty <$$>
    text "Type \"help\" followed by command name for full documentation." <$$>
    text "Command name abbreviations are allowed if unambiguous."
  where help = matcherHelp cmds

newtype NameMap = NameMap (M.Map String NameMapPair)
type NameMapPair = (Maybe SymBlockID, NameMap)

emptyNameMap :: NameMap
emptyNameMap = NameMap M.empty

splitName :: String -> [String]
splitName nm =
    case break (=='.') nm of
      (l,[]) -> [l]
      (l,_:r) -> l : splitName r

mergeName :: SymBlockID -> [String] -> NameMapPair -> NameMapPair
mergeName b [] (_,m) = (Just b,m)
mergeName b (h:r) (o,NameMap m) = (o, NameMap $ m & at h ?~ mergeName b r mr)
  where mr = fromMaybe (Nothing, NameMap M.empty) $ m^.at h

insertName :: NameMap -> SymBlockID -> NameMap
insertName m b = snd $ mergeName b (splitName (show (ppSymBlockID b)))
                                   (Nothing,m)

pairBlocks :: SymDefine t -> NameMapPair -> Grammar m (Symbol, Breakpoint)
pairBlocks d (mb,m@(NameMap m')) = switch $ others ++ end
  where others | M.null m' = []
               | otherwise = [(,) "." $ nameBlocks d m]
        end = case mb of
                Nothing -> []
                Just b -> [(,) "" $ pure (sdName d,(b,0))]

nameBlocks :: SymDefine t -> NameMap -> Grammar m (Symbol, Breakpoint)
nameBlocks d (NameMap m) = switch $ (entry <$> M.toList m)
  where entry (nm,p) = (fromString nm, pairBlocks d p)

-- | Parses a program location for a breakpoint.
locSeq :: forall sbe m . Codebase sbe -> Grammar m (Symbol, Breakpoint)
locSeq cb = argLabel (text "<loc>") *> hide (switch $ fmap matchDef (cbDefs cb))
  where matchDef :: SymDefine (SBETerm sbe)
                 -> (SwitchKey, Grammar m (Symbol, Breakpoint))
        matchDef d = ( fromString nm
                     , switch [ (,) ":" (nameBlocks d m)
                              , (,) "" $ end
                              ]
                     )
         where Symbol nm = sdName d
               end :: Grammar m (Symbol, Breakpoint)
               end = pure (sdName d, (sdEntry d, 0))
               m = foldl insertName emptyNameMap (M.keys (sdBody d))


-- | List of commands for debugger.
allCmds :: Codebase sbe -> SimGrammar sbe m
allCmds cb = res 
  where res -- Breakpoints
            =    keyword "continue" *> continueCmd
            <||> keyword "break"  *> breakCmd cb
            -- Execution
            <||> keyword "delete" *> deleteCmd cb
            <||> keyword "finish" *> finishCmd
            <||> hide (keyword "s" *> stepiCmd)
            <||> keyword "stepi" *> stepiCmd
            -- Information about current path.
            <||> keyword "info"   *> infoCmd
            <||> keyword "list"   *> listCmd cb
            -- Function for switching between paths
            <||> keyword "path" *> pathCmd
            -- Control and information about debugger.
            <||> keyword "help" *> helpCmd res
            <||> keyword "quit"   *> quitCmd
            
helpCmd :: SimGrammar sbe m -> SimGrammar sbe m
helpCmd cmdList = cmdDef "Print list of commands." $ do
  liftIO $ print (commandHelp cmdList)

pathCmd :: SimGrammar sbe m
pathCmd
  =    hide pathListCmd
  <||> keyword "kill" *> pathKillCmd
  <||> keyword "list" *> pathListCmd
  <||> keyword "sat" *> pathSatCmd


pathListCmd :: SimGrammar sbe m
pathListCmd = cmdDef "List all current execution paths." $ runSim $ do
  mcs <- use ctrlStk
  case mcs of
    Nothing ->
      dbugM "No active paths."
    Just cs -> dbugM $ show $ printTable table
      where ppPathItem :: Integer -> Path sbe -> [String]
            ppPathItem i p = [ 3 `padLeft` (show i)
                             , show (ppPathNameAndLoc p)
                             ]              
            -- Header for table.
            header = [ " Num", "Location"]
            -- Entries for table.
            table = header : zipWith ppPathItem [1..] (cs^..currentPaths)


pathSatCmd :: SimGrammar sbe m
pathSatCmd =
  cmdDef "Check satisfiability of path assertions with SAT solver." $ do
    withActiveCS () $ do
      sat <- runSim $ do
        sbe <- gets symBE
        cond <- assumptionsForActivePath
        liftSBE $ termSAT sbe cond
      case sat of
        UnSat -> do
          dbugM "The current path is infeasible.  Should simulation of the path be terminated?"
          yn <- promptYesNo
          when (yn == Yes) $ do
            killPathByDebugger
        Sat _ ->
          dbugM "Conditions along path are satisfiable."
        Unknown ->
          dbugM "Could not determine if path is feasible."

-- | Kills the current path with the debugger.
-- Assumes that there is an active path.
killPathByDebugger :: (Functor sbe, Functor m, MonadException m)
                   => Debugger sbe m ()
killPathByDebugger = do
  runSim $ killCurrentPath (FailRsn "Terminated by debugger.")
  resumeThrowsError .= False

pathKillCmd :: SimGrammar sbe m
pathKillCmd = cmdDef "Kill the current execution path." $ do
  withActiveCS () $ do
    killPathByDebugger
    mp <- runSim $ preuse currentPathOfState
    case mp of
      Nothing -> dbugM "Killed last path."
      Just p -> dbugM $ show $
        text "Switched to path:" <+> ppPathNameAndLoc p

quitCmd :: SimGrammar sbe m
quitCmd = cmdDef "Exit LSS." $ do
  mcs <- runSim $ use ctrlStk
  case mcs of
    Just cs | pathIsActive (cs^.currentPath) ->
      liftIO $ exitWith ExitSuccess
    _ -> resume

breakCmd :: Codebase sbe -> SimGrammar sbe m
breakCmd cb = (locSeq cb <**>) $ cmdDef desc $ \(b,p) -> do
    runSim $ addBreakpoint b p
  where desc = "Set a breakpoint at a specified location."

deleteCmd :: Codebase sbe -> SimGrammar sbe m
deleteCmd cb = (opt (locSeq cb) <**>) $ cmdDef desc $ \mbp -> do
      runSim $ 
        case mbp of
          Just (b,p) -> removeBreakpoint b p
          Nothing -> dbugM "Remove all breakpoints"
  where desc = "Clear a breakpoint at a function."

concatBreakpoints :: M.Map Symbol (S.Set Breakpoint)
                  -> [(Symbol, Breakpoint)]
concatBreakpoints m =
  [ (sym,bp) | (sym, bps) <- M.toList m, bp <- S.toList bps ]

infoCmd :: SimGrammar sbe m
infoCmd
  =    keyword "args"        *> infoArgsCmd
  <||> keyword "block"       *> infoBlockCmd
  <||> keyword "breakpoints" *> infoBreakpointsCmd
  <||> keyword "ctrlstk"     *> infoCtrlStkCmd
  <||> keyword "function"    *> infoFunctionCmd
  <||> keyword "locals"      *> infoLocalsCmd
  <||> keyword "memory"      *> infoMemoryCmd
  <||> keyword "stack"       *> infoStackCmd

infoArgsCmd :: SimGrammar sbe m
infoArgsCmd =
  cmdDef "Print argument variables in the current stack frame." $ do
    runSim $ withActiveCallFrame () $ \cf -> do
      sbe <- gets symBE
      dbugM $ show $ ppLocals sbe $ cfArgValues cf

infoBlockCmd :: SimGrammar sbe m
infoBlockCmd =
  cmdDef "Print block information." $ do
    runSim $ withActiveCallFrame () $ \cf -> do
      dbugM $ show $ ppSymBlock $ cfBlock cf

infoBreakpointsCmd :: SimGrammar sbe m
infoBreakpointsCmd =
  cmdDef "List breakpoints." $ runSim $ do
    bps <- concatBreakpoints <$> use breakpoints
    let ppRow :: Integer -> (Symbol,Breakpoint) -> [String]
        ppRow i (Symbol sym,bp) =
          [show i, sym ++ ":" ++ show (ppBreakpoint bp)]
    dbugM $ show $
      case bps of
        [] -> text "No breakpoints."
        _  -> printTable (header : zipWith ppRow [1..] bps)
          where header = ["Num", "Location"]

infoCtrlStkCmd :: SimGrammar sbe m
infoCtrlStkCmd =
  cmdDef "Print the entire control stack." $ do
    runSim dumpCtrlStk

infoFunctionCmd :: SimGrammar sbe m
infoFunctionCmd =
  cmdDef "Print function information." $ do
    runSim $ withActiveCallFrame () $ \cf -> do
      dbugM $ show $ ppSymDefine (cfFunc cf)

infoLocalsCmd :: SimGrammar sbe m
infoLocalsCmd =
  cmdDef "Print local variables in the current stack frame." $ do
    runSim $ withActiveCallFrame () $ \cf -> do
      sbe <- gets symBE
      dbugM $ show $ ppLocals sbe $ cfLocalValues cf

infoMemoryCmd :: SimGrammar sbe m
infoMemoryCmd =
  cmdDef "Print memory information." $ do
    runSim $ dumpMem 0 "memory"

--TODO: Revisit this command to check formatting.
infoStackCmd :: SimGrammar sbe m
infoStackCmd = cmdDef "Print backtrace of the stack." $ do
  runSim $ withActivePath () $ \p -> do
    sbe <- gets symBE
    let ppFrameLoc i cf = char '#' <> text (show i `padRight` 2)
                          <+> ppSymbol (sdName (cfFunc cf))
                          <> parens (commaSepList argDocs)
                          <+> text "at"
                          <+> ppLocation (cf^.cfLocation)
          where argDocs = ppArg <$> cfArgValues cf 
                ppArg (nm,v) =
                  ppIdent nm <> char '=' <+> prettyTermD sbe v

    dbugM $ show $ indent 2 $ vcat $
      zipWith ppFrameLoc [(0::Int)..] (p^..pathCallFrames)

listCmd :: Codebase sbe -> SimGrammar sbe m
listCmd cb = (locSeq cb <**>) $ cmdDef "List specified function in line." $ \loc -> do
  error "internal: listCmd undefined" loc

-- | @padLeft n s@ adds extra spaces before @s@ so that the result has
-- at least @n@ characters.
padLeft :: Int -> String -> String
padLeft l s = replicate (l - length s) ' ' ++ s

-- | @padRight n s@ adds extra spaces after @s@ so that the result has
-- at least @n@ characters.
padRight :: String -> Int -> String
padRight s l = s ++ replicate (l - length s) ' '

printTable :: [[String]] -> Doc
printTable m = vcat padded
  where maxl (i:x) (j:y) = max i j : maxl x y
        maxl [] y = y
        maxl x [] = x
        -- Maximum lengths of each column.          
        ll = fmap length <$> m
        maxLengths = foldr maxl (repeat 0) ll
        padded = hsep . fmap text . zipWith padLeft maxLengths <$> m

ppBreakpoint :: Breakpoint -> Doc
ppBreakpoint (sbid,0) = ppSymBlockID sbid 
ppBreakpoint (sbid,i) = ppSymBlockID sbid PP.<> char ':' PP.<> int i

-- | Run a computation if there is an active 
withActiveCS :: (Functor sbe, Functor m, MonadException m)
             => a -- ^ Result to return if there is no execution path.
             -> Debugger sbe m a -- ^ Action to run if there is an active path.
             -> Debugger sbe m a
withActiveCS v action = do
  mcs <- runSim $ use ctrlStk
  case mcs of
    Just cs | pathIsActive (cs^.currentPath) -> action
    _ -> dbugM "No active execution path." >> return v

-- | @withActivePath v act@ runs @act@ with the active path
withActivePath :: MonadIO m
               => a
               -> (Path sbe -> Simulator sbe m a)
               -> Simulator sbe m a
withActivePath v action = do
  mcs <- use ctrlStk
  case mcs of
    Just cs | pathIsActive (cs^.currentPath) -> action (cs^.currentPath)
    _ -> dbugM "No active execution path." >> return v

-- | @withActiveCallFrame v act@ runs @act@ with the active call frame.
withActiveCallFrame :: MonadIO m
                    => a
                    -> (CallFrame sbe -> Simulator sbe m a)
                    -> Simulator sbe m a
withActiveCallFrame v action = do
  mcf <- preuse currentCallFrameOfState
  case mcf of
    Just cf -> action cf
    _ -> dbugM "No active execution path." >> return v

continueCmd :: SimGrammar sbe m
continueCmd = cmdDef "Continue execution." $ do
  dr <- getDebuggerRef
  resumeActivePath $ \_ -> do
    onPathPosChange .= checkForBreakpoint dr

onReturnFrom :: (Functor sbe, Functor m, MonadIO m, MonadException m)
             => DebuggerRef sbe m -> Int -> Simulator sbe m ()
onReturnFrom dr ht = do
  Just p <- preuse currentPathOfState
  if pathStackHt p < ht then
    enterDebuggerAtBreakpoint dr
  else
    checkForBreakpoint dr

finishCmd :: SimGrammar sbe m
finishCmd = cmdDef desc $ do
    dr <- getDebuggerRef
    resumeActivePath $ \p -> do
      onPathPosChange .= onReturnFrom dr (pathStackHt p)
  where desc = "Execute until the current stack frame returns."

enterDebuggerAfterNSteps
  :: (Functor sbe, Functor m, MonadIO m, MonadException m)
  => DebuggerRef sbe m -> Integer -> Simulator sbe m ()
enterDebuggerAfterNSteps dr n
  | n <= 1 = enterDebugger dr False
  | otherwise = onPathPosChange .= enterDebuggerAfterNSteps dr (n-1)

stepiCmd :: SimGrammar sbe m
stepiCmd = (opt nat <**>) $
  cmdDef "Execute one symbolic statement" $ \mc -> do
    let c = fromMaybe 1 mc
    when (c > 0) $ do
      dr <- getDebuggerRef
      resumeActivePath $ \_ -> do
        onPathPosChange .= enterDebuggerAfterNSteps dr c