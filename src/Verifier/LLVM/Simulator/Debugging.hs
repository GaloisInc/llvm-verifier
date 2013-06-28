{- |
Module           : $Header$
Description      : Debugger implementation for LSS
Stability        : provisional
Point-of-contact : acfoltzer

Debugger for the LLVM Symbolic Simulator. This module provides
implementations of the 'SEH' event handlers. Commands and their
semantics are loosely based on gdb.

-}

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

module Verifier.LLVM.Simulator.Debugging (
    breakOnMain
  , debuggerREPL
  , resetInterrupt
  ) where

import Control.Applicative
import Control.Concurrent (myThreadId)
import Control.Exception (AsyncException(UserInterrupt))
import Control.Monad
import Control.Monad.Error
import Control.Monad.State.Class
import Control.Lens hiding (createInstance)
import Data.Char
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.String
import qualified Data.Vector as V
import System.Console.Haskeline
import System.Console.Haskeline.History
import System.Exit
import Text.PrettyPrint.Leijen hiding ((<$>))

import Verifier.LLVM.AST
import Verifier.LLVM.Backend
import Verifier.LLVM.Codebase
import Verifier.LLVM.Simulator.Internals
import Verifier.LLVM.Simulator.SimUtils

import System.Posix.Signals

#if __GLASGOW_HASKELL__ < 706
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.Read as R

readEither :: Read a => String -> Either String a
readEither s =
  case [ x | (x,"") <- R.readPrec_to_S read' R.minPrec s ] of
    [x] -> Right x
    []  -> Left "Prelude.read: no parse"
    _   -> Left "Prelude.read: ambiguous parse"
 where
  read' =
    do x <- R.readPrec
       R.lift P.skipSpaces
       return x

-- | Parse a string using the 'Read' instance.
-- Succeeds if there is exactly one valid result.
readMaybe :: Read a => String -> Maybe a
readMaybe s = case readEither s of
                Left _  -> Nothing
                Right a -> Just a
#else
import Text.Read (readMaybe)
#endif

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

-- | Add a breakpoint to @main@ in the current codebase
breakOnMain :: (Functor m, Monad m) => Simulator sbe m ()
breakOnMain = addBreakpoint (Symbol "main") (initSymBlockID,0)

logBreakpoint :: (Functor m, Monad m, MonadIO m)
              => Simulator sbe m ()
logBreakpoint = do
  Just p <- preuse currentPathOfState
  let sym = pathFuncSym p
      psym = ppSymbol sym
  Just def <- lookupDefine (pathFuncSym p) <$> gets codebase
  let bld =
        case p^.pathPC of
          Nothing -> psym
          Just (pcb,pc) -> psym <> ppSymBlockID pcb <> colon <+> ppStmt stmt
            where sb = lookupSymBlock def pcb
                  stmt = sbStmts sb V.! pc
  dbugM $ show $ text "at" <+> bld <> colon

debuggerREPL :: (Functor sbe, Functor m, MonadIO m, MonadException m)
             => Simulator sbe m ()
debuggerREPL = do
    trBreakpoints .= NoTransientBreakpoint
    logBreakpoint
    let settings = setComplete completer $
                     defaultSettings { historyFile = Just ".lssdb" }
    runInputT settings loop
  where loop = do
          mline <- getInputLine "(lss) "
          case mline of
            Nothing -> return ()
            Just "" -> do
              -- repeat last command if nothing entered
              hist <- getHistory
              case historyLines hist of
                (l:_) -> doCmd (words l)
                _ -> loop
            Just input -> doCmd (words input)
        doCmd (cmdStr:args) = do
          case M.lookup cmdStr commandMap of
            Just cmd -> do
              let go = cmdAction cmd args
                  handleErr (FailRsn rsn) = do
                    dbugM $ "error: " ++ rsn
                    return False
              continue <- lift $ catchError go handleErr
              when (continue == False) loop
            Nothing -> do
              outputStrLn $ "unknown command '" ++ cmdStr ++ "'"
              outputStrLn $ "type 'help' for more info"
              loop
        doCmd [] = error "unreachable"

data Command m = Cmd {
    cmdNames :: [String]
  , cmdArgs :: [String]
  , cmdDesc :: String
  , cmdCompletion :: CompletionFunc m
  , cmdAction :: [String] -> m Bool
  }

commandMap :: (Functor sbe, Functor m, Monad m, MonadIO m)
           => M.Map String (Command (Simulator sbe m))
commandMap = M.fromList . concatMap expandNames $ cmds
  where expandNames cmd = do
          name <- cmdNames cmd
          return (name, cmd)

cmds :: (Functor sbe, Functor m, Monad m, MonadIO m)
     => [Command (Simulator sbe m)]
cmds = [
    helpCmd
  , whereCmd
  , localsCmd
  , dumpCmd
  , contCmd
  , killCmd
  , satpathCmd
  , exitCmd
  , stopCmd
  , clearCmd
  , infoCmd
  -- , stepCmd
  , stepupCmd
  , stepiCmd
  ]

helpCmd :: forall sbe m . (Functor sbe, Functor m, Monad m, MonadIO m)
        => Command (Simulator sbe m)
helpCmd = Cmd {
    cmdNames = ["help", "?"]
  , cmdArgs = []
  , cmdDesc = "show this help"
  , cmdCompletion = noCompletion
  , cmdAction = \_ -> do
      dbugM $ helpString (cmds :: [Command (Simulator sbe m)])
      return False
  }

helpString :: [Command m] -> String
helpString cs = show . vcat $
  [ invs <> colon <$$> nest 2 (text $ cmdDesc cmd)
  | cmd <- cs
  , let invs = hsep . map text $ (cmdNames cmd ++ cmdArgs cmd)
  ]

failHelp :: Monad m => m a
failHelp = fail "invalid arguments; type 'help' for details"

whereCmd :: (Functor m, Monad m, MonadIO m) => Command (Simulator sbe m)
whereCmd = Cmd {
    cmdNames = ["where", "w"]
  , cmdArgs = []
  , cmdDesc = "print call stack"
  , cmdCompletion = noCompletion
  , cmdAction = \_ -> do
      mp <- preuse currentPathOfState
      case mp of
        Nothing -> dbugM "no active execution path"
        Just p -> dbugM . show . ppStackTrace . pathStack $ p
      return False
  }

localsCmd :: (Functor m, Monad m, MonadIO m) => Command (Simulator sbe m)
localsCmd = Cmd {
    cmdNames = ["locals"]
  , cmdArgs = []
  , cmdDesc = "print local variables in current stack frame"
  , cmdCompletion = noCompletion
  , cmdAction = \_ -> do
      mp <- preuse currentPathOfState
      case mp of
        Nothing -> dbugM "no active execution path"
        Just p -> do
          sbe <- gets symBE
          dbugM . show $ ppRegMap sbe (p^.pathRegs)
      return False
  }

dumpCmd :: (Functor m, Monad m, MonadIO m) => Command (Simulator sbe m)
dumpCmd = let arglist = ["ctrlstk", "block", "function", "memory"]
          in Cmd {
    cmdNames = ["dump", "d"]
  , cmdArgs = arglist
  , cmdDesc = "dump an object in the simulator"
  , cmdCompletion = completeWordWithPrev Nothing " " $ \revleft word -> do
      case length . words $ revleft of
        -- only dump one thing at a time
        1 -> return . map simpleCompletion
                    . filter (word `isPrefixOf`)
                    $ arglist
        _ -> return []
  , cmdAction = \args -> do
      case args of
        ["ctrlstk"] -> dumpCtrlStk
        ["block"] -> do
          mp <- preuse currentPathOfState
          case mp of
            Nothing -> fail "no active execution path"
            Just p -> do
              let sym = pathFuncSym p
                  Just (pcb,_) = p^.pathPC
              Just def <- lookupDefine sym <$> gets codebase
              dbugM . show . ppSymBlock $ lookupSymBlock def pcb
        ["function"] -> do
          mp <- preuse currentPathOfState
          case mp of
            Nothing -> fail "no active execution path"
            Just p -> dumpSymDefine (gets codebase) (unSym . pathFuncSym $ p)
        ["memory"] -> dumpMem 0 "memory"
        _ -> dbugM $ "dump: unsupported object " ++ unwords args
      return False
  }

contCmd :: Monad m => Command m
contCmd = Cmd {
    cmdNames = ["cont", "c"]
  , cmdArgs = []
  , cmdDesc = "continue execution"
  , cmdCompletion = noCompletion
  , cmdAction = \_ -> return True
  }

killCmd :: (Functor sbe, Functor m, MonadIO m) => Command (Simulator sbe m)
killCmd = Cmd {
    cmdNames = ["kill"]
  , cmdArgs = ["[<msg>]"]
  , cmdDesc = "kill the current execution path"
  , cmdCompletion = noCompletion
  , cmdAction = errorPath . unwords
  }

exitCmd :: MonadIO m => Command m
exitCmd = Cmd {
    cmdNames = ["exit", "quit", "q"]
  , cmdArgs = []
  , cmdDesc = "exit LSS"
  , cmdCompletion = noCompletion
  , cmdAction = \_ -> liftIO $ exitWith ExitSuccess
  }

satpathCmd :: (Functor sbe, Functor m, MonadIO m)
           => Command (Simulator sbe m)
satpathCmd = Cmd {
    cmdNames = ["satpath", "sat"]
  , cmdArgs = []
  , cmdDesc = "check whether the current path's assertions are satisfiable, killing this path if they are not"
  , cmdCompletion = noCompletion
  , cmdAction = \_ -> do
      Just p <- preuse currentPathOfState
      sbe <- gets symBE
      sat <- liftSBE $ termSAT sbe (p^.pathAssertions)
      case sat of
        UnSat -> do dbugM "path assertions unsatisfiable; killed"
                    errorPath "path assertions unsatisfiable: killed by debugger"
        Sat _ -> dbugM "path assertions satisfiable"
        Unknown -> dbugM "pat assertions possibly satisfiable"
      return False
  }

stopCmd :: (Functor sbe, Functor m, Monad m, MonadIO m)
        => Command (Simulator sbe m)
stopCmd = Cmd {
    cmdNames = ["stop"]
  , cmdArgs = ["<function_symbol>[%block.id]"]
  , cmdDesc = "set a breakpoint at a function; if no block is specified, break at the entry to the function"
  , cmdCompletion = funcSymCompletion
  , cmdAction = \args ->
      case args of
        [arg] -> do
          cb <- gets codebase
          either dbugM (uncurry addBreakpoint) $ bpsForArg cb arg
          return False
        _ -> failHelp
  }

clearCmd :: (Functor sbe, Functor m, Monad m, MonadIO m)
         => Command (Simulator sbe m)
clearCmd = Cmd {
    cmdNames = ["clear"]
  , cmdArgs = ["<function_symbol>[%block.id]"]
  , cmdDesc = "clear a breakpoint at a function"
  , cmdCompletion = funcSymCompletion
  , cmdAction = \args ->
      case args of
        [arg] -> do
          cb <- gets codebase
          either dbugM (uncurry removeBreakpoint) (bpsForArg cb arg)
          return False
        _ -> failHelp
  }

bpsForArg :: Codebase sbe
          -> String
          -> Either String (Symbol, Breakpoint)
bpsForArg cb arg =
  case break (== '%') arg of
    (sym0, '%' : bbid) -> do
     let sym = Symbol sym0
     case lookupDefine sym cb of
       Nothing  -> Left $ "Could not find symbol " ++ show (ppSymbol sym) ++ "."
       Just def ->
         case break (=='.') (reverse (drop 1 bbid)) of
           (nstr,'.':rest) | Just n <- readMaybe (reverse nstr) -> do
             let sbid = symBlockID (fromString (reverse rest)) n
             case M.lookup sbid (sdBody def) of
               Just _ -> Right (sym, (sbid,0))
               Nothing -> Left "Could not find basic block in function."
           _ -> Left "Could not parse basic block."
    _ -> Left "Could not parse basic block."

infoCmd :: MonadIO m => Command (Simulator sbe m)
infoCmd = Cmd {
    cmdNames = ["info"]
  , cmdArgs = []
  , cmdDesc = "list breakpoints"
  , cmdCompletion = noCompletion
  , cmdAction = \_ -> dumpBPs >> return False
  }

dumpBPs :: MonadIO m => Simulator sbe m ()
dumpBPs = do
  bps <- use breakpoints
  let msg | all S.null (M.elems bps) = "no breakpoints set"
          | otherwise = show (ppBreakpoints bps)
  dbugM msg

stepupCmd :: (Functor m, Monad m) => Command (Simulator sbe m)
stepupCmd = Cmd {
    cmdNames = ["finish"]
  , cmdArgs = []
  , cmdDesc = "execute until the current function returns to its caller."
  , cmdCompletion = noCompletion
  , cmdAction = \_ -> do
      Just p <- preuse currentPathOfState
      trBreakpoints .= BreakReturnFrom (pathStackHt p)
      return True
  }

stepiCmd :: (Functor m, Monad m) => Command (Simulator sbe m)
stepiCmd = Cmd {
    cmdNames = ["stepi", "si"]
  , cmdArgs = []
  , cmdDesc = "execute current symbolic statement"
  , cmdCompletion = noCompletion
  , cmdAction = \_ -> do
      trBreakpoints .= BreakNextStmt
      return True
  }


completer :: forall sbe m . (Functor sbe, Functor m, Monad m, MonadIO m)
          => CompletionFunc (Simulator sbe m)
completer (revleft, right) = do
    let (revword, revleft') = break isSpace revleft
        word = reverse revword
        cmdComps = map simpleCompletion . filter (word `isPrefixOf`) . M.keys $ m
    case all isSpace revleft' of
      True -> return (revleft', cmdComps)
      False -> do
        -- partial pattern ok:
        --   not (all isSpace revleft') => not (null (words revleft))
        let (cmd:_) = words (reverse revleft)
        case M.lookup cmd m of
          Nothing -> return (revleft, [])
          Just c -> cmdCompletion c (revleft, right)
  where
    m :: Functor sbe => M.Map String (Command (Simulator sbe m))
    m = commandMap

-- | Complete a function symbol as the current word
funcSymCompletion :: (Functor sbe, Functor m, Monad m, MonadIO m)
                  => CompletionFunc (Simulator sbe m)
funcSymCompletion = completeWordWithPrev Nothing " " fn
  where
    fn _revleft word = do
      cb <- gets codebase
      ovrs <- map unSym . M.keys <$> use fnOverrides
      let isOverride = (`elem` ovrs)
          syms = filter (not . isOverride)
               . map unSym
               $ M.keys (cb^.cbFunctionTypes)
          strictPrefixOf p xs = p `isPrefixOf` xs && p /= xs
          matches = filter (word `strictPrefixOf`) syms
      case matches of
        -- still working on a function symbol
        _:_ -> return . map (notFinished . simpleCompletion) $ matches
        -- otherwise completing a block name, so figure out which
        -- functions we completed so far
        [] -> do
          let matches' = filter (`isPrefixOf` word) syms
          concat <$> forM matches' (\sym -> do
            mdef <- lookupDefine (Symbol sym) <$> gets codebase
            case mdef of
              Nothing -> return []
              Just def -> do
                -- expect bbids in their pretty-printed form
                let bbids = filter (/= "init")
                          . map (show . ppSymBlockID)
                          . M.keys . sdBody $ def
                    cleanup = notFinished . simpleCompletion . (sym ++)
                case stripPrefix sym word of
                  Just prefix -> do
                    return . map cleanup . filter (prefix `isPrefixOf`) $ bbids
                  Nothing ->
                    return . map cleanup $ bbids)

notFinished :: Completion -> Completion
notFinished c = c { isFinished = False }

unSym :: Symbol -> String
unSym (Symbol str) = str