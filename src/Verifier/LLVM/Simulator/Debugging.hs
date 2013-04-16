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
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

module Verifier.LLVM.Simulator.Debugging (
    breakOnMain
  , logBreakpoints
  , enableDebugger
  , debuggerREPL
  , runAtEntryBreakpoints
  , runAtTransientBreakpoints
  , sanityChecks
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans
import Control.Monad.State.Class
import Control.Lens hiding (createInstance)

import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tuple.Curry
import Data.Word (Word16)

import System.Console.Haskeline
import System.Console.Haskeline.Completion
import System.Console.Haskeline.History
import System.Exit

import Text.PrettyPrint

import qualified Text.LLVM.AST as L

import Verifier.LLVM.Backend
import Verifier.LLVM.Simulator
import Verifier.LLVM.Simulator.Common

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

-- | Add a breakpoint to @main@ in the current codebase
breakOnMain :: (Functor m, Monad m) => Simulator sbe m ()
breakOnMain = addBreakpoint (Symbol "main") BreakEntry

-- | Given a step handler, return a new step handler that runs it when
-- Symbol entry or Basic Block entry breakpoints are encountered
runAtEntryBreakpoints :: (Functor m, Monad m)
                      => (SymBlock (SBETerm sbe) -> Simulator sbe m ())
                      -> SymBlock (SBETerm sbe)
                      -> Simulator sbe m ()
runAtEntryBreakpoints sh sb = do
  mp <- preuse currentPathOfState
  case pathFuncSym <$> mp of
    Nothing -> return ()
    Just sym -> do
      mbps <- M.lookup sym <$> use breakpoints
      let atBB = Just True == (S.member (BreakBBEntry (sbId sb)) <$> mbps)
          atEntry = (sbId sb == initSymBlockID)
                      && (Just True == (S.member BreakEntry <$> mbps))
      when (atBB || atEntry) $ sh sb

-- | Check whether we're at a transient breakpoint, and if so,
-- deactivate it and return 'True'
runAtTransientBreakpoints :: (Functor m, Monad m)
                          => (SymStmt (SBETerm sbe) -> Simulator sbe m ())
                          -> SymStmt (SBETerm sbe)
                          -> Simulator sbe m ()
runAtTransientBreakpoints sh stmt = do
  mp <- preuse currentPathOfState
  sym <- case pathFuncSym <$> mp of
    Nothing -> fail "no current function symbol"
    Just sym -> return sym
  let rember bp = do
        tbps <- use trBreakpoints
        if S.member bp tbps
          then do trBreakpoints %= S.delete bp
                  return True
          else return False
      handleStep = rember BreakNextStmt
      handleRet = case stmt of
                    Return _ -> rember (BreakReturnFrom sym)
                    _        -> return False
  callSH <- or <$> sequence [handleStep, handleRet]
  when callSH $ sh stmt

logBreakpoints :: (Functor m, Monad m, MonadIO m)
               => Simulator sbe m ()
logBreakpoints = do
  evHandlers.onBlockEntry .= (runAtEntryBreakpoints $ \sb -> do
    logBreakpoint Nothing)
  evHandlers.onPreStep .= (runAtTransientBreakpoints $ \stmt -> do
    logBreakpoint (Just stmt))

logBreakpoint :: (Functor m, Monad m, MonadIO m)
              => Maybe (SymStmt (SBETerm sbe))
              -> Simulator sbe m ()
logBreakpoint mstmt = do
  mp <- preuse currentPathOfState
  sym <- case pathFuncSym <$> mp of
    Nothing -> fail "no current function symbol"
    Just sym -> return sym
  let base = "hit breakpoint" <+> parens (ppSymbol sym)
      rest = maybe "" (\stmt -> colon <+> ppStmt stmt) mstmt
  dbugM . render $ base <> rest

enableDebugger :: (Functor m, Monad m, MonadIO m, MonadException m)
               => Simulator sbe m ()
enableDebugger = do
  evHandlers.onBlockEntry .= (runAtEntryBreakpoints $ \sb -> do
    debuggerREPL Nothing)
  evHandlers.onPreStep .= (runAtTransientBreakpoints $ \stmt -> do
    debuggerREPL (Just stmt))

debuggerREPL :: (Functor m, Monad m, MonadIO m, MonadException m)
             => Maybe (SymStmt (SBETerm sbe))
             -> Simulator sbe m ()
debuggerREPL mstmt = do
    logBreakpoint mstmt
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
        doCmd (cmd:args) = do
          case M.lookup cmd commandMap of
            Just cmd -> do
              let go = cmdAction cmd mstmt args
                  handleErr epe@(ErrorPathExc _ _) = throwError epe
                  handleErr (UnknownExc (Just (FailRsn rsn))) = do
                    dbugM $ "error: " ++ rsn
                    return False
                  handleErr _ = do dbugM "unknown error"; return False
              continue <- lift $ catchError go handleErr
              unless continue loop
            Nothing -> do
              outputStrLn $ "unknown command '" ++ cmd ++ "'"
              outputStrLn $ "type 'help' for more info"
              loop
        doCmd [] = error "unreachable"

data Command sbe m = Cmd {
    cmdNames :: [String]
  , cmdArgs :: [String]
  , cmdDesc :: String
  , cmdCompletion :: CompletionFunc m
  , cmdAction :: Maybe (SymStmt (SBETerm sbe)) -> [String] -> m Bool
  }

commandMap :: (Functor m, Monad m, MonadIO m)
           => M.Map String (Command sbe (Simulator sbe m))
commandMap = M.fromList . concatMap expandNames $ cmds
  where expandNames cmd = do
          name <- cmdNames cmd
          return (name, cmd)

cmds :: (Functor m, Monad m, MonadIO m) => [Command sbe (Simulator sbe m)]
cmds = [
    helpCmd
  , whereCmd
  , localsCmd
  , dumpCmd
  , contCmd
  -- , killCmd
  -- , satpathCmd
  -- , exitCmd
  -- , clearCmd
  , stopinCmd
  , clearinCmd
  -- , stopatCmd
  -- , clearatCmd
  -- , stoppcCmd
  -- , clearpcCmd
  -- , stepCmd
  -- , stepupCmd
  -- , stepiCmd
  ]

helpCmd :: forall sbe m . (Functor m, Monad m, MonadIO m)
        => Command sbe (Simulator sbe m)
helpCmd = Cmd {
    cmdNames = ["help", "?"]
  , cmdArgs = []
  , cmdDesc = "show this help"
  , cmdCompletion = noCompletion
  , cmdAction = \_ _ -> do
      dbugM $ helpString (cmds :: [Command sbe (Simulator sbe m)])
      return False
  }

helpString :: [Command sbe m] -> String
helpString cmds = render . vcat $
  [ invs <> colon $$ nest 2 (text $ cmdDesc cmd)
  | cmd <- cmds
  , let invs = hsep . map text $ (cmdNames cmd ++ cmdArgs cmd)
  ]

failHelp :: Monad m => m a
failHelp = fail "invalid arguments; type 'help' for details"

whereCmd :: (Functor m, Monad m, MonadIO m) => Command sbe (Simulator sbe m)
whereCmd = Cmd {
    cmdNames = ["where", "w"]
  , cmdArgs = []
  , cmdDesc = "print call stack"
  , cmdCompletion = noCompletion
  , cmdAction = \_ _ -> do
      mp <- preuse currentPathOfState
      case mp of
        Nothing -> dbugM "no active execution path"
        Just p -> dbugM . render . ppStackTrace . pathStack $ p
      return False
  }

localsCmd :: (Functor m, Monad m, MonadIO m) => Command sbe (Simulator sbe m)
localsCmd = Cmd {
    cmdNames = ["locals"]
  , cmdArgs = []
  , cmdDesc = "print local variables in current stack frame"
  , cmdCompletion = noCompletion
  , cmdAction = \_ _ -> do
      mp <- preuse currentPathOfState
      case mp of
        Nothing -> dbugM "no active execution path"
        Just p -> do
          sbe <- gets symBE
          dbugM . render $ ppRegMap sbe (p^.pathRegs)
      return False
  }

dumpCmd :: (Functor m, Monad m, MonadIO m) => Command sbe (Simulator sbe m)
dumpCmd = let args = ["ctrlstk", "function", "memory"]
          in Cmd {
    cmdNames = ["dump", "d"]
  , cmdArgs = args
  , cmdDesc = "dump an object in the simulator"
  , cmdCompletion = completeWordWithPrev Nothing " " $ \revleft word -> do
      case length . words $ revleft of
        -- only dump one thing at a time
        1 -> return . map simpleCompletion . filter (word `isPrefixOf`) $ args
        _ -> return []
  , cmdAction = \_ args -> do
      case args of
        ["ctrlstk"] -> dumpCtrlStk
        ["function"] -> do
          mp <- preuse currentPathOfState
          case mp of
            Nothing -> fail "no active execution path"
            Just p -> dumpSymDefine (gets codebase) (unSym . pathFuncSym $ p)
        ["memory"] -> dumpMem 6 "memory"
        _ -> dbugM $ "dump: unsupported object " ++ unwords args
      return False
  }

contCmd :: Monad m => Command sbe m
contCmd = Cmd {
    cmdNames = ["cont", "c"]
  , cmdArgs = []
  , cmdDesc = "continue execution"
  , cmdCompletion = noCompletion
  , cmdAction = \_ _ -> return True
  }


stopinCmd :: (Functor m, Monad m, MonadIO m) => Command sbe (Simulator sbe m)
stopinCmd = Cmd {
    cmdNames = ["stopin"]
  , cmdArgs = ["<function_symbol>"]
  , cmdDesc = "set a breakpoint at the entry to a function"
  , cmdCompletion = funcSymCompletion
  , cmdAction = \_ args ->
      case args of
        [arg] -> do
          bps <- entriesForArg arg
          forM_ bps $ uncurryN addBreakpoint
          return False
        _ -> failHelp
  }

clearinCmd :: (Functor m, Monad m, MonadIO m) => Command sbe (Simulator sbe m)
clearinCmd = Cmd {
    cmdNames = ["clearin"]
  , cmdArgs = ["<class id>.<method>[<type_descriptor>]"]
  , cmdDesc = "clear a breakpoint at the entry to a function"
  , cmdCompletion = funcSymCompletion
  , cmdAction = \_ args ->
      case args of
        [arg] -> do
          bps <- entriesForArg arg
          forM_ bps $ uncurryN removeBreakpoint
          return False
        _ -> failHelp
  }

entriesForArg :: Monad m => String -> Simulator sbe m [(Symbol, Breakpoint)]
entriesForArg arg = do
  cb <- gets codebase
  let sym = Symbol arg
  case lookupFunctionType sym cb of
    Nothing -> fail $ "unknown symbol: " ++ arg
    Just _ -> return [(sym, BreakEntry)]


completer :: forall sbe m . (Functor m, Monad m, MonadIO m)
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
        let (cmd:args) = words (reverse revleft)
        case M.lookup cmd m of
          Nothing -> return (revleft, [])
          Just c -> cmdCompletion c (revleft, right)
  where
    m :: M.Map String (Command sbe (Simulator sbe m))
    m = commandMap

-- | Complete a function symbol as the current word
funcSymCompletion :: (Functor m, Monad m, MonadIO m)
                  => CompletionFunc (Simulator sbe m)
funcSymCompletion = completeWordWithPrev Nothing " " fn
  where
    fn _revleft word = do
      cb <- gets codebase
      let syms = M.keys (cb^.cbFunctionTypes)
      return . map simpleCompletion
             . filter (word `isPrefixOf`)
             . map unSym
             $ syms

unSym :: Symbol -> String
unSym (Symbol str) = str

-- NB: Currently only valid for SBEBitBlast mems
sanityChecks ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => SEH sbe m
sanityChecks = SEH
  {
    _onPreStep        = \_ -> return ()
  , onPostStep        = \_ -> return ()
  , _onBlockEntry     = \_ -> return ()
  , onBlockExit       = \_ -> return ()
  , onMkGlobTerm      = \_ -> return ()
  , onPostOverrideReg = return ()

  , onPreGlobInit     = \_ _ -> return ()

{-
  , onPreGlobInit = \g (Typed ty gdata) -> do
      CE.assert (L.globalType g == ty) $ return ()
      sz  <- withLC (`llvmStoreSizeOf` ty)
      szt <- withSBE' $ \sbe -> termWidth sbe gdata
      when (szt `shiftR` 3 /= sz) $ do
        dbugM $ "onPreGlobInit assert failure on " ++ show (L.ppSymbol $ L.globalSym g)
                ++ " (size check)"
        CE.assert False $ return ()
-}

  , onPostGlobInit = \_g _ -> do
      {-
      Just mem       <- getMem
      sz        <- withLC (`llvmStoreSizeOf` ty)
      addrWidth <- withLC llvmAddrWidthBits
      -- Read back and check
      gstart <- withSBE $ \sbe -> termInt sbe addrWidth (bmDataAddr mem - sz)
      (cond, gdata') <- withSBE $ \sbe -> memLoad sbe mem (Typed (L.PtrTo ty) gstart)
      processMemCond cond
      eq <- uval =<< (Typed i1 <$> withSBE (\sbe -> applyICmp sbe L.Ieq gdata gdata'))
      when (eq /= 1) $ do
        dbugM $ "onPostGlobInit assert failure on " ++ show (L.ppSymbol $ L.globalSym g)
                ++ " (read-back) "
        CE.assert False $ return ()
        -}
      return ()
  }
  {-
  where
    uval (typedValue -> v) =
      fromJust <$> withSBE' (\sbe -> snd $ asUnsignedInteger sbe v)
-}
