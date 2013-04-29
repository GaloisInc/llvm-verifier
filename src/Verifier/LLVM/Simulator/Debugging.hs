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
import Control.Monad.State.Class
import Control.Lens hiding (createInstance)
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S
import Data.String
import Data.Tuple.Curry
import System.Console.Haskeline
import System.Console.Haskeline.History
import System.Exit
import Text.PrettyPrint.Leijen hiding ((<$>))


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
    logBreakpoint (Just sb) Nothing)
  evHandlers.onPreStep .= (runAtTransientBreakpoints $ \stmt -> do
    logBreakpoint Nothing (Just stmt))

logBreakpoint :: (Functor m, Monad m, MonadIO m)
              => Maybe (SymBlock (SBETerm sbe))
              -> Maybe (SymStmt (SBETerm sbe))
              -> Simulator sbe m ()
logBreakpoint msb mstmt = do
  mp <- preuse currentPathOfState
  sym <- case pathFuncSym <$> mp of
    Nothing -> fail "no current function symbol"
    Just sym -> return sym
  let psym = ppSymbol sym
      pblock = maybe (text "") (\sb -> ppSymBlockID . sbId $ sb) msb
      pstmt = maybe (text "") (\stmt -> colon <+> ppStmt stmt) mstmt
  dbugM . show $ text "at" <+> (psym <> pblock <> pstmt) <> colon

enableDebugger :: (Functor sbe, Functor m, Monad m, MonadIO m, MonadException m)
               => Simulator sbe m ()
enableDebugger = do
  evHandlers.onBlockEntry .= (runAtEntryBreakpoints $ \sb -> do
    debuggerREPL (Just sb) Nothing)
  evHandlers.onPreStep .= (runAtTransientBreakpoints $ \stmt -> do
    debuggerREPL Nothing (Just stmt))

debuggerREPL :: (Functor sbe, Functor m, Monad m, MonadIO m, MonadException m)
             => Maybe (SymBlock (SBETerm sbe))
             -> Maybe (SymStmt (SBETerm sbe))
             -> Simulator sbe m ()
debuggerREPL msb mstmt = do
    logBreakpoint msb mstmt
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
              let go = cmdAction cmd mstmt args
                  handleErr epe@(ErrorPathExc _ _) = throwError epe
                  handleErr (UnknownExc (Just (FailRsn rsn))) = do
                    dbugM $ "error: " ++ rsn
                    return False
                  handleErr _ = do dbugM "unknown error"; return False
              continue <- lift $ catchError go handleErr
              unless continue loop
            Nothing -> do
              outputStrLn $ "unknown command '" ++ cmdStr ++ "'"
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

commandMap :: (Functor sbe, Functor m, Monad m, MonadIO m)
           => M.Map String (Command sbe (Simulator sbe m))
commandMap = M.fromList . concatMap expandNames $ cmds
  where expandNames cmd = do
          name <- cmdNames cmd
          return (name, cmd)

cmds :: (Functor sbe, Functor m, Monad m, MonadIO m)
     => [Command sbe (Simulator sbe m)]
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
helpString cs = show . vcat $
  [ invs <> colon <$$> nest 2 (text $ cmdDesc cmd)
  | cmd <- cs
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
        Just p -> dbugM . show . ppStackTrace . pathStack $ p
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
          dbugM . show $ ppRegMap sbe (p^.pathRegs)
      return False
  }

dumpCmd :: (Functor m, Monad m, MonadIO m) => Command sbe (Simulator sbe m)
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
  , cmdAction = \_ args -> do
      case args of
        ["ctrlstk"] -> dumpCtrlStk
        ["block"] -> do
          mp <- preuse currentPathOfState
          case mp of
            Nothing -> fail "no active execution path"
            Just p -> do
              let sym = pathFuncSym p
                  Just pcb = pathCB p
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

contCmd :: Monad m => Command sbe m
contCmd = Cmd {
    cmdNames = ["cont", "c"]
  , cmdArgs = []
  , cmdDesc = "continue execution"
  , cmdCompletion = noCompletion
  , cmdAction = \_ _ -> return True
  }

killCmd :: (Functor sbe, Functor m, MonadIO m) => Command sbe (Simulator sbe m)
killCmd = Cmd {
    cmdNames = ["kill"]
  , cmdArgs = ["[<msg>]"]
  , cmdDesc = "kill the current execution path"
  , cmdCompletion = noCompletion
  , cmdAction = \_ -> errorPath . unwords
  }

exitCmd :: MonadIO m => Command sbe m
exitCmd = Cmd {
    cmdNames = ["exit", "quit", "q"]
  , cmdArgs = []
  , cmdDesc = "exit LSS"
  , cmdCompletion = noCompletion
  , cmdAction = \_ _ -> liftIO $ exitWith ExitSuccess
  }

satpathCmd :: (Functor sbe, Functor m, MonadIO m)
           => Command sbe (Simulator sbe m)
satpathCmd = Cmd {
    cmdNames = ["satpath", "sat"]
  , cmdArgs = []
  , cmdDesc = "check whether the current path's assertions are satisfiable, killing this path if they are not"
  , cmdCompletion = noCompletion
  , cmdAction = \_ _ -> do
      (Just p) <- preuse currentPathOfState
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
        => Command sbe (Simulator sbe m)
stopCmd = Cmd {
    cmdNames = ["stop"]
  , cmdArgs = ["<function_symbol>[%block.id]"]
  , cmdDesc = "set a breakpoint at a function; if no block is specified, break at the entry to the function"
  , cmdCompletion = funcSymCompletion
  , cmdAction = \_ args ->
      case args of
        [arg] -> do
          bps <- bpsForArg arg
          forM_ bps $ uncurryN addBreakpoint
          return False
        _ -> failHelp
  }

clearCmd :: (Functor sbe, Functor m, Monad m, MonadIO m)
         => Command sbe (Simulator sbe m)
clearCmd = Cmd {
    cmdNames = ["clear"]
  , cmdArgs = ["<function_symbol>[%block.id]"]
  , cmdDesc = "clear a breakpoint at a function"
  , cmdCompletion = funcSymCompletion
  , cmdAction = \_ args ->
      case args of
        [arg] -> do
          bps <- bpsForArg arg
          forM_ bps $ uncurryN removeBreakpoint
          return False
        _ -> failHelp
  }

bpsForArg :: (Functor sbe, Functor m, Monad m, MonadIO m)
          => String
          -> Simulator sbe m [(Symbol, Breakpoint)]
bpsForArg arg = do
  let (sym, bbid) = break (== '%') arg
  def <- lookupSymbolDef (Symbol sym)
  let failbbid = fail $ "unexpected basic block identifier: " ++ bbid
  (name, n) <- case splitOn "." . reverse . drop 1 $ bbid of
                 [] -> fail bbid
                 (nstr:rest) -> do
                   n <- maybe failbbid return (readMaybe nstr)
                   return (reverse . intercalate "." $ rest, n)
  let sbid = symBlockID (fromString name) n
  case M.lookup sbid (sdBody def) of
    Nothing -> fail $ "unknown basic block: " ++ arg
    Just _ -> return [(Symbol sym, BreakBBEntry sbid)]

infoCmd :: MonadIO m => Command sbe (Simulator sbe m)
infoCmd = Cmd {
    cmdNames = ["info"]
  , cmdArgs = []
  , cmdDesc = "list breakpoints"
  , cmdCompletion = noCompletion
  , cmdAction = \_ _ -> dumpBPs >> return False
  }

dumpBPs :: MonadIO m => Simulator sbe m ()
dumpBPs = do
  bps <- use breakpoints
  if all S.null (M.elems bps)
     then dbugM "no breakpoints set"
     else dbugM . show . ppBreakpoints $ bps


stepupCmd :: (Functor m, Monad m) => Command sbe (Simulator sbe m)
stepupCmd = Cmd {
    cmdNames = ["stepup"]
  , cmdArgs = []
  , cmdDesc = "execute until the current function returns to its caller (may not happen for tail calls)"
  , cmdCompletion = noCompletion
  , cmdAction = \_ _ -> do
      mp <- preuse currentPathOfState
      case mp of
        Nothing -> fail "no active execution path"
        Just p ->
          trBreakpoints %= S.insert (BreakReturnFrom (pathFuncSym p))
      return True
  }

stepiCmd :: (Functor m, Monad m) => Command sbe (Simulator sbe m)
stepiCmd = Cmd {
    cmdNames = ["stepi", "si"]
  , cmdArgs = []
  , cmdDesc = "execute current symbolic statement"
  , cmdCompletion = noCompletion
  , cmdAction = \_ _ -> do
      trBreakpoints %= S.insert BreakNextStmt
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
    m :: Functor sbe => M.Map String (Command sbe (Simulator sbe m))
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
