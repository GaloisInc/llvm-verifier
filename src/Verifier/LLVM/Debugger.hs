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
import Control.Monad.State as MTL
import Control.Lens hiding (createInstance)
import Data.Char
import qualified Data.Foldable as Fold
import Data.IORef
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
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

import Verifier.LLVM.AST
import Verifier.LLVM.Backend
import Verifier.LLVM.Codebase
import Verifier.LLVM.FreeApp
import Verifier.LLVM.Simulator.Internals
import Verifier.LLVM.Simulator.SimUtils

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
import Control.Concurrent (myThreadId)
import System.Posix.Signals
#endif

-- | Break on entry to the function.
breakOnEntry :: (Functor m, Monad m)
             => SymDefine (SBETerm sbe) -> Simulator sbe m ()
breakOnEntry def = addBreakpoint (sdName def) (sdEntry def, 0)

commaSepList :: [Doc] -> Doc
commaSepList l = hcat (punctuate (comma PP.<> char ' ') l)

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
-- List utilities

-- | Split a list into the first elements and the last elemt.
rcons :: [a] -> Maybe ([a], a)
rcons l =
  case reverse l of
    [] -> Nothing
    (e:r) -> Just (reverse r,e)

-- | @nonemptyPrefixes l@ returns non-empty prefixes of @l@.
nonemptyPrefixes ::[a] -> [[a]]
nonemptyPrefixes = tail . inits

-- | @strictNonemptyPrefixes l@ returns non-empty strict prefixes of @l@.
strictNonemptyPrefixes :: [a] -> [[a]]
strictNonemptyPrefixes i =
  case rcons i of
    Nothing -> []
    Just (r,_) -> nonemptyPrefixes r

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
  Just p <- preuse currentPathOfState
  mbps <- use (breakpoints . at (p^.pathFuncSym))
  let atBP = fromMaybe False $ S.member <$> (p^.pathPC) <*> mbps
  when atBP (enterDebuggerAtBreakpoint r)

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
      let pr = runIdentity $ runGrammar (dsGrammar ds) (convertToTokens cmdStr) 
      case resolveParse pr of
        Left d -> do
          outputStrLn (show d)
          setPrevCommand dr $ return ()
          runNextCommand dr
        Right cmd -> do
          setPrevCommand dr cmd
          runDebugger cmd (\() -> runNextCommand) dr
--                       `catch` handleErr
--          where handleErr (FailRsn rsn) = do
--                  dbugM ("error: " ++ rsn)
--                  runWithNoNextCommand ds

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
      => MonadState (DebuggerState sbe m) (Debugger sbe m) where
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
                         , _onNoInput = undefined
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
  ctrlStk ?= ActiveCS cs
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
    dbugM $ show $ indent 2 (text "at" <+> ppPathInfo p)
  grammar <- liftIO $ dsGrammar <$> readIORef r
  historyPath <- liftIO getLSSHistoryPath
  let settings = setComplete (matcherCompletions grammar)
               $ defaultSettings { historyFile = historyPath }
  runInputT settings (runNextCommand r)

{-
debuggerREPL :: (Functor sbe, Functor m, MonadIO m, MonadException m)
             => Simulator sbe m ()
debuggerREPL = initializeDebugger >>= enterDebugger
-}

------------------------------------------------------------------------
-- Tokenization

type Pos = Int

startPos :: Pos
startPos = 0

incPos :: Pos -> Char -> Pos
incPos p _ = p+1

data TokenSeq t
  = SeqCons Pos t (TokenSeq t)
  | SeqLast Pos t Pos
  | SeqEnd Pos

tokenSeqPos :: TokenSeq t -> Pos
tokenSeqPos (SeqCons p _ _) = p
tokenSeqPos (SeqLast p _ _) = p
tokenSeqPos (SeqEnd p) = p

tokenSeqTokens :: Traversal (TokenSeq s) (TokenSeq t) s t
tokenSeqTokens f (SeqCons p t s) = SeqCons p <$> f t <*> tokenSeqTokens f s
tokenSeqTokens f (SeqLast p t pe) = (\u -> SeqLast p u pe) <$> f t
tokenSeqTokens _ (SeqEnd p) = pure (SeqEnd p)

-- | A "type" for topens.  Tokens with the same type need
-- a space between them.
data TokenType
   = WordTokenType
   | OpTokenType
   | BadCharType
  deriving (Eq, Show)

-- | Token from parser.
data Token
   = Keyword String
   | StringLit String
   | NatToken Integer -- | A non-negative integer.
   | OpToken String
   | BadChar String
  deriving (Eq, Ord, Show)

-- | Type of token.
tokenType :: Token -> TokenType
tokenType Keyword{}   = WordTokenType
tokenType StringLit{} = WordTokenType
tokenType NatToken{}  = WordTokenType
tokenType OpToken{}   = OpTokenType
tokenType BadChar{}   = BadCharType

-- | Pretty print a token.
ppToken :: Token -> Doc
ppToken (Keyword k) = text k
ppToken (StringLit s) = dquotes (text s)
ppToken (NatToken i) = integer i
ppToken (OpToken k) = text k
ppToken (BadChar s) = text s

data CharStream = CSNext !Pos !Char !CharStream
                | CSEnd !Pos

csString :: CharStream -> String
csString (CSNext _ c cs) = c : csString cs
csString (CSEnd _) = []

csEndPos :: CharStream -> Pos
csEndPos (CSNext _ _ cs) = csEndPos cs
csEndPos (CSEnd p) = p

charStream :: String -> CharStream
charStream = impl startPos
  where impl p (c:l) = CSNext p c $ impl (incPos p c) l
        impl p [] = CSEnd p

convertToTokens :: String -> TokenSeq Token
convertToTokens = tokenize . charStream
  where tokenize :: CharStream -> TokenSeq Token
        tokenize (CSNext p c r)
          | isSpace c  = tokenize r
          | c == '\"'  = stringLit p [] r
          | isDigit c  = charSeq isDigit isSpace (NatToken . read) p [c] r 
          | isToken1 c = charSeq isToken isSpace Keyword p [c] r
          | isOp c     = charSeq isOp (\_ -> True) OpToken p [c] r
          | otherwise  = badChar p [c] r
        tokenize (CSEnd p) = SeqEnd p
        
        badChar :: Pos -> String -> CharStream -> TokenSeq Token
        badChar p s r = SeqLast p (BadChar (s ++ csString r)) (csEndPos r)

        isToken1 c = isAlpha c || c `elem` ['_', '%']
        isToken  c = isAlphaNum c || c `elem` ['_', '%']

        -- Recognize operator characters.
        isOp = (`elem` ['.', ':'])

        -- @charSeq whilePred endPred ctor pos p cs@ recognizes a word
        -- with characters satisfying whilePred that ends with the end
        -- of the CharStream or a character satisfying endPred.
        charSeq :: (Char -> Bool) -- ^ Character to accumulate.
                -> (Char -> Bool) -- ^ Character for end.
                -> (String -> Token) -- ^ Constructor for making tokens.
                -> Pos -> String -> CharStream -> TokenSeq Token
        charSeq accRec endRec ctor = go
          where go pos p cs@(CSNext _ c r)
                  | accRec c = go pos (c:p) r
                  | not (endRec c) = badChar pos (reverse p) cs
                go pos p r = resolve pos (ctor (reverse p)) r

        -- Recognize string literals
        stringLit :: Pos -> String -> CharStream -> TokenSeq Token
        stringLit pos p r0@(CSNext _ '\\' r) =
          case r of
            CSNext _ '\\' s -> stringLit pos ('\\':p) s
            CSNext _ '\"' s -> stringLit pos ('\"':p) s
            _ -> badChar pos (reverse p) r0
        stringLit pos p (CSNext _ '\"' r) =
          resolve pos (StringLit (reverse p)) r
        stringLit pos p (CSNext _ c r) = stringLit pos (c:p) r
        stringLit pos p (CSEnd p') = badChar pos (reverse p) (CSEnd p')

        resolve p k (CSEnd p') = SeqLast p k p'
        resolve p k r  = SeqCons p k (tokenize r)

------------------------------------------------------------------------
-- Grammar primitives

type Grammar m a = App (Parser m) a

data Parser (m :: * -> *) (a :: *) where
  
  -- | Label an argument to a command.
  ArgumentLabel :: PP.Doc -> Parser m ()
  
  -- | Label the command that this runs.
  CommandLabel :: PP.Doc -> Parser m ()

  -- | Hide matcher from help system.
  Hide :: Grammar m a -> Parser m a

  -- | Switch expression.
  SwitchExpr :: SwitchMap m a -> Parser m a

  UnionGrammar :: Grammar m a -> Grammar m a -> Parser m a


keyword :: SwitchKey -> Grammar m ()
keyword k = switch [(k,pure ())]

argLabel :: PP.Doc -> Grammar m ()
argLabel = atom . ArgumentLabel

cmdLabel :: String -> Grammar m ()
cmdLabel d = atom (CommandLabel (PP.text d))

cmdDef :: String -> a -> Grammar m a
cmdDef d v = cmdLabel d *> pure v

hide :: Grammar m a -> Grammar m a
hide = atom . Hide

(<||>) :: Grammar m a -> Grammar m a -> Grammar m a
x <||> y = atom (UnionGrammar x y)

infixl 3 <||>

data SwitchMatch a
  = SwitchMatch { _switchExact :: Maybe a
                , _switchExtensions :: [(Token, a)]
                }

emptySwitchMatch :: SwitchMatch a
emptySwitchMatch = SwitchMatch Nothing []

switchExact :: Simple Lens (SwitchMatch a) (Maybe a)
switchExact = lens _switchExact (\s v -> s { _switchExact = v })

switchExtensions :: Simple Lens (SwitchMatch a) [(Token, a)]
switchExtensions = lens _switchExtensions (\s v -> s { _switchExtensions = v })

switchMatchGrammars :: Traversal (SwitchMatch a) (SwitchMatch b) a b
switchMatchGrammars f (SwitchMatch e x) =
  SwitchMatch <$> _Just f e <*> traverse (_2 f) x

data SwitchKey = SwitchKey [Token]

instance IsString SwitchKey where
  fromString s = SwitchKey (toListOf tokenSeqTokens tokens)
    where tokens = convertToTokens s

-- | A token predicate recognizes tokens with a user-definable action.
data TokenPred m a where
  -- | Includes what to show when printing, a predicate, anda  continuation.
  TokenPred :: (Maybe PP.Doc)
            -> TokenType
            -> (Token -> Maybe b)
            -> Cont (Parser m) b a
            -> TokenPred m a


tokenPredAddCont :: TokenPred m a
                 -> Cont (Parser m) a c
                 -> TokenPred m c
tokenPredAddCont (TokenPred d t f c) c' =
  TokenPred d t f (c `composeCont` c')

tokenPredType :: Simple Lens (TokenPred m a) TokenType
tokenPredType f (TokenPred d tp p c) =
  (\tp' -> TokenPred d tp' p c) <$> f tp

data SwitchMap m a = SwitchMap { _smWords :: M.Map Token (SwitchMatch (Grammar m a))
                               , _smPreds :: [TokenPred m a]
                               , _smEnd :: Maybe (Grammar m a)
                               }


fromTokenPred :: TokenPred m a -> SwitchMap m a
fromTokenPred p = SwitchMap { _smWords = M.empty
                            , _smPreds = [p]
                            , _smEnd = Nothing
                            }

-- | Map of ground towns to switch matches.
smWords :: Simple Lens (SwitchMap m a) (M.Map Token (SwitchMatch (Grammar m a)))
smWords = lens _smWords (\s v -> s { _smWords = v })

-- | List of token predicates in map.
smPreds :: Simple Lens (SwitchMap m a) [TokenPred m a]
smPreds = lens _smPreds (\s v -> s { _smPreds = v })

smEnd :: Simple Lens (SwitchMap m a) (Maybe (Grammar m a))
smEnd = lens _smEnd (\s v -> s { _smEnd = v })

emptySwitchMap :: SwitchMap m a
emptySwitchMap = SwitchMap { _smWords = M.empty
                           , _smPreds = []
                           , _smEnd = Nothing
                           }

unionMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
unionMaybe u (Just x) y = Just (maybe x (u x) y)
unionMaybe _ Nothing y  = y

unionSwitchMatch :: (a -> a -> a)
                 -> SwitchMatch a -> SwitchMatch a -> SwitchMatch a
unionSwitchMatch u (SwitchMatch ex xx) (SwitchMatch ey xy) =
  SwitchMatch (unionMaybe u ex ey) (xx ++ xy)

switchHelp :: SwitchMap m a -> HelpResult
switchHelp m = unionHelpList $
    itemHelp <$> mapMaybe (_2 (^. switchExact)) matches
  where matches = M.toList $ m^.smWords 
        itemHelp (nm,k) = HelpResult (singleArg (ppToken nm)) [] 
                         `seqHelp` matcherHelp k

matchPred :: TokenPred m a -> Token -> Maybe (App (Parser m) a)
matchPred (TokenPred _ _ f c) t = (`evalCont` c) <$> f t

parseSwitchToken :: (Applicative m, Monad m)
                 => NextState m a
                 -> Pos
                 -> Token
                 -> TokenSeq Token
                 -> m (ParseResult a)
parseSwitchToken m p t ts =
   case tokenMatches ++ predMatches of
     a:_ -> runGrammar a ts
     [] ->
       case mma of
         Nothing -> pure (parseFail p d [])
           where d = text "Unexpected symbol" <+> dquotes (ppToken t)
                       PP.<> expectedOneOf cl
                 cl = nextStateChoices m

         Just ma ->
           case ma^.switchExtensions of
             [] -> error "internal: parsing unexpectedly failed."
             [(t',a)] -> addCompletions p [mkCompletion t' a]
                           <$> runGrammar a ts
             _ -> pure (incompleteParse p msg cl)
               where msg = text "Ambiguous input" <+> dquotes (ppToken t)
                     cl = ma^.switchExtensions

  where mma = nsWords m^.at t
        tokenMatches =
          case mma of
            Nothing -> []
            Just ma -> case ma^.switchExact of
                         Just a -> [a]
                         Nothing -> []
        predMatches = mapMaybe (`matchPred` t) (nsPreds m)

parseSwitch :: (Applicative m, Monad m)
            => NextState m a
            -> TokenSeq Token
            -> m (ParseResult a)
parseSwitch m ts =
  case ts of
    SeqCons p t ts'  -> parseSwitchToken m p t ts'
    SeqLast p t pend -> parseSwitchToken m p t (SeqEnd pend)
    SeqEnd p ->
      case nsMatches m of
        [] -> pure (incompleteParse p msg cl)
          where msg = text "Unexpected end of command"
                cl = nextStateChoices m
        [v] -> pure $ parseValue p v []
        _ -> pure $ parseFail p msg []
          where msg = text "Internal error: Unexpected ambiguous command."

switch :: forall m a . [(SwitchKey, Grammar m a)] -> Grammar m a
switch = atom . SwitchExpr . impl
    where -- Get map from strings to associated match
          impl cases =
            execState (mapM_ (uncurry addMatch) cases)
                      emptySwitchMap
          -- Update switch match at a given identifier.
          alter :: Token
                -> (SwitchMatch (Grammar m a) -> SwitchMatch (Grammar m a))
                -> MTL.State (SwitchMap m a) ()
          alter k f =
            smWords . at k
              %= (\l -> Just (f (fromMaybe emptySwitchMatch l)))

          keywordSeq :: [Token] -> Grammar m a -> Grammar m a
          keywordSeq [] m = m
          keywordSeq (t:r) m =
           switch [(SwitchKey [t], keywordSeq r m)]

          -- Functions for building initial map.
          addMatch :: SwitchKey -> Grammar m a -> MTL.State (SwitchMap m a) ()
          addMatch (SwitchKey (k:r)) v = do
            let v' = keywordSeq r v
            alter k (switchExact ?~ v')
            case k of
              Keyword nm -> mapM_ addPrefix (strictNonemptyPrefixes nm)
                where addPrefix p =
                        alter (Keyword p) (switchExtensions %~ (++[(k,v')]))
              _ -> return ()
          addMatch (SwitchKey []) v = smEnd ?= v

------------------------------------------------------------------------
-- NextState

data NextState m a = NextState { nsWords   :: M.Map Token (SwitchMatch (Grammar m a))
                               , nsPreds   :: [TokenPred m a]
                               , nsMatches :: [a]
                               }

pureNextState :: a -> NextState m a
pureNextState v = NextState { nsWords = M.empty
                            , nsPreds = []
                            , nsMatches = [v]
                            }

unionNextState :: NextState m a -> NextState m a -> NextState m a
unionNextState x y =
  NextState { nsWords   = zw
            , nsPreds   = zp
            , nsMatches = zm
            }
  where xw = nsWords x
        yw = nsWords y
        zw = M.unionWith (unionSwitchMatch (<||>)) xw yw
        zp = nsPreds x   ++ nsPreds y
        zm = nsMatches x ++ nsMatches y

composeNextState :: SwitchMap m a -> Cont (Parser m) a b -> NextState m b
composeNextState m c =
    case m^.smEnd of
      Just a -> ns `unionNextState` ns'
        where ns' = resolveToNextState (a `runCont` c)
      Nothing -> ns
  where w = over switchMatchGrammars (`runCont` c) <$> m^.smWords
        ns = NextState { nsWords = w
                       , nsPreds = (`tokenPredAddCont` c) <$> m^.smPreds
                       , nsMatches = []
                       }


nextStateChoices :: NextState m a -> [(Token, Grammar m a)]
nextStateChoices m = 
  [ (nm, a)
  | (nm,(^.switchExact) -> Just a) <- M.toList (nsWords m)
  ]

nextTokenTypes :: NextState m a -> [TokenType]
nextTokenTypes m = wordTypes ++ predTypes
  where wordTypes = tokenType <$> M.keys (nsWords m)
        predTypes = view tokenPredType <$> nsPreds m

------------------------------------------------------------------------
-- Help system

data ArgsI
   = NoArgs
   | EmptyArgs
   | SingleArg Doc
   | SeqArgs ArgsI ArgsI
   | UnionArgs (Seq ArgsI)
   | OptArgs ArgsI 
  deriving (Show)

emptyArgs :: ArgsI
emptyArgs = EmptyArgs

singleArg :: PP.Doc -> ArgsI
singleArg = SingleArg

seqArgs :: ArgsI -> ArgsI -> ArgsI
seqArgs NoArgs _ = NoArgs
seqArgs _ NoArgs = NoArgs
seqArgs EmptyArgs x = x
seqArgs x EmptyArgs = x
seqArgs x y = SeqArgs x y

-- | Returns true if empty string may be argument.
allowEmpty :: ArgsI -> Bool
allowEmpty EmptyArgs = True
allowEmpty OptArgs{} = True
allowEmpty _ = False

-- | Returns sequence of arguments that may appear.
asUnion :: ArgsI -> Seq ArgsI
asUnion EmptyArgs = Seq.empty
asUnion (OptArgs x) = asUnion x
asUnion (UnionArgs x) = x
asUnion x = Seq.singleton x

unionArgs :: ArgsI -> ArgsI -> ArgsI
unionArgs NoArgs y = y
unionArgs x NoArgs = x
unionArgs x y | Seq.null z = EmptyArgs
              | Seq.length z == 1 = OptArgs (z `Seq.index` 0)
              | allowEmpty x || allowEmpty y  = OptArgs (UnionArgs z)
              | otherwise = UnionArgs z
  where z = asUnion x Seq.>< asUnion y

ppArgsI :: ArgsI -> Doc
ppArgsI NoArgs = PP.empty
ppArgsI EmptyArgs = PP.empty
ppArgsI (SingleArg d) = d
ppArgsI (SeqArgs x y) = ppArgsI x PP.<+> ppArgsI y
ppArgsI (UnionArgs s) = 
  PP.encloseSep PP.lparen PP.rparen (PP.char '|') (Fold.toList (ppArgsI <$> s))
ppArgsI (OptArgs x) = ppArgsI x PP.<> PP.char '?'

-- | Help for a specific command.
data CmdHelp = CmdHelp { _cmdHelpArgs :: ArgsI
                       , cmdHelpDesc :: PP.Doc
                       }
  deriving (Show)

cmdHelpArgs :: Simple Lens CmdHelp ArgsI
cmdHelpArgs = lens _cmdHelpArgs (\s v -> s { _cmdHelpArgs = v })

ppCmdHelp :: CmdHelp -> PP.Doc
ppCmdHelp cmd = PP.indent 2 $
  ppArgsI (cmd^.cmdHelpArgs) PP.<+> PP.char '-' PP.<+> cmdHelpDesc cmd


-- | @HelpResult args cmds@ contains a doc with the arguments
-- and a list of commands if any.
data HelpResult = HelpResult { _helpArgs :: ArgsI
                             , _helpCmds :: [CmdHelp]
                             }
  deriving (Show)

helpCmds :: Simple Lens HelpResult [CmdHelp]
helpCmds = lens _helpCmds (\s v -> s { _helpCmds = v })

emptyHelp :: HelpResult
emptyHelp = HelpResult emptyArgs []

noHelp :: HelpResult
noHelp = HelpResult NoArgs []

seqHelp :: HelpResult -> HelpResult -> HelpResult
seqHelp (HelpResult xa xc) (HelpResult ya yc) =
    HelpResult (seqArgs xa ya) (xc ++ over cmdHelpArgs (seqArgs xa) `fmap` yc)

unionHelp :: HelpResult -> HelpResult -> HelpResult
unionHelp (HelpResult xa xc) (HelpResult ya yc) =
  HelpResult (xa `unionArgs` ya) (xc ++ yc)

unionHelpList :: [HelpResult] -> HelpResult
unionHelpList = foldr unionHelp noHelp

-- | Prints a list of commands for help purposes
matcherHelp :: Grammar m a -> HelpResult
matcherHelp (PureApp _) = emptyHelp
matcherHelp (NP u v) = parserHelp u `seqHelp` matcherHelp (contToApp v)

parserHelp :: forall m a .Parser m a -> HelpResult
parserHelp (ArgumentLabel d) = HelpResult (singleArg d) []
parserHelp (CommandLabel d) = HelpResult emptyArgs [CmdHelp emptyArgs d]
parserHelp (SwitchExpr m) = switchHelp m
parserHelp (Hide _) = emptyHelp
parserHelp (UnionGrammar x y) = matcherHelp x `unionHelp` matcherHelp y

------------------------------------------------------------------------
-- Parsing

-- | A ParseResult consists of a 
data ParseResult a
  = ParseResult (Either Doc a) (Pos,[Completion])

resolveParse :: ParseResult r -> Either Doc r
resolveParse (ParseResult v _) = v

parseCompletions :: Simple Lens (ParseResult r) (Pos,[Completion])
parseCompletions f (ParseResult v cl) = ParseResult v <$> f cl

parseValue :: Pos -> r -> [Completion] -> ParseResult r
parseValue p v cl = ParseResult (Right v) (p, cl)

parseFail :: Pos -> Doc -> [Completion] -> ParseResult r
parseFail p d cl = ParseResult (Left d) (p,cl)

addCompletions :: Pos -> [Completion] -> ParseResult r -> ParseResult r
addCompletions p' cl = parseCompletions .~ (p',cl)

-- | Create a completion token from the target word and the
-- following grammar.
mkCompletion :: Token -- ^ Target word
             -> Grammar m a -- ^ Followup matcher
             -> Completion
mkCompletion tgt g =
  Completion { replacement = show (ppToken tgt)
             , display = show (ppToken tgt)
             , isFinished = spaceAfterToken tgt (resolveToNextState g)
             }

-- | @spaceAfterToken t m@ holds if there may need to be a space between
-- @t@ and the next token to be parsed.
spaceAfterToken :: Token -> NextState m a -> Bool
spaceAfterToken t m = any tokenCompat (nextTokenTypes m)
  where tokenCompat tp = tokenType t == tp

expectedOneOf :: [(Token,a)] -> Doc
expectedOneOf [] = text "."
expectedOneOf [(x,_)] = text "; Expected:" <+> ppToken x
expectedOneOf l = text "; Expected one of:"
                  <+> commaSepList (ppToken . fst <$> l)

-- | Parse result when the user's command is incomplete.
-- Arguments include an explanation, prefix if the user
-- has entered a token, and a list of completions.
incompleteParse :: Pos -> Doc -> [(Token,Grammar m a)] -> ParseResult b
incompleteParse p msg cl = parseFail p d completions
  where d = msg PP.<> expectedOneOf cl
        completions = uncurry mkCompletion <$> cl

runGrammar :: (Applicative m, Monad m)
           => Grammar m a
           -> TokenSeq Token
           -> m (ParseResult a)
runGrammar (PureApp a) ts = 
  pure (parseValue (tokenSeqPos ts) a [])
runGrammar m ts = 
  parseSwitch (resolveToNextState m) ts

resolveToNextState :: Grammar m a -> NextState m a
resolveToNextState (PureApp v) = pureNextState v
resolveToNextState (NP u c) =
  case u of
    ArgumentLabel{} -> resolveToNextState (evalCont () c)
    CommandLabel{} -> resolveToNextState (evalCont () c)
    SwitchExpr m -> composeNextState m c
    Hide m -> resolveToNextState (runCont m c)
    UnionGrammar x y -> resolveToNextState (runCont x c) 
       `unionNextState` resolveToNextState (runCont y c)

------------------------------------------------------------------------
-- Completions

matcherCompletions :: (Functor m, Monad m)
                   => Grammar Identity a
                   -> (String, String)
                   -> Simulator sbe m (String, [Completion])
matcherCompletions m (l,_r) = pure (finalize a)
  where rl = reverse l        
        a = runIdentity $ runGrammar m (convertToTokens rl)
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
    Just (ActiveCS cs) | rte -> do
      dbugM "Resuming execution on this path should rethrow the simulation error."
      if length (cs^..activePaths) == 1 then do
        dbugM "Should lss kill the current path, and stop simulation?"
        ync <- promptYesNoCancel
        case ync of
          Just Yes -> do
            killPathByDebugger cs
            resume
          Just No -> return True
          Nothing -> return False
      else do
        dbugM "Should lss kill the current path, and resume on a new path?"
        ync <- promptYesNoCancel
        case ync of
          Just Yes -> do
            killPathByDebugger cs
            return True
          Just No -> return True
          Nothing -> return False
    Just ActiveCS{} -> return True
    _ -> return False

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

nat :: Grammar m Integer
nat = atom (SwitchExpr (fromTokenPred p))
  where p = TokenPred (Just (text "<nat>")) WordTokenType f idCont
        f (NatToken i) = Just i
        f _ = Nothing

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
      where paths = case cs of
                      FinishedCS p -> [p]
                      ActiveCS acs -> acs^..activePaths
            ppPathItem :: Integer -> Path sbe -> [String]
            ppPathItem i p = [ padRightMin 3 (show i), show (ppPathInfo p)]              
            header = [ " Num", "Location"]
            table = header : zipWith ppPathItem [1..] paths

pathSatCmd :: SimGrammar sbe m
pathSatCmd =
  cmdDef "Check satisfiability of path assertions with SAT solver." $ do
    withActiveCS () $ \cs -> do
      sat <- runSim $ do
        sbe <- gets symBE
        cond <- assumptionsForActivePath cs
        liftSBE $ termSAT sbe cond
      case sat of
        UnSat -> do
          dbugM "The current path is infeasible.  Should simulation of the path be terminated?"
          yn <- promptYesNo
          when (yn == Yes) $ do
            killPathByDebugger cs
        Sat _ ->
          dbugM "Conditions along path are satisfiable."
        Unknown ->
          dbugM "Could not determine if path is feasible."

-- | Kills the current path with the debugger.
-- Assumes that there is an active path.
killPathByDebugger :: (Functor sbe, Functor m, MonadException m)
                   => ActiveCS sbe
                   -> Debugger sbe m ()
killPathByDebugger _ = do
  runSim $ killCurrentPath (FailRsn "Terminated by debugger.")
  resumeThrowsError .= False


pathKillCmd :: SimGrammar sbe m
pathKillCmd = cmdDef "Kill the current execution path." $ do
  withActiveCS () $ \cs -> do
    killPathByDebugger cs
    mp <- runSim $ preuse currentPathOfState
    case mp of
      Nothing -> dbugM "Killed last path."
      Just p -> dbugM $ show $
        text "Switched to path:" <+> ppPathInfo p

opt :: Grammar m a -> Grammar m (Maybe a)
opt m = (Just <$> m) <||> pure Nothing 

quitCmd :: SimGrammar sbe m
quitCmd = cmdDef "Exit LSS." $ do
  liftIO $ exitWith ExitSuccess

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

equalSep :: Doc -> Doc -> Doc
equalSep x y = x <+> char '=' <+> y

infoCmd :: SimGrammar sbe m
infoCmd
  = keyword "block" *> infoBlockCmd
  <||> keyword "breakpoints" *> infoBreakpointsCmd
  <||> keyword "ctrlstk"  *> infoCtrlStkCmd
  <||> keyword "function" *> infoFunctionCmd
  <||> keyword "locals" *> infoLocalsCmd
  <||> keyword "memory" *> infoMemoryCmd
  <||> keyword "stack"  *> infoStackCmd

infoBlockCmd :: SimGrammar sbe m
infoBlockCmd =
  cmdDef "Print block information." $ do
    runSim $ withActivePath () $ \p -> do
      let sym = p^.pathFuncSym
          Just (pcb,_) = p^.pathPC
      Just def <- lookupDefine sym <$> gets codebase
      dbugM $ show $ ppSymBlock $ lookupSymBlock def pcb

infoBreakpointsCmd :: SimGrammar sbe m
infoBreakpointsCmd =
  cmdDef "List breakpoints." $ do
    runSim dumpBPs

infoCtrlStkCmd :: SimGrammar sbe m
infoCtrlStkCmd =
  cmdDef "Print the entire control stack." $ do
    runSim dumpCtrlStk

infoFunctionCmd :: SimGrammar sbe m
infoFunctionCmd =
  cmdDef "Print function information." $ do
    runSim $ withActivePath () $ \p -> do
      dumpSymDefine (gets codebase) (unSym (p^.pathFuncSym))

infoLocalsCmd :: SimGrammar sbe m
infoLocalsCmd =
  cmdDef "Print local variables in current stack frame." $ do
    runSim $ withActivePath () $ \p -> do
      sbe <- gets symBE
      let locals = M.toList (p^.pathRegs)
          ppLocal (nm, (v,_)) =
            ppIdent nm `equalSep` prettyTermD sbe v           
      dbugM $ show $ vcat $ ppLocal <$> locals

infoMemoryCmd :: SimGrammar sbe m
infoMemoryCmd =
  cmdDef "Print memory information." $ do
    runSim $ dumpMem 0 "memory"

--TODO: Revisit this command to check formatting.
infoStackCmd :: SimGrammar sbe m
infoStackCmd = cmdDef "Print backtrace of the stack." $ do
  runSim $ withActivePath () $ \p -> do
    let safeInit [] = []
        safeInit l = init l
    let syms = p^.pathFuncSym : safeInit (cfFuncSym <$> (p^.pathStack))
    dbugM $ show $ indent 2 $ vcat $ ppSymbol <$> syms

padRightMin :: Int -> String -> String
padRightMin l s | length s < l = replicate (l - length s) ' ' ++ s
                | otherwise = s

printTable :: [[String]] -> Doc
printTable m = vcat padded
  where maxl (i:x) (j:y) = max i j : maxl x y
        maxl [] y = y
        maxl x [] = x
        -- Maximum lengths of each column.          
        ll = fmap length <$> m
        maxLengths = foldr maxl (repeat 0) ll
        pad l s = text (s ++ replicate (l - length s) ' ')
        padded = hsep . zipWith pad maxLengths <$> m

ppBreakpoint :: Breakpoint -> Doc
ppBreakpoint (sbid,0) = ppSymBlockID sbid 
ppBreakpoint (sbid,i) = ppSymBlockID sbid PP.<> char ':' PP.<> int i

dumpBPs :: (Functor m, MonadIO m) => Simulator sbe m ()
dumpBPs = do
  bps <- concatBreakpoints <$> use breakpoints
  let ppRow :: Integer -> (Symbol,Breakpoint) -> [String]
      ppRow i (Symbol sym,bp) =
        [show i, sym ++ ":" ++ show (ppBreakpoint bp)]
  let msg = case bps of
              [] -> text "No breakpoints."
              _ -> printTable (header : zipWith ppRow [1..] bps)
                where header = ["Num", "Location"]
  dbugM (show msg)

withActiveCS :: (Functor sbe, Functor m, MonadException m)
             => a
             -> (ActiveCS sbe -> Debugger sbe m a)
             -> Debugger sbe m a
withActiveCS v action = do
  mcs <- runSim $ use ctrlStk
  case mcs of
    Just (ActiveCS cs) -> action cs
    _ -> dbugM "No active execution path." >> return v

-- | @withActivePath v act@ runs @act p@ with the active path
withActivePath :: MonadIO m
               => a
               -> (Path sbe -> Simulator sbe m a)
               -> Simulator sbe m a
withActivePath v action = do
  mcs <- use ctrlStk
  case mcs of
    Just (ActiveCS cs) -> action (cs^.activePath)
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
  if p^.pathStackHt < ht then
    enterDebuggerAtBreakpoint dr
  else
    checkForBreakpoint dr

finishCmd :: SimGrammar sbe m
finishCmd = cmdDef desc $ do
    dr <- getDebuggerRef
    resumeActivePath $ \p -> do
      onPathPosChange .= onReturnFrom dr (p^.pathStackHt)
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

unSym :: Symbol -> String
unSym (Symbol str) = str