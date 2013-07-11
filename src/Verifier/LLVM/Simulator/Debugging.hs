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
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

module Verifier.LLVM.Simulator.Debugging (
    breakOnMain
  , debuggerREPL
  , resetInterrupt
  ) where

import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State as MTL
import Control.Lens hiding (createInstance)
import Data.Char
import qualified Data.Foldable as Fold
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.String
import qualified Data.Vector as V
import System.Console.Haskeline
import System.Directory
import System.Exit
import System.FilePath
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

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__) && !defined(BAD)
import Control.Concurrent (myThreadId)
import System.Posix.Signals
#endif



commaSepList :: [Doc] -> Doc
commaSepList l = hcat (punctuate (comma PP.<> char ' ') l)

safeGetAppUserDataDirectory :: String -> IO (Maybe FilePath)
safeGetAppUserDataDirectory nm = 
    catch (Just <$> getAppUserDataDirectory nm)
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

#if defined(mingw32_HOST_OS) || defined(__MINGW32__) || defined(BAD)
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

debuggerREPL :: forall sbe m 
              . (Functor sbe, Functor m, MonadIO m, MonadException m)
             => Simulator sbe m ()
debuggerREPL = do
    trBreakpoints .= NoTransientBreakpoint
    logBreakpoint
    cb <- gets codebase
    let cmds = allCmds cb
    historyPath <- liftIO getLSSHistoryPath
    let settings = setComplete (matcherCompletions cmds) $
                     defaultSettings { historyFile = historyPath }
    runInputT settings (loop cmds (return False))
  where loop :: SimCmd sbe m
             -> Simulator sbe m Bool
             -> InputT (Simulator sbe m) ()
        loop cmds mprev = do
          mline <- getInputLine "(lss) "
          case dropWhile isSpace <$> mline of
            Nothing -> return ()
            Just "" -> do
              -- repeat last command if nothing entered
              continue <- lift mprev
              unless continue (loop cmds mprev)
            Just cmdStr -> do
              cb <- lift $ gets codebase
              case selectCommand cb cmds cmdStr of
                Left emsg -> do
                  outputStrLn emsg
                  loop cmds (return False)
                Right cmd -> do
                  let handleErr (FailRsn rsn) = do
                        dbugM $ "error: " ++ rsn
                        return False
                  continue <- lift $ catchError cmd handleErr
                  unless continue (loop cmds cmd)

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

data Token
  = Keyword String
  | OpToken String
  | StringLit String
  | BadChar String
  deriving (Eq, Ord, Show)

-- | Pretty print a token.
ppToken :: Token -> Doc
ppToken (Keyword k) = text k
ppToken (OpToken k) = text k
ppToken (StringLit s) = dquotes (text s)
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
          | isToken1 c = charSeq isToken Keyword p [c] r
          | isOp c     = charSeq isOp OpToken p [c] r
          | otherwise  = SeqLast p (BadChar (c:csString r)) (csEndPos r)
        tokenize (CSEnd p) = SeqEnd p
        
        isToken1 c = isAlphaNum c || c `elem` ['_', '%']
        isToken  c = isAlphaNum c || c `elem` ['_', '%']

        -- Recognize operator characters.
        isOp = (`elem` ['.', ':'])

        -- @charSeq rec pos p cs@ recognizes a word with characters
        -- satisfying rec.
        charSeq :: (Char -> Bool)
                -> (String -> Token)
                -> Pos -> String -> CharStream -> TokenSeq Token
        charSeq rec tkn pos p (CSNext _ c r) | rec c = charSeq rec tkn pos (c:p) r
        charSeq _ tkn pos p r = resolve pos (tkn (reverse p)) r

        -- Recognize string literals
        stringLit :: Pos -> String -> CharStream -> TokenSeq Token
        stringLit pos p r0@(CSNext _ '\\' r) =
          case r of
            CSNext _ '\\' s -> stringLit pos ('\\':p) s
            CSNext _ '\"' s -> stringLit pos ('\"':p) s
            _ -> SeqLast pos (BadChar (reverse p ++ csString r0)) (csEndPos r)
        stringLit pos p (CSNext _ '\"' r) =
          resolve pos (StringLit (reverse p)) r
        stringLit pos p (CSNext _ c r) = stringLit pos (c:p) r
        stringLit pos p (CSEnd p') = SeqLast pos (BadChar (reverse p)) p'


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
  Switch :: SwitchMap m a -> Parser m a

  UnionGrammar :: Grammar m a -> Grammar m a -> Parser m a


keyword :: SwitchKey -> Grammar m ()
keyword k = switch [(k,pure ())]

argLabel :: PP.Doc -> Grammar m ()
argLabel = atom . ArgumentLabel

cmdLabel :: String -> Grammar m ()
cmdLabel d = atom (CommandLabel (PP.text d))

cmdDef :: String -> a -> Grammar m a
cmdDef d a = cmdLabel d *> pure a

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

data SwitchMap m a = SwitchMap { _smWords :: M.Map Token (SwitchMatch (Grammar m a))
                               , _smEnd :: Maybe (Grammar m a)
                               }

smWords :: Simple Lens (SwitchMap m a) (M.Map Token (SwitchMatch (Grammar m a)))
smWords = lens _smWords (\s v -> s { _smWords = v })

smEnd :: Simple Lens (SwitchMap m a) (Maybe (Grammar m a))
smEnd = lens _smEnd (\s v -> s { _smEnd = v })

smGrammars :: Traversal (SwitchMap m a)
                        (SwitchMap n b)
                        (Grammar m a)
                        (Grammar n b)
smGrammars f (SwitchMap w e) =
  SwitchMap <$> traverse (switchMatchGrammars f) w <*> traverse f e

smNextTokens :: SwitchMap m a -> [Token]
smNextTokens = M.keys . (^.smWords)

emptySwitchMap :: SwitchMap m a
emptySwitchMap = SwitchMap { _smWords = M.empty, _smEnd = Nothing }

unionSwitchMap :: SwitchMap m a -> SwitchMap m a -> SwitchMap m a
unionSwitchMap (SwitchMap nx ex) (SwitchMap ny ey) =
  SwitchMap (M.unionWith (unionSwitchMatch (<||>)) nx ny)
            (unionMaybe (<||>) ex ey)

unionMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
unionMaybe u (Just x) y = Just (maybe x (u x) y)
unionMaybe _ Nothing y  = y

unionSwitchMatch :: (a -> a -> a)
                 -> SwitchMatch a -> SwitchMatch a -> SwitchMatch a
unionSwitchMatch u (SwitchMatch ex xx) (SwitchMatch ey xy) =
  SwitchMatch (unionMaybe u ex ey) (xx ++ xy)

pureSwitchMap :: a -> SwitchMap m a
pureSwitchMap v = SwitchMap { _smWords = M.empty, _smEnd = Just (pure v) }

composeSwitchMap :: SwitchMap m a -> Cont (Parser m) a b -> SwitchMap m b
composeSwitchMap m c = m & smGrammars %~ (`runCont` c)

switchMapChoices :: SwitchMap m a -> [(Token, Grammar m a)]
switchMapChoices m = 
  [ (nm, a)
  | (nm,(^.switchExact) -> Just a) <- M.toList (m^.smWords)
  ]

switchHelp :: SwitchMap m a -> HelpResult
switchHelp m = unionHelpList $
    itemHelp <$> mapMaybe (_2 (^. switchExact)) matches
  where matches = M.toList $ m^.smWords 
        itemHelp (nm,k) = HelpResult (singleArg (ppToken nm)) [] 
                         `seqHelp` matcherHelp k

parseSwitchToken :: (Applicative m, Monad m)
                 => SwitchMap m a
                 -> Pos
                 -> Token
                 -> TokenSeq Token
                 -> m (ParseResult a)
parseSwitchToken m p t ts
  | Just ma <- m^.smWords^.at t =
    case ma^.switchExact of
      Just a -> runGrammar a ts
      Nothing -> do
        case ma^.switchExtensions of
          [] -> error "internal error: Unexpected empty match"
          [(nm,a)] -> addCompletions p [mkCompletion nm a]
                      <$> runGrammar a ts
          _ -> pure (incompleteParse p msg cl)
            where msg = text "Ambiguous input" <+> dquotes (ppToken t)
                  cl = ma^.switchExtensions
parseSwitchToken m p t _ = pure (parseFail p d [])
  where d = text "Unexpected symbol" <+> dquotes (ppToken t)
            PP.<> expectedOneOf cl
        cl = switchMapChoices m

parseSwitch :: (Applicative m, Monad m)
            => SwitchMap m a
            -> TokenSeq Token
            -> m (ParseResult a)
parseSwitch m ts =
  case ts of
    SeqCons p t ts'  -> parseSwitchToken m p t ts'
    SeqLast p t pend -> parseSwitchToken m p t (SeqEnd pend)
    SeqEnd p ->
      case m^.smEnd of
        Just ma -> runGrammar ma ts
        Nothing -> pure (incompleteParse p msg cl)
          where msg = text "Unexpected end of command"
                cl = switchMapChoices m

switch :: forall m a . [(SwitchKey, Grammar m a)] -> Grammar m a
switch = atom . Switch . impl
    where -- Get map from strings to associated match
          impl cases =
            execState (traverseOf_ folded (uncurry addMatch) cases)
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
parserHelp (Switch m) = switchHelp m
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

mkCompletion :: Token -- ^ Target word
             -> Grammar m a -- ^ Followup matcher
             -> Completion
mkCompletion tgt a =
  Completion { replacement = show (ppToken tgt)
             , display = show (ppToken tgt)
             , isFinished = shouldTerm tgt (resolveToSwitchMap a)
             }

-- | @shouldTerm returns true if switchMap contains a token with 
shouldTerm :: Token -> SwitchMap m a -> Bool
shouldTerm t m = any (tokenCompat t) (smNextTokens m)
  where tokenCompat Keyword{} Keyword{} = True
        tokenCompat OpToken{} OpToken{} = True
        tokenCompat StringLit{} _ = True
        tokenCompat _ _ = False

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
  parseSwitch (resolveToSwitchMap m) ts

resolveToSwitchMap :: Grammar m a -> SwitchMap m a
resolveToSwitchMap (PureApp v) = pureSwitchMap v
resolveToSwitchMap (NP u c) =
  case u of
    ArgumentLabel{} -> resolveToSwitchMap (evalCont () c)
    CommandLabel{} -> resolveToSwitchMap (evalCont () c)
    Switch m -> composeSwitchMap m c
    Hide m -> resolveToSwitchMap (runCont m c)
    UnionGrammar x y -> resolveToSwitchMap (runCont x c) 
       `unionSwitchMap` resolveToSwitchMap (runCont y c)

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

type Command m = Grammar Identity (m Bool)

selectCommand :: MonadIO m
              => Codebase sbe
              -> Command m
              -> String
              -> Either String (m Bool)
selectCommand _cb l s =
    case resolveParse pr of
      Left d -> Left $ show d
      Right v -> Right $ v
  where pr = runIdentity (runGrammar l (convertToTokens s)) 

commandHelp :: Command m -> Doc
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

locSeq :: forall sbe m . Codebase sbe -> Grammar m (Symbol, Breakpoint)
locSeq cb = argLabel (text "<loc>") *> hide (switch $ fmap matchDef (cbDefs cb))
  where matchDef :: SymDefine (SBETerm sbe)
                 -> (SwitchKey, Grammar m (Symbol, Breakpoint))
        matchDef d = ( fromString nm
                     , switch [ (,) ":" (nameBlocks d m)
                              , (,) "" $ end
                              ]
                     )
         where sym@(Symbol nm) = sdName d
               end :: Grammar m (Symbol, Breakpoint)
               end = pure (sym, (initSymBlockID,0))
               m = foldl insertName emptyNameMap (M.keys (sdBody d))

-- | List of commands for debugger.
allCmds :: Codebase sbe -> SimCmd sbe m
allCmds cb = res 
  where res -- Breakpoints
            =    keyword "continue" *> continueCmd
            <||> keyword "break"  *> breakCmd cb
            -- Execution
            <||> keyword "delete" *> deleteCmd cb
            <||> keyword "finish" *> finishCmd
            <||> keyword "s"     *> hide stepiCmd
            <||> keyword "stepi" *> stepiCmd
            -- Information about current path.
            <||> keyword "info"   *> infoCmd
            -- Function for switching between paths
            <||> keyword "path" *> pathCmd
            -- Control and information about debugger.
            <||> keyword "help" *> helpCmd res
            <||> keyword "quit"   *> quitCmd
            
helpCmd :: MonadIO m => Command m -> Command m
helpCmd cmdList = cmdDef "Print list of commands." $ do
  liftIO $ print (commandHelp cmdList)
  return False

type SimCmd sbe m = (Functor m, MonadIO m) => Command (Simulator sbe m)

continueCmd :: Monad m => Command m
continueCmd = cmdDef "Continue execution." $ do
  return True

pathCmd :: SimCmd sbe m
pathCmd
  =    pathListCmd
  <||> keyword "kill" *> pathKillCmd
  <||> keyword "sat" *> pathSatCmd

pathListCmd :: SimCmd sbe m
pathListCmd = cmdDef "List all current execution paths." $ do
  --TODO: Implement 
  return False

-- TODO: Fix path cmd.
pathSatCmd :: SimCmd sbe m
pathSatCmd =
  cmdDef "Check whether the current path's assertions are satisfiable, killing this path if they are not." $ do
      Just p <- preuse currentPathOfState
      sbe <- gets symBE
      sat <- liftSBE $ termSAT sbe (p^.pathAssertions)
      case sat of
        UnSat -> do dbugM "path assertions unsatisfiable; killed"
                    errorPath "path assertions unsatisfiable: killed by debugger"
        Sat _ -> dbugM "path assertions satisfiable"
        Unknown -> dbugM "pat assertions possibly satisfiable"
      return False

pathKillCmd :: SimCmd sbe m
pathKillCmd = cmdDef "Kill the current execution path." $ do
  errorPath "Terminated by debugger"
  -- TODO: Fix kill cmd.

opt :: Grammar m a -> Grammar m (Maybe a)
opt m = (Just <$> m) <||> pure Nothing 

quitCmd :: MonadIO m => Command m
quitCmd = cmdDef "Exit LSS." $ do
  liftIO $ exitWith ExitSuccess

breakCmd :: Codebase sbe -> SimCmd sbe m
breakCmd cb = (locSeq cb <**>) $ cmdDef desc $ \(b,p) -> do
    addBreakpoint b p
    return False
  where desc = "Set a breakpoint at a specified location."

deleteCmd :: Codebase sbe -> SimCmd sbe m
deleteCmd cb = (opt (locSeq cb) <**>) $ cmdDef desc $ \mbp -> do
      case mbp of
        Just (b,p) -> removeBreakpoint b p
        Nothing -> dbugM "Remove all breakpoints"
      return False
  where desc = "Clear a breakpoint at a function."

concatBreakpoints :: M.Map Symbol (S.Set Breakpoint)
                  -> [(Symbol, Breakpoint)]
concatBreakpoints m =
  [ (sym,bp) | (sym, bps) <- M.toList m, bp <- S.toList bps ]

equalSep :: Doc -> Doc -> Doc
equalSep x y = x <+> char '=' <+> y

withCurrentFrame :: MonadIO m
                 => (Path sbe -> Simulator sbe m ())
                 -> Simulator sbe m ()
withCurrentFrame a = do
  mp <- preuse currentPathOfState
  case mp of
    Nothing -> dbugM "No frame selected."
    Just p -> a p

withActivePath :: MonadIO m
               => (Path sbe -> Simulator sbe m Bool)
               -> Simulator sbe m Bool
withActivePath a = do
  mcs <- use ctrlStk
  case mcs of
    Just (ActiveCS cs) -> a (cs^.activePath)
    _ -> do dbugM "The program is not being run."
            return False

infoCmd :: SimCmd sbe m
infoCmd
  = keyword "block" *> infoBlockCmd
  <||> keyword "breakpoints" *> infoBreakpointsCmd
  <||> keyword "ctrlstk" *> infoCtrlStkCmd
  <||> keyword "function" *> infoFunctionCmd
  <||> keyword "locals" *> infoLocalsCmd
  <||> keyword "memory" *> infoMemoryCmd
  <||> keyword "stack" *> infoStackCmd

infoBlockCmd :: SimCmd sbe m
infoBlockCmd =
  cmdDef "Print block information." $ do
    withCurrentFrame $ \p -> do
      let sym = pathFuncSym p
          Just (pcb,_) = p^.pathPC
      Just def <- lookupDefine sym <$> gets codebase
      dbugM $ show $ ppSymBlock $ lookupSymBlock def pcb
    return False

infoBreakpointsCmd :: SimCmd sbe m
infoBreakpointsCmd =
  cmdDef "List breakpoints." $ do
    dumpBPs >> return False

infoCtrlStkCmd :: SimCmd sbe m
infoCtrlStkCmd =
  cmdDef "Print the entire control stack." $ do
    dumpCtrlStk >> return False

infoFunctionCmd :: SimCmd sbe m
infoFunctionCmd =
  cmdDef "Print function information." $ do
    withCurrentFrame $ \p -> do
      dumpSymDefine (gets codebase) (unSym (pathFuncSym p))
    return False

infoLocalsCmd :: SimCmd sbe m
infoLocalsCmd =
  cmdDef "Print local variables in current stack frame." $ do
    withCurrentFrame $ \p -> do
      sbe <- gets symBE
      let locals = M.toList (p^.pathRegs)
          ppLocal (nm, (v,_)) =
            ppIdent nm `equalSep` prettyTermD sbe v           
      dbugM $ show $ vcat $ ppLocal <$> locals
    return False

infoMemoryCmd :: SimCmd sbe m
infoMemoryCmd =
  cmdDef "Print memory information." $ do
    dumpMem 0 "memory" >> return False

--TODO: Revisit this command to check formatting.
infoStackCmd :: SimCmd sbe m
infoStackCmd = cmdDef "Print backtrace of the stack." $ do
  withCurrentFrame $ \p -> do
    dbugM $ show $ ppStackTrace $ pathStack p
  return False


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

finishCmd :: SimCmd sbe m
finishCmd = cmdDef desc $ do
    withActivePath $ \p -> do
      trBreakpoints .= BreakReturnFrom (pathStackHt p)
      return True
  where desc = "Execute until the current stack frame returns."

stepiCmd :: SimCmd sbe m
stepiCmd = cmdDef "Execute one symbolic statement" $ do
  withActivePath $ \_ -> do
    trBreakpoints .= BreakNextStmt
    return True

unSym :: Symbol -> String
unSym (Symbol str) = str