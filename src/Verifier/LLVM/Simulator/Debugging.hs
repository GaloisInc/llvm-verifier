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
{-# LANGUAGE DoAndIfThenElse     #-}
{- LANGUAGE FlexibleContexts    #-}
{- LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{- LANGUAGE PatternGuards       #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- LANGUAGE TupleSections #-}
{- LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
module Verifier.LLVM.Simulator.Debugging (
    breakOnMain
  , debuggerREPL
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

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
import Control.Concurrent (myThreadId)
import System.Posix.Signals
#endif

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

-- | Add a breakpoint to @main@ in the current codebase
breakOnMain :: (Functor m, Monad m) => Simulator sbe m ()
breakOnMain = addBreakpoint (Symbol "main") (initSymBlockID,0)

-- | Prints out message where current path is.
logBreakpoint :: (Functor m, Monad m, MonadIO m)
              => Simulator sbe m ()
logBreakpoint = do
  withActivePath () $ \p -> do
    let sym = p^.pathFuncSym
    cb <- gets codebase
    case lookupDefine sym cb of
      Nothing -> dbugM "Could not determine location."
      Just def -> do
        let psym = ppSymbol sym  
        let bld =
              case p^.pathPC of
                Nothing -> psym
                Just (pcb,pc) ->
                    psym <> ppSymBlockID pcb <> colon <+> ppStmt stmt
                  where sb = lookupSymBlock def pcb
                        stmt = sbStmts sb V.! pc
        dbugM $ show $ text "at" <+> bld <> colon


checkForBreakpoint :: (Functor sbe, Functor m, MonadIO m, MonadException m)
                   => Simulator sbe m ()
checkForBreakpoint = do
  Just p <- preuse currentPathOfState
  mbps <- use (breakpoints . at (p^.pathFuncSym))
  let atBP = fromMaybe False $ S.member <$> (p^.pathPC) <*> mbps
  when atBP debuggerREPL

debuggerREPL :: forall sbe m 
              . (Functor sbe, Functor m, MonadIO m, MonadException m)
             => Simulator sbe m ()
debuggerREPL = do
    pathPosChangeEvent .= checkForBreakpoint
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
switch = atom . Switch . impl
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

{-
emptyNextState :: NextState m a
emptyNextState = NextState { nsWords = M.empty
                           , nsPreds = []
                           , nsMatches = []
                           }
-}

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

{-
-- | Traverse all grammars in switch map.
smGrammars :: Traversal (SwitchMap m a)
                        (SwitchMap n b)
                        (Grammar m a)
                        (Grammar n b)
smGrammars f (SwitchMap w e) =
  SwitchMap <$> traverse (switchMatchGrammars f) w <*> traverse f e
-}

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
    Switch m -> composeNextState m c
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



nat :: Grammar m Integer
nat = atom (Switch (fromTokenPred p))
  where p = TokenPred (Just (text "<nat>")) WordTokenType f idCont
        f (NatToken i) = Just i
        f _ = Nothing

-- | List of commands for debugger.
allCmds :: Codebase sbe -> SimCmd sbe m
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
            
helpCmd :: MonadIO m => Command m -> Command m
helpCmd cmdList = cmdDef "Print list of commands." $ do
  liftIO $ print (commandHelp cmdList)
  return False

type SimCmd sbe m = (Functor sbe, Functor m, MonadIO m, MonadException m)
                 => Command (Simulator sbe m)

pathCmd :: SimCmd sbe m
pathCmd
  =    hide pathListCmd
  <||> keyword "kill" *> pathKillCmd
  <||> keyword "list" *> pathListCmd
  <||> keyword "sat" *> pathSatCmd


pathListCmd :: SimCmd sbe m
pathListCmd = cmdDef "List all current execution paths." $ do
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
  return False

pathSatCmd :: SimCmd sbe m
pathSatCmd =
  cmdDef "Check satisfiability of path assertions with SAT solver." $ do
      Just p <- preuse currentPathOfState
      sbe <- gets symBE
      sat <- liftSBE $ termSAT sbe (p^.pathAssertions)
      case sat of
        UnSat -> 
           dbugM "Path has unsatisfiable assertions."
        Sat _ ->
           dbugM "Path assertions are satisfiable."
        Unknown ->
           dbugM "Could not determine if path assertions are feasible."
      return False

pathKillCmd :: SimCmd sbe m
pathKillCmd = cmdDef "Kill the current execution path." $ do
  killCurrentPath (FailRsn "Terminated by debugger.")
  mp <- preuse currentPathOfState
  case mp of
    Nothing -> dbugM "Killed last path."
    Just p -> dbugM $ show $
      text "Switched to path:" <+> ppPathInfo p
  return False
 
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
    withActivePath False $ \p -> do
      let sym = p^.pathFuncSym
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
    withActivePath False $ \p -> do
      dumpSymDefine (gets codebase) (unSym (p^.pathFuncSym))
      return False

infoLocalsCmd :: SimCmd sbe m
infoLocalsCmd =
  cmdDef "Print local variables in current stack frame." $ do
    withActivePath False $ \p -> do
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
  withActivePath False $ \p -> do
    dbugM $ show $ ppStackTrace $ p^.pathStack
    return False

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

withActivePath :: MonadIO m
               => a
               -> (Path sbe -> Simulator sbe m a)
               -> Simulator sbe m a
withActivePath v action = do
  mcs <- use ctrlStk
  case mcs of
    Just (ActiveCS cs) -> action (cs^.activePath)
    _ -> do dbugM "No active execution path."
            return v

continueCmd :: SimCmd sbe m
continueCmd = cmdDef "Continue execution." $ do
  withActivePath False $ \_ -> do
     return True

onReturnFrom :: (Functor sbe, Functor m, MonadIO m, MonadException m)
             => Int -> Simulator sbe m ()
onReturnFrom ht = do
  Just p <- preuse currentPathOfState
  if p^.pathStackHt < ht then
    debuggerREPL
  else
    checkForBreakpoint

finishCmd :: SimCmd sbe m
finishCmd = cmdDef desc $ do
    withActivePath False $ \p -> do
      pathPosChangeEvent .= onReturnFrom (p^.pathStackHt)
      return True
  where desc = "Execute until the current stack frame returns."

enterDebuggerAfterNSteps
  :: (Functor sbe, Functor m, MonadIO m, MonadException m)
  => Integer -> Simulator sbe m ()
enterDebuggerAfterNSteps n
  | n <= 1 = debuggerREPL
  | otherwise = pathPosChangeEvent .= enterDebuggerAfterNSteps (n-1)

stepiCmd :: SimCmd sbe m
stepiCmd = (opt nat <**>) $
  cmdDef "Execute one symbolic statement" $ \mc -> do
    withActivePath False $ \_ -> do
      let c = fromMaybe 1 mc
      if c <= 0 then
        return False
      else do
        pathPosChangeEvent .= enterDebuggerAfterNSteps c
        return True

unSym :: Symbol -> String
unSym (Symbol str) = str