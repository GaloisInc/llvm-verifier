{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module           : $Header$
Description      : Symbolic execution tests
License          : BSD3
Stability        : provisional
Point-of-contact : acfoltzer
-}
module Verifier.LLVM.Debugger.Grammar
  ( Grammar
  , SwitchKey
  , switch
  , hide
  , cmdDef
  , keyword
  , argLabel
  , (<||>)
  , opt
  , nat
  , HelpResult
  , helpCmds
  , ppCmdHelp
  , matcherHelp
  , ParseResult(..)
  , resolveParse
  , parseString
  , parseCompletions
  ) where

import Control.Lens
import Control.Monad.State as MTL
import Data.Char
import qualified Data.Foldable as Fold
import Data.Kind ( Type )
import Data.List.Compat
import qualified Data.Map as Map
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.String
import System.Console.Haskeline (Completion(..))
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Prelude ()
import Prelude.Compat hiding (mapM_)

import Verifier.LLVM.Debugger.FreeApp
import Verifier.LLVM.Utils.PrettyPrint

------------------------------------------------------------------------
-- Generic

unionMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
unionMaybe u (Just x) y = Just (maybe x (u x) y)
unionMaybe _ Nothing y  = y

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
-- Token and Tokentype

-- | Token from parser.
data Token
   = Keyword String
   | StringLit String
   | NatToken Integer -- | A non-negative integer.
   | OpToken String
   | BadChar String
  deriving (Eq, Ord, Show)

-- | A "type" for topens.  Tokens with the same type need
-- a space between them.
data TokenType
   = WordTokenType
   | OpTokenType
   | BadCharType
  deriving (Eq, Show)

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

------------------------------------------------------------------------
-- CharStream

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

------------------------------------------------------------------------
-- TokenSeq

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
tokenSeqTokens f (SeqLast p t pe) = (\ u -> SeqLast p u pe) <$> f t
tokenSeqTokens _ (SeqEnd p) = pure (SeqEnd p)

convertToTokens :: String -> TokenSeq Token
convertToTokens = tokenize . charStream
  where tokenize :: CharStream -> TokenSeq Token
        tokenize (CSNext p c r)
          | isSpace c  = tokenize r
          | c == '\"'  = stringLit p [] r
          -- Hex literal.
          | c == '0'
          , CSNext p' 'x' r' <- r
          = charSeq isHexDigit isSpace (NatToken . read) p' ['x','0'] r'
          -- Binary literal.
          | c == '0'
          , CSNext p' 'b' r' <- r
          = charSeq isBinDigit isSpace (NatToken . readBinary) p' ['b','0'] r'
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

        -- Read a "0b" prefixed binary literal like "0b1010111".
        --
        -- This succeeds on "0b" returning zero, which is
        -- non-standard.
        readBinary :: String -> Integer
        readBinary ('0':'b':bits) = foldl (\acc bit -> 2*acc + read [bit]) 0 bits
        readBinary lit = error $ "convertToTokens: bad binary literal: " ++ lit

        isBinDigit :: Char -> Bool
        isBinDigit = (`elem` ['0','1'])

------------------------------------------------------------------------
-- Grammar and Parser

type Grammar m a = App (Parser m) a

data Parser (m :: Type -> Type) (a :: Type) where
  
  -- Label an argument to a command.
  ArgumentLabel :: PP.Doc -> Parser m ()
  
  -- Label the command that this runs.
  CommandLabel :: PP.Doc -> Parser m ()

  -- Hide matcher from help system.
  Hide :: Grammar m a -> Parser m a

  -- Switch expression.
  SwitchExpr :: SwitchMap m a -> Parser m a

  UnionGrammar :: Grammar m a -> Grammar m a -> Parser m a

data SwitchKey = SwitchKey [Token]

instance IsString SwitchKey where
  fromString s = SwitchKey (toListOf tokenSeqTokens tokens)
    where tokens = convertToTokens s

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

opt :: Grammar m a -> Grammar m (Maybe a)
opt m = (Just <$> m) <||> pure Nothing 

nat :: Grammar m Integer
nat = atom (SwitchExpr (fromTokenPred p))
  where p = TokenPred (Just (text "<nat>")) WordTokenType f idCont
        f (NatToken i) = Just i
        f _ = Nothing

------------------------------------------------------------------------
-- TokenPred

-- | A token predicate recognizes tokens with a user-definable action.
data TokenPred m a where
  -- Includes what to show when printing, a predicate, anda  continuation.
  TokenPred :: (Maybe PP.Doc)
            -> TokenType
            -> (Token -> Maybe b)
            -> Cont (Parser m) b a
            -> TokenPred m a

tokenPredType :: Simple Lens (TokenPred m a) TokenType
tokenPredType f (TokenPred d tp p c) =
  (\tp' -> TokenPred d tp' p c) <$> f tp

tokenPredAddCont :: TokenPred m a
                 -> Cont (Parser m) a c
                 -> TokenPred m c
tokenPredAddCont (TokenPred d t f c) c' =
  TokenPred d t f (c `composeCont` c')

fromTokenPred :: TokenPred m a -> SwitchMap m a
fromTokenPred p = SwitchMap { _smWords = Map.empty
                            , _smPreds = [p]
                            , _smEnd = Nothing
                            }


------------------------------------------------------------------------
-- SwitchMap

data SwitchMap m a = SwitchMap { _smWords :: Map.Map Token (SwitchMatch (Grammar m a))
                               , _smPreds :: [TokenPred m a]
                               , _smEnd :: Maybe (Grammar m a)
                               }

emptySwitchMap :: SwitchMap m a
emptySwitchMap = SwitchMap { _smWords = Map.empty
                           , _smPreds = []
                           , _smEnd = Nothing
                           }

-- | List of token predicates in map.
smPreds :: Simple Lens (SwitchMap m a) [TokenPred m a]
smPreds = lens _smPreds (\s v -> s { _smPreds = v })

smEnd :: Simple Lens (SwitchMap m a) (Maybe (Grammar m a))
smEnd = lens _smEnd (\s v -> s { _smEnd = v })

-- | Map of ground towns to switch matches.
smWords :: Simple Lens (SwitchMap m a) (Map.Map Token (SwitchMatch (Grammar m a)))
smWords = lens _smWords (\s v -> s { _smWords = v })

------------------------------------------------------------------------
-- SwitchMatch

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

unionSwitchMatch :: (a -> a -> a)
                 -> SwitchMatch a -> SwitchMatch a -> SwitchMatch a
unionSwitchMatch u (SwitchMatch ex xx) (SwitchMatch ey xy) =
  SwitchMatch (unionMaybe u ex ey) (xx ++ xy)

------------------------------------------------------------------------
-- NextState

data NextState m a = NextState { nsWords   :: Map.Map Token (SwitchMatch (Grammar m a))
                               , nsPreds   :: [TokenPred m a]
                               , nsMatches :: [a]
                               }

pureNextState :: a -> NextState m a
pureNextState v = NextState { nsWords = Map.empty
                            , nsPreds = []
                            , nsMatches = [v]
                            }

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

unionNextState :: NextState m a -> NextState m a -> NextState m a
unionNextState x y =
  NextState { nsWords   = zw
            , nsPreds   = zp
            , nsMatches = zm
            }
  where xw = nsWords x
        yw = nsWords y
        zw = Map.unionWith (unionSwitchMatch (<||>)) xw yw
        zp = nsPreds x   ++ nsPreds y
        zm = nsMatches x ++ nsMatches y

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

nextStateChoices :: NextState m a -> [(Token, Grammar m a)]
nextStateChoices m = 
  [ (nm, a)
  | (nm,(^.switchExact) -> Just a) <- Map.toList (nsWords m)
  ]

nextTokenTypes :: NextState m a -> [TokenType]
nextTokenTypes m = wordTypes ++ predTypes
  where wordTypes = tokenType <$> Map.keys (nsWords m)
        predTypes = view tokenPredType <$> nsPreds m

matchPred :: TokenPred m a -> Token -> Maybe (App (Parser m) a)
matchPred (TokenPred _ _ f c) t = (`evalCont` c) <$> f t

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

parseString :: (Applicative m, Monad m)
            => Grammar m a
            -> String
            -> m (ParseResult a)
parseString g s = runGrammar g (convertToTokens s)

runGrammar :: (Applicative m, Monad m)
           => Grammar m a
           -> TokenSeq Token
           -> m (ParseResult a)
runGrammar (PureApp a) ts = 
  pure (parseValue (tokenSeqPos ts) a [])
runGrammar m ts = 
  parseSwitch (resolveToNextState m) ts

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

cmdHelpArgs :: Lens' CmdHelp ArgsI
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

helpCmds :: Lens' HelpResult [CmdHelp]
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

switchHelp :: SwitchMap m a -> HelpResult
switchHelp m = unionHelpList $
    itemHelp <$> mapMaybe (_2 (^. switchExact)) matches
  where matches = Map.toList $ m^.smWords 
        itemHelp (nm,k) = HelpResult (singleArg (ppToken nm)) [] 
                         `seqHelp` matcherHelp k

parserHelp :: forall m a .Parser m a -> HelpResult
parserHelp (ArgumentLabel d) = HelpResult (singleArg d) []
parserHelp (CommandLabel d) = HelpResult emptyArgs [CmdHelp emptyArgs d]
parserHelp (SwitchExpr m) = switchHelp m
parserHelp (Hide _) = emptyHelp
parserHelp (UnionGrammar x y) = matcherHelp x `unionHelp` matcherHelp y
