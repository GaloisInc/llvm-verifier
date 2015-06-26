{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -O0 #-}

{- |
Module           : $Header$
Description      : The symbolic simulator for LLVM-Symbolic programs
License          : Free for non-commercial use. See LICENSE.
Stability        : provisional
Point-of-contact : jhendrix
-}
module Verifier.LLVM.Codebase.DebugInfo 
  ( -- * Reader
    DebugInfo
  , initialDebugInfo
  , dsCompileUnits
  , DebugReader
  , runDebugReader
    -- * CompileUnit
  , CompileUnit(..)
    -- * Language
  , Language
  , lang_c89
  , lang_c
  , lang_c_plus_plus
    -- * Scope
  , Scope(..)
  , LexicalBlock(..)
  , Context(..)
  , Subprogram(..)  
    -- * Location information
  , LocationInfo(..)
  , readLocationInfo
    -- * ClassType
  , ClassType(..)
  , readClassType
    -- * Re-exports  
  , Ident
  , FilePath
  , Int32
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Lens hiding (Context)
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.State
import Data.Bits (testBit)
import Data.Int (Int32)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Numeric (showHex)
import System.FilePath ((</>))
import Text.LLVM

--------------------------------------------
-- Metadata lookup utilities

-- | Maps each metadata name appearing in a program to the
-- unnamed variables that it maps to.
type NamedMdMap = Map String [Int]

namedMdMap :: Module -> NamedMdMap
namedMdMap mo = foldl' f Map.empty (modNamedMd mo)
  where f m md = Map.insertWith (flip (++)) (nmName md) (nmValues md) m

lookupNamedMd :: String -> NamedMdMap -> [Int]
lookupNamedMd nm m = fromMaybe [] $ Map.lookup nm m

type UnnamedMdMap = Map Int [Typed Value]

unnamedMdMap :: Module -> UnnamedMdMap
unnamedMdMap mdl = Map.fromList
  [ (umIndex md, umValues md) |  md <- modUnnamedMd mdl ]

--------------------------------------------
-- DebugReader

data DebugInfo 
   = DebugInfo { dsMdMap :: UnnamedMdMap
                , _dsCompileUnits :: [CompileUnit]
                , _dsFilePathCache :: Map Int FilePath
                , _dsContextCache  :: Map Int Context
                , _dsScopeCache    :: Map Int Scope
                }

dsCompileUnits :: DebugInfo -> [CompileUnit]
dsCompileUnits = _dsCompileUnits

-- | Cache for filepaths
dsFilePathCache :: Simple Lens DebugInfo (Map Int FilePath)
dsFilePathCache = lens _dsFilePathCache (\s v -> s { _dsFilePathCache = v })

-- | Cache for contexts
dsContextCache :: Simple Lens DebugInfo (Map Int Context)
dsContextCache = lens _dsContextCache (\s v -> s { _dsContextCache = v })

-- | Cache for contexts
dsScopeCache :: Simple Lens DebugInfo (Map Int Scope)
dsScopeCache = lens _dsScopeCache (\s v -> s { _dsScopeCache = v })


type DebugReader = ExceptT String (State DebugInfo)

#if !MIN_VERSION_mtl(2,2,0)
#if !MIN_VERSION_transformers_compat(0,4,0)
instance MonadState DebugInfo DebugReader where
  get = lift get
  put = lift . put
#endif
#endif

runDebugReader :: DebugInfo -> DebugReader a -> (Either String a, DebugInfo)
runDebugReader s r = runState (runExceptT r) s

initialDebugInfo :: Module -> Either String DebugInfo
initialDebugInfo mdl = fst $ runDebugReader s0 $ do
    let mnm = namedMdMap mdl
    cul <- mapM readCompileUnit (lookupNamedMd "llvm.dbg.cu" mnm)
    s <- get
    return $ s { _dsCompileUnits = cul }
  where s0 = DebugInfo { dsMdMap = unnamedMdMap mdl
                        , _dsCompileUnits = []           
                        , _dsFilePathCache = Map.empty
                        , _dsContextCache  = Map.empty
                        , _dsScopeCache    = Map.empty
                        }

lookupMetadata :: Int -> DebugReader [Typed Value]
lookupMetadata i = do
  m <- gets dsMdMap
  case Map.lookup i m of
    Nothing -> fail $ "Could not find reference " ++ show i ++ "\n"
       ++ "  Valid references include: " ++ show (Map.keys m)
    Just v -> return v

--------------------------------------------
-- FieldReader

type FieldReader = ExceptT String (StateT [Typed Value] (State DebugInfo))

#if !MIN_VERSION_mtl(2,2,0)
#if !MIN_VERSION_transformers_compat(0,4,0)
instance MonadState [Typed Value] FieldReader where
  get = lift get
  put = lift . put
#endif
#endif

readNext' :: FieldReader (Typed Value)
readNext' = do
  l <- get
  case l of
    [] -> fail "Unexpected end of fields."
    (tpv:r) -> tpv <$ put r

skipNext :: FieldReader ()
skipNext = void $ readNext'

readField :: (Typed Value -> DebugReader a) -> FieldReader a
readField f = do
  tpv <- readNext'
  r <- lift $ lift $ runExceptT $ f tpv
  either fail return r

-- | @readCached lens nm r i@ reads node @i@ using the reader @r@.
-- Reads are cached in a map that can be accessed via @lens@, to
-- minimize redundant reads.
readCached :: Simple Lens DebugInfo (Map Int a)
           -> String
           -> FieldReader a
           -> Int
           -> DebugReader a
readCached l nm fr i = do
  m <- use l
  case Map.lookup i m of
    Just v -> return v
    Nothing -> do
      v <- runFieldReader nm i fr
      l .= Map.insert i v m
      return v

runFieldReader :: String -> Int -> FieldReader r -> DebugReader r
runFieldReader nm i v = do
  l <- lookupMetadata i
  runFieldReader' ("node !" ++ show i) nm l v

runFieldReader' :: String
                -> String
                -> [Typed Value]
                -> FieldReader r
                -> DebugReader r
runFieldReader' loc nm l fr = do
  (mr,l') <- lift $ runStateT (runExceptT fr) l
  case mr of
    Left msg -> do 
      let cnt = length l - length l'
      let field | cnt > 0 = " at field " ++ show (cnt-1)
                | otherwise = ""
      fail $ msg ++ "\n  when reading " ++ loc ++ " as a " ++ nm ++ field
    Right res -> do
      unless (null l') $ do
        fail $ "Failed to read all fields\n  when reading " ++ loc ++ " as a " ++ nm
      return res

-- | Check tag satisfies predicate.
checkTag :: (Int32 -> Bool) -> FieldReader ()
checkTag p = do
  tag <- readInt32
  unless (p tag) $ 
    fail $ "Unexpected tag " ++ showHex tag ""

------------------------------------------------------------------------
-- Typed Value updates

uncurryTypeValue :: (Type -> Value -> a) -> Typed Value -> a
uncurryTypeValue f tpv = f (typedType tpv) (typedValue tpv)

asInt32 :: Monad m => Type -> Value -> m Int32 
asInt32 (PrimType (Integer 32)) (ValInteger v) = return (fromInteger v)
asInt32 _ _ = fail "Expected int32"

asString :: Monad m => Type -> Value -> m String 
asString (PrimType Metadata) (ValMd (ValMdString nm)) = return nm
asString _ _ = fail "Expected String"

asBool :: Monad m => Type -> Value -> m Bool
asBool (PrimType (Integer 1)) (ValBool b) = return b
asBool (PrimType (Integer 1)) (ValInteger v) = return (v `testBit` 0)
asBool tp v = fail $ "Expected Bool and found " ++ show (tp,v)

asMdRef :: Monad m => Type -> Value -> m Int
asMdRef (PrimType Metadata) (ValMd (ValMdRef i)) = return i
asMdRef tp v = fail $ "Expected metadata and found " ++ show (tp,v)

readMdRef :: (Int -> DebugReader a) -> FieldReader a
readMdRef f = readField $ f <=< uncurryTypeValue asMdRef

readBool :: FieldReader Bool
readBool = readField $ uncurryTypeValue asBool

readInt32 :: FieldReader Int32
readInt32 = readField $ uncurryTypeValue asInt32

readString :: FieldReader String
readString = readField $ uncurryTypeValue asString

--------------------------------------------
-- FilePathPair


-- | Read a file path pair.
readFilePathPair :: Int -> DebugReader FilePath
readFilePathPair = readCached dsFilePathCache "file path pair" $ do
  nm  <- readField $ uncurryTypeValue asString
  dir <- readField $ uncurryTypeValue asString
  return $ dir </> nm

readFilePathField :: FieldReader FilePath
readFilePathField = readMdRef readFilePathPair

--------------------------------------------
-- MetadataValue

class MetadataValue a where
  -- | Read a metadata value.
  evalValue :: Type -> Value -> DebugReader a

readNext :: MetadataValue a => FieldReader a
readNext = readField (uncurryTypeValue evalValue)

{-
instance MetadataValue String where
  evalValue = asString

instance MetadataValue Int32 where
  evalValue = asInt32


instance MetadataValue Bool where
  evalValue = asBool

instance MetadataValue [Typed Value] where
  evalValue tp v = lookupMetadata =<< asMdRef tp v
-}

----------------------------------------------
-- Reader instances

-- | Context of an expression.
data Context
      -- | Appears in a class with given name
    = ClassContext Ident
      -- | Appears at the top level of a file.
    | FileContext FilePath
      -- | Is defined inside a function (e.g. C++ lambda functions)
    | SubprogramContext Subprogram
  deriving (Show)

readContext :: Int -> DebugReader Context
readContext = do
  readCached dsContextCache "context" $ do
    readContextFields =<< readInt32

readContextFields :: Int32 -> FieldReader Context
readContextFields tag =
  case tag of
    0xc0029 -> FileContext         <$> readFilePathField
    0xc002e -> SubprogramContext   <$> readSubprogramFields
    _ -> fail $ "Unexpected context tag 0x" ++ showHex tag ""

instance MetadataValue Context where
  evalValue (PtrTo (Alias c)) (ValSymbol _) = return $ ClassContext c
  evalValue (PrimType Metadata) (ValMd (ValMdRef i)) = readContext i
  evalValue tp v = fail $ "Unexpected metadata context " ++ show (tp,v)

------------------------------------------------------------------------
-- ClassType

data ClassType = ClassType { -- | File of compile unit where this debug information comes.
                             ctFile :: FilePath
                             -- | Context where class appears.
                           , ctContext :: Context
                             -- | Name of class (may be empty for anonymous classes).
                           , ctName :: String
                             -- Line number where class appears
                           , ctLineNum :: Int32
                           }
  deriving (Show)

readClassType :: Int -> DebugReader ClassType
readClassType i = runFieldReader "class type" i readClassFields

readClassFields :: FieldReader ClassType
readClassFields = do
  file <- readFilePathField
  ctx  <- readNext
  nm   <- readString
  lineNum  <- readInt32
  skipNext -- SizeInBits
  skipNext -- AlignInBits
  skipNext -- Offset in bits.
  skipNext -- Flags
  skipNext -- DerivedFrom
  skipNext -- Elements
  skipNext -- Zero?
  skipNext -- VTableHolder
  skipNext -- Template parameters
  return ClassType { ctFile = file
                   , ctContext = ctx
                   , ctName = nm
                   , ctLineNum = lineNum
                   }

data Subprogram
   = Subprogram
   { spFile :: FilePath
     -- | Context where subprogram returns.
   , spContext :: Context
     -- | Name of method in original source.
   , spName :: String
     -- | Name of method to linker.
   , spLinkageName :: String
     -- | Line number for method declaration.
   , spLineNum :: Int32
     -- | Indicates this is local to compile unit.
   , spIsLocal :: Bool
     -- | Compile unit contains subprogram definition, and not just a declaration.
   , spIsDef :: Bool     
     -- | Subprogram has been compiled with optimization.
   , spIsOptimized :: Bool
   } deriving (Show)

instance MetadataValue Subprogram where
  evalValue tp v = readSubprogram =<< asMdRef tp v

readSubprogram :: Int -> DebugReader Subprogram
readSubprogram i = do
  runFieldReader "subprogram" i $ do
    checkTag (== 0xc002e)
    readSubprogramFields

readSubprogramFields :: FieldReader Subprogram
readSubprogramFields = do
  file <- readFilePathField
  ctx  <- readNext
  nm   <- readString
  skipNext -- This is the "display name", but seems a duplicate of name.
  linkageName <- readString
  lineNum     <- readInt32
  skipNext -- This is the type information.
  isLocal <- readBool
  isDef   <- readBool
  skipNext -- VK??
  skipNext -- VIndex??
  skipNext -- VTableHolder??
  skipNext -- Flags??
  isOptimized <- readBool
  skipNext -- Fn??
  skipNext -- TParam??
  skipNext -- VMContext?
  skipNext -- Temporary??
  skipNext -- Second line number?
  return Subprogram { spFile    = file
                    , spContext = ctx
                    , spName    = nm
                    , spLinkageName = if linkageName == "" then
                                        nm
                                      else
                                        linkageName
                    , spLineNum = lineNum
                    , spIsLocal = isLocal
                    , spIsDef   = isDef
                    , spIsOptimized = isOptimized
                    }

evalList :: MetadataValue a => Type -> Value -> DebugReader [a]
evalList tp v = do
  l <- lookupMetadata =<< asMdRef tp v
  case l of
    [ Typed (PrimType (Integer 32)) (ValInteger 0) ] -> return []
    _ -> traverse (uncurryTypeValue evalValue) l

instance MetadataValue v => MetadataValue [v] where
  evalValue = evalList

------------------------------------------------------------------------
-- Language

newtype Language = Language Int32

instance Show Language where
  showsPrec d (Language i) = 
    case i of
      0x1 -> showString "lang_c89"
      0x2 -> showString "lang_c"
      0x4 -> showString "lang_c_plus_plus"
      _ -> showParen (d > 10) $ showString "Language" . shows i
  

lang_c89 :: Language
lang_c89 = Language 0x1

lang_c :: Language
lang_c = Language 0x2

lang_c_plus_plus :: Language
lang_c_plus_plus = Language 0x4

------------------------------------------------------------------------
-- Compile unit

data CompileUnit = CompileUnit { cuFilePath :: FilePath
                               , cuLanguage :: Language
                               , cuProducer :: String
                               , cuIsOptimized :: Bool
                               , cuSubprograms :: [Subprogram]
                               }
  deriving (Show)

readCompileUnit :: Int -> DebugReader CompileUnit
readCompileUnit i = do
  runFieldReader "compile unit" i $ do
    checkTag (== 0xc0011)
    file <- readFilePathField
    lang <- readInt32
    producer <- readString
    isOpt <- readBool
    skipNext -- Flags
    skipNext -- RunTimeVer
    skipNext -- Enum types
    skipNext -- Retain types
    subprograms <- readNext
    skipNext -- Global variables
    skipNext -- Imported modules
    skipNext -- Split name
    return CompileUnit { cuFilePath = file
                       , cuLanguage = Language lang
                       , cuProducer = producer
                       , cuIsOptimized = isOpt
                       , cuSubprograms = subprograms
                       }

------------------------------------------------------------------------
-- LexicalBlock

-- | A lexical block in the program
data LexicalBlock = LexicalBlock { lbScope :: Scope
                                 , lbFile :: FilePath
                                 , lbLine :: Int32
                                 , lbCol  :: Int32
                                 }
  deriving (Show)

-- | Read fields in a lexical block after tag.
readLexicalBlockFields :: FieldReader LexicalBlock
readLexicalBlockFields = do
  file <- readFilePathField
  scope <- readNext
  line <- readInt32 -- Line 
  col <- readInt32 -- Col
  skipNext -- Unique id
  return LexicalBlock { lbScope = scope
                      , lbFile  = file
                      , lbLine  = line
                      , lbCol   = col
                      }

------------------------------------------------------------------------
-- Scope

data Scope
   = ContextScope Context 
     -- | Appears within a block inside a function.
   | LexicalBlockScope LexicalBlock
  deriving (Show)

readScopeFields :: FieldReader Scope
readScopeFields = do
    tag <- readInt32
    case tag of
      0xc000b -> LexicalBlockScope <$> readLexicalBlockFields
      _ -> ContextScope <$> readContextFields tag

readScope :: Int -> DebugReader Scope
readScope = do
  readCached dsScopeCache "scope" readScopeFields

instance MetadataValue Scope where
  evalValue (PrimType Metadata) (ValMd (ValMdRef i)) = readScope i
  evalValue tp v = ContextScope <$> evalValue tp v

readValMdScope :: ValMd -> DebugReader Scope
readValMdScope (ValMdRef i)   = readScope i
readValMdScope (ValMdNode tv) =
  runFieldReader' "inline debug info" "scope" tv readScopeFields
readValMdScope _ = fail "Expected metadata node or reference in scope"

------------------------------------------------------------------------
-- LocationInfo

-- | A program location.
data LocationInfo = LocInfo { liLineNumber :: Int32
                            , liColNumber :: Int32
                            , liScope :: Scope
                            , liOrigin :: Maybe Scope
                            } deriving (Show)

-- | Read location info.
readLocationInfo :: DebugLoc -> DebugReader LocationInfo
readLocationInfo dl = do
  scope <- readValMdScope (dlScope dl)
  origin <- _Just readValMdScope (dlIA dl)
  return LocInfo { liLineNumber = dlLine dl
                 , liColNumber = dlCol dl
                 , liScope = scope
                 , liOrigin = origin
                 }

{-
------------------------------------------------------------------------
-- Test code

runTest :: Show a => FilePath -> DebugReader a -> IO ()
runTest path action = do
  Right mdl <- parseBitCode =<< BS.readFile path
  let Right s = initialDebugInfo mdl
  case runDebugReader s action of
    (Left e,_) -> putStrLn e
    (Right v,_) -> putStrLn $ ppShow v


test = do
  Right mdl <- parseBitCode =<< BS.readFile "cpp_test-3.3.bc"
  let def:_ = modDefines mdl
  let bb:_ = defBody def
  let Result _ _ [("dbg",ValMdLoc md)]:_ = drop 7 $ bbStmts bb
  let Right s = initialDebugInfo mdl
  case runDebugReader s $ readLocationInfo md of
    (Right v,_) -> putStrLn $ ppShow v
-}
