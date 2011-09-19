{- |
Module           : $Header$
Description      : The command line driver for the LLVM symbolic simulator
Stability        : provisional
Point-of-contact : jstanley
-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns               #-}

module Main where

import           Control.Applicative             hiding (many)
import           Control.Monad
import           Control.Monad.Trans
import           Data.Char
import           Data.Int
import           Data.LLVM.Memory
import           Data.LLVM.Symbolic.AST
import           Data.LLVM.Symbolic.Translation  (liftDefine)
import           Data.Vector.Storable            (Storable)
import           LSS.Execution.Codebase
import           LSS.Execution.Common
import           LSS.LLVMUtils
import           LSS.SBEBitBlast
import           LSS.Simulator
import           Numeric
import           System.Console.CmdArgs.Implicit hiding (args, setVerbosity, verbosity)
import           System.Environment              (getArgs)
import           System.Exit
import           Text.LLVM                       ((=:), Typed(..))
import           Text.ParserCombinators.Parsec
import           Verinf.Symbolic.Common          (ConstantProjection(..), createBitEngine)
import           Verinf.Utils.LogMonad
import qualified System.Console.CmdArgs.Implicit as Args
import qualified Text.LLVM                       as L

data LSS = LSS
  { dbug    :: DbugLvl
--   , stack   :: StackSz
  , argv     :: String
  , mname    :: Maybe String
  , memtype  :: Maybe String
  , errpaths :: Bool
  , xlate    :: Bool
  } deriving (Show, Data, Typeable)

data MemType = BitBlast | DagBased deriving (Show)

newtype DbugLvl = DbugLvl { unD :: Int32 }
  deriving (Data, Enum, Eq, Integral, Num, Ord, Real, Show, Typeable)
instance Default DbugLvl where def = DbugLvl 1

-- newtype StackSz = StackSz { unS :: Int32 }
--   deriving (Data, Enum, Eq, Integral, Num, Ord, Real, Show, Typeable)
-- instance Default StackSz where def = StackSz 8

main :: IO ()
main = do
  numArgs <- length <$> getArgs
  when (numArgs == 0) $ do
    putStrLn $ "lss: No command line options given. Try --help for more information."
    exitFailure

  args <- cmdArgs $ LSS
            { dbug    = def &= help "Debug verbosity level (1-7)"
--             , stack   = def &= opt "8" &= help "Stack size in megabytes (default: 8)"
            , argv     = def &= typ "\"arg1 arg2 ...\""
                             &= help "Space-delimited arguments to main()"
            , memtype  = def &= typ "[bitblast|dagbased]"
                             &= help "Memory model to use (default: bitblast)"
            , errpaths = def &= help "Dump error path details upon program completion (potentially very verbose)."
            , xlate    = def &= help "Prints the symbolic AST translation to stdout, and then terminates."
            , mname    = def &= typ "Fully-linked .bc containing main()"
                             &= Args.args
            }
            &= summary ("LLVM Symbolic Simulator (lss) 0.1a Sep 2011. "
                        ++ "Copyright 2011 Galois, Inc. All rights reserved.")

  mem <- case eatWS <$> memtype args of
           Just "dagbased" -> return DagBased
           Just "bitblast" -> return BitBlast
           Nothing         -> return BitBlast
           _               -> do
             putStrLn "Invalid memory model specified.  Please choose 'bitblast' or 'dagbased'."
             exitFailure

  bcFile <- case mname args of
    Nothing -> do putStrLn $ "lss: No LLVM bitcode file provided. "
                             ++ "Try --help for more information."
                  exitFailure
    Just nm -> return nm

  let p     = many $ between spaces spaces $ many1 $ satisfy $ not . isSpace
      argv' = case runParser p () "argv values" (argv args) of
                Left _e -> error "Unable to parse command-line arguments (argv)."
                Right x -> "lss" : x

  cb <- loadCodebase bcFile

  when (xlate args) $ do
    -- Dump the translated module and exit
    let via s f = mapM_ (putStrLn . show  . f) (s $ origModule cb)
    ((:[]) . L.modDataLayout) `via` L.ppDataLayout
    L.modTypes                `via` L.ppTypeDecl
    L.modGlobals              `via` L.ppGlobal
    L.modDeclares             `via` L.ppDeclare
    L.modDefines              `via` (ppSymDefine . liftDefine)
    exitWith ExitSuccess

  mainDef <- case lookupDefine' (L.Symbol "main") cb of
               Nothing -> error "Provided bitcode does not contain main()."
               Just mainDef -> do
                 when (null (sdArgs mainDef) && length argv' > 1) warnNoArgv
                 return mainDef

  let lc = cbLLVMCtx cb
  be <- createBitEngine
  let mg = defaultMemGeom (cbLLVMCtx cb)
  case mem of
    DagBased -> do
      (mm, mem') <- createDagMemModel lc be mg
      let sbe = sbeBitBlast lc be mm
      exitWith =<< runBitBlast sbe mem' cb mg argv' args mainDef
    BitBlast -> do
      (mm, mem') <- createBuddyMemModel lc be mg
      let sbe = sbeBitBlast lc be mm
      exitWith =<< runBitBlast sbe mem' cb mg argv' args mainDef

runBitBlast :: (Eq l, Storable l)
            => SBE (BitIO mem l) -> mem
            -> Codebase -> MemGeom -> [String] -> LSS -> SymDefine
            -> IO ExitCode
runBitBlast sbe mem cb mg argv' args mainDef = do
  let opts = Just $ LSSOpts (errpaths args)
      seh' = defaultSEH
      lft  = SM . lift . lift . liftSBEBitBlast
  runSimulator cb sbe mem lft seh' opts $ do
    setVerbosity $ fromIntegral $ dbug args
    whenVerbosity (>=5) $ do
      let sr (a,b) = "[0x" ++ showHex a "" ++ ", 0x" ++ showHex b "" ++ ")"
      dbugM $ "Memory model regions:"
      dbugM $ "Stack range : " ++ sr (mgStack mg)
      dbugM $ "Code range  : " ++ sr (mgCode mg)
      dbugM $ "Data range  : " ++ sr (mgData mg)
      dbugM $ "Heap range  : " ++ sr (mgHeap mg)

    callDefine_ (L.Symbol "main") i32 $
      if mainHasArgv then buildArgv numArgs argv' else return []

    mrv <- getProgramReturnValue
    case mrv of
      Nothing -> dbugM "Obtained no return value from main()" >> return ExitSuccess
      Just rv -> do
        let mval = getUVal . closeTerm sbe $ rv
        case mval of
          Nothing -> do
            dbugM "Obtained symbolic return value from main()"
            return ExitSuccess
          Just 0 -> return ExitSuccess
          Just n -> return . ExitFailure . fromIntegral $ n
  where
      mainHasArgv = not $ null $ sdArgs mainDef
      numArgs     = fromIntegral (length argv') :: Int32

buildArgv ::
  ( MonadIO m
  , Functor sbe
  , Functor m
  , ConstantProjection (SBEClosedTerm sbe)
  )
  => Int32 -> [String] -> Simulator sbe m [Typed (SBETerm sbe)]
buildArgv numArgs argv' = do
  argc     <- withSBE $ \s -> termInt s 32 (fromIntegral numArgs)
  strVals  <- mapM (getTypedTerm' Nothing . cstring) argv'
  strPtrs  <- mapM (\ty -> tv <$> alloca ty Nothing Nothing) (tt <$> strVals)
  num      <- getTypedTerm (int32const numArgs)
  argvBase <- alloca i8p (Just num) Nothing
  argvArr  <- (L.Array numArgs i8p =:) <$> withSBE (\s -> termArray s strPtrs)
  -- Write argument string data and argument string pointers
  forM_ (strPtrs `zip` strVals) $ \(p,v) -> store v p
  store argvArr (tv argvBase)
  return [i32 =: argc, argvBase]
  where
    tv = typedValue
    tt = typedType

eatWS :: String -> String
eatWS (' ':cs) = eatWS cs
eatWS cs       = cs

warnNoArgv :: IO ()
warnNoArgv = putStrLn "WARNING: main() takes no argv; ignoring provided arguments."
