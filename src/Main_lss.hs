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
  , argv    :: String
  , mname   :: Maybe String
  , memtype :: Maybe String
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
            , argv    = def &= typ "\"arg1 arg2 ...\""
                            &= help "Space-delimited arguments to main()"
            , memtype = def &= typ "[bitblast|dagbased]"
                            &= help "Memory model to use (default: bitblast)"
            , mname = def &= typ "Fully-linked .bc containing main()"
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

  cb      <- loadCodebase bcFile
  mainDef <- case lookupDefine' (L.Symbol "main") cb of
               Nothing -> error "Provided bitcode does not contain main()."
               Just mainDef -> do
                 when (null (sdArgs mainDef) && not (null argv')) warnNoArgv
                 return mainDef
  case mem of
    DagBased -> error "Support for DAG-based memory NYI"
    BitBlast -> runBitBlast cb argv' args mainDef

runBitBlast :: Codebase -> [String] -> LSS -> SymDefine -> IO ()
runBitBlast cb argv' args mainDef = do
  let lc = cbLLVMCtx cb
  (sbe,mem) <- do
    be <- createBitEngine
    let mm = buddyMemModel lc be
        mem = buddyInitMemory stk code data' heap
    return (sbeBitBlast lc be mm,mem)
  runSimulator cb sbe mem (SM . lift . liftSBEBitBlast) defaultSEH $ do
    setVerbosity $ fromIntegral $ dbug args
    whenVerbosity (>=5) $ do
      let sr (a,b) = "[0x" ++ showHex a "" ++ ", 0x" ++ showHex b "" ++ ")"
      dbugM $ "Memory model regions:"
      dbugM $ "Stack range : " ++ sr stk
      dbugM $ "Code range  : " ++ sr code
      dbugM $ "Data range  : " ++ sr data'
      dbugM $ "Heap range  : " ++ sr heap

    callDefine_ (L.Symbol "main") i32 $
      if mainHasArgv then buildArgv numArgs argv' else return []

{-
    -- tmp: hacky manual eval aiger until the overrides are all in place and working
    mrv <- getProgramReturnValue
    case mrv of
      Nothing -> dbugM "no retval"
      Just rv -> do
        dbugTerm "rv" rv
        -- for multiply-lss.c:multiply() with uint16_t VecTypes: manually check that 3 * 4 = 12.
        eval <- withSBE $ \sbe -> evalAiger sbe ((True : True : replicate 14 False) ++ (False : False : True : replicate 13 False)) rv
        dbugTerm "eval" eval
    return ()
-}
  where
      mainHasArgv              = not $ null $ sdArgs mainDef
      numArgs                  = fromIntegral (length argv') :: Int32
      (stk, code, data', heap) = defaultMemGeom (cbLLVMCtx cb)

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
  argvBase <- alloca i8p (Just $ int32const numArgs) Nothing
  argvArr  <- (L.Array numArgs i8p =:) <$> withSBE (\s -> termArray s strPtrs)
  -- Write argument string data and argument string pointers
  forM_ (strPtrs `zip` strVals) $ \(p,v) -> do
    processMemCond =<< mutateMem (\s m -> memStore s m v p)
    return ()
  processMemCond =<< mutateMem (\s m -> memStore s m argvArr (tv argvBase))
  return [i32 =: argc, argvBase]
  where
    tv = typedValue
    tt = typedType

eatWS :: String -> String
eatWS (' ':cs) = eatWS cs
eatWS cs       = cs

warnNoArgv :: IO ()
warnNoArgv = putStrLn "WARNING: main() takes no argv; ignoring provided arguments."
