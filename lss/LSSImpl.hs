{- |
Module           : $Header$
Description      : Implementation details for the command line driver;
                   primarily to facilitate programmatic invocation
Stability        : provisional
Point-of-contact : jhendrix
-}

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE ExistentialQuantification  #-}

module LSSImpl where

import           Control.Applicative ((<$>))
import           Control.Lens
import           Control.Monad.State
import           Data.Char
import           Data.Int
import           Numeric
import           System.Console.CmdArgs.Implicit hiding (args, setVerbosity, verbosity)
import           Verinf.Utils.LogMonad
import qualified Text.LLVM                       as L

import           Verifier.LLVM.AST
import           Verifier.LLVM.Backend
import           Verifier.LLVM.Codebase
import           Verifier.LLVM.Simulator
import           Verifier.LLVM.Simulator.Common

data LSS = LSS
  { dbug    :: DbugLvl
  , argv     :: String
  , backend  :: Maybe String
  , errpaths :: Bool
  , xlate    :: Bool
  , mname    :: Maybe String
  } deriving (Show, Data, Typeable)

newtype DbugLvl = DbugLvl { unD :: Int32 }
  deriving (Data, Enum, Eq, Integral, Num, Ord, Real, Show, Typeable)
instance Default DbugLvl where def = DbugLvl 1

-- newtype StackSz = StackSz { unS :: Int32 }
--   deriving (Data, Enum, Eq, Integral, Num, Ord, Real, Show, Typeable)
-- instance Default StackSz where def = StackSz 8

data BackendType = BitBlastBuddyAlloc
                 | BitBlastDagBased
                 | SAWBackendType
  deriving (Show)

data ExecRslt sbe crt
  = NoMainRV [ErrorPath sbe] (Maybe (SBEMemory sbe))
  | SymRV    [ErrorPath sbe] (Maybe (SBEMemory sbe)) (SBETerm sbe)
  | ConcRV   [ErrorPath sbe] (Maybe (SBEMemory sbe)) crt

type ExecRsltHndlr sbe crt a =
     SBE sbe          -- ^ SBE that was used used during a test
  -> SBEMemory sbe    -- ^ Typed initial memory that was used during a test
  -> ExecRslt sbe crt -- ^ Execution results; final memory is embedded here
  -> IO a

lssImpl :: Functor sbe
        => SBE sbe
        -> SBEMemory sbe
        -> Codebase
        -> [String]
        -> LSS
        -> IO (ExecRslt sbe Integer)
lssImpl sbe mem cb argv0 args = do
  let mainDef =
        case lookupDefine (L.Symbol "main") cb of
          Nothing -> error "Provided bitcode does not contain main()."
          Just md -> md
  runBitBlast sbe mem cb mg argv' args mainDef
  where
    argv' = "lss" : argv0
    mg    = defaultMemGeom (cbDataLayout cb)


runBitBlast :: Functor sbe
            => SBE sbe -- ^ SBE to use
            -> SBEMemory sbe     -- ^ SBEMemory to use
            -> Codebase
            -> MemGeom
            -> [String]          -- ^ argv
            -> LSS               -- ^ LSS command-line arguments
            -> SymDefine         -- ^ Define of main()
            -> IO (ExecRslt sbe Integer)
runBitBlast sbe mem cb mg argv' args mainDef = do
  runSimulator cb sbe mem seh' opts $ do
    setVerbosity $ fromIntegral $ dbug args
    whenVerbosity (>=5) $ do
      let sr (a,b) = "[0x" ++ showHex a "" ++ ", 0x" ++ showHex b "" ++ ")"
      dbugM $ "Memory model regions:"
      dbugM $ "Stack range : " ++ sr (mgStack mg)
      dbugM $ "Code range  : " ++ sr (mgCode mg)
      dbugM $ "Data range  : " ++ sr (mgData mg)
      dbugM $ "Heap range  : " ++ sr (mgHeap mg)
    let mainSymbol = L.Symbol "main"
    argsv <- buildArgv (snd <$> sdArgs mainDef) argv'
    --TODO: Verify main has expected signature.
    callDefine_ mainSymbol (sdRetType mainDef) argsv
    mrv <- getProgramReturnValue
    mm  <- getProgramFinalMem
    eps <- use errorPaths
    case mrv of
      Nothing -> return (NoMainRV eps mm)
      Just rv -> do
        let mval = asUnsignedInteger sbe undefined rv
        return $ maybe (SymRV eps mm rv) (\x -> (ConcRV eps mm x)) mval
  where
    opts        = Just $ LSSOpts (errpaths args)
    seh'        = defaultSEH

buildArgv ::
  ( MonadIO m
  , Functor sbe
  , Functor m
  )
  => [MemType] -- ^ Types of arguments expected by main.
  -> [String] -- ^ Arguments
  -> Simulator sbe m [(MemType,SBETerm sbe)]
buildArgv [] argv' = do
  liftIO $ when (length argv' > 1) $ do
    putStrLn $ "WARNING: main() takes no argv; ignoring provided arguments:\n" ++ show argv'
  return []
buildArgv [IntType argcw, ptype@PtrType{}] argv' | length argv' < 2^argcw = do
  sbe <- gets symBE
  argc     <- liftSBE $ termInt sbe argcw (toInteger (length argv'))
  let ec = EvalContext { evalContextName = "buildArgv"
                       , evalRegs = Nothing
                       }
  aw <- withDL ptrBitwidth
  one <- liftSBE $ termInt sbe aw 1
  strPtrs  <- forM argv' $ \str -> do
     let len = length str + 1
     let tp = ArrayType len (IntType 8)
     v <- evalExpr' ec (sValString (str ++ [chr 0]))
     p <- alloca tp aw one 0
     store tp v p 0
     return p
  argvBase <- alloca i8p argcw argc 0
  argvArr  <- liftSBE $ termArray sbe i8p strPtrs
  -- Write argument string data and argument string pointers
  store (ArrayType (length argv') i8p) argvArr argvBase 0
  return [ (IntType argcw, argc)
         , (ptype, argvBase)
         ]
buildArgv _ _ = error "main() has an unsupported type."

warnNoArgv :: IO ()
warnNoArgv = putStrLn "WARNING: main() takes no argv; ignoring provided arguments."
