{- |
Module           : $Header$
Description      : Implementation details for the command line driver;
                   primarily to facilitate programmatic invocation
Stability        : provisional
Point-of-contact : jstanley
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

import           Control.Applicative             hiding (many)
import           Control.Monad.State
import           Data.Int
import           Data.LLVM.Memory
import           Data.LLVM.Symbolic.AST
import           LSS.Execution.Codebase
import           LSS.Execution.Common
import           LSS.LLVMUtils
import           Verifier.LLVM.Backend
import           Verifier.LLVM.BitBlastBackend
import           LSS.Simulator
import           Numeric
import           System.Console.CmdArgs.Implicit hiding (args, setVerbosity, verbosity)
import           Text.LLVM                       ((=:), Typed(..))
import           Verinf.Utils.LogMonad
import qualified Data.Vector.Storable as SV
import qualified Text.LLVM                       as L

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
  mainDef <- case lookupDefine (L.Symbol "main") cb of
               Nothing -> error "Provided bitcode does not contain main()."
               Just mainDef -> do
                 when (null (sdArgs mainDef) && length argv' > 1) warnNoArgv
                 return mainDef
  runBitBlast sbe mem cb mg argv' args mainDef
  where
    argv' = "lss" : argv0
    lc    = cbLLVMCtx cb
    mg    = defaultMemGeom lc

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
    argsv <- if mainHasArgv then buildArgv numArgs argv' else return []
    let mainSymbol = L.Symbol "main"
    mainSymDef <- lookupSymbolDef mainSymbol
    --TODO: Verify main has expected signature.
    callDefine_ mainSymbol (sdRetType mainSymDef) argsv
    mrv <- getProgramReturnValue
    mm  <- getProgramFinalMem
    eps <- gets errorPaths
    case mrv of
      Nothing -> return (NoMainRV eps mm)
      Just rv -> do
        let mval = asUnsignedInteger sbe rv
        return $ maybe (SymRV eps mm rv) (\(_,x) -> (ConcRV eps mm x)) mval
  where
    opts        = Just $ LSSOpts (errpaths args)
    seh'        = defaultSEH
    mainHasArgv = not $ null $ sdArgs mainDef
    numArgs     = fromIntegral (length argv') :: Int32

buildArgv ::
  ( MonadIO m
  , Functor sbe
  , Functor m
  )
  => Int32 -> [String] -> Simulator sbe m [Typed (SBETerm sbe)]
buildArgv numArgs argv' = do
  argc     <- withSBE $ \s -> termInt s 32 (fromIntegral numArgs)
  ec <- getEvalContext "buildArgv" Nothing
  strVals  <- mapM (getTypedTerm' ec . cstring) argv'
  one <- getSizeT 1
  strPtrs  <- mapM (\ty -> tv <$> alloca ty one Nothing) (tt <$> strVals)
  num <- getSizeT (toInteger numArgs)
  argvBase <- alloca i8p num Nothing
  argvArr  <- (L.Array numArgs i8p =:) <$> withSBE (\s -> termArray s strPtrs)
  -- Write argument string data and argument string pointers
  forM_ (strPtrs `zip` strVals) $ \(p,v) -> store v p
  store argvArr (tv argvBase)
  return [i32 =: argc, argvBase]
  where
    tv = typedValue
    tt = typedType

warnNoArgv :: IO ()
warnNoArgv = putStrLn "WARNING: main() takes no argv; ignoring provided arguments."
