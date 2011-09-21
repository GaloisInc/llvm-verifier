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
import           Control.Arrow
import           Control.Monad.State
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
import           Text.LLVM                       ((=:), Typed(..))
import           Verinf.Symbolic.Common          (ConstantProjection(..), Lit, createBitEngine)
import           Verinf.Utils.LogMonad
import qualified System.Console.CmdArgs.Implicit as Args
import qualified Text.LLVM                       as L

data MemType = BitBlast | DagBased deriving (Show)

data LSS = LSS
  { dbug    :: DbugLvl
--   , stack   :: StackSz
  , argv     :: String
  , mname    :: Maybe String
  , memtype  :: Maybe String
  , errpaths :: Bool
  , xlate    :: Bool
  } deriving (Show, Data, Typeable)

newtype DbugLvl = DbugLvl { unD :: Int32 }
  deriving (Data, Enum, Eq, Integral, Num, Ord, Real, Show, Typeable)
instance Default DbugLvl where def = DbugLvl 1

-- newtype StackSz = StackSz { unS :: Int32 }
--   deriving (Data, Enum, Eq, Integral, Num, Ord, Real, Show, Typeable)
-- instance Default StackSz where def = StackSz 8

liftSim :: BitIO m l a -> Simulator sbe IO a
liftSim = SM . lift . lift . liftSBEBitBlast

data ExecRslt sbe crt
  = NoMainRV [ErrorPath sbe] (Maybe (SBEMemory sbe))
  | SymRV    [ErrorPath sbe] (Maybe (SBEMemory sbe)) (SBETerm sbe)
  | ConcRV   [ErrorPath sbe] (Maybe (SBEMemory sbe)) crt

type ExecRsltHndlr crt a = forall sbe. SBE sbe -> (ExecRslt sbe crt) -> IO a

lssImpl :: Codebase -> [String] -> MemType -> LSS -> ExecRsltHndlr Integer a -> IO a
lssImpl cb argv' memType args hndlr = do
  be      <- createBitEngine
  mainDef <- case lookupDefine' (L.Symbol "main") cb of
               Nothing -> error "Provided bitcode does not contain main()."
               Just mainDef -> do
                 when (null (sdArgs mainDef) && length argv' > 1) warnNoArgv
                 return mainDef
  case memType of
    DagBased -> do
      (sbe, mem) <- first (sbeBitBlast lc be) <$> createDagMemModel lc be mg
      hndlr sbe =<< runBitBlast sbe mem cb mg argv' args mainDef

    BitBlast -> do
      (sbe, mem) <- first (sbeBitBlast lc be) <$> createBuddyMemModel lc be mg
      hndlr sbe =<< runBitBlast sbe mem cb mg argv' args mainDef
  where
    lc = cbLLVMCtx cb
    mg = defaultMemGeom lc

runBitBlast :: BitBlastSBE mem Lit           -- ^ SBE to use
            -> mem                           -- ^ SBEMemory to use
            -> Codebase
            -> MemGeom
            -> [String]                      -- ^ argv
            -> LSS                           -- ^ LSS command-line arguments
            -> SymDefine                     -- ^ Define of main()
            -> IO (ExecRslt (BitIO mem Lit) Integer)
runBitBlast sbe mem cb mg argv' args mainDef = do
  runSimulator cb sbe mem liftSim seh' opts $ do
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
    mm  <- getProgramFinalMem
    eps <- gets errorPaths
    case mrv of
      Nothing -> return (NoMainRV eps mm)
      Just rv -> do
        let mval = getUVal . closeTerm sbe $ rv
        return $ maybe (SymRV eps mm rv) (\x -> (ConcRV eps mm x)) mval
  where
    opts        = Just $ LSSOpts (errpaths args)
    seh'        = defaultSEH
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

warnNoArgv :: IO ()
warnNoArgv = putStrLn "WARNING: main() takes no argv; ignoring provided arguments."
