{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}

{- |
Module           : $Header$
Description      : Symbolic execution tests
License          : BSD3
Stability        : provisional
Point-of-contact : atomb
-}
module Tests.Common where

import qualified Numeric
import           Control.Monad (unless, void)
import           Control.Monad.Fail ( MonadFail )
import           Control.Monad.State (gets, MonadIO, liftIO)
import           Control.Lens ( (^.) )

import           Data.Int
import           Data.Typeable
import           Test.Tasty
import           Test.Tasty.Options
import           Test.Tasty.QuickCheck
import qualified Test.Tasty.HUnit as HU
import Prelude ()
import Prelude.Compat

import           System.FilePath

import qualified Data.ABC as ABC

import           LSSImpl

import Verifier.LLVM.Codebase
import Verifier.LLVM.Backend.BitBlast
import Verifier.LLVM.Backend.SAW
import Verifier.LLVM.Simulator hiding (run)

import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC
import qualified Text.LLVM               as L


qctest :: Bool -> String -> QC.PropertyM IO () -> TestTree
qctest shouldFail desc propM = testProperty desc (handleNeg $ QC.monadicIO $ propM)
  where
    handleNeg = if shouldFail then QC.expectFailure else id

data ExpectedRV a = AllPathsErr | VoidRV | RV a deriving (Eq, Functor)

type SBEPropM m = forall sbe. (Functor sbe, Ord (SBETerm sbe)) => Simulator sbe m ()
type SBECreateFn = DataLayout -> IO SBEPair

abcNetwork :: ABC.Proxy ABC.GIALit ABC.GIA
abcNetwork = ABC.giaNetwork

-- | Create buddy backend and initial memory.
createBuddyModel :: SBECreateFn
createBuddyModel dl = do
  (ABC.SomeGraph g) <- (ABC.newGraph abcNetwork)
  let sbe = sbeBitBlast g dl (buddyMemModel dl g)
      mem = buddyInitMemory (defaultMemGeom dl)
  return (SBEPair sbe mem)

-- | Create buddy backend and initial memory.
createDagModel ::SBECreateFn
createDagModel dl = do
  (ABC.SomeGraph g) <- ABC.newGraph abcNetwork
  (mm,mem) <- createDagMemModel dl g (defaultMemGeom dl)
  let sbe = sbeBitBlast g dl mm
  return (SBEPair sbe mem)

createSAWModel :: SBECreateFn
createSAWModel dl = do
  (sbe,mem) <- liftIO $ createSAWBackend abcNetwork dl
  return (SBEPair sbe mem)

supportDir :: FilePath
supportDir = "test" </> "src" </> "support"

testsDir :: FilePath
testsDir = supportDir

testMDL :: FilePath -> IO L.Module
testMDL bcFile = loadModule $ testsDir </> bcFile

data VerbosityOption = VerbosityOption Int
 deriving (Eq, Show, Typeable)

instance IsOption VerbosityOption where
  defaultValue = VerbosityOption 0
  parseValue = \s ->
      case Numeric.readDec s of
        (i,[]):_ -> Just (VerbosityOption i)
        _ -> Nothing
  optionName = return "verbosity"
  optionHelp = return "verbosity level for the simulator"

withVerbModel :: FilePath -> (Int -> IO L.Module -> TestTree) -> TestTree
withVerbModel bcFile f =
  askOption $ \(VerbosityOption v) ->
  withResource (testMDL bcFile) (\_ -> return ()) $ \getmdl ->
  f v getmdl

forAllMemModels :: String -> FilePath -> (String -> Int -> SBECreateFn -> IO L.Module -> TestTree) -> TestTree
forAllMemModels groupName bcFile mkTest =
  withVerbModel bcFile $ \v getmdl ->
     testGroup groupName
        [ mkTest "buddy model"     v createBuddyModel    getmdl
        , mkTest "dag model"       v createDagModel      getmdl
        , mkTest "SAW model"       v createSAWModel      getmdl
        ]

runTestSimulator :: (MonadIO m, MonadException m, Functor m, MonadFail m)
                 => Int
                 -> SBECreateFn
                 -> IO L.Module -- ^ Code to run in.
                 -> SBEPropM m
                 -> m ()
runTestSimulator v createFn mdlio action = do
  mdl <- liftIO mdlio
  let dl = parseDataLayout (L.modDataLayout mdl)
  (SBEPair sbe mem) <- liftIO $ createFn dl
  ([],cb) <- liftIO $ mkCodebase sbe dl mdl
  runSimulator cb sbe mem Nothing $ do
    setVerbosity v
    action

runCInt32Fn :: (SimulatorContext sbe m, MonadException m)
            => L.Symbol
            -> [Int32]
            -> ExpectedRV Integer
            -> Simulator sbe m ()
runCInt32Fn sym cargs erv = do
    sbe <- gets symBE
    args <- mapM (liftSBE . termInt sbe 32 . fromIntegral) cargs
    let rvt = if erv == VoidRV then Nothing else Just i32
    void $ callDefine sym rvt ((IntType 32,) <$> args)
    mrv <- getProgramReturnValue
    checkReturnValue sbe erv mrv

runVoidFn :: (SimulatorContext sbe m, MonadException m)
            => L.Symbol
            -> ExpectedRV Integer
            -> Simulator sbe m ()
runVoidFn sym erv = do
    sbe <- gets symBE
    void $ callDefine sym Nothing []
    mrv <- getProgramReturnValue
    checkReturnValue sbe erv mrv

checkReturnValue :: Monad m => SBE sbe -> ExpectedRV Integer -> Maybe (SBETerm sbe) -> m ()
checkReturnValue sbe erv mrv =
    case (erv,mrv) of
      (RV{}, Nothing) -> fail "Missing return value"
      (RV chk, Just rv) ->
        case asSignedInteger sbe 32 rv of
          Nothing -> fail $ "Symbolic return value when constant expected.\n"
                              ++ show (prettyTermD sbe rv)
          Just val ->
            unless (val == chk) $
              fail $ "Expected " ++ show chk ++ ", got " ++ show val

      (VoidRV,Nothing) -> return ()
      (VoidRV, Just{}) -> fail $ "Received return value when none expected."

      (AllPathsErr, Nothing) -> return ()
      (AllPathsErr, Just{}) ->
        fail "Received return value when all paths were expected to error."

lssTestAll :: String
           -> [String]    -- arguments to main
           -> Maybe Int   -- expected number of error paths
           -> ExpectedRV Integer -- expected return value
           -> TestTree
lssTestAll nm args expectErr expectRV =
   forAllMemModels nm (nm <.> "bc") $ \bkName v sbeCF mdlio ->
       runLssTest bkName v sbeCF mdlio args expectErr expectRV

runLssTest :: String
           -> Int
           -> SBECreateFn
           -> IO L.Module
           -> [String]
           -> Maybe Int
           -> ExpectedRV Integer
           -> TestTree
runLssTest bkName v sbeCF mdlio args expectErr expectRV =
   HU.testCase bkName $ runTestSimulator v sbeCF mdlio $ do
          sbe <- gets symBE
          execResult <- testRunMain args
          liftIO $ checkExecResult sbe expectRV execResult
          liftIO $ checkErrPaths expectErr execResult

testRunMain :: (SimulatorContext sbe m, MonadException m)
            => [String] -> Simulator sbe m (ExecRslt sbe Integer)
testRunMain args = do
  cb <- gets codebase
  case lookupDefine (L.Symbol "main") cb of
    Nothing -> error "Provided bitcode does not contain main()."
    Just mainDef -> runMainFn mainDef ("lss" : args)

checkErrPaths :: Maybe Int -> ExecRslt sbe Integer -> IO ()
checkErrPaths Nothing _ = return ()
checkErrPaths (Just n) execRslt =
   HU.assertEqual "error path mismatch" n (length (execRslt^.execRsltErrorPaths))

checkExecResult :: SBE sbe -> ExpectedRV Integer -> ExecRslt sbe Integer -> IO ()
checkExecResult sbe mexpectedRV execRslt = do
  case execRslt of
    ConcRV _ _mm r -> do
      case mexpectedRV of
        VoidRV -> HU.assertFailure "Unexpected return value"
        AllPathsErr -> HU.assertFailure "all paths resulted in errors"
        RV expectedRV -> HU.assertEqual "incorrect return value" r expectedRV
    NoMainRV _ _mm -> do
      case mexpectedRV of
        VoidRV -> return ()
        AllPathsErr -> return ()
        RV{} -> HU.assertFailure "Missing return value"
    SymRV _ _ tm -> do
       HU.assertFailure $ "Unexpected sym exec result: "++show (prettyTermD sbe tm)

constTermEq :: Maybe Integer -> Integer -> Bool
constTermEq (Just v) = (==v)
constTermEq _ = const False
