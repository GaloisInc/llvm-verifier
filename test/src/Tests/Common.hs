{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ViewPatterns         #-}
module Tests.Common 
  ( module Tests.Common
  , PropertyM
  , run
  , view
  ) where

import           Control.Applicative
import           Control.Lens hiding (act, (<.>))
import           Control.Monad 
import           Control.Monad.State (gets, MonadIO)
import           Data.Int
import           System.FilePath
import qualified Text.LLVM                     as L
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import qualified Test.QuickCheck.Test          as T
import           Verinf.Symbolic               (Lit, createBitEngine)

import           LSSImpl

import           Verifier.LLVM.BitBlastBackend
import           Verifier.LLVM.Codebase
import           Verifier.LLVM.SAWBackend
import           Verifier.LLVM.Simulator
import           Verifier.LLVM.Simulator.SimUtils

data ExpectedRV a = AllPathsErr | VoidRV | RV a deriving Functor

newtype FailMsg = FailMsg String
instance Show FailMsg where show (FailMsg s) = s

padTy :: Int -> MemType
padTy bytes = ArrayType (fromIntegral bytes) i8

supportDir :: FilePath
supportDir = "test" </> "src" </> "support"

testsDir :: FilePath
testsDir = supportDir

testMDL :: FilePath -> PropertyM IO L.Module
testMDL bcFile = run $ loadModule $ testsDir </> bcFile

assertMsg :: Bool -> String -> PropertyM IO ()
assertMsg b s = when (not b) (run $ putStrLn s) >> assert b

test :: Int -> Bool -> String -> PropertyM IO () -> (Args, Property)
test n shouldFail desc propM =
  ( stdArgs{ maxSuccess = n}
  , label desc $ handleNeg $ monadicIO $ withFailMsg propM
  )
  where
    handleNeg   = if shouldFail then expectFailure else id
    withFailMsg = if not shouldFail then forAllM (return msg) . const else id
    msg         = FailMsg $ "Test failed: '" ++ desc ++ "'"

runTests :: [(Args, Property)] -> IO ()
runTests tests = do
  results <- mapM (uncurry quickCheckWithResult) tests
  if all T.isSuccess results
    then putStrLn "All tests successful."
    else putStrLn "One or more tests failed."

constTermEq :: Maybe Integer -> Integer -> Bool
constTermEq (Just v) = (==v)
constTermEq _ = const False

type SBEPropM a = forall sbe . (Functor sbe, Ord (SBETerm sbe)) => Simulator sbe IO a

forAllMemModels :: forall a. Int -> FilePath -> SBEPropM a -> PropertyM IO [a]
forAllMemModels v bcFile testProp = do
  mdl <- testMDL bcFile
  let runTest :: (Functor sbe, Ord (SBETerm sbe))
              => SBECreateFn sbe
              -> PropertyM IO a
      runTest createFn = run $ runTestSimulator createFn v mdl testProp
  sequence
    [ 
{-
      runTest =<< run (createBuddyModel dl)
    , runTest =<< run (createDagModel dl)
-}
      runTest createSAWModel
    ]

type AllMemModelTest = Functor sbe => Simulator sbe IO Bool

runAllMemModelTest :: Int -> FilePath -> AllMemModelTest -> PropertyM IO ()
runAllMemModelTest v bcFile act = do
  assert . and =<< forAllMemModels v bcFile act

type ExecRsltHndlr sbe crt a =
     SBE sbe          -- ^ SBE that was used used during a test
  -> SBEMemory sbe    -- ^ Typed initial memory that was used during a test
  -> ExecRslt sbe crt -- ^ Execution results; final memory is embedded here
  -> IO a

runTestSimulator :: (Functor sbe, Ord (SBETerm sbe))
                 => SBECreateFn sbe
                 -> Int -- ^ Verbosity
                 -> L.Module -- ^ Code to run in.
                 -> Simulator sbe IO a
                 -> IO a
runTestSimulator createFn v mdl action = do
  let dl = parseDataLayout (L.modDataLayout mdl)
  (sbe, mem) <- createFn dl
  cb <- mkCodebase sbe dl mdl
  runSimulator cb sbe mem defaultSEH Nothing $ do
    setVerbosity v
    action

testRunMain :: (Functor sbe, Functor m, MonadIO m, MonadException m)
            => [String] -> Simulator sbe m (ExecRslt sbe Integer)
testRunMain args = do
  cb <- gets codebase
  case lookupDefine (L.Symbol "main") cb of
    Nothing -> error "Provided bitcode does not contain main()."
    Just mainDef -> runMainFn mainDef ("lss" : args)

type SBECreateFn sbe = DataLayout -> IO (SBE sbe, SBEMemory sbe)

runTestLSSCommon :: (Functor sbe, Ord (SBETerm sbe))
                 => SBECreateFn sbe
                 -> Int
                 -> L.Module
                 -> [String]
                 -> Maybe Int     -- ^ Expected number of error paths
                 -> Maybe Integer -- ^ Expected return value
                 -> PropertyM IO ()
runTestLSSCommon createFn v mdl argv' mepsLen mexpectedRV = do
  run $ runTestSimulator createFn v mdl $ do
    when (v > 0) breakOnMain
    case mepsLen of
      -- Set error policy to just kill paths if errors are expected.
      Just i | i > 0 && v == 0 -> errorHandler .= killPathOnError
      _ -> return ()
    execRslt <- testRunMain argv'
    case mepsLen of
      Nothing -> return ()
      Just epsLen -> checkErrorPaths epsLen execRslt
    checkReturnValue mexpectedRV execRslt


-- | Create buddy backend and initial memory.
createBuddyModel :: SBECreateFn (BitIO (BitMemory Lit) Lit)
createBuddyModel dl = do
  be <- createBitEngine
  let sbe = let ?be = be in sbeBitBlast dl (buddyMemModel dl be)
      mem = buddyInitMemory (defaultMemGeom dl)
  return (sbe,mem)

-- | Create buddy backend and initial memory.
createDagModel :: SBECreateFn (BitIO (DagMemory Lit) Lit)
createDagModel dl = do
  be <- createBitEngine
  (mm,mem) <- createDagMemModel dl be (defaultMemGeom dl)
  let sbe = let ?be = be in sbeBitBlast dl mm
  return (sbe,mem)

createSAWModel :: SBECreateFn (SAWBackend s Lit)
createSAWModel dl = do
  be <- createBitEngine
  createSAWBackend be dl (defaultMemGeom dl)

runTestLSSBuddy :: Int           -- ^ Verbosity
                -> L.Module      -- ^ Module 
                -> [String]      -- ^ Arguments
                -> Maybe Int     -- ^ Expected number of error paths
                -> Maybe Integer -- ^ Expected return value
                -> PropertyM IO ()
runTestLSSBuddy = runTestLSSCommon createBuddyModel

runTestLSSDag :: Int           -- ^ Verbosity
              -> L.Module      -- ^ Module 
              -> [String]      -- ^ Arguments
              -> Maybe Int     -- ^ Expected number of error paths
              -> Maybe Integer -- ^ Expected return value
              -> PropertyM IO ()
runTestLSSDag = runTestLSSCommon createDagModel

lssTest :: Int -> FilePath -> (Int -> L.Module -> PropertyM IO ()) -> (Args, Property)
lssTest v bc act = test 1 False bc $ act v =<< testMDL (bc <.> "bc")

testEachBackend :: FilePath
                -> (forall sbe . (Functor sbe, Ord (SBETerm sbe))
                               => L.Module
                               -> String
                               -> SBECreateFn sbe
                               -> PropertyM IO ())
                -> (Args,Property)
testEachBackend nm f = do
  test 1 False nm $ do
    mdl <- testMDL (nm <.> "bc")
    f mdl "bitblast" createBuddyModel
    f mdl "dag" createDagModel
--    f "saw" createSAWModel

-- | Run test on all backends
lssTestAll :: Int
           -> String
           -> [String]
           -> Maybe Int  -- ^ Expected number of error paths.
           -> Maybe Integer -- ^ Expected return value
           -> (Args,Property)
lssTestAll v nm args elen erv =
  testEachBackend nm $ \mdl _backendName createFn -> do
    runTestLSSCommon createFn v mdl args elen erv

checkErrorPaths :: Monad m => Int -> ExecRslt sbe crt -> m ()
checkErrorPaths epsLen execRslt = do
  let actualLen = length (execRslt^.execRsltErrorPaths)
  unless (actualLen == epsLen) $ do
    fail $ "Expected " ++ show epsLen ++ " error paths, but found " 
           ++ show actualLen

checkReturnValue :: Monad m => Maybe Integer -> ExecRslt sbe Integer -> m ()
checkReturnValue mexpectedRV execRslt = do
  case execRslt of
    ConcRV _ _mm r -> do
      case mexpectedRV of
        Nothing -> fail "Unexpected return value"
        Just expectedRV ->
          unless (expectedRV == r) $ do
            fail $ "Expected " ++ show expectedRV ++ " return value, but found " ++ show r  
    NoMainRV _ _mm -> do
      case mexpectedRV of
        Nothing -> return ()
        Just{} -> fail $ "Missing return value"
    SymRV{} -> fail "Unexpected sym exec result"


-- TODO: At some point we may want to inspect error paths and ensure
-- that they are the /right/ error paths, rather just checking the
-- count!.  We'll need something better than FailRsn to do that, though.
chkLSS :: Maybe Int
       -> Maybe Integer
       -> ExecRsltHndlr sbe Integer Bool
chkLSS mepsLen mexpectedRV _ _ execRslt = do
  case mepsLen of
    Nothing -> return ()
    Just epsLen -> checkErrorPaths epsLen execRslt
  checkReturnValue mexpectedRV execRslt
  return True

chkNullaryCInt32Fn :: Int
                   -> FilePath
                   -> L.Symbol
                   -> ExpectedRV Int32
                   -> PropertyM IO ()
chkNullaryCInt32Fn v bc sym chkVal =
  runCInt32Fn v bc sym [] (fromIntegral <$> chkVal)

runCInt32Fn :: Int
            -> FilePath
            -> L.Symbol
            -> [Int32]
            -> ExpectedRV Integer
            -> PropertyM IO ()
runCInt32Fn v bcFile sym cargs erv = do
  void $ forAllMemModels v bcFile $ do
    sbe <- gets symBE
    args <- mapM (liftSBE . termInt sbe 32 . fromIntegral) cargs
    void $ callDefine sym (Just i32) ((IntType 32,) <$> args)
    mrv <- getProgramReturnValue
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

-- possibly skip a test
psk :: Int -> PropertyM IO () -> PropertyM IO ()
psk v act = if (v > 0) then act else disabled

disabled :: PropertyM IO ()
disabled = do
  run $ putStrLn $ "Warning: Next test is DISABLED! (will report success)"

incomplete :: PropertyM IO () -> PropertyM IO ()
incomplete act = do
  run $ putStrLn $ "Warning: Next test is INCOMPLETE! (will report failure)"
  act
  assert False

runMain :: Int -> FilePath -> ExpectedRV Int32 -> PropertyM IO ()
runMain = runMain' False

runMain' :: Bool -> Int -> FilePath -> ExpectedRV Int32 -> PropertyM IO ()
runMain' quiet v bc erv = do
  psk v $ chkNullaryCInt32Fn (if quiet then 0 else v) bc (L.Symbol "main") erv

runMainVoid :: Int -> FilePath -> PropertyM IO ()
runMainVoid v bcFile = psk v $ do
  void $ forAllMemModels 0 bcFile $
    void $ callDefine (L.Symbol "main") Nothing []