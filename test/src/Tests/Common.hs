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

import qualified Data.ABC as ABC
import qualified Data.ABC.AIG
import qualified Data.ABC.GIA

import           LSSImpl

import Verifier.LLVM.Backend.BitBlastNew
import qualified Verifier.LLVM.Backend.BitBlast as Old
import qualified Verinf.Symbolic as Verinf

import Verifier.LLVM.Backend.SAW
import Verifier.LLVM.Codebase
import Verifier.LLVM.Debugger
import Verifier.LLVM.Simulator hiding (run)


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
testMDL bcFile = run $ do
  loadModule $ testsDir </> bcFile

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
  let runTest :: SBECreateFn -> PropertyM IO a
      runTest createFn = run $ runTestSimulator createFn v mdl testProp
  sequence
    [ (run $ putStrLn "--- old buddy model ---") >> runTest createOldBuddyModel
    , (run $ putStrLn "--- old dag model ---") >> runTest createOldDagModel
    , (run $ putStrLn "--- buddy model ---") >> runTest createBuddyModel
    , (run $ putStrLn "--- dag model ---") >> runTest createDagModel
    , (run $ putStrLn "--- SAW model ---") >> runTest createSAWModel
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

runTestSimulator :: SBECreateFn
                 -> Int -- ^ Verbosity
                 -> L.Module -- ^ Code to run in.
                 -> SBEPropM a -- Simulator sbe IO a
                 -> IO a
runTestSimulator createFn v mdl action = do
  let dl = parseDataLayout (L.modDataLayout mdl)
  createFn dl $ \sbe mem -> do
  ([],cb) <- mkCodebase sbe dl mdl
  runSimulator cb sbe mem Nothing $ do
    setVerbosity v
    action

testRunMain :: (Functor sbe, Functor m, MonadIO m, MonadException m)
            => [String] -> Simulator sbe m (ExecRslt sbe Integer)
testRunMain args = do
  cb <- gets codebase
  case lookupDefine (L.Symbol "main") cb of
    Nothing -> error "Provided bitcode does not contain main()."
    Just mainDef -> runMainFn mainDef ("lss" : args)

type SBECreateFn = forall a.
                       DataLayout -> 
                       (forall sbe. (Functor sbe, Ord (SBETerm sbe)) => SBE sbe -> SBEMemory sbe -> IO a) ->
                       IO a

runTestLSSCommon :: SBECreateFn
                 -> Int
                 -> L.Module
                 -> [String]
                 -> Maybe Int     -- ^ Expected number of error paths
                 -> Maybe Integer -- ^ Expected return value
                 -> PropertyM IO ()
runTestLSSCommon createFn v mdl argv' mepsLen mexpectedRV = do
  run $ runTestSimulator createFn v mdl $ do
    case mepsLen of
      Just i | i > 0 && v == 0 -> return ()
      _ -> do
        _ <- initializeDebugger
        when (v > 0) $ do
          breakOnEntry =<< lookupSymbolDef (L.Symbol "main")
    execRslt <- testRunMain argv'
    case mepsLen of
      Nothing -> return ()
      Just epsLen -> checkErrorPaths epsLen execRslt
    checkReturnValue mexpectedRV execRslt

abcNetwork = ABC.giaNetwork
cnfWriter = giaCnfWriter

--abcNetwork = ABC.aigNetwork
--cnfWriter = aigCnfWriter

giaCnfWriter :: Data.ABC.GIA.GIA s -> FilePath -> Data.ABC.GIA.Lit s -> IO [Maybe Int]
giaCnfWriter g fp l = fmap (map Just) $ Data.ABC.GIA.writeCNF g l fp

aigCnfWriter :: Data.ABC.AIG.AIG s -> FilePath -> Data.ABC.AIG.Lit s -> IO [Maybe Int]
aigCnfWriter g fp l = fmap (map Just) $ Data.ABC.AIG.writeToCNF g l fp


-- | Create buddy backend and initial memory.
createBuddyModel :: SBECreateFn
createBuddyModel dl k = do
  (ABC.SomeGraph g) <- ABC.newGraph abcNetwork
  let sbe = sbeBitBlast g (cnfWriter g) dl (buddyMemModel dl g)
      mem = buddyInitMemory (defaultMemGeom dl)
  k sbe mem

createOldBuddyModel :: SBECreateFn
createOldBuddyModel dl k = do
  be <- Verinf.createBitEngine
  let sbe = let ?be = be in Old.sbeBitBlast dl (Old.buddyMemModel dl be)
      mem = Old.buddyInitMemory (defaultMemGeom dl)
  k sbe mem


-- | Create buddy backend and initial memory.
createDagModel :: SBECreateFn
createDagModel dl k = do
  (ABC.SomeGraph g) <- ABC.newGraph abcNetwork
  (mm,mem) <- createDagMemModel dl g (defaultMemGeom dl)
  let sbe = sbeBitBlast g (cnfWriter g) dl mm
  k sbe mem

createOldDagModel :: SBECreateFn
createOldDagModel dl k = do
  be <- Verinf.createBitEngine
  (mm,mem) <- Old.createDagMemModel dl be (defaultMemGeom dl)
  let sbe = let ?be = be in Old.sbeBitBlast dl mm
  k sbe mem

createSAWModel :: SBECreateFn
createSAWModel dl k = do
  ABC.SomeGraph g <- ABC.newGraph abcNetwork
  (sbe,mem) <- createSAWBackend g dl
  k sbe mem

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

lssTest :: FilePath -> (L.Module -> PropertyM IO ()) -> (Args, Property)
lssTest bc act = test 1 False bc $ act =<< testMDL (bc <.> "bc")

testEachBackend :: FilePath
                -> 
                                ( L.Module
                               -> String
                               -> SBECreateFn
                               -> PropertyM IO ())
                -> (Args,Property)
testEachBackend nm f = do
  test 1 False nm $ do
    mdl <- testMDL (nm <.> "bc")
    f mdl "bitblast.old" createOldBuddyModel
    f mdl "dag.old" createOldDagModel
    f mdl "bitblast" createBuddyModel
    f mdl "dag" createDagModel
    f mdl "SAW" createSAWModel

-- | Run test on all backends
lssTestAll :: Int
           -> String
           -> [String]
           -> Maybe Int  -- ^ Expected number of error paths.
           -> Maybe Integer -- ^ Expected return value
           -> (Args,Property)
lssTestAll v nm args elen erv =
  testEachBackend nm $ \mdl backendName createFn -> do
    run $ putStrLn $ unwords ["--- Backend:", backendName,"---"]
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
