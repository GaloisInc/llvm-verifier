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
  , view
  ) where

import           Control.Applicative
import           Control.Lens hiding (act, (<.>))
import           Control.Monad 
import           Control.Monad.State (gets)
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
  mdl <- run $ loadModule $ testsDir </> bcFile
  let dl = parseDataLayout (L.modDataLayout mdl)
  let runTest :: (Functor sbe, Ord (SBETerm sbe)) => (SBE sbe,SBEMemory sbe) -> PropertyM IO a
      runTest (sbe,mem) = do
        cb <- run (mkCodebase sbe dl mdl)
        run $ do
          runSimulator cb sbe mem defaultSEH Nothing $ withVerbosity v $ testProp
  sequence
    [ 
{-
runTest =<< run (createBuddyModel dl)
    , runTest =<< run (createDagModel dl)
-}
      runTest =<< run (createSAWModel dl)
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

type RunLSSTest sbe = Int
                    -> L.Module
                    -> [String]
                    -> ExecRsltHndlr sbe Integer Bool
                    -> PropertyM IO ()

runTestLSSCommon :: (Functor sbe, Ord (SBETerm sbe))
                 => String
                 -> (DataLayout -> IO (SBE sbe, SBEMemory sbe))
                 -> RunLSSTest sbe
runTestLSSCommon nm createFn v mdl argv' hndlr = do
   when (v >= 2) $ run $ putStrLn nm
   assert =<< run m
  where m = do let dl = parseDataLayout (L.modDataLayout mdl)
               (sbe, mem) <- createFn dl
               cb <- mkCodebase sbe dl mdl
               rslt <- lssImpl sbe mem cb argv' cmdLineOpts
               hndlr sbe mem rslt
        dbugLvl     = DbugLvl (fromIntegral v)
        cmdLineOpts = LSS { dbug = dbugLvl
                          , argv = "" 
                          , backend = Nothing
                          , errpaths = False
                          , xlate = False
                          , mname = Nothing
                          , startDebugger = False
                          , satBranches = False
                          }

-- | Create buddy backend and initial memory.
createBuddyModel :: DataLayout -> IO (SBE (BitIO (BitMemory Lit) Lit), BitMemory Lit)
createBuddyModel dl = do
  be <- createBitEngine
  let sbe = let ?be = be in sbeBitBlast dl (buddyMemModel dl be)
      mem = buddyInitMemory (defaultMemGeom dl)
  return (sbe,mem)

-- | Create buddy backend and initial memory.
createDagModel :: DataLayout -> IO (SBE (BitIO (DagMemory Lit) Lit), DagMemory Lit)
createDagModel dl = do
  be <- createBitEngine
  (mm,mem) <- createDagMemModel dl be (defaultMemGeom dl)
  let sbe = let ?be = be in sbeBitBlast dl mm
  return (sbe,mem)

createSAWModel :: DataLayout -> IO (SBE (SAWBackend s Lit), SAWMemory s)
createSAWModel dl = do
  be <- createBitEngine
  createSAWBackend be dl (defaultMemGeom dl)

runTestLSSBuddy :: RunLSSTest (BitIO (BitMemory Lit) Lit)
runTestLSSBuddy = runTestLSSCommon "runTestLSSBuddy" createBuddyModel

runTestLSSDag :: RunLSSTest (BitIO (DagMemory Lit) Lit)
runTestLSSDag = runTestLSSCommon "runTestLSSDag" createDagModel

runTestSAWBackend :: RunLSSTest (SAWBackend s Lit)
runTestSAWBackend = runTestLSSCommon "runTestLSSDag" createSAWModel

lssTest :: Int -> FilePath -> (Int -> L.Module -> PropertyM IO ()) -> (Args, Property)
lssTest v bc act = test 1 False bc $ act v =<< testMDL (bc <.> "bc")

-- | Run test on all backends
lssTestAll :: Int
           -> String
           -> [String]
           -> (forall sbe . ExecRsltHndlr sbe Integer Bool)
           -> (Args,Property)
lssTestAll v nm args hndlr =
  lssTest v nm $ \_ mdl -> do
    runTestLSSBuddy v mdl args hndlr
    runTestLSSDag v mdl args hndlr
    --runTestSAWBackend v mdl args hndlr

-- TODO: At some point we may want to inspect error paths and ensure
-- that they are the /right/ error paths, rather just checking the
-- count!.  We'll need something better than FailRsn to do that, though.
chkLSS :: Maybe Int
       -> Maybe Integer
       -> ExecRsltHndlr sbe Integer Bool
chkLSS mepsLen mexpectedRV _ _ execRslt = do
  let chkLen eps =
        case mepsLen of
          Nothing -> return ()
          Just epsLen ->
            unless (length eps == epsLen) $ do
              fail $ "Expected " ++ show epsLen ++ " error paths, but found " 
                       ++ show (length eps)
  case execRslt of
    ConcRV eps _mm r -> do
      chkLen eps
      case mexpectedRV of
        Nothing -> fail "Unexpected return value"
        Just expectedRV ->
          unless (expectedRV == r) $ do
            fail $ "Expected " ++ show expectedRV ++ " return value, but found " ++ show r  
    NoMainRV eps _mm -> do
      chkLen eps
      case mexpectedRV of
        Nothing -> return ()
        Just{} -> fail $ "Missing return value"
    SymRV{} -> fail "Unexpected sym exec result"
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