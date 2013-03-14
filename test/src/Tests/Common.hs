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
  , ExecRsltHndlr
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

type AllMemModelTest =
  (Functor sbe)
  => Simulator sbe IO Bool

runAllMemModelTest :: Int -> FilePath -> AllMemModelTest -> PropertyM IO ()
runAllMemModelTest v bcFile act = do
  mdl <- run $ loadModule $ testsDir </> bcFile
  assert . and =<< do
    forAllMemModels v mdl $ \s m cb -> run $ do
      runSimulator cb s m defaultSEH
        Nothing (withVerbosity v act)

type RunLSSTest sbe = Int
                    -> L.Module
                    -> [String]
                    -> ExecRsltHndlr sbe Integer Bool
                    -> PropertyM IO ()

runTestLSSCommon :: Functor sbe
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
                          }


runTestLSSBuddy :: RunLSSTest (BitIO (BitMemory Lit) Lit)
runTestLSSBuddy =
  runTestLSSCommon "runTestLSSBuddy" $ \dl -> do
    be <- createBitEngine
    let sbe = let ?be = be in sbeBitBlast dl (buddyMemModel dl be)
        mem = buddyInitMemory (defaultMemGeom dl)
    return (sbe,mem)


runTestLSSDag :: RunLSSTest (BitIO (DagMemory Lit) Lit)
runTestLSSDag =
  runTestLSSCommon "runTestLSSDag" $ \dl -> do
    be <- createBitEngine
    (mm,mem) <- createDagMemModel dl be (defaultMemGeom dl)
    let sbe = let ?be = be in sbeBitBlast dl mm
    return (sbe,mem)

runTestSAWBackend :: RunLSSTest (SAWBackend s)
runTestSAWBackend =
  runTestLSSCommon "runTestLSSDag" $ \dl -> do
    createSAWBackend dl (defaultMemGeom dl)

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


type SBEPropM a = forall sbe . Functor sbe => 
  SBE sbe -> SBEMemory sbe -> Codebase sbe -> PropertyM IO a

forAllMemModels :: forall a. Int -> L.Module -> SBEPropM a -> PropertyM IO [a]
forAllMemModels _v mdl testProp = do
  sequence
    [ do be <- run createBitEngine
         let pair = createBuddyAll be dl (defaultMemGeom dl)
         runTest pair
    , do pair <- run $ do
           be <- createBitEngine
           createDagAll be dl (defaultMemGeom dl)
         runTest pair
    ]
 where dl = parseDataLayout (L.modDataLayout mdl)
       runTest (SBEPair sbe mem) = do
         cb <- run $ mkCodebase sbe dl mdl
         testProp sbe mem cb


chkBinCInt32Fn :: Maybe (Gen (Int32, Int32))
               -> Int
               -> FilePath
               -> L.Symbol
               -> (Int32 -> Int32 -> ExpectedRV Int32)
               -> PropertyM IO ()
chkBinCInt32Fn mgen v bc sym chkOp = do
  forAllM (maybe arbitrary id mgen) $ \(x,y) -> do
    runCInt32Fn v bc sym [x, y] (fromIntegral <$> x `chkOp` y)

chkUnaryCInt32Fn :: Maybe (Gen Int32)
                 -> Int
                 -> FilePath
                 -> L.Symbol
                 -> (Int32 -> ExpectedRV Int32)
                 -> PropertyM IO ()
chkUnaryCInt32Fn mgen v bc sym chkOp =
  forAllM (maybe arbitrary id mgen) $ \x -> do
    runCInt32Fn v bc sym [x] (fromIntegral <$> chkOp x)

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
  mdl <- run $ loadModule $ testsDir </> bcFile
  void $ forAllMemModels v mdl $ \s m cb -> do
    run $ do
      runSimulator cb s m defaultSEH Nothing $ withVerbosity v $ do
        sbe <- gets symBE
        args <- forM cargs $ \x -> liftSBE $ termInt sbe 32 $ fromIntegral x
        callDefine_ sym (Just i32) ((IntType 32,) <$> args)
        let fn rv = asSignedInteger sbe 32 rv
        mrv <- fmap fn <$> getProgramReturnValue
        case erv of
          RV chk ->
            case mrv of
              Nothing -> fail "Missing return value"
              Just Nothing -> 
                fail "Symbolic return value when constant expected."
              Just (Just val)
                | val == chk -> return ()
                | otherwise -> 
                  fail $ "Expected " ++ show chk ++ ", got " ++ show val
          VoidRV ->
            case mrv of
              Nothing -> return ()
              Just{} -> fail $ "Received return value when none expected."
          AllPathsErr ->
            case mrv of
              Nothing -> return ()
              Just{} -> fail "Received return value when all paths were expected to error."

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
runMainVoid v bc = psk v $ do
  mdl <- run $ loadModule $ testsDir </> bc
  void $ forAllMemModels 0 mdl $ \s m cb -> run $ do
    runSimulator cb s m defaultSEH Nothing $ withVerbosity 0 $ do
      callDefine_ (L.Symbol "main") Nothing []