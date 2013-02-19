{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ViewPatterns         #-}
module Tests.Common 
  ( module Tests.Common
  , ExecRsltHndlr
  , PropertyM
  ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad hiding (mapM)
import           Data.Int
import           Data.LLVM.TargetData
import           Data.Traversable (mapM)
import           LSS.Execution.Codebase
import           LSS.Execution.Common
import           LSS.Execution.Utils
import           LSS.LLVMUtils
import           LSS.Simulator
import           LSSImpl
import           System.FilePath
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Verifier.LLVM.BitBlastBackend
import           Verifier.LLVM.SAWBackend

import           Text.LLVM                     ((=:))
import           Verinf.Symbolic               (Lit, createBitEngine)
import           Verinf.Symbolic.Lit.DataTypes (BitEngine)
import qualified Test.QuickCheck.Test          as T
import qualified Text.LLVM                     as L
import Prelude hiding (mapM)

data ExpectedRV a = AllPathsErr | VoidRV | RV a deriving Functor

newtype FailMsg = FailMsg String
instance Show FailMsg where show (FailMsg s) = s

padTy :: Int -> L.Type
padTy bytes = L.Array (fromIntegral bytes) i8

supportDir :: FilePath
supportDir = "test" </> "src" </> "support"

testsDir :: FilePath
testsDir = supportDir

commonCB :: FilePath -> PropertyM IO Codebase
commonCB bcFile = run $ loadCodebase $ supportDir </> bcFile

testCB :: FilePath -> PropertyM IO Codebase
testCB bcFile = run $ loadCodebase $ testsDir </> bcFile

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

chkRslt :: L.Symbol -> ExpectedRV Integer -> Maybe (Maybe Integer) -> PropertyM IO ()
chkRslt _ (RV chk) (Just (Just v))
  | v == chk  = assert True
  | otherwise = assertMsg False $ "Expected " ++ show chk ++ ", got " ++ show v
chkRslt _ VoidRV Nothing
  = assert True
chkRslt _ AllPathsErr Nothing
  = assert True
chkRslt sym _ _
  = assertMsg False $ show (L.ppSymbol sym) ++ ": unexpected return value"

constTermEq :: Maybe (Int,Integer) -> Integer -> Bool
constTermEq (Just (_,v)) = (==v)
constTermEq _                   = const False

type AllMemModelTest =
  (Functor sbe)
  => Simulator sbe IO Bool

runAllMemModelTest :: Int -> PropertyM IO Codebase -> AllMemModelTest -> PropertyM IO ()
runAllMemModelTest v getCB act = do
  cb <- getCB
  assert . and =<< do
    forAllMemModels v cb $ \s m -> run $
      runSimulator cb s m defaultSEH
        Nothing (withVerbosity v act)

type CreateSBEFn sbe =
       BitEngine Lit -> LLVMContext -> MemGeom
                     -> IO (SBE sbe, SBEMemory sbe)

runTestLSSCommon :: Functor sbe
                 => String
                 -> CreateSBEFn sbe
                 -> RunLSSTest sbe
runTestLSSCommon nm createFn v cb argv' hndlr = do
   when (v >= 2) $ run $ putStrLn nm
   assert =<< run m
  where m = do be <- createBitEngine
               (sbe, mem) <- createFn be (lc cb) (defaultMemGeom (lc cb))
               rslt       <- lssImpl sbe mem cb argv' cmdLineOpts
               hndlr sbe mem rslt
        lc          = cbLLVMCtx
        dbugLvl     = DbugLvl (fromIntegral v)
        cmdLineOpts = LSS { dbug = dbugLvl
                          , argv = "" 
                          , backend = Nothing
                          , errpaths = False
                          , xlate = False
                          , mname = Nothing
                          }

type RunLSSTest sbe = Int
                    -> Codebase
                    -> [String]
                    -> ExecRsltHndlr sbe Integer Bool
                    -> PropertyM IO ()

runTestLSSBuddy :: RunLSSTest (BitIO (BitMemory Lit) Lit)
runTestLSSBuddy = runTestLSSCommon "runTestLSSBuddy" createBuddyAll 

runTestLSSDag :: RunLSSTest (BitIO (DagMemory Lit) Lit)
runTestLSSDag = runTestLSSCommon "runTestLSSDag" createDagAll

runTestSAWBackend :: RunLSSTest (SAWBackend s)
runTestSAWBackend = runTestLSSCommon "runTestLSSDag" (\_ -> createSAWBackend)

lssTest :: Int -> String -> (Int -> Codebase -> PropertyM IO ()) -> (Args, Property)
lssTest v bc act = test 1 False bc $ act v =<< testCB (bc <.> "bc")

-- | Run test on all backends
lssTestAll :: Int
           -> String
           -> [String]
           -> (forall sbe . ExecRsltHndlr sbe Integer Bool)
           -> (Args,Property)
lssTestAll vi nm args hndlr =
  lssTest vi nm $ \v cb -> do
    runTestLSSBuddy v cb args hndlr
    runTestLSSDag v cb args hndlr
    --runTestSAWBackend v cb args hndlr

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


type SBEPropM a =
  forall mem.
  SBE (BitIO mem Lit)          -- ^ SBE used for the given prop
  -> SBEMemory (BitIO mem Lit) -- ^ Initial memory for prop
  -> PropertyM IO a

type MemCreator mem =
  LLVMContext -> BitEngine Lit -> MemGeom -> IO (BitBlastMemModel mem Lit, mem)

forAllMemModels :: forall a. Int -> Codebase -> SBEPropM a -> PropertyM IO [a]
forAllMemModels _v cb testProp = do
  sequence
    [ runMemTest "buddy" createBuddyMemModel
    , runMemTest "dag"   createDagMemModel
    ]
  where
    runMemTest :: String -> MemCreator mem -> PropertyM IO a
    runMemTest _lbl act = do
--       run $ putStrLn $ "forAllMemModels: " ++ lbl
      be         <- run createBitEngine
      (sbe, mem) <- first (let ?be = be in sbeBitBlast lc) <$> run (act lc be mg)
      testProp sbe mem
      where
        lc = cbLLVMCtx cb
        mg = defaultMemGeom lc

chkBinCInt32Fn :: Maybe (Gen (Int32, Int32))
               -> Int
               -> PropertyM IO Codebase
               -> L.Symbol
               -> (Int32 -> Int32 -> ExpectedRV Int32)
               -> PropertyM IO ()
chkBinCInt32Fn mgen v getCB sym chkOp = do
  forAllM (maybe arbitrary id mgen) $ \(x,y) -> do
    mapM_ (chkRslt sym (fromIntegral <$> x `chkOp` y))
      =<< runCInt32Fn v getCB sym [x, y]

chkUnaryCInt32Fn :: Maybe (Gen Int32)
                 -> Int
                 -> PropertyM IO Codebase
                 -> L.Symbol
                 -> (Int32 -> ExpectedRV Int32)
                 -> PropertyM IO ()
chkUnaryCInt32Fn mgen v getCB sym chkOp =
  forAllM (maybe arbitrary id mgen) $ \x -> do
    mapM_ (chkRslt sym (fromIntegral <$> chkOp x))
      =<< runCInt32Fn v getCB sym [x]

chkNullaryCInt32Fn :: Int -> PropertyM IO Codebase -> L.Symbol -> ExpectedRV Int32 -> PropertyM IO ()
chkNullaryCInt32Fn v getCB sym chkVal =
  mapM_ (chkRslt sym (fromIntegral <$> chkVal)) =<< runCInt32Fn v getCB sym []

runCInt32Fn :: Int -> PropertyM IO Codebase -> L.Symbol -> [Int32] -> PropertyM IO [Maybe (Maybe Integer)]
runCInt32Fn v getCB sym cargs = do
  cb <- getCB
  forAllMemModels v cb $ \s m -> run $ do
    runSimulator cb s m defaultSEH Nothing $ withVerbosity v $ do
      args <- forM cargs $ \x -> withSBE (\sbe -> termInt sbe 32 $ fromIntegral x)
      callDefine_ sym i32 args
      let fn rv = withSBE' $ \sbe -> snd <$> asSignedInteger sbe rv
      mapM fn =<< getProgramReturnValue

runVoidFn :: Int -> PropertyM IO Codebase -> L.Symbol -> [Int32] -> PropertyM IO ()
runVoidFn v getCB sym cargs = do
  cb <- getCB
  _ <- forAllMemModels v cb $ \s m -> run $ do
    runSimulator cb s m defaultSEH Nothing $ withVerbosity v $ do
      args <- forM cargs $ \x -> withSBE (\sbe -> termInt sbe 32 $ fromIntegral x)
      callDefine_ sym voidTy args
  return ()

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
  psk v $ chkNullaryCInt32Fn (if quiet then 0 else v) (commonCB bc) (L.Symbol "main") erv

runMainVoid :: Int -> FilePath -> PropertyM IO ()
runMainVoid v bc = psk v $ runVoidFn 0 (commonCB bc) (L.Symbol "main") []
