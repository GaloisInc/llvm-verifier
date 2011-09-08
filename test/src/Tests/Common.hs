{-# LANGUAGE ViewPatterns #-}

module Tests.Common where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Data.Int
import           LSS.Execution.Codebase
import           LSS.Execution.Common
import           LSS.Execution.Utils
import           LSS.LLVMUtils
import           LSS.SBEBitBlast
import           LSS.Simulator
import           System.FilePath
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Text.LLVM                     ((=:))
import           Verinf.Symbolic.Common        (ConstantProjection(..), Lit, createBitEngine)
import           Verinf.Symbolic.Lit.DataTypes (BitEngine)
import qualified Test.QuickCheck.Test          as T
import qualified Text.LLVM                     as L

newtype FailMsg = FailMsg String
instance Show FailMsg where show (FailMsg s) = s

padTy :: Int -> L.Type
padTy bytes = L.Array (fromIntegral bytes) i8

supportDir :: FilePath
supportDir = "test" </> "src" </> "support"

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

chkRslt :: ConstantProjection t => L.Symbol -> Maybe Integer -> Maybe t -> PropertyM IO ()
chkRslt _ (Just chk) (Just (getSVal -> Just v))
  | v == chk  = assert True
  | otherwise = assertMsg False $ "Expected " ++ show chk ++ ", got " ++ show v
chkRslt _ Nothing Nothing
  = assert True
chkRslt sym _ _
  = assertMsg False $ show (L.ppSymbol sym) ++ ": unexpected return value"

constTermEq :: ConstantProjection t => t -> Integer -> Bool
constTermEq (getSVal -> Just v) = (==v)
constTermEq _                   = const False

chkBinCInt32Fn :: Maybe (Gen (Int32, Int32))
               -> Int
               -> FilePath
               -> L.Symbol
               -> (Int32 -> Int32 -> Maybe Int32)
               -> PropertyM IO ()
chkBinCInt32Fn mgen v bcFile sym chkOp = do
  forAllM (maybe arbitrary id mgen) $ \(x,y) -> do
    chkRslt sym (fromIntegral <$> x `chkOp` y)
      =<< run (runCInt32Fn v bcFile sym [x, y])

chkUnaryCInt32Fn :: Maybe (Gen Int32)
                 -> Int
                 -> FilePath
                 -> L.Symbol
                 -> (Int32 -> Maybe Int32)
                 -> PropertyM IO ()
chkUnaryCInt32Fn mgen v bcFile sym chkOp =
  forAllM (maybe arbitrary id mgen) $ \x -> do
    chkRslt sym (fromIntegral <$> chkOp x)
      =<< run (runCInt32Fn v bcFile sym [x])

chkNullaryCInt32Fn :: Int -> FilePath -> L.Symbol -> Maybe Int32 -> PropertyM IO ()
chkNullaryCInt32Fn v bcFile sym chkVal =
  chkRslt sym (fromIntegral <$> chkVal)
    =<< run (runCInt32Fn v bcFile sym [])

runCInt32Fn :: Int -> FilePath -> L.Symbol -> [Int32] -> IO (Maybe (BitTermClosed Lit))
runCInt32Fn v bcFile sym cargs = runBitBlastSim v bcFile defaultSEH $ \be -> do
  args <- withSBE $ \sbe -> mapM (termInt sbe 32 . fromIntegral) cargs
  callDefine_ sym i32 (return $ map ((=:) i32) args)
  rv <- getProgramReturnValue
  return $ BitTermClosed . (,) be <$> rv

type StdMemory     = BitMemory Lit
type StdBitEngine     = BitEngine Lit
type StdBitBlastSim mem a = Simulator (BitIO mem Lit) IO a
type StdBitBlastTest = StdBitEngine -> StdBitBlastSim StdMemory Bool
type StdBitBlastSEH mem = SEH (BitIO mem Lit) IO

runBitBlastSim :: Int
               -> FilePath
               -> StdBitBlastSEH StdMemory
               -> (StdBitEngine -> StdBitBlastSim StdMemory a)
               -> IO a
runBitBlastSim v bcFile seh act = do
  (cb, be, backend, mem) <- stdBitBlastInit bcFile
  runSimulator cb backend mem stdBitBlastLift seh Nothing $ withVerbosity v (act be)

runBitBlastSimTest :: Int
                   -> FilePath
                   -> StdBitBlastSEH StdMemory
                   -> StdBitBlastTest
                   -> PropertyM IO ()
runBitBlastSimTest v bcFile seh = assert <=< run . runBitBlastSim v bcFile seh

stdBitBlastInit :: FilePath -> IO ( Codebase
                                  , StdBitEngine
                                  , BitBlastSBE StdMemory Lit
                                  , StdMemory
                                  )
stdBitBlastInit bcFile = do
  cb <- loadCodebase $ supportDir </> bcFile
  be <- createBitEngine
  let lc      = cbLLVMCtx cb
      mg      = defaultMemGeom lc
  --(mm, mem) <- dagMemModel lc be stack code gdata heap
  let mm  = buddyMemModel lc be
      mem = buddyInitMemory mg
  return (cb, be, sbeBitBlast lc be mm, mem)

stdBitBlastLift :: BitIO m l a -> Simulator sbe IO a
stdBitBlastLift = SM . lift . lift . liftSBEBitBlast

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

runMain :: Int -> FilePath -> Maybe Int32 -> PropertyM IO ()
runMain v bc = psk v . chkNullaryCInt32Fn v bc (L.Symbol "main")


