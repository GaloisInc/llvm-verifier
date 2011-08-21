{-# LANGUAGE ViewPatterns #-}

module Tests.Common where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Data.Int
import           LSS.Execution.Codebase
import           LSS.Execution.Common
import           LSS.Execution.Utils
import           LSS.SBEBitBlast
import           LSS.Simulator
import           System.FilePath
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Text.LLVM               ((=:))
import           Verinf.Symbolic.Common  (ConstantProjection(..))
import           Verinf.Symbolic.Common  (Lit, createBitEngine)
import qualified Test.QuickCheck.Test    as T
import qualified Text.LLVM               as L

newtype FailMsg = FailMsg String
instance Show FailMsg where show (FailMsg s) = s

i32 :: L.Type
i32 = L.iT 32

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

chkBinCInt32Fn :: Int -> FilePath -> L.Symbol -> (Int32 -> Int32 -> Maybe Int32) -> PropertyM IO ()
chkBinCInt32Fn v bcFile sym chkOp = do
  forAllM arbitrary $ \(x,y) -> do
    chkRslt sym (fromIntegral <$> x `chkOp` y)
      =<< run (runCInt32Fn v bcFile sym [x, y])

chkUnaryCInt32Fn :: Int -> FilePath -> L.Symbol -> (Int32 -> Maybe Int32) -> PropertyM IO ()
chkUnaryCInt32Fn v bcFile sym chkOp =
  forAllM arbitrary $ \x -> do
    chkRslt sym (fromIntegral <$> chkOp x)
      =<< run (runCInt32Fn v bcFile sym [x])

chkNullaryCInt32Fn :: Int -> FilePath -> L.Symbol -> Maybe Int32 -> PropertyM IO ()
chkNullaryCInt32Fn v bcFile sym chkVal =
  chkRslt sym (fromIntegral <$> chkVal)
    =<< run (runCInt32Fn v bcFile sym [])

runCInt32Fn :: Int -> FilePath -> L.Symbol -> [Int32] -> IO (Maybe (BitTermClosed Lit))
runCInt32Fn v bcFile sym cargs = do
  cb <- loadCodebase $ supportDir </> bcFile
  be <- createBitEngine

  let lc            = LLVMContext 32 (error "LLVM Context has no ident -> type alias map defined")
      backend       = sbeBitBlast lc be
      mem           = sbeBitBlastMem (stkSt, stkEnd) (codeSt, codeEnd) (heapSt, heapEnd)
                      where
                        defaultSz         = 2^(16 :: Int)
                        ext st len        = (st, st + len)
                        (stkSt, stkEnd)   = ext 0 defaultSz
                        (codeSt, codeEnd) = ext stkEnd defaultSz
                        (heapSt, heapEnd) = ext codeEnd defaultSz

  runSimulator cb backend mem (SM . lift . liftSBEBitBlast) $ withVerbosity v $ do
    args <- withSBE $ \sbe -> mapM (termInt sbe 32 . fromIntegral) cargs
    callDefine sym i32 $ map (\x -> i32 =: x) args
    rv <- getProgramReturnValue
    return $ BitTermClosed . (,) be <$> rv

psk :: Int -> PropertyM IO () -> PropertyM IO ()
psk v act = if (v > 0) then act else disabledWarn
  where disabledWarn = run $ putStrLn "Warning: Next test is currently DISABLED!"

runMain :: Int -> FilePath -> Maybe Int32 -> PropertyM IO ()
runMain v bc = psk v . chkNullaryCInt32Fn v bc (L.Symbol "main")


