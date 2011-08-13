module Tests.Common where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Test.QuickCheck.Test as T
import System.FilePath

newtype FailMsg = FailMsg String
instance Show FailMsg where show (FailMsg s) = s

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
