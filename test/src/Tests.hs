{-# LANGUAGE Rank2Types #-}

module Main where

import Control.Monad
import System.Environment   (getArgs)
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Test

import Tests.AES
import Tests.Aggregates
import Tests.Errors
import Tests.IO
import Tests.MemModel
import Tests.PrimOps
import Tests.Symbolic

main :: IO ()
main = do
  tags <- getArgs
  runTests (\(t, _) -> if null tags then True else t `elem` tags)

runTests :: (forall a. ((String, a) -> Bool)) -> IO ()
runTests chooseTest = do
  results <- fmap concat $ forM qcProps $ \(desc, props) -> do
             putStrLn $ "=== " ++ desc ++ " ==="
             mapM (uncurry quickCheckWithResult) props
  when (noOfTests /= noOfSelected) $
    putStrLn $ "Selected " ++ show noOfSelected ++ " of "
               ++ show noOfTests ++ " test categories."
  if all isSuccess results
    then putStrLn "All tests successful."     >> exitWith ExitSuccess
    else putStrLn "One or more tests failed." >> exitWith (ExitFailure 1)
  where
    noOfTests    = length allTests
    noOfSelected = length qcProps
    qcProps      = filter chooseTest allTests
    allTests     = [ ("PrimOps", primOpTests)
                   , ("Aggregates", aggTests)
                   , ("Symbolic", symTests)
                   , ("AES", aesTests)
                   , ("I/O", ioTests)
                   , ("MemModel", memModelTests)
                   , ("Errors", errorTests)
                   ]
