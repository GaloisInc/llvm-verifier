{- |
Module           : $Header$
Description      : LLVM input/output tests
Stability        : provisional
Point-of-contact : atomb
-}

module Tests.IO (ioTests) where

import           Test.QuickCheck

import           Tests.Common

ioTests :: [(Args, Property)]
ioTests =
  [
    lssTest "ctests/test-call-printf" $ \cb -> do
      runTestLSSBuddy 0 cb [] Nothing (Just 3)
      runTestLSSDag   0 cb [] Nothing (Just 3)
      runTestLSSCommon createSAWModel 0 cb [] Nothing (Just 3)

  , lssTest "ctests/test-printf-str" $ \cb -> do
      runTestLSSBuddy 0 cb [] Nothing (Just 8)
      runTestLSSDag   0 cb [] Nothing (Just 8)
      runTestLSSCommon createSAWModel 0 cb [] Nothing (Just 8)
  ]

--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests ioTests
