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
    lssTest 0 "test-call-printf" $ \v cb -> do
      runTestLSSBuddy v cb [] $ chkLSS Nothing (Just 3)
      runTestLSSDag v cb []   $ chkLSS Nothing (Just 3)

  , lssTest 0 "test-printf-str" $ \v cb -> do
      runTestLSSBuddy v cb [] $ chkLSS Nothing (Just 8)
      runTestLSSDag v cb []   $ chkLSS Nothing (Just 8)
  ]

--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests ioTests
