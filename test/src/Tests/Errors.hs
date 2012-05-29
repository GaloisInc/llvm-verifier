{- |
Module           : $Header$
Description      : Error path and error handling tests
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}

module Tests.Errors (errorTests) where

import           Test.QuickCheck
import           Tests.Common

errorTests :: [(Args, Property)]
errorTests =
  [
    lssTest 0 "ctests/test-error-paths-all" $ \v cb -> do
      runTestLSSBuddy v cb [] $ chkLSS (Just 2) Nothing
      runTestLSSDag v cb []   $ chkLSS (Just 2) Nothing

  , lssTest 0 "ctests/test-error-paths-some" $ \v cb -> do
      runTestLSSBuddy v cb [] $ chkLSS (Just 1) (Just 0)
      runTestLSSDag v cb [] $ chkLSS (Just 1) (Just 0)

  , lssTest 0 "ctests/test-error-paths-some-more" $ \v cb -> do
      runTestLSSBuddy v cb [] $ chkLSS (Just 2) (Just 0)
      runTestLSSDag v cb []   $ chkLSS (Just 2) (Just 0)

  , lssTest 0 "ctests/test-error-paths-bad-mem-trivial" $ \v cb -> do
      runTestLSSBuddy v cb [] $ chkLSS (Just 1) Nothing
      runTestLSSDag v cb []   $ chkLSS (Just 1) Nothing

  , lssTest 0 "ctests/test-error-paths-bad-mem-symsize" $ \v cb -> do
      runTestLSSBuddy v cb [] $ chkLSS (Just 1) (Just 1)
       -- This seems to hang in GHCI but not from the command line =/
      runTestLSSDag v cb []   $ chkLSS Nothing (Just 1)

  , lssTest 0 "ctests/test-error-paths-bad-mem-diverge" $ \v cb -> do
      runTestLSSBuddy v cb [] $ chkLSS (Just 1) (Just 1)
      runTestLSSDag v cb []   $ chkLSS (Just 1) (Just 1)
  ]

--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests errorTests
