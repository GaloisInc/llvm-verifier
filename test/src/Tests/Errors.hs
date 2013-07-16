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
  [ lssTest "ctests/test-error-paths-all" $ \mdl -> do
      runTestLSSBuddy 0 mdl [] (Just 2) Nothing
      runTestLSSDag   0 mdl [] (Just 2) Nothing

  , lssTest "ctests/test-error-paths-some" $ \cb -> do
      runTestLSSBuddy 0 cb [] (Just 1) (Just 0)
      runTestLSSDag   0 cb [] (Just 1) (Just 0)

  , lssTest "ctests/test-error-paths-some-more" $ \cb -> do
      runTestLSSBuddy 0 cb [] (Just 2) (Just 0)
      runTestLSSDag   0 cb [] (Just 2) (Just 0)

  , lssTest "ctests/test-error-paths-bad-mem-trivial" $ \cb -> do
      runTestLSSBuddy 0 cb [] (Just 1) Nothing
      runTestLSSDag   0 cb [] (Just 1) Nothing

  , lssTest "ctests/test-error-paths-bad-mem-symsize" $ \cb -> do
      runTestLSSBuddy 0 cb [] (Just 1) (Just 1)
       -- This seems to hang in GHCI but not from the command line =/
      runTestLSSDag   0 cb [] Nothing  (Just 1)

  , lssTest "ctests/test-error-paths-bad-mem-diverge" $ \cb -> do
      runTestLSSBuddy 0 cb [] (Just 1) (Just 1)
      runTestLSSDag   0 cb [] (Just 1) (Just 1)
  ]

--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests errorTests