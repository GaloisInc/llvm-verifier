{- |
Module           : $Header$
Description      : LLVM array tests
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}

module Tests.Array (arrayTests) where

import           Test.QuickCheck
import           Tests.Common

arrayTests :: [(Args, Property)]
arrayTests =
  [
    test 1 False "test-array-index" $ arrayIdx 5
  ]
  where
    arrayIdx v = runMain v "test-array-simple.bc" (Just 42)

--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests arrayTests
