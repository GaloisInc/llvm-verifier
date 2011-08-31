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
  [ test 1 False "printf" $ testPrintf 1
  ]
  where
    testPrintf v = runMain v "test-call-printf.bc" (Just 3)

--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests ioTests
