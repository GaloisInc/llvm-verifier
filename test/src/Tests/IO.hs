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
    test  1  False "printf"              $ testPrintf    1
  , test  1  False "printf-str"          $ testPrintfStr 1
  ]
  where
    testPrintf v = runMain' True v "test-call-printf.bc" (RV 3)
    testPrintfStr v = runMain' True v "test-printf-str.bc" (RV 8)

--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests ioTests
