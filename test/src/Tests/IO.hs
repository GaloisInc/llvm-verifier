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
    testPrintf v = runMain v "test-call-printf.bc" (Just 3)
    testPrintfStr v = runMain v "test-printf-str.bc" (Just 8)
