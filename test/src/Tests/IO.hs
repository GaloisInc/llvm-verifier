{- |
Module           : $Header$
Description      : LLVM input/output tests
License          : Free for non-commercial use. See LICENSE.
Stability        : provisional
Point-of-contact : atomb
-}

module Tests.IO (ioTests) where

import           Test.Tasty
import           Tests.Common

ioTests :: [TestTree]
ioTests =
  [ lssTestAll "ctests/test-call-printf" [] Nothing (RV 3)
  , lssTestAll "ctests/test-printf-str"  [] Nothing (RV 8)
  ]
