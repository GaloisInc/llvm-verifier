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
import qualified Text.LLVM as L

arrayTests :: [(Args, Property)]
arrayTests =
  [
    test 1 False "test-array-index-base"      $ arrayBaseIdx   1
  , test 1 False "test-array-index-offset"    $ arrayOffsetIdx 1
  , test 1 False "test-array-1d-initializer"  $ arrayInit1D    1
--   , test 1 False "test-array-2d-initializer"  $ arrayInit2D    6
  ]
  where
    arrayBaseIdx v     = arraySimple v "arr1" (Just 42)
    arrayOffsetIdx v   = arraySimple v "arr2" (Just 141)
    arrayInit1D v      = arraySimple v "onedim_init" (Just 3)
    arrayInit2D v      = arraySimple v "twodim_init" (Just 21)
    arraySimple v name = psk v . chkNullaryCInt32Fn v "test-array-simple.bc" (L.Symbol name)

--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests arrayTests
