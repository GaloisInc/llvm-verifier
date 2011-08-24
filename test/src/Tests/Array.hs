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
  , test 1 False "test-array-2d-initializer"  $ arrayInit2D    1
  , test 1 False "test-array-matmult4x4"      $ incomplete $ arrayMat4x4    1
  ]
  where
    arrayBaseIdx v        = t1 v "arr1" (Just 42)
    arrayOffsetIdx v      = t1 v "arr2" (Just 141)
    arrayInit1D v         = t1 v "onedim_init" (Just 3)
    arrayInit2D v         = t1 v "twodim_init" (Just 21)
    arrayMat4x4 v         = t2 v "matrix_mul_4x4" (Just 304)
    t1                    = mkNullaryTest "test-arrays.bc"
    t2                    = mkNullaryTest "test-mat4x4.bc"
    mkNullaryTest fn v nm = psk v . chkNullaryCInt32Fn v fn (L.Symbol nm)

--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests arrayTests
