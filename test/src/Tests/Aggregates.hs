{- |
Module           : $Header$
Description      : LLVM array tests
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}

module Tests.Aggregates (aggTests) where

import           LSS.SBEInterface
import           LSS.SBEBitBlast
import           LSS.Simulator
import           Test.QuickCheck
import           Tests.Common
import qualified Text.LLVM        as L

aggTests :: [(Args, Property)]
aggTests =
  [
    test 1 False "test-array-index-base"       $ arrayBaseIdx     1
  , test 1 False "test-array-index-offset"     $ arrayOffsetIdx   1
  , test 1 False "test-array-1d-initializer"   $ arrayInit1D      1
  , test 1 False "test-array-2d-initializer"   $ arrayInit2D      1
  , test 1 False "test-array-mat4x4-mult"      $ arrayMat4x4      1
  , test 1 False "test-struct-init-and-access" $ structInitAccess 1
  ]
  where
    arrayBaseIdx v        = t1 v "arr1" (Just 42)
    arrayOffsetIdx v      = t1 v "arr2" (Just 141)
    arrayInit1D v         = t1 v "onedim_init" (Just 3)
    arrayInit2D v         = t1 v "twodim_init" (Just 21)
    arrayMat4x4 v         = t2 v "matrix_mul_4x4" (Just 304)
    structInitAccess v    = psk v $ runStruct v structInitAccessImpl
    t1                    = mkNullaryTest "test-arrays.bc"
    t2                    = mkNullaryTest "test-mat4x4.bc"
    runStruct v           = runBitBlastSimTest v "test-structs.bc"
    mkNullaryTest fn v nm = psk v . chkNullaryCInt32Fn v fn (L.Symbol nm)

structInitAccessImpl :: StdBitBlastTest
structInitAccessImpl be = do
  callDefine (L.Symbol "struct_test") i64 []
  mrv <- getProgramReturnValue
  case mrv of
    Nothing -> dbugM "No return value (fail)" >> return False
    Just rv -> do
--       dbugTerm "rv" rv

      [L.Typed _ bx, L.Typed _ by, _] <- do
        withSBE $ \sbe -> termDecomp sbe [i32, i8, padTy 3] rv

--       dbugTerm "bx" bx
--       dbugTerm "by" by
      return $ bx `teq` 42 && by `teq` fromIntegral (fromEnum 'z')
  where
    close t = BitTermClosed (be, t)
    teq t v = close t `constTermEq` v


--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests aggTests
