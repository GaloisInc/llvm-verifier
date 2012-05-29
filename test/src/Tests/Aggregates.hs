{- |
Module           : $Header$
Description      : LLVM array tests
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Tests.Aggregates (aggTests) where

import           Control.Applicative
import           LSS.LLVMUtils
import           LSS.SBEInterface
import           LSS.Simulator
import           Test.QuickCheck
import           Tests.Common
import qualified Text.LLVM        as L

aggTests :: [(Args, Property)]
aggTests =
  [
    test 1 False "test-array-index-base"            $ arrayBaseIdx     1
  , test 1 False "test-array-index-offset"          $ arrayOffsetIdx   1
  , test 1 False "test-array-1d-initializer"        $ arrayInit1D      1
  , test 1 False "test-array-2d-initializer"        $ arrayInit2D      1
  , test 1 False "test-array-mat4x4-mult"           $ arrayMat4x4      1
  , test 1 False "test-struct-init-and-access"      $ structInitAccess 1
  , test 1 False "test-array-of-structs"            $ structArray      1
  , lssTest 0 "ctests/test-struct-member-indirect-call" $ \v cb -> do
      runTestLSSBuddy v cb [] $ chkLSS Nothing (Just 0)
      runTestLSSDag v cb []   $ chkLSS Nothing (Just 0)
  ]
  where
    arrayBaseIdx v        = t1 v "arr1" (RV 42)
    arrayOffsetIdx v      = t1 v "arr2" (RV 141)
    arrayInit1D v         = t1 v "onedim_init" (RV 3)
    arrayInit2D v         = t1 v "twodim_init" (RV 21)
    arrayMat4x4 v         = t2 v "matrix_mul_4x4" (RV 304)
    structInitAccess v    = psk v $ runStruct v structInitAccessImpl
    structArray v         = psk v $ runStruct v structArrayImpl
    t1                    = mkNullaryTest "test-arrays.bc"
    t2                    = mkNullaryTest "test-mat4x4.bc"
    mkNullaryTest bc v nm = psk v . chkNullaryCInt32Fn v (commonCB bc) (L.Symbol nm)
    runStruct v           = \(f :: AllMemModelTest) ->
                              runAllMemModelTest v (commonCB "test-structs.bc") f

structInitAccessImpl :: AllMemModelTest
structInitAccessImpl = do
  callDefine_ (L.Symbol "struct_test") i64 (return [])
  mrv <- getProgramReturnValue
  case mrv of
    Nothing -> dbugM "No return value (fail)" >> return False
    Just rv -> do
      [L.Typed _ bx, L.Typed _ by, _] <- do
        withSBE $ \sbe -> termDecomp sbe [i32, i8, padTy 3] rv
      bxc <- withSBE' (`closeTerm` bx)
      byc <- withSBE' (`closeTerm` by)
      return $ bxc `constTermEq` 42
               &&
               byc `constTermEq` fromIntegral (fromEnum 'z')

structArrayImpl :: AllMemModelTest
structArrayImpl = do
  callDefine_ (L.Symbol "struct_test_two") i32 (return [])
  mrv <- getProgramReturnValue
  case mrv of
    Nothing -> dbugM "No return value (fail)" >> return False
    Just rv -> (`constTermEq` 1) <$> withSBE' (`closeTerm` rv)

--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests aggTests
