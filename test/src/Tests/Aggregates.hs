{- |
Module           : $Header$
Description      : LLVM array tests
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Tests.Aggregates (aggTests) where

import           Control.Applicative
import           Control.Monad.State
import           Test.QuickCheck
import           Tests.Common

import           Verifier.LLVM.Backend
import           Verifier.LLVM.Simulator

aggTests :: [(Args, Property)]
aggTests =
  [
    test 1 False "test-array-index-base"            $ arrayBaseIdx     1
  , test 1 False "test-array-index-offset"          $ arrayOffsetIdx   1
  , test 1 False "test-array-1d-initializer"        $ do
      let v = 1 -- verbosity
      testArrays v "onedim_init" (RV 3)
  , test 1 False "test-array-2d-initializer"        $ arrayInit2D      1
  , test 1 False "test-array-mat4x4-mult"           $ do
      let v = 1 -- verbosity
      chkNullaryCInt32Fn v "test-mat4x4.bc" (Symbol "matrix_mul_4x4") (RV 304)
  , test 1 False "test-struct-init-and-access"      $ 
      runStruct 1 structInitAccessImpl
  , test 1 False "test-array-of-structs"            $
      runStruct 1 structArrayImpl
  {-
  , lssTest 0 "ctests/test-struct-member-indirect-call" $ \v cb -> do
      runTestLSSBuddy v cb [] $ chkLSS Nothing (Just 0)
      runTestLSSDag v cb []   $ chkLSS Nothing (Just 0)
  -}
  ]
  where
    arrayBaseIdx v        = testArrays v "arr1" (RV 42)
    arrayOffsetIdx v      = testArrays v "arr2" (RV 141)
    arrayInit2D v         = testArrays v "twodim_init" (RV 21)
    testArrays v nm       = chkNullaryCInt32Fn v "test-arrays.bc" (Symbol nm)
    runStruct v           = \(f :: AllMemModelTest) ->
                              runAllMemModelTest v "test-structs.bc" f

structInitAccessImpl :: AllMemModelTest
structInitAccessImpl = do
  dl <- withDL id
  let si = mkStructInfo dl False [i32, i8]
  void $ callDefine (Symbol "struct_test") (Just (StructType si)) []
  mrv <- getProgramReturnValue
  case mrv of
    Nothing -> dbugM "No return value (fail)" >> return False
    Just rv -> do
      sbe <- gets symBE
      bx <- liftSBE $ applyTypedExpr sbe (GetStructField si rv 0)
      by <- liftSBE $ applyTypedExpr sbe (GetStructField si rv 1)
      let bxc = asSignedInteger sbe 32 bx
          byc = asSignedInteger sbe  8 by
      return $ bxc == Just 42
               &&
               byc == Just (fromIntegral (fromEnum 'z'))

structArrayImpl :: AllMemModelTest
structArrayImpl = do
  void $ callDefine (Symbol "struct_test_two") (Just i32) []
  mrv <- getProgramReturnValue
  case mrv of
    Nothing -> dbugM "No return value (fail)" >> return False
    Just rv -> (`constTermEq` 1) <$> withSBE' (\s -> asSignedInteger s 32 rv)

--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests aggTests
