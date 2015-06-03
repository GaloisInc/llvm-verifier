{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

{- |
Module           : $Header$
Description      : LLVM array tests
License          : Free for non-commercial use. See LICENSE.
Stability        : provisional
Point-of-contact : atomb
-}
module Tests.Aggregates (aggTests) where

import           Control.Monad
import           Control.Monad.State (liftIO, gets)

import           Test.Tasty
import qualified Test.Tasty.HUnit as HU

import Verifier.LLVM.Simulator
import Verifier.LLVM.Codebase.AST
import Verifier.LLVM.Backend

import Tests.Common


aggTests :: [TestTree]
aggTests =
  [ testArrays "test-array-index-base"     "test-arrays.bc" "arr1" (RV 42)
  , testArrays "test-array-index-offset"   "test-arrays.bc" "arr2" (RV 141)
  , testArrays "test-array-1d-initializer" "test-arrays.bc" "onedim_init" (RV 3)
  , testArrays "test-array-2d-initializer" "test-arrays.bc" "twodim_init" (RV 21)
  , testArrays "test-array-mat4x4-mult"    "test-mat4x4.bc" "matrix_mul_4x4" (RV 304)

  , runStruct "test-struct-init-and-access" "test-structs.bc" structInitAccessImpl
  , runStruct "test-array-of-structs"       "test-structs.bc" structArrayImpl

-- We appear to be missing the C source for this test....
--  , lssTestAll "ctests/test-struct-member-indirect-call" [] (RV 0)
  ]
  where
    testArrays gnm bc nm res = 
          forAllMemModels gnm bc $ \bkName v sbeCF mdlio ->
                HU.testCase bkName $ runTestSimulator v sbeCF mdlio $
                     runCInt32Fn (Symbol nm) [] res

    runStruct :: String -> String -> (forall sbe. Functor sbe => Simulator sbe IO ()) -> TestTree
    runStruct gnm bc m =
          forAllMemModels gnm bc $ \bkName v sbeCF mdlio ->
                HU.testCase bkName $ runTestSimulator v sbeCF mdlio m

structInitAccessImpl :: Functor sbe => Simulator sbe IO ()
structInitAccessImpl = do
  dl <- withDL id
  let si = mkStructInfo dl False [i32, i8]
  void $ callDefine (Symbol "struct_test") (Just (StructType si)) []
  mrv <- getProgramReturnValue
  case mrv of
    Nothing -> liftIO $ HU.assertFailure "No return value (fail)"
    Just rv -> do
      sbe <- gets symBE
      bx <- liftSBE $ applyTypedExpr sbe (GetStructField si rv 0)
      by <- liftSBE $ applyTypedExpr sbe (GetStructField si rv 1)
      let bxc = asSignedInteger sbe 32 bx
          byc = asSignedInteger sbe  8 by
          res = bxc == Just 42
                &&
                byc == Just (fromIntegral (fromEnum 'z'))
      liftIO $ HU.assertBool "incorrect value returned" res

structArrayImpl :: Functor sbe => Simulator sbe IO ()
structArrayImpl = do
  void $ callDefine (Symbol "struct_test_two") (Just i32) []
  mrv <- getProgramReturnValue
  case mrv of
    Nothing -> liftIO $ HU.assertFailure "No return value (fail)"
    Just rv -> liftIO . HU.assertBool "Expected 1" . (`constTermEq` 1) =<< withSBE' (\s -> asSignedInteger s 32 rv)
