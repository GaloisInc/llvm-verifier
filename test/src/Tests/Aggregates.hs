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
import           Test.QuickCheck
import           Tests.Common
import qualified Text.LLVM        as L

import           Verifier.LLVM.Backend
import           Verifier.LLVM.LLVMContext
import           Verifier.LLVM.Simulator

aggTests :: [(Args, Property)]
aggTests =
  [
    test 1 False "test-array-index-base"            $ arrayBaseIdx     1
  , test 1 False "test-array-index-offset"          $ arrayOffsetIdx   1
  , test 1 False "test-array-1d-initializer"        $ arrayInit1D      1
  , test 1 False "test-array-2d-initializer"        $ arrayInit2D      1
  , test 1 False "test-array-mat4x4-mult"           $ arrayMat4x4      1
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
    arrayBaseIdx v        = t1 v "arr1" (RV 42)
    arrayOffsetIdx v      = t1 v "arr2" (RV 141)
    arrayInit1D v         = t1 v "onedim_init" (RV 3)
    arrayInit2D v         = t1 v "twodim_init" (RV 21)
    arrayMat4x4 v         = t2 v "matrix_mul_4x4" (RV 304)
    t1                    = mkNullaryTest "test-arrays.bc"
    t2                    = mkNullaryTest "test-mat4x4.bc"
    mkNullaryTest bc v nm = psk v . chkNullaryCInt32Fn v (commonCB bc) (L.Symbol nm)
    runStruct v           = \(f :: AllMemModelTest) ->
                              runAllMemModelTest v (commonCB "test-structs.bc") f

structInitAccessImpl :: AllMemModelTest
structInitAccessImpl = do
  dl <- withDL id
  let si = mkStructInfo dl False [i32, i8]
  callDefine_ (L.Symbol "struct_test") (Just (StructType si)) []
  mrv <- getProgramReturnValue
  case mrv of
    Nothing -> dbugM "No return value (fail)" >> return False
    Just rv -> do
      bx <- withSBE $ \sbe -> applyTypedExpr sbe (GetStructField si rv 0)
      by <- withSBE $ \sbe -> applyTypedExpr sbe (GetStructField si rv 1)
      bxc <- withSBE' $ \s -> asSignedInteger s bx
      byc <- withSBE' $ \s -> asSignedInteger s by
      return $ bxc `constTermEq` 42
               &&
               byc `constTermEq` fromIntegral (fromEnum 'z')

structArrayImpl :: AllMemModelTest
structArrayImpl = do
  callDefine_ (L.Symbol "struct_test_two") (Just i32) []
  mrv <- getProgramReturnValue
  case mrv of
    Nothing -> dbugM "No return value (fail)" >> return False
    Just rv -> (`constTermEq` 1) <$> withSBE' (\s -> asSignedInteger s rv)

--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests aggTests
