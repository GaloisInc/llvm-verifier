{- |
Module           : $Header$
Description      : LLVM primitive operation tests
License          : Free for non-commercial use. See LICENSE.
Stability        : provisional
Point-of-contact : atomb
-}

{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams   #-}
{-# LANGUAGE ViewPatterns     #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveDataTypeable      #-}

module Tests.PrimOps (primOpTests) where

import qualified Control.Exception as CE
import           Control.Lens hiding ((<.>), op)
import           Control.Monad.State (liftIO)

import           Data.Int
import           Data.Maybe
import           Data.Word

import           System.FilePath

import           Test.Tasty
import           Test.Tasty.QuickCheck
import qualified Test.Tasty.HUnit as HU
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC
import Prelude ()
import Prelude.Compat

import qualified Text.LLVM               as L


import qualified Data.ABC as ABC

import Verifier.LLVM.Codebase
import Verifier.LLVM.Backend
import Verifier.LLVM.Backend.BitBlast
import Verifier.LLVM.Simulator hiding (run)

import Tests.Common


primOpTests :: [TestTree]
primOpTests =
  [ testBinaryPrim "int32_add" (+)
  , testBinaryPrim "int32_muladd" $ \x y -> sqr (x + y)
  , testUnaryPrim  "int32_square" QC.arbitrary sqr
  , testUnaryPrim  "factorial" (QC.elements [0..12]) fact

  , qctest False "direct int32 add"      $ dirInt32add
  , qctest False "direct int32 sub"      $ dirInt32sub
  , qctest False "direct int32 mul"      $ dirInt32mul
  , qctest False "direct int32 sdiv"     $ dirInt32sdiv
  , qctest False "direct int32 udiv"     $ dirInt32udiv
  , qctest False "direct int32 srem"     $ dirInt32srem
  , qctest False "direct int32 urem"     $ dirInt32urem

  , testMainBasic "test-arith"  (RV 0)
  , testMainBasic "test-branch" (RV 0)
  , testMainBasic "test-call-voidrty" VoidRV
  , testMainBasic "test-ptr-simple" (RV 99)

  , forAllMemModels "test-setup-ptr-arg" "test-primops.bc" $ \bkName v sbeCF mdlio ->
          HU.testCase bkName $ runTestSimulator v sbeCF mdlio $ testSetupPtrArgImpl

  , testMainBasic "test-call-exit" AllPathsErr

  , lssTestAll "test-union-le" [] Nothing (RV 4)
  , lssTestAll "test-union-be" [] Nothing (RV 1)
  , lssTestAll "test-call-simple" [] Nothing (RV 1)
  , lssTestAll "test-call-simple" [] Nothing (RV 1)
  , lssTestAll "ctests/test-call-alloca" [] Nothing (RV 34289)
  , lssTestAll "ctests/test-call-malloc" [] Nothing (RV 34289)
  , lssTestAll "ctests/test-main-return" [] Nothing (RV 42)
  , lssTestAll "ctests/test-select" [] Nothing (RV 1)
  , lssTestAll "ctests/test-user-override-by-name" [] Nothing (RV 1)
  , lssTestAll "ctests/test-user-override-by-addr" [] Nothing  (RV 1)
  , lssTestAll "ctests/test-user-override-by-addr-cycle" [] Nothing (RV 1)
  , lssTestAll "ctests/test-user-override-reset" [] Nothing (RV 1)
  , lssTestAll "ctests/test-user-override-intrinsic" [] Nothing (RV 1)
  , lssTestAll "ctests/test-merge-mem-problem" [] Nothing (RV 1)

  ]
  where
    dirInt32add      = chkArithBitEngineFn 32 True (Add False False) add
    dirInt32sub      = chkArithBitEngineFn 32 True (Sub False False) sub
    dirInt32mul      = chkArithBitEngineFn 32 True (Mul False False) mul
    dirInt32sdiv     = chkArithBitEngineFn 32 True (SDiv False) idiv
    dirInt32udiv     = chkArithBitEngineFn 32 False (UDiv False) wdiv
    dirInt32srem     = chkArithBitEngineFn 32 True SRem irem
    dirInt32urem     = chkArithBitEngineFn 32 False URem wrem

    add, sub, mul, idiv, irem :: Int32 -> Int32 -> Int32
    add               = (+)
    sub               = (-)
    mul               = (*)
    idiv              = quot
    irem              = rem
    wdiv, wrem        :: Word32 -> Word32 -> Word32
    wdiv              = div
    wrem              = rem

    sqr x             = x * x

    fact 0            = 1
    fact x            = x * fact (x-1)

    testUnaryPrim :: String -> Gen Int32 -> (Int32 -> Int32) -> TestTree
    testUnaryPrim nm g f =
       forAllMemModels ("concrete " ++ nm) "test-primops.bc" $ \bkName v sbeCF mdlio ->
           testProperty bkName $ QC.monadicIO $ QC.forAllM g $ \x ->
               liftIO $ runTestSimulator v sbeCF mdlio $
                   runCInt32Fn (L.Symbol nm) [x] (RV (toInteger (f x)))

    testBinaryPrim nm f =
      forAllMemModels ("concrete " ++ nm) "test-primops.bc" $ \bkName v sbeCF mdlio ->
          testProperty bkName $ QC.monadicIO $ QC.forAllM QC.arbitrary $ \(x,y) ->
              liftIO $ runTestSimulator v sbeCF mdlio $
                  runCInt32Fn (L.Symbol nm) [x,y] (RV (toInteger (f x y)))

    testMainBasic nm ev =
      forAllMemModels nm (nm <.> "bc") $ \bkName v sbeCF mdlio ->
          HU.testCase bkName $ runTestSimulator v sbeCF mdlio $
              runCInt32Fn (L.Symbol "main") [] ev

chkArithBitEngineFn :: (Integral a, QC.Arbitrary a, Show a)
                    => BitWidth -> Bool -> IntArithOp -> (a -> a -> a)
                    -> QC.PropertyM IO ()
chkArithBitEngineFn w s op fn = do
  (ABC.SomeGraph g) <- QC.run $ ABC.newGraph ABC.giaNetwork
  let dl = defaultDataLayout
  let sbe = sbeBitBlast g dl (buddyMemModel dl g)
  QC.forAllM QC.arbitrary $ \(QC.NonZero x, QC.NonZero y) -> do
    let r = fn x y
        proj = if s then asSignedInteger else asUnsignedInteger
    x' <- QC.run . liftSBEBitBlast $ termInt sbe w (fromIntegral x)
    y' <- QC.run . liftSBEBitBlast $ termInt sbe w (fromIntegral y)
    r' <- QC.run . liftSBEBitBlast $ applyTypedExpr sbe (IntArith op Nothing w x' y')
    QC.assert (proj sbe w r' == Just (fromIntegral r))

testSetupPtrArgImpl ::
  ( Functor sbe
  )
  => Simulator sbe IO ()
testSetupPtrArgImpl = do
  a <- withDL (view ptrAlign)
  let w = 1
  one <- withSBE $ \sbe -> termInt sbe w 1
  p <- alloca i32 w one a
  callDefine (L.Symbol "ptrarg") Nothing [(i32p, p)]
  mrv <- getProgramReturnValue
  CE.assert (isNothing mrv) $ return ()
  mm  <- getProgramFinalMem
  case mm of
    Nothing  -> liftIO $ HU.assertFailure "no final memory!"
    Just mem -> do
      (_,r) <- withSBE (\sbe -> memLoad sbe mem i32 p a)
      liftIO . HU.assertBool "Expected value 42"
         =<< (`constTermEq` 42) <$> withSBE' (\s -> asUnsignedInteger s 32 r)
