{- |
Module           : $Header$
Description      : LLVM primitive operation tests
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams   #-}
{-# LANGUAGE ViewPatterns     #-}

module Tests.PrimOps (primOpTests) where

import           Control.Applicative
import           Data.Int
import           Data.Maybe
import           Data.Word
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Tests.Common
import           Verinf.Symbolic         (createBitEngine)
import qualified Control.Exception       as CE
import qualified Text.LLVM               as L

import           Verifier.LLVM.BitBlastBackend
import           Verifier.LLVM.LLVMContext
import           Verifier.LLVM.Simulator

primOpTests :: [(Args, Property)]
primOpTests =
  [ 
{-
    testBinaryPrim "int32_add" (+)
  , testBinaryPrim "int32_muladd" $ \x y -> sqr (x + y)
  , testUnaryPrim  "int32_square" arbitrary sqr
  , testUnaryPrim  "factorial" (elements [0..12]) fact

  , test 10  False "direct int32 add"      $ dirInt32add     1
  , test 10  False "direct int32 mul"      $ dirInt32mul     1
  , test 10  False "direct int32 sdiv"     $ dirInt32sdiv    1
  , test 10  False "direct int32 udiv"     $ dirInt32udiv    1
  , test 10  False "direct int32 srem"     $ dirInt32srem    1
  , test 10  False "direct int32 urem"     $ dirInt32urem    1
-}
    testMain "test-arith"  0
  , testMain "test-branch" 0

  , test  1  False "test-call-voidrty" $ 
      runMainVoid 1 "test-call-voidrty.bc"

  , testMain "test-ptr-simple" 99

  , test  1  False "test-setup-ptr-arg"    $ testSetupPtrArg 1
  , test  1  False  "test-call-exit"       $ testCallExit    1
  , lssTestAll 0  "test-call-simple" [] $
      chkLSS Nothing (Just 1)
  , lssTestAll 0 "ctests/test-call-alloca" [] $
      chkLSS Nothing (Just 34289)
  , lssTestAll 0 "ctests/test-call-malloc" [] $
      chkLSS Nothing (Just 34289)
  , lssTestAll 0 "ctests/test-main-return" [] $
      chkLSS Nothing (Just 42)
  , lssTestAll 0 "ctests/test-select" [] $
      chkLSS Nothing (Just 1)
  , lssTestAll 0 "ctests/test-user-override-by-name" [] $
      chkLSS Nothing (Just 1)
  , lssTestAll 0 "ctests/test-user-override-by-addr" [] $
      chkLSS Nothing (Just 1)
  , lssTestAll 0 "ctests/test-user-override-by-addr-cycle" [] $
      chkLSS Nothing (Just 1)
  , lssTestAll 0 "ctests/test-user-override-reset" [] $
      chkLSS Nothing (Just 1)
  , lssTestAll 0 "ctests/test-user-override-intrinsic" [] $
      chkLSS Nothing (Just 1)
  , lssTestAll 0 "ctests/test-merge-mem-problem" [] $
      chkLSS Nothing (Just 1)
  ]
  where
    -- The 'v' parameter to all of these tests controls the verbosity; a
    -- verbosity of '0' turns the test into a successful no-op, but issues a
    -- warning.

    dirInt32add v     = psk v $ chkArithBitEngineFn 32 True (Add False False) add
    dirInt32mul v     = psk v $ chkArithBitEngineFn 32 True (Mul False False) mul
    dirInt32sdiv v    = psk v $ chkArithBitEngineFn 32 True (SDiv False) idiv
    dirInt32udiv v    = psk v $ chkArithBitEngineFn 32 False (UDiv False) wdiv
    dirInt32srem v    = psk v $ chkArithBitEngineFn 32 True SRem irem
    dirInt32urem v    = psk v $ chkArithBitEngineFn 32 False URem wrem

    testSetupPtrArg v = psk v $ runAllMemModelTest v "test-primops.bc"
                                  testSetupPtrArgImpl
    testCallExit v    = runMain' True v "test-call-exit.bc" AllPathsErr

    add, mul, idiv, irem :: Int32 -> Int32 -> Int32
    add               = (+)
    mul               = (*)
    idiv              = quot
    irem              = rem
    wdiv, wrem        :: Word32 -> Word32 -> Word32
    wdiv              = div
    wrem              = rem
    sqr x             = x * x
    fact 0            = 1
    fact x            = x * fact (x-1)

    testUnaryPrim nm g f = do
      let v = 1 -- verbosity
      test 10 False ("test " ++ nm) $ do
        forAllM g $ \x -> do
          runCInt32Fn v "test-primops.bc" (L.Symbol nm) [x] (RV (toInteger (f x)))

    testBinaryPrim nm f = do 
      let v = 1 -- verbosity
      test 10 False ("concrete " ++ nm) $
        forAllM arbitrary $ \(x,y) ->
          runCInt32Fn v "test-primops.bc" (L.Symbol nm) [x, y] (RV (toInteger (f x y)))

    testMain nm ev = do
      let v = 1 -- verbosity
      test 1 False nm $ runMain v (nm ++ ".bc") (RV ev)


chkArithBitEngineFn :: (Integral a, Arbitrary a, Show a)
                    => BitWidth -> Bool -> IntArithOp -> (a -> a -> a)
                    -> PropertyM IO ()
chkArithBitEngineFn w s op fn = do
  be <- run createBitEngine
  let dl = defaultDataLayout
  let sbe = let ?be = be in sbeBitBlast dl (buddyMemModel dl be)
  forAllM arbitrary $ \(NonZero x,NonZero y) -> do
    let r = fn x y
        proj = if s then asSignedInteger else asUnsignedInteger
    x' <- run . liftSBEBitBlast $ termInt sbe w (fromIntegral x)
    y' <- run . liftSBEBitBlast $ termInt sbe w (fromIntegral y)
    r' <- run . liftSBEBitBlast $ applyTypedExpr sbe (IntArith op Nothing w x' y')
    assert (proj sbe w r' == Just (fromIntegral r))

testSetupPtrArgImpl ::
  ( Functor sbe
  )
  => Simulator sbe IO Bool
testSetupPtrArgImpl = do
  a <- withDL (view ptrAlign)
  let w = 1
  one <- withSBE $ \sbe -> termInt sbe w 1
  p <- alloca i32 w one a
  callDefine_ (L.Symbol "ptrarg") Nothing [(i32p, p)]
  mrv <- getProgramReturnValue
  CE.assert (isNothing mrv) $ return ()
  mm  <- getProgramFinalMem
  case mm of
    Nothing  -> return False
    Just mem -> do
      (_,r) <- withSBE (\sbe -> memLoad sbe mem i32 p a)
      (`constTermEq` 42) <$> withSBE' (\s -> asUnsignedInteger s 32 r)

--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests primOpTests