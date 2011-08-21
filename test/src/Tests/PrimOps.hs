{- |
Module           : $Header$
Description      : LLVM primitive operation tests
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}

module Tests.PrimOps (primOpTests) where

import           Data.Int
import           Data.Word
import           LSS.SBEBitBlast
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Tests.Common
import           Verinf.Symbolic.Common  (ConstantProjection(..), createBitEngine)
import qualified Text.LLVM               as L

primOpTests :: [(Args, Property)]
primOpTests =
  [
    test 10  False "concrete int32 add"    $ int32add       1
  , test 10  False "concrete int32 sqr"    $ int32sqr       1
  , test 10  False "concrete int32 muladd" $ int32muladd    1
  , test 100 False "direct int32 add"      $ dirInt32add    1
  , test 100 False "direct int32 mul"      $ dirInt32mul    1
  , test 100 False "direct int32 sdiv"     $ dirInt32sdiv   1
  , test 100 False "direct int32 udiv"     $ dirInt32udiv   1
  , test 100 False "direct int32 srem"     $ dirInt32srem   1
  , test 100 False "direct int32 urem"     $ dirInt32urem   1
  , test  1  False "test-arith"            $ testArith      1
  , test  1  False "test-branch"           $ testBranch     1
  , test  1  False "test-call-voidrty"     $ testCallVR     1
  , test  1  False "test-call-simple"      $ testCallSimple 1
  , test  1  False "test-ptr-simple"       $ testPtrSimple  1
  , test  1  False "test-call-exit"        $ testCallExit   0
  ]
  where
    -- The 'v' parameter to all of these tests controls the verbosity; a
    -- verbosity of '0' turns the test into a successful no-op, but issues a
    -- warning.
    int32add v       = psk v $ chkBinCInt32Fn v "test-primops.bc"  (L.Symbol "int32_add") (\x y -> Just (x + y))
    int32sqr v       = psk v $ chkUnaryCInt32Fn v "test-primops.bc" (L.Symbol "int32_square") (Just . sqr)
    int32muladd v    = psk v $ chkBinCInt32Fn v "test-primops.bc" (L.Symbol "int32_muladd") (\x y -> Just $ sqr (x + y))
    dirInt32add v    = psk v $ chkArithBitEngineFn 32 True L.Add add
    dirInt32mul v    = psk v $ chkArithBitEngineFn 32 True L.Mul mul
    dirInt32sdiv v   = psk v $ chkArithBitEngineFn 32 True L.SDiv idiv
    dirInt32udiv v   = psk v $ chkArithBitEngineFn 32 False L.UDiv wdiv
    dirInt32srem v   = psk v $ chkArithBitEngineFn 32 True L.SRem irem
    dirInt32urem v   = psk v $ chkArithBitEngineFn 32 False L.URem wrem
    testArith v      = runMain v "test-arith.bc" (Just 0)
    testBranch v     = runMain v "test-branch.bc" (Just 0)
    testCallVR v     = runMain v "test-call-voidrty.bc" Nothing
    testCallSimple v = runMain v "test-call-simple.bc" (Just 1)
    testPtrSimple v  = runMain v "test-ptr-simple.bc" (Just 99)
    testCallExit v   = runMain v "test-call-exit.bc" (Just 0)

    add, mul, idiv, irem :: Int32 -> Int32 -> Int32
    add              = (+)
    mul              = (*)
    idiv             = quot
    irem             = rem
    wdiv, wrem       :: Word32 -> Word32 -> Word32
    wdiv             = div
    wrem             = rem
    sqr x            = x * x

chkArithBitEngineFn :: (Integral a, Arbitrary a) =>
                       Int -> Bool -> L.ArithOp -> (a -> a -> a)
                    -> PropertyM IO ()
chkArithBitEngineFn w s op fn = do
  be <- run createBitEngine
  let sbe = sbeBitBlast
            (LLVMContext 32
             (error "LLVM Context has no ident -> type alias map defined"))
            be
  forAllM arbitrary $ \(NonZero x,NonZero y) -> do
    let r = fn x y
        proj = if s then getSVal else getUVal
    x' <- run . liftSBEBitBlast $ termInt sbe w (fromIntegral x)
    y' <- run . liftSBEBitBlast $ termInt sbe w (fromIntegral y)
    r' <- run . liftSBEBitBlast $ applyArith sbe op x' y'
    assert (proj (BitTermClosed (be, r')) == Just (fromIntegral r))

--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests primOpTests
