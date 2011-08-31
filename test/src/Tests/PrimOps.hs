{- |
Module           : $Header$
Description      : LLVM primitive operation tests
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}

module Tests.PrimOps (primOpTests) where

import           Control.Applicative
import           Data.Int
import           Data.Maybe
import           Data.Word
import           LSS.SBEBitBlast
import           LSS.Simulator
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Tests.Common
import           Verinf.Symbolic.Common  (ConstantProjection(..), createBitEngine)
import qualified Control.Exception       as CE
import qualified Text.LLVM               as L

primOpTests :: [(Args, Property)]
primOpTests =
  [
    test 10  False "concrete int32 add"    $ int32add        1
  , test 10  False "concrete int32 sqr"    $ int32sqr        1
  , test 10  False "concrete int32 muladd" $ int32muladd     1
  , test 10  False "direct int32 add"      $ dirInt32add     1
  , test 10  False "direct int32 mul"      $ dirInt32mul     1
  , test 10  False "direct int32 sdiv"     $ dirInt32sdiv    1
  , test 10  False "direct int32 udiv"     $ dirInt32udiv    1
  , test 10  False "direct int32 srem"     $ dirInt32srem    1
  , test 10  False "direct int32 urem"     $ dirInt32urem    1
  , test  1  False "test-arith"            $ testArith       1
  , test  1  False "test-branch"           $ testBranch      1
  , test 10  False "test-factorial"        $ testFactorial   1
  , test  1  False "test-call-voidrty"     $ testCallVR      1
  , test  1  False "test-call-simple"      $ testCallSimple  1
  , test  1  False "test-ptr-simple"       $ testPtrSimple   1
  , test  1  False "test-setup-ptr-arg"    $ testSetupPtrArg 1
  , test  1  False "test-call-exit"        $ testCallExit    1
  ]
  where
    -- The 'v' parameter to all of these tests controls the verbosity; a
    -- verbosity of '0' turns the test into a successful no-op, but issues a
    -- warning.

    int32add v        = primB v "int32_add"    Nothing $ \x y -> Just (x + y)
    int32sqr v        = primU v "int32_square" Nothing $ Just . sqr
    int32muladd v     = primB v "int32_muladd" Nothing $ \x y -> Just $ sqr (x + y)
    testFactorial v   = primU v "factorial" (Just $ elements [0..12]) (Just . fact)
    dirInt32add v     = psk v $ chkArithBitEngineFn 32 True L.Add add
    dirInt32mul v     = psk v $ chkArithBitEngineFn 32 True L.Mul mul
    dirInt32sdiv v    = psk v $ chkArithBitEngineFn 32 True L.SDiv idiv
    dirInt32udiv v    = psk v $ chkArithBitEngineFn 32 False L.UDiv wdiv
    dirInt32srem v    = psk v $ chkArithBitEngineFn 32 True L.SRem irem
    dirInt32urem v    = psk v $ chkArithBitEngineFn 32 False L.URem wrem
    testArith v       = runMain v "test-arith.bc" (Just 0)
    testBranch v      = runMain v "test-branch.bc" (Just 0)
    testCallVR v      = runMain v "test-call-voidrty.bc" Nothing
    testCallSimple v  = runMain v "test-call-simple.bc" (Just 1)
    testPtrSimple v   = runMain v "test-ptr-simple.bc" (Just 99)
    testSetupPtrArg v = psk v $ runBitBlastSimTest v "test-primops.bc"
                                  defaultSEH testSetupPtrArgImpl
    testCallExit v    = runMain v "test-call-exit.bc" (Just 1)

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

    primU v nm mg f = psk v $ chkUnaryCInt32Fn mg v "test-primops.bc" (L.Symbol nm) f
    primB v nm mg f = psk v $ chkBinCInt32Fn   mg v "test-primops.bc" (L.Symbol nm) f

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

testSetupPtrArgImpl :: StdBitBlastTest
testSetupPtrArgImpl be = do
  callDefine (L.Symbol "ptrarg") (L.PrimType L.Void) $ do
    p <- alloca i32 Nothing (Just 8)
    return [p]
  mrv <- getProgramReturnValue
  CE.assert (isNothing mrv) $ return ()
  mm  <- getProgramFinalMem
  case mm of
    Nothing  -> return False
    Just mem -> do
      p <- L.Typed (L.PtrTo i32) <$> withSBE (\sbe -> termInt sbe 32 0)
      r <- withSBE $ \sbe -> memLoad sbe mem p
      return $ BitTermClosed (be, r) `constTermEq` 42

--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests primOpTests
