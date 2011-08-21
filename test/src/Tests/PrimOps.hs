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
import           Control.Monad.Trans
import           Data.Int
import           Data.Word
import           LSS.Execution.Codebase
import           LSS.Execution.Common
import           LSS.Execution.Utils
import           LSS.SBEBitBlast
import           LSS.Simulator
import           System.FilePath
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Tests.Common
import           Text.LLVM               ((=:))
import           Verinf.Symbolic.Common  (ConstantProjection(..), Lit, createBitEngine)
import qualified Text.LLVM               as L

i32 :: L.Type
i32 = L.iT 32

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
    runMain v bc     = psk v . chkNullaryCInt32Fn v bc (L.Symbol "main")

    add, mul, idiv, irem :: Int32 -> Int32 -> Int32
    add              = (+)
    mul              = (*)
    idiv             = quot
    irem             = rem
    wdiv, wrem       :: Word32 -> Word32 -> Word32
    wdiv             = div
    wrem             = rem
    sqr x            = x * x

    psk              :: Int -> PropertyM IO () -> PropertyM IO ()
    psk v act        = if (v > 0) then act else disabledWarn
    disabledWarn     = run $ putStrLn "Warning: Next test is currently DISABLED!"


chkBinCInt32Fn :: Int -> FilePath -> L.Symbol -> (Int32 -> Int32 -> Maybe Int32) -> PropertyM IO ()
chkBinCInt32Fn v bcFile sym chkOp = do
  forAllM arbitrary $ \(x,y) -> do
    chkRslt sym (fromIntegral <$> x `chkOp` y)
      =<< run (runCInt32Fn v bcFile sym [x, y])

chkUnaryCInt32Fn :: Int -> FilePath -> L.Symbol -> (Int32 -> Maybe Int32) -> PropertyM IO ()
chkUnaryCInt32Fn v bcFile sym chkOp =
  forAllM arbitrary $ \x -> do
    chkRslt sym (fromIntegral <$> chkOp x)
      =<< run (runCInt32Fn v bcFile sym [x])

chkNullaryCInt32Fn :: Int -> FilePath -> L.Symbol -> Maybe Int32 -> PropertyM IO ()
chkNullaryCInt32Fn v bcFile sym chkVal =
  chkRslt sym (fromIntegral <$> chkVal)
    =<< run (runCInt32Fn v bcFile sym [])

runCInt32Fn :: Int -> FilePath -> L.Symbol -> [Int32] -> IO (Maybe (BitTermClosed Lit))
runCInt32Fn v bcFile sym cargs = do
  cb <- loadCodebase $ supportDir </> bcFile
  be <- createBitEngine

  let lc            = LLVMContext 32 (error "LLVM Context has no ident -> type alias map defined")
      backend       = sbeBitBlast lc be
      mem           = sbeBitBlastMem (stkSt, stkEnd) (codeSt, codeEnd) (heapSt, heapEnd)
                      where
                        defaultSz         = 2^(16 :: Int)
                        ext st len        = (st, st + len)
                        (stkSt, stkEnd)   = ext 0 defaultSz
                        (codeSt, codeEnd) = ext stkEnd defaultSz
                        (heapSt, heapEnd) = ext codeEnd defaultSz

  runSimulator cb backend mem (SM . lift . liftSBEBitBlast) $ withVerbosity v $ do
    args <- withSBE $ \sbe -> mapM (termInt sbe 32 . fromIntegral) cargs
    callDefine sym i32 $ map (\x -> i32 =: x) args
    rv <- getProgramReturnValue
    return $ BitTermClosed . (,) be <$> rv

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

chkRslt :: ConstantProjection t => L.Symbol -> Maybe Integer -> Maybe t -> PropertyM IO ()
chkRslt _ (Just chk) (Just (getSVal -> Just v))
  | v == chk  = assert True
  | otherwise = assertMsg False $ "Expected " ++ show chk ++ ", got " ++ show v
chkRslt _ Nothing Nothing
  = assert True
chkRslt sym _ _
  = assertMsg False $ show (L.ppSymbol sym) ++ ": unexpected return value"

--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests primOpTests
