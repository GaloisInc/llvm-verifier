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
    test 10 False "concrete int32 add"    $ int32add       1
  , test 10 False "concrete int32 sqr"    $ int32sqr       1
  , test 10 False "concrete int32 muladd" $ int32muladd    1
  , test  1 False "test-arith"            $ testArith      1
  , test  1 False "test-branch"           $ testBranch     1
  , test  1 False "test-call-voidrty"     $ testCallVR     1
  , test  1 False "test-call-simple"      $ testCallSimple 1
  , test  1 False "test-ptr-simple"       $ testPtrSimple  1
  , test  1 False "test-call-exit"        $ testCallExit   0
  ]
  where
    -- The 'v' parameter to all of these tests controls the verbosity; a
    -- verbosity of '0' turns the test into a successful no-op, but issues a
    -- warning.

    int32add v       = psk v $ chkBinCInt32Fn v "test-primops.bc"  (L.Symbol "int32_add") (\x y -> Just (x + y))
    int32sqr v       = psk v $ chkUnaryCInt32Fn v "test-primops.bc" (L.Symbol "int32_square") (Just . sqr)
    int32muladd v    = psk v $ chkBinCInt32Fn v "test-primops.bc" (L.Symbol "int32_muladd") (\x y -> Just $ sqr (x + y))
    testArith v      = runMain v "test-arith.bc" (Just 0)
    testBranch v     = runMain v "test-branch.bc" (Just 0)
    testCallVR v     = runMain v "test-call-voidrty.bc" Nothing
    testCallSimple v = runMain v "test-call-simple.bc" (Just 1)
    testPtrSimple v  = runMain v "test-ptr-simple.bc" (Just 99)
    testCallExit v   = runMain v "test-call-exit.bc" (Just 0)
    runMain v bc     = psk v . chkNullaryCInt32Fn v bc (L.Symbol "main")
    psk v act        = if (v > 0) then act else disabledWarn
    sqr x            = x * x
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

  let addrWidthBits = 32
      lc            = LLVMContext addrWidthBits (error "LLVM Context has no ident -> type alias map defined")
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
