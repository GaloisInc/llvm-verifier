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
import           Data.Bits
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
    test 10 False "concrete int32 add" $
      chkBinCInt32Fn 1 "primOps.bc"  (L.Symbol "int32_add") (+)
  ,
    test 10 False "concrete int32 sqr" $
      chkUnaryCInt32Fn 1 "primOps.bc" (L.Symbol "int32_square") sqr
  ,
    test 10 False "concrete int32 muladd" $
      chkBinCInt32Fn 1 "primOps.bc" (L.Symbol "int32_muladd") (\x y -> sqr (x + y))
  ,
    test1 "test-arith"  $ chkMain 1 "test-arith.bc" 0
  ,
    test1 "test-branch" $ chkMain 1 "test-branch.bc" 0
--   ,
--     test1 "test-call-simple" $ chkMain 5 "test-call-simple.bc" 0
--   ,
--     test1 "test-call-exit"   $ chkMain 1 "test-call-exit.bc" 0
  ]
  where
    sqr x   = x * x
    test1   = test 1 False

chkBinCInt32Fn :: Int -> FilePath -> L.Symbol -> (Int32 -> Int32 -> Int32) -> PropertyM IO ()
chkBinCInt32Fn v bcFile sym chkOp = do
  forAllM arbitrary $ \(x,y) -> do
    chkRslt sym (fromIntegral (x `chkOp` y))
      =<< run (runCInt32Fn v bcFile sym [x, y])

chkUnaryCInt32Fn :: Int -> FilePath -> L.Symbol -> (Int32 -> Int32) -> PropertyM IO ()
chkUnaryCInt32Fn v bcFile sym chkOp =
  forAllM arbitrary $ \x -> do
    chkRslt sym (fromIntegral (chkOp x))
      =<< run (runCInt32Fn v bcFile sym [x])

chkNullaryCInt32Fn :: Int -> FilePath -> L.Symbol -> Int32 -> PropertyM IO ()
chkNullaryCInt32Fn v bcFile sym chkVal =
  chkRslt sym (fromIntegral chkVal)
    =<< run (runCInt32Fn v bcFile sym [])

chkMain :: Int -> FilePath -> Int32 -> PropertyM IO ()
chkMain v bcFile = chkNullaryCInt32Fn v bcFile (L.Symbol "main")

runCInt32Fn :: Int -> FilePath -> L.Symbol -> [Int32] -> IO (Maybe (BitTermClosed Lit))
runCInt32Fn v bcFile sym cargs = do
  cb <- loadCodebase $ supportDir </> bcFile
  be <- createBitEngine

  let addrWidthBits = 32
      lc            = LLVMContext addrWidthBits (error "LLVM Context has no ident -> type alias map defined")
      backend       = sbeBitBlast lc be
      sz st bytes   = let ws = bytes `div` fromIntegral (addrWidthBits `shiftR` 3) in (st + ws, st + ws)
      mem           = sbeBitBlastMem (sst, send) (cst, cend) (hst, hend)
                      where
                        sst            = 0 :: Integer
                        defaultSzBytes = 32768
                        (send, cst)    = sz sst defaultSzBytes
                        (cend, hst)    = sz cst defaultSzBytes
                        (hend, _)      = sz hst defaultSzBytes

  runSimulator cb backend mem (SM . lift . liftSBEBitBlast) $ withVerbosity v $ do
    args <- withSBE $ \sbe -> mapM (termInt sbe 32 . fromIntegral) cargs
    callDefine sym i32 $ map (\x -> i32 =: x) args
    rv <- getProgramReturnValue
    return $ BitTermClosed . (,) be <$> rv

chkRslt :: ConstantProjection t => L.Symbol -> Integer -> Maybe t -> PropertyM IO ()
chkRslt _ chk (Just (getSVal -> Just v)) = do
  -- run $ putStrLn $ "chkRslt: constant v = " ++ show v
  assert $ v == chk
chkRslt sym _ _ =
  assertMsg False $ show (L.ppSymbol sym) ++ ": unexpected return value"

--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests primOpTests
