{- |
Module           : $Header$
Description      : LLVM primitive operation tests
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE ViewPatterns #-}


module Tests.PrimOps (primOpTests) where

import           Control.Monad.Trans
import           Data.Int
import           LSS.Execution.Codebase
import           LSS.Execution.Common
import           LSS.Execution.Utils
import           LSS.SBESymbolic
import           LSS.Simulator
import           System.FilePath
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Tests.Common
import           Text.LLVM               ((=:))
import           Verinf.Symbolic         (SymbolicTerm)
import           Verinf.Symbolic.Common  (getSVal)
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
    test 1 False "test-arith.bc" $
      chkNullaryCInt32Fn 1 "test-arith.bc" (L.Symbol "main") 0
  ]
  where
    sqr x = x * x

chkBinCInt32Fn :: Int -> FilePath -> L.Symbol -> (Int32 -> Int32 -> Int32) -> PropertyM IO ()
chkBinCInt32Fn v bcFile sym chkOp = forAllM arbitrary $ \(x,y) -> do
  chkRslt sym (fromIntegral (x `chkOp` y))
    =<< run (runCInt32Fn v bcFile sym [x, y])

chkUnaryCInt32Fn :: Int -> FilePath -> L.Symbol -> (Int32 -> Int32) -> PropertyM IO ()
chkUnaryCInt32Fn v bcFile sym chkOp = forAllM arbitrary $ \x -> do
  chkRslt sym (fromIntegral (chkOp x))
    =<< run (runCInt32Fn v bcFile sym [x])

chkNullaryCInt32Fn :: Int -> FilePath -> L.Symbol -> Int32 -> PropertyM IO ()
chkNullaryCInt32Fn v bcFile sym chkVal = do
  chkRslt sym (fromIntegral chkVal) =<< run (runCInt32Fn v bcFile sym [])

runCInt32Fn :: Int -> FilePath -> L.Symbol -> [Int32] -> IO (Maybe SymbolicTerm)
runCInt32Fn v bcFile sym cargs = do
  cb <- loadCodebase $ supportDir </> bcFile
  runSimulator cb sbeSymbolic (SM . lift . liftSBESymbolic) $ withVerbosity v $ do
    args <- withSBE $ \sbe -> mapM (termInt sbe 32 . fromIntegral) cargs
    callDefine sym i32 $ map (\x -> i32 =: x) args
    getProgramReturnValue

chkRslt :: L.Symbol -> Integer -> Maybe SymbolicTerm -> PropertyM IO ()
chkRslt _ chk (Just (getSVal -> Just v)) = assert $ v == chk
chkRslt sym _ _                          = assertMsg False
                                           $ show (L.ppSymbol sym) ++ ": unexpected return value"

--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests primOpTests
