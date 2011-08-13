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
import           Verinf.Symbolic.Common  ( PrettyTerm(..)
                                         , getSVal
                                         )
import qualified Text.LLVM               as L

i32 :: L.Type
i32 = L.iT 32

int32 :: Gen Int32
int32 = arbitrary

primOpTests :: [(Args, Property)]
primOpTests =
  [ test 10 False "concrete int32 add" $
      binCInt32Fn "primOps.bc"  (L.Symbol "int32_add") (+)
  , test 10 False "concrete int32 sqr" $
      unaryCInt32Fn "primOps.bc" (L.Symbol "int32_square") sqr
  , test 10 False "concrete int32 muladd" $
      binCInt32Fn "primOps.bc" (L.Symbol "int32_muladd") (\x y -> sqr (x + y))
  ]
  where
    sqr x = x * x

binCInt32Fn :: FilePath -> L.Symbol -> (Int32 -> Int32 -> Int32) -> PropertyM IO ()
binCInt32Fn bcFile sym chkOp = do
  forAllM int32 $ \x -> do
  forAllM int32 $ \y -> do
  mrv <- run $ do
    cb <- loadCodebase $ supportDir </> bcFile
    runSimulator cb sbeSymbolic (SM . lift . liftSBESymbolic) $ do
      i1 <- withSBE $ \sbe -> termInt sbe 32 (fromIntegral x)
      i2 <- withSBE $ \sbe -> termInt sbe 32 (fromIntegral y)
      callDefine sym i32 [ i32 =: IValue 32 i1 , i32 =: IValue 32 i2 ]
      getProgramReturnValue
  case mrv of
    Just (getSVal -> Just v) ->
      assert $ v == fromIntegral (x `chkOp` y)
    _ ->
      assertMsg False $ show (L.ppSymbol sym) ++ ": unexpected return value"

--runCInt32Fn :: FilePath -> L.Symbol -> [Int32] ->
runCInt32Fn bcFile sym cargs = do
    cb <- loadCodebase $ supportDir </> bcFile
    runSimulator cb sbeSymbolic (SM . lift . liftSBESymbolic) $ do
      args <- withSBE $ \sbe -> mapM (termInt sbe 32 . fromIntegral) cargs
      callDefine sym i32 $ map (\x -> i32 =: IValue 32 x) args
      getProgramReturnValue

unaryCInt32Fn :: FilePath -> L.Symbol -> (Int32 -> Int32) -> PropertyM IO ()
unaryCInt32Fn bcFile sym chkOp = do
  forAllM int32 $ \x -> do
  mrv <- run $ do
    cb <- loadCodebase $ supportDir </> bcFile
    runSimulator cb sbeSymbolic (SM . lift . liftSBESymbolic) $ do
     i1 <- withSBE $ \sbe -> termInt sbe 32 (fromIntegral x)
     callDefine sym i32 [ i32 =: IValue 32 i1 ]
     getProgramReturnValue
  case mrv of
    Just (getSVal -> Just v) ->
      assert $ v == fromIntegral (chkOp x)
    _ ->
      assertMsg False $ show (L.ppSymbol sym) ++ ": unexpected return value"

nullaryCInt32Fn :: FilePath -> L.Symbol -> Int32 -> PropertyM IO ()
nullaryCInt32Fn bcFile sym chkVal = undefined



--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests primOpTests
