{- |
Module           : $Header$
Description      : Error path and error handling tests
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}

module Tests.Errors (errorTests) where

import           Control.Monad
import           LSS.LLVMUtils
import           LSS.SBEBitBlast
import           LSS.Simulator
import           System.FilePath
import           Test.QuickCheck
import           Tests.Common
import           Text.LLVM              ((=:))
import           Verinf.Symbolic.Common (ConstantProjection(..))
import qualified Text.LLVM              as L

errorTests :: [(Args, Property)]
errorTests =
  [
    test 1 False "test-error-paths-all"          $ errPathAll 5
  ]
  where
    errPathAll v = psk v $ runMain v ("ctests" </> "test-error-paths-all.bc") AllPathsErr

--     trivBranch v = psk v $ runSimple v $ trivBranchImpl "trivial_branch" $
--                      \r0 r1 -> r0 == Just 0 && r1 == Just 1
--     trivSymRd  v = psk v $ runSimple v $ trivBranchImpl "sym_read" $
--                      \r0 r1 -> r0 == Just 99 && r1 == Just 42
--     trivFreshInt v = psk v $ runMain v "test-fresh.bc" (RV 16)
--     trivFreshArr v = psk v $ runMain v "test-fresh-array.bc" (RV 0)
--     runSimple v  = runAllMemModelTest v "test-sym-simple.bc"

-- trivBranchImpl :: String -> (Maybe Integer -> Maybe Integer -> Bool) -> AllMemModelTest
-- trivBranchImpl symName chk = do
--   b <- withSBE $ \sbe -> freshInt sbe 32
--   callDefine_ (L.Symbol symName) i32 $ return [i32 =: b]
--   mrv <- getProgramReturnValue
--   case mrv of
--     Nothing -> dbugM "No return value (fail)" >> return False
--     Just rv -> do
--       let inps0 = replicate 32 False
--           inps1 = replicate 31 False ++ [True]
--           f x   = withSBE $ \s -> evalClosed s rv x
--       liftM2 chk (f inps0) (f inps1)

--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests errorTests
