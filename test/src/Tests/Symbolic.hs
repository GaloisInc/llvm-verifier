{- |
Module           : $Header$
Description      : Symbolic execution tests
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}

module Tests.Symbolic (symTests) where

import           Control.Monad
import           LSS.SBEBitBlast
import           LSS.Simulator
import           LSS.Execution.Utils
import           Test.QuickCheck
import           Tests.Common
import           Text.LLVM              ((=:))
import           Verinf.Symbolic.Common (ConstantProjection(..))
import qualified Text.LLVM              as L

symTests :: [(Args, Property)]
symTests =
  [
    test 1 False "test-trivial-divergent-branch" $ trivBranch 1
  ]
  where
    trivBranch v = psk v $ runSimple v trivBranchImpl
    runSimple v  = runBitBlastSimTest v "test-sym-simple.bc"

trivBranchImpl :: StdBitBlastTest
trivBranchImpl _be = do
  b <- withSBE $ \sbe -> freshInt sbe 32
  callDefine (L.Symbol "trivial_branch") i32 [i32 =: b]
  mrv <- getProgramReturnValue
  case mrv of
    Nothing -> dbugM "No return value (fail)" >> return False
    Just rv -> do
      (r0, r1) <- withSBE $ \sbe -> do
        let inps0 = replicate 32 False
            inps1 = replicate 31 False ++ [True]
            q     = fmap (getSVal . closeTerm sbe) . flip (evalAiger sbe) rv
        liftM2 (,) (q inps0) (q inps1)
      return (r0 == Just 0 && r1 == Just 1)

--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests symTests
