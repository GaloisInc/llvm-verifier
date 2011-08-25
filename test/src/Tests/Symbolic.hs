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
import           Test.QuickCheck.Monadic
import           Tests.Common
import           Text.LLVM                     ((=:))
import qualified Text.LLVM                     as L

symTests :: [(Args, Property)]
symTests =
  [
    test 1 False "test-trivial-divergent-branch" $ incomplete $ trivBranch 1
  ]
  where
    trivBranch v = psk v $ runSimple v trivBranchImpl
    runSimple v  = assert <=< run . runBitBlastSim v "test-sym-simple.bc"

trivBranchImpl :: StdBitEngine -> StdBitBlastSim Bool
trivBranchImpl _be = do
  b <- withSBE $ \sbe -> termInt sbe 32 1
  callDefine (L.Symbol "trivial_branch") i32 [i32 =: b]
  mrv <- getProgramReturnValue
  case mrv of
    Just rv -> dbugTerm "rv" rv
    Nothing -> dbugM "No program return value"
  return True

--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests symTests
