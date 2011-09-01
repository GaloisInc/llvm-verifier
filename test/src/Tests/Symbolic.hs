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
import           LSS.LLVMUtils
import           LSS.SBEBitBlast
import           LSS.Simulator
import           Test.QuickCheck
import           Tests.Common
import           Text.LLVM              ((=:))
import           Verinf.Symbolic.Common (ConstantProjection(..))
import qualified Text.LLVM              as L

symTests :: [(Args, Property)]
symTests =
  [
    test 1 False "test-trivial-divergent-branch" $ trivBranch 1
    -- symbolic reads not yet supported, so this is currently disabled
  , test 1 False "test-trivial-symbolic-read"    $ trivSymRd 0
  , test 1 False "test-trivial-fresh-int"        $ trivFreshInt 1
  , test 1 False "test-trivial-fresh-array"      $ trivFreshArr 1
  ]
  where
    trivBranch v = psk v $ runSimple v trivBranchImpl
    trivSymRd  v = psk v $ runSimple v trivSymRdImpl
    trivFreshInt v = psk v $ runMain v "test-fresh.bc" (Just 16)
    trivFreshArr v = psk v $ runMain v "test-fresh-array.bc" (Just 0)
    runSimple v  = runBitBlastSimTest v "test-sym-simple.bc" defaultSEH

trivBranchImpl :: StdBitBlastTest
trivBranchImpl _be = do
  b <- withSBE $ \sbe -> freshInt sbe 32
  callDefine (L.Symbol "trivial_branch") i32 $ return [i32 =: b]
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

trivSymRdImpl :: StdBitBlastTest
trivSymRdImpl _be = do
  b <- withSBE $ \sbe -> freshInt sbe 32
  callDefine (L.Symbol "sym_read") i32 $ return [i32 =: b]
  mrv <- getProgramReturnValue
  case mrv of
    Nothing -> dbugM "No return value (fail)" >> return False
    Just rv -> do
      (r0, r1) <- withSBE $ \sbe -> do
        let inps0 = replicate 32 False
            inps1 = replicate 31 False ++ [True]
            q     = fmap (getSVal . closeTerm sbe) . flip (evalAiger sbe) rv
        liftM2 (,) (q inps0) (q inps1)
      return (r0 == Just 99 && r1 == Just 42)

--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests symTests
