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
  [ test 1 False "test-trivial-divergent-branch" $ trivBranch 1
  , test 1 False "test-trivial-symbolic-read"    $ trivSymRd 1
  , lssTest 0 "ctests/test-symbolic-alloc" $ \v cb -> do
      runTestLSSBuddy v cb [] $ chkLSS (Just 1) Nothing
       -- This seems to hang in GHCI but not from the command line =/
      runTestLSSDag v cb []   $ chkLSS Nothing (Just 0)
  , lssTest 0 "ctests/test-fresh" $ \v cb -> do
      runTestLSSBuddy v cb [] $ chkLSS Nothing (Just 16)
      runTestLSSDag v cb []   $ chkLSS Nothing (Just 16)
  , lssTest 0 "ctests/test-fresh-array" $ \v cb -> do
      -- NB: This test writes an .aig file; we are just testing
      -- essentially that we don't crash.  At some point this really
      -- should be beefed up to automatically equivalence check the
      -- output against a golden AIG file.
      runTestLSSBuddy v cb [] $ chkLSS Nothing (Just 0)
      runTestLSSDag v cb []   $ chkLSS Nothing (Just 0)
  , lssTest 0 "ctests/test-const-false-path" $ \v cb -> do
      runTestLSSBuddy v cb [] $ chkLSS (Just 0) (Just 1)
      runTestLSSDag v cb []   $ chkLSS (Just 0) (Just 1)
  , lssTest 0 "ctests/test-divergent-unreachables" $ \v cb -> do
      runTestLSSBuddy v cb [] $ chkLSS (Just 1) (Just 1)
      runTestLSSDag v cb []   $ chkLSS (Just 1) (Just 1)
  , lssTest 0 "ctests/test-missing-define" $ \v cb -> do
      runTestLSSBuddy v cb [] $ chkLSS (Just 1) (Just 1)
      runTestLSSDag v cb []   $ chkLSS (Just 1) (Just 1)
  , lssTest 0 "ctests/test-fresh-incremental" $ \v cb -> do
      runTestLSSBuddy v cb [] $ chkLSS (Just 0) (Just 0)
      runTestLSSDag v cb []   $ chkLSS (Just 0) (Just 0)
  , lssTest 0 "ctests/test-fresh-array-incremental" $ \v cb -> do
      runTestLSSBuddy v cb [] $ chkLSS (Just 0) (Just 0)
      runTestLSSDag v cb []   $ chkLSS (Just 0) (Just 0)
  ]
  where
    trivBranch v = psk v $ runSimple v $ trivBranchImpl "trivial_branch" $
                     \r0 r1 -> r0 == Just 0 && r1 == Just 1
    trivSymRd  v = psk v $ runSimple v $ trivBranchImpl "sym_read" $
                     \r0 r1 -> r0 == Just 99 && r1 == Just 42
    runSimple v  = runAllMemModelTest v (commonCB "test-sym-simple.bc")

evalClosed ::
  ( Functor sbe
  , ConstantProjection (SBEClosedTerm sbe)
  )
  => SBE sbe -> SBETerm sbe -> [Bool] -> sbe (Maybe Integer)
evalClosed sbe v = fmap (getSVal . closeTerm sbe) . flip (evalAiger sbe) v

trivBranchImpl :: String -> (Maybe Integer -> Maybe Integer -> Bool) -> AllMemModelTest
trivBranchImpl symName chk = do
  b <- withSBE $ \sbe -> freshInt sbe 32
  callDefine_ (L.Symbol symName) i32 [i32 =: b]
  mrv <- getProgramReturnValue
  case mrv of
    Nothing -> dbugM "No return value (fail)" >> return False
    Just rv -> do
      let inps0 = replicate 32 False
          inps1 = replicate 31 False ++ [True]
          f x   = withSBE $ \s -> evalClosed s rv x
      liftM2 chk (f inps0) (f inps1)

--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests symTests
