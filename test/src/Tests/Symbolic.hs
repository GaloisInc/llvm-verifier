{- |
Module           : $Header$
Description      : Symbolic execution tests
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns     #-}
module Tests.Symbolic (symTests) where

import Control.Monad (unless)
import Control.Monad.State (gets)
import Test.QuickCheck
import Tests.Common

import Verifier.LLVM.BitBlastBackend
import Verifier.LLVM.Simulator

symTests :: [(Args, Property)]
symTests =
  [ test 1 False "test-trivial-divergent-branch" $ do
      let v = 1
      runAllMemModelTest v "test-sym-simple.bc" $
        trivBranchImpl "trivial_branch" (0,1)
  , test 1 False "test-trivial-symbolic-read"    $ do
      let v = 1
      runAllMemModelTest v "test-sym-simple.bc" $
        trivBranchImpl "sym_read" (99,42)
  , lssTest 0 "ctests/test-symbolic-alloc" $ \v cb -> do
      runTestLSSBuddy v cb [] $ chkLSS (Just 1) Nothing
       -- This seems to hang in GHCI but not from the command line =/
      runTestLSSDag v cb []   $ chkLSS (Just 2) Nothing
  , lssTestAll 0 "ctests/test-fresh" [] $
      chkLSS Nothing (Just 16)
  , lssTestAll 0 "ctests/test-fresh-array" [] $
      -- NB: This test writes an .aig file; we are just testing
      -- essentially that we don't crash.  At some point this really
      -- should be beefed up to automatically equivalence check the
      -- output against a golden AIG file.
      chkLSS Nothing (Just 0)
  , lssTestAll 0 "ctests/test-const-false-path" [] $
      chkLSS (Just 0) (Just 1)
  , lssTestAll 0 "ctests/test-divergent-unreachables" [] $
      chkLSS (Just 1) (Just 1)
  , lssTestAll 0 "ctests/test-missing-define" [] $
      chkLSS (Just 1) (Just 1)
  , lssTestAll 0 "ctests/test-fresh-incremental" [] $
      chkLSS (Just 0) (Just 0)
  , lssTestAll 0 "ctests/test-fresh-array-incremental" [] $
      chkLSS (Just 0) (Just 0)
  , lssTestAll 0 "ctests/test-write-cnf" [] $
      chkLSS (Just 0) (Just 0)
  ]

trivBranchImpl :: String -> (Integer, Integer) -> AllMemModelTest
trivBranchImpl symName (e0,e1) = do
  sbe <- gets symBE
  b <- liftSBE $ freshInt sbe 32
  callDefine_ (Symbol symName) (Just i32) [(IntType 32, b)]
  mrv <- getProgramReturnValue
  case mrv of
    Nothing -> dbugM "No return value (fail)" >> return False
    Just rv -> do f inps0 e0
                  f inps1 e1
                  return True
      where inps0 = replicate 32 False
            inps1 = replicate 31 False ++ [True]
            f x e  = do
              mr <- liftSBE $ evalAiger sbe x (IntType 32) rv
              case asSignedInteger sbe 32 mr of
                Nothing -> do
                  fail $ "Could not evaluate return value:\n"
                           ++ show (prettyTermD sbe mr)
                Just r ->
                  unless (r == e) $ fail "Unexpected return value"


--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests symTests
