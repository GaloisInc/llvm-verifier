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

import Verifier.LLVM.Backend.BitBlast
import Verifier.LLVM.Codebase.AST
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
  , lssTest "ctests/test-symbolic-alloc" $ \mdl -> do
      let mkTest createFn expectedFails expectedRV = run $ do
            runTestSimulator createFn 0 mdl $ do
              er <- testRunMain []
              checkErrorPaths expectedFails er
              checkReturnValue expectedRV er
      mkTest createBuddyModel 1 Nothing
      mkTest createDagModel 0 (Just 0)

  , lssTestAll 0 "ctests/test-fresh" [] Nothing (Just 16)
    -- NB: This test writes an .aig file; we are just testing
    -- essentially that we don't crash.  At some point this really
    -- should be beefed up to automatically equivalence check the
    -- output against a golden AIG file.
  , lssTestAll 0 "ctests/test-fresh-array"      [] Nothing (Just 0)
  , lssTestAll 0 "ctests/test-const-false-path" [] (Just 0) (Just 1)
  , lssTestAll 0 "ctests/test-divergent-unreachables" [] (Just 1) (Just 1)
  , lssTestAll 0 "ctests/test-missing-define" [] (Just 1) (Just 1)
  , lssTestAll 0 "ctests/test-fresh-incremental" [] (Just 0) (Just 0)
  , lssTestAll 0 "ctests/test-fresh-array-incremental" [] (Just 0) (Just 0)
  , lssTestAll 0 "ctests/test-write-cnf" [] (Just 0) (Just 0)
  ]

trivBranchImpl :: String -> (Integer, Integer) -> AllMemModelTest
trivBranchImpl symName (e0,e1) = do
  sbe <- gets symBE
  b <- liftSBE $ freshInt sbe 32
  callDefine (Symbol symName) (Just i32) [(IntType 32, b)]
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
