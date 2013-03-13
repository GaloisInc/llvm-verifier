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

import           Control.Applicative
import           Test.QuickCheck
import           Tests.Common

import           Verifier.LLVM.BitBlastBackend
import           Verifier.LLVM.Simulator

symTests :: [(Args, Property)]
symTests =
  [ test 1 False "test-trivial-divergent-branch" $ trivBranch 1
  , test 1 False "test-trivial-symbolic-read"    $ trivSymRd 1
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
  where
    trivBranch v = psk v $ runSimple v $ trivBranchImpl "trivial_branch" $
                     \r0 r1 -> r0 == Just 0 && r1 == Just 1
    trivSymRd  v = psk v $ runSimple v $ trivBranchImpl "sym_read" $
                     \r0 r1 -> r0 == Just 99 && r1 == Just 42
    runSimple v  = runAllMemModelTest v "test-sym-simple.bc"

evalClosed :: (Functor sbe)
  => SBE sbe -> BitWidth -> SBETerm sbe -> [Bool] -> sbe (Maybe Integer)
evalClosed sbe w v i = asSignedInteger sbe w <$> evalAiger sbe i v

trivBranchImpl :: String -> (Maybe Integer -> Maybe Integer -> Bool) -> AllMemModelTest
trivBranchImpl symName chk = do
  b <- withSBE $ \sbe -> freshInt sbe 32
  callDefine_ (Symbol symName) (Just i32) [(IntType 32, b)]
  mrv <- getProgramReturnValue
  case mrv of
    Nothing -> dbugM "No return value (fail)" >> return False
    Just rv -> chk <$> f inps0 <*> f inps1
      where inps0 = replicate 32 False
            inps1 = replicate 31 False ++ [True]
            f x   = withSBE $ \s -> evalClosed s 32 rv x

--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests symTests
