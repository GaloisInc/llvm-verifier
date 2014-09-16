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

import Control.Monad.State (gets, liftIO)
import Test.Tasty
import qualified Test.Tasty.HUnit as HU

import Tests.Common

import Verifier.LLVM.Backend.BitBlast
import Verifier.LLVM.Codebase.AST
import Verifier.LLVM.Simulator hiding (run)

symTests :: [TestTree]
symTests =
  [ forAllMemModels "test-trivial-divergent-branch" "test-sym-simple.bc" $ \bkName v sbeCF mdlio ->
        HU.testCase bkName $ runTestSimulator v sbeCF mdlio $
            trivBranchImpl "trivial_branch" (0,1)

  , forAllMemModels "test-trivial-symbolic-read" "test-sym-simple.bc" $ \bkName v sbeCF mdlio ->
        HU.testCase bkName $ runTestSimulator v sbeCF mdlio $
            trivBranchImpl "sym_read" (99,42)

    -- FIXME?? is this right? getting different results in the various bakends?
  , withVerbModel "ctests/test-symbolic-alloc.bc" $ \v getmdl ->
        testGroup "ctests/test-symbolic-alloc"
            [ runLssTest "buddy model"     v createBuddyModel    getmdl [] (Just 1) AllPathsErr
            , runLssTest "dag model"       v createDagModel      getmdl [] (Just 0) (RV 0)
            , runLssTest "SAW model"       v createSAWModel      getmdl [] (Just 0) (RV 0)
            ]

  , lssTestAll "ctests/test-fresh" [] Nothing (RV 16)
    -- NB: This test writes an .aig file; we are just testing
    -- essentially that we don't crash.  At some point this really
    -- should be beefed up to automatically equivalence check the
    -- output against a golden AIG file.
  , lssTestAll "ctests/test-fresh-array"      [] Nothing (RV 0)
  , lssTestAll "ctests/test-const-false-path" [] (Just 0) (RV 1)
  , lssTestAll "ctests/test-divergent-unreachables" [] (Just 1) (RV 1)
  , lssTestAll "ctests/test-missing-define" [] (Just 1) (RV 1)
  , lssTestAll "ctests/test-fresh-incremental" [] (Just 0) (RV 0)
  , lssTestAll "ctests/test-fresh-array-incremental" [] (Just 0) (RV 0)
  , lssTestAll "ctests/test-write-cnf" [] (Just 0) (RV 0)
  ]

trivBranchImpl :: Functor sbe => String -> (Integer, Integer) -> Simulator sbe IO ()
trivBranchImpl symName (e0,e1) = do
  sbe <- gets symBE
  b <- liftSBE $ freshInt sbe 32
  callDefine (Symbol symName) (Just i32) [(IntType 32, b)]
  mrv <- getProgramReturnValue
  case mrv of
    Nothing -> liftIO $ HU.assertFailure "No return value (fail)"
    Just rv -> do f inps0 e0
                  f inps1 e1
                  return ()
      where inps0 = replicate 32 False
            inps1 = replicate 31 False ++ [True]
            f x e  = do
              mr <- liftSBE $ evalAiger sbe x (IntType 32) rv
              case asSignedInteger sbe 32 mr of
                Nothing -> do
                  liftIO $ HU.assertFailure $ "Could not evaluate return value:\n"
                                                 ++ show (prettyTermD sbe mr)
                Just r ->
                  liftIO $ HU.assertEqual "Unexpected return value" e r 
