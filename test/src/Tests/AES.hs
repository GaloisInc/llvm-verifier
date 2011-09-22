{- |
Module           : $Header$
Description      : LLVM array tests
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}

module Tests.AES (aesTests) where

import           Control.Applicative
import           Data.Maybe
import           LSS.Execution.Debugging
import           LSS.LLVMUtils
import           LSS.SBEInterface
import           LSS.Simulator
import           Test.QuickCheck
import           Tests.Common
import           Text.LLVM               ((=:), Typed(..), typedValue)
import           Verinf.Symbolic.Common  (ConstantProjection(..))
import qualified Text.LLVM               as L

aesTests :: [(Args, Property)]
aesTests =
  [
    test 1 False "test-aes128-concrete" $ aes128Concrete 1
  ]
  where
    aes128Concrete v = psk v $ runAES v aes128ConcreteImpl
    runAES v         = runAllMemModelTest v (commonCB "aes128BlockEncrypt.bc")

aes128ConcreteImpl :: AllMemModelTest
aes128ConcreteImpl = do
  setSEH sanityChecks
  [_, _, typedValue -> ctRawPtr] <-
    callDefine (L.Symbol "aes128BlockEncrypt") voidTy $ do
      ptptr  <- initArr ptVals
      keyptr <- initArr keyVals
      ctptr  <- typedValue <$> alloca arrayTy Nothing (Just 4)
      return $ map (i32p =:) [ptptr, keyptr, ctptr]

  Just mem <- getProgramFinalMem
  ctarr  <- load' mem (L.PtrTo arrayTy =: ctRawPtr)
  ctVals <- withSBE $ \s ->
              map (getVal s) <$> termDecomp s (replicate 4 i32) ctarr
  return (ctVals == ctChks)
  where
    getVal s   = typedValue . fmap (fromJust . getUVal . closeTerm s)
    initArr xs = do
       arr <- withSBE . flip termArray
                =<< mapM (withSBE . \x s -> termInt s 32 x) xs
       p   <- typedValue <$> alloca arrayTy Nothing (Just 4)
       store (arrayTy =: arr) p
       return p

    arrayTy = L.Array 4 i32
    ptVals  = [0x00112233, 0x44556677, 0x8899aabb, 0xccddeeff]
    keyVals = [0x00010203, 0x04050607, 0x08090a0b, 0x0c0d0e0f]
    ctChks  = [0x69c4e0d8, 0x6a7b0430, 0xd8cdb780, 0x70b4c55a]

--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests aesTests
