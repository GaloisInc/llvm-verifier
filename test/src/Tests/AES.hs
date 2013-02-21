{- |
Module           : $Header$
Description      : LLVM array tests
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns     #-}

module Tests.AES (aesTests) where

import           Control.Applicative
import           Control.Monad (forM)
import           Data.Maybe
import           Test.QuickCheck
import           Tests.Common
import           Text.LLVM               ((=:))
import qualified Text.LLVM               as L

import           Verifier.LLVM.Backend
import           Verifier.LLVM.Simulator
import           Verifier.LLVM.Simulator.Debugging
import           Verifier.LLVM.Utils

aesTests :: [(Args, Property)]
aesTests =
  [
    test 1 False "test-aes128-concrete" $ aes128Concrete 1
  ]
  where
    aes128Concrete v = psk v $ runAES v aes128ConcreteImpl
    runAES v         = runAllMemModelTest v (commonCB "aes128BlockEncrypt.bc")

aes128ConcreteImpl :: forall sbe . Functor sbe => Simulator sbe IO Bool
aes128ConcreteImpl = do
  setSEH sanityChecks
  ptptr  <- initArr ptVals
  keyptr <- initArr keyVals
  one <- getSizeT 1
  ctptr  <- alloca arrayTy one (Just 4)
  let args :: [SBETerm sbe]
      args = [ptptr, keyptr, ctptr]
  [_, _, ctRawPtr] <-
    callDefine (L.Symbol "aes128BlockEncrypt") voidTy args
  Just mem <- getProgramFinalMem
  ctarr <- withSBE $ \s -> snd <$> memLoad s mem arrayTy ctRawPtr
  ctVals <- forM [0..3] $ \i ->
    withSBE $ \s -> getVal s <$> applyTypedExpr s (GetConstArrayElt 4 i32 ctarr i)
  return (ctVals == ctChks)
  where
    getVal :: SBE sbe -> SBETerm sbe -> Integer
    getVal s v = snd $ fromJust $ asUnsignedInteger s v
    initArr :: [Integer] -> Simulator sbe IO (SBETerm sbe)
    initArr xs = do
       arrElts <- mapM (withSBE . \x s -> termInt s 32 x) xs
       arr <- withSBE $ \sbe -> termArray sbe (L.PrimType (L.Integer 32)) arrElts
       one <- getSizeT 1
       p   <- alloca arrayTy one (Just 4)
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
