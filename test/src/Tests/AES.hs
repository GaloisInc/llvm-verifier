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
import           LSS.SBEInterface
import           LSS.Simulator
import           Text.LLVM              ((=:), Typed(..), typedValue)
import           Test.QuickCheck
import           Tests.Common
import qualified Text.LLVM              as L

import LSS.Execution.Debugging

aesTests :: [(Args, Property)]
aesTests =
  [
    test 1 False "test-aes128-concrete"        $ aes128Concrete 1
  ]
  where
    aes128Concrete v = psk v $ runAES v aes128ConcreteImpl
    runAES v         = runBitBlastSimTest v "aes128BlockEncrypt.bc" sanityChecks

aes128ConcreteImpl :: StdBitBlastTest
aes128ConcreteImpl _be = do
  callDefine (L.Symbol "aes128BlockEncrypt") voidTy $ do
    ptptr  <- initArr ptVals
    keyptr <- initArr keyVals
    ctptr  <- typedValue <$> alloca arrayTy Nothing (Just 4)
    return $ map (i32p =:) [ptptr, keyptr, ctptr]
  -- error "aes early term"
  return False
  where
    initArr xs = do
       arr <- withSBE $ \s -> termArray s =<< mapM (termInt s 32) xs
       p   <- typedValue <$> alloca arrayTy Nothing (Just 4)
       mutateMem_ $ \s m -> memStore s m (arrayTy =: arr) p
       return p
    i32p    = L.PtrTo i32
    arrayTy = L.Array 4 i32
    voidTy  = L.PrimType L.Void
    ptVals  = [0x00112233, 0x44556677, 0x8899aabb, 0xccddeeff]
    keyVals = [0x00010203, 0x04050607, 0x08090a0b, 0x0c0d0e0f]

--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests aesTests
