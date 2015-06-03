{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns     #-}
{-# LANGUAGE CPP     #-}

{- |
Module           : $Header$
Description      : LLVM array tests
License          : Free for non-commercial use. See LICENSE.
Stability        : provisional
Point-of-contact : atomb
-}
module Tests.AES (aesTests) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Control.Monad (forM)
import           Control.Monad.State (gets, liftIO)
import qualified Data.Vector as V
import           Test.Tasty
import qualified Test.Tasty.HUnit as HU

import qualified Text.LLVM               as L

import Tests.Common
import Verifier.LLVM.Backend
import Verifier.LLVM.Codebase.AST
import Verifier.LLVM.Simulator

aesTests :: [TestTree]
aesTests =
  [ forAllMemModels "test-aes128-concrete" "aes128BlockEncrypt.bc" $ \bkName v sbeCF mdlio ->
        HU.testCase bkName $ runTestSimulator v sbeCF mdlio aes128ConcreteImpl
  ]


aes128ConcreteImpl :: forall sbe . Functor sbe => Simulator sbe IO ()
aes128ConcreteImpl = do
  ptptr  <- initArr ptVals
  keyptr <- initArr keyVals
  let aw = 8
  one <- withSBE $ \sbe -> termInt sbe aw 1
  ctptr  <- alloca arrayTy aw one 2
  let args :: [(MemType, SBETerm sbe)]
      args = [ptptr, keyptr, (IntType aw, ctptr)]
  callDefine (L.Symbol "aes128BlockEncrypt") Nothing args
  Just mem <- getProgramFinalMem
  ctarr <- withSBE $ \s -> snd <$> memLoad s mem arrayTy ctptr 2
  sbe <- gets symBE
  ctVals <- forM [0..3] $ \i ->
    liftSBE $ getVal sbe <$> applyTypedExpr sbe (GetConstArrayElt 4 i32 ctarr i)

  liftIO $ HU.assertEqual "unexpected result" (fmap Just ctChks) ctVals

  where
    getVal :: SBE sbe -> SBETerm sbe -> Maybe Integer
    getVal s v = asUnsignedInteger s 32 v
    initArr :: [Integer] -> Simulator sbe IO (MemType,SBETerm sbe)
    initArr xs = do
       sbe <- gets symBE
       arrElts <- mapM (liftSBE . termInt sbe 32) xs
       arr <- liftSBE $ termArray sbe i32 (V.fromList arrElts)
       let aw = 8
       one <- liftSBE $ termInt sbe aw 1
       p   <- alloca arrayTy aw one 2
       store arrayTy arr p 2
       return (i32p, p)

    arrayTy = ArrayType 4 i32
    ptVals  = [0x00112233, 0x44556677, 0x8899aabb, 0xccddeeff]
    keyVals = [0x00010203, 0x04050607, 0x08090a0b, 0x0c0d0e0f]
    ctChks  = [0x69c4e0d8, 0x6a7b0430, 0xd8cdb780, 0x70b4c55a]


