{- |
Module           : $Header$
Description      : A concrete backend
Stability        : provisional
Point-of-contact : atomb
-}

{-# LANGUAGE TypeFamilies #-}
module LSS.SBEConcrete where

import Data.Bits
import qualified Text.LLVM.AST as LLVM

import LSS.SBEInterface

--------------------------------------------------------------------------------
-- Purely concrete backend

newtype SBEConcrete a = SBEConcrete { runConcrete :: a }
type instance SBETerm SBEConcrete = Integer
type instance SBEMemory SBEConcrete = () -- TODO

concBool :: Bool -> SBEConcrete Integer
concBool = SBEConcrete . fromIntegral . fromEnum

sbeConcrete :: SBE SBEConcrete
sbeConcrete = SBE
  { termInt  = const (SBEConcrete . fromIntegral)
  --, termWord = const (SBEConcrete . fromIntegral)
  , termBool = concBool
  , applyEq  = \a b -> concBool $ a == b
  , applyIte = \a b c -> SBEConcrete $ if a == 0 then b else c
  --, applyBNot = \a ->
  --, applyBAnd = \a b ->
  --, applyBOr = \a b ->
  --, applyBXor = \a b ->
  , applyINot = SBEConcrete . complement
  , applyIAnd = \a b -> SBEConcrete $ a .&. b
  , applyIOr = \a b -> SBEConcrete $ a .|. b
  , applyIXor = \a b -> SBEConcrete $ a `xor` b
  , applyShl = \a b -> SBEConcrete $ a `shiftL` fromIntegral b
  , applyShr = \a b -> SBEConcrete $ a `shiftR` fromIntegral b
  , applyArith = \op a b -> case op of
                              LLVM.Add -> SBEConcrete $ a + b
                              LLVM.Mul -> SBEConcrete $ a * b
                              LLVM.Sub -> SBEConcrete $ a - b
                              _ -> error $
                                   "unsupported arithmetic op: " ++
                                   show op
  , memInitMemory = SBEConcrete undefined
  , memAlloca = \_mem _eltType _len _a -> SBEConcrete undefined
  , memLoad = \_mem _ptr -> SBEConcrete undefined
  , memStore = \_mem _val _ptr -> SBEConcrete undefined
  , memAddDefine = \_mem _sym _id -> SBEConcrete (undefined, undefined)
  , memLookupDefine = \_mem _t -> SBEConcrete undefined
  , memBlockAddress = \_mem _s _b -> SBEConcrete undefined
  , memSelect = \_t _mem _mem' -> SBEConcrete undefined
  }

liftSBEConcrete :: SBEConcrete a -> IO a
liftSBEConcrete = return . runConcrete
