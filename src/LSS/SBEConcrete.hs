{- |
Module           : $Header$
Description      : A concrete backend
Stability        : provisional
Point-of-contact : atomb
-}

{-# LANGUAGE TypeFamilies #-}
module LSS.SBEConcrete where

import LSS.SBEInterface

--------------------------------------------------------------------------------
-- Purely concrete backend

newtype SBEConcrete a = SBEConcrete { runConcrete :: a }
type instance SBETerm SBEConcrete = Integer
type instance SBEMemory SBEConcrete = ...

concBool :: Bool -> SBEConcrete Integer
concBool = SBEConcrete . fromIntegral . fromEnum

sbeConcrete :: SBE SBEConcrete
sbeConcrete = SBE
  { termInt  = const (SBEConcrete . fromIntegral)
  --, termWord = const (SBEConcrete . fromIntegral)
  , termBool = concBool
  , applyEq  = \a b -> concBool $ a == b
  , applyAdd = \a b -> SBEConcrete $ a + b
  , memInitMemory = SBEConcrete undefined
  , memAlloca = \_mem _eltType _len _a -> SBEConcrete undefined
  , memLoad = \_mem _ptr -> SBEConcrete undefined
  , memStore = \_mem _val _ptr -> SBEConcrete undefined
  , memAddDefine = \_mem _sym _id -> SBEConcrete (undefined, undefined)
  , memLookupDefine = \_mem _t -> SBEConcrete undefined
  , memBlockAddress = \_mem _s _b -> SBEConcrete undefined
  }

