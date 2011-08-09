{- |
Module           : $Header$
Description      : A concrete backend
Stability        : provisional
Point-of-contact : atomb
-}

{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module LSS.SBEConcrete where

import           Data.Bits
import           LSS.SBEInterface
import           Verinf.Symbolic.Common (PrettyTerm(..))
import qualified Text.PrettyPrint.HughesPJ as PP
import qualified Text.LLVM.AST    as LLVM

--------------------------------------------------------------------------------
-- Purely concrete backend

instance PrettyTerm Integer where
  prettyTermWithD = const PP.integer

newtype SBEConcrete a = SBEConcrete { runConcrete :: a }
type instance SBETerm SBEConcrete = Integer
type instance SBEMemory SBEConcrete = () -- TODO

concBool :: Bool -> SBEConcrete Integer
concBool = SBEConcrete . fromIntegral . fromEnum

sbeConcrete :: SBE SBEConcrete
sbeConcrete = SBE
  { termInt  = const (SBEConcrete . fromIntegral)
  , termBool = concBool
  , applyIte = \a b c -> SBEConcrete $ if a == 0 then b else c
  , applyICmp  = \op a b ->
                   case op of
                     LLVM.Ieq -> concBool $ a == b
                     _ -> error $
                          "unsupported comparison op: " ++
                          show op
  , applyBitwise = \op a b ->
                     case op of
                       LLVM.And -> SBEConcrete $ a .&. b
                       LLVM.Or -> SBEConcrete $ a .|. b
                       LLVM.Xor -> SBEConcrete $ a `xor` b
                       LLVM.Shl -> SBEConcrete $ a `shiftL` fromIntegral b
                       _ -> error $
                            "unsupported bitwise op: " ++
                            show op
  , applyArith = \op a b -> case op of
                              LLVM.Add -> SBEConcrete $ a + b
                              LLVM.Mul -> SBEConcrete $ a * b
                              LLVM.Sub -> SBEConcrete $ a - b
                              _ -> error $
                                   "SBEConcrete: unsupported arithmetic op: " ++
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
