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
                                   "unsupported arithmetic op: " ++
                                   show op
  , memLoad = \_mem _ptr -> SBEConcrete undefined
  , memStore = \_mem _val _ptr -> SBEConcrete undefined
  , memMerge = \_t _mem _mem' -> SBEConcrete undefined
  , memAddDefine = \_mem _sym _id -> SBEConcrete (undefined, undefined)
  , codeBlockAddress = \_mem _s _b -> SBEConcrete undefined
  , codeLookupDefine = \_mem _t -> SBEConcrete undefined
  , stackAlloca = \_mem _eltTp _n _a -> SBEConcrete undefined
  , stackPushFrame = \_mem -> SBEConcrete undefined
  , stackPopFrame = \_mem -> SBEConcrete undefined
  }

liftSBEConcrete :: SBEConcrete a -> IO a
liftSBEConcrete = return . runConcrete
