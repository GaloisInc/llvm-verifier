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
import           Verinf.Symbolic.Common    (PrettyTerm(..))
import qualified Text.LLVM.AST             as LLVM
import qualified Text.PrettyPrint.HughesPJ as PP
import qualified Verinf.Symbolic           as S

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
  , applyBAnd = \x y -> case (x, y) of
                          (0,_) -> SBEConcrete 0
                          (_,0) -> SBEConcrete 0
                          (1,1) -> SBEConcrete 1
                          _     -> error "SBEConcrete: applyBAnd applied to non-bool-valued terms"

  , getBool = \term -> if term == 0
                       then SBEConcrete $ Just False
                       else if term == 1
                            then SBEConcrete $ Just True
                            else SBEConcrete $ Nothing
  , memInitMemory = SBEConcrete undefined
  , memAlloca = \_mem _eltType _len _a -> SBEConcrete undefined
  , memLoad = \_mem _ptr -> SBEConcrete undefined
  , memStore = \_mem _val _ptr -> SBEConcrete undefined
  , memMerge = \_t _mem _mem' -> SBEConcrete undefined
  , memAddDefine = \_mem _sym _id -> SBEConcrete (undefined, undefined)
  , codeBlockAddress = \_mem _s _b -> SBEConcrete undefined
  , codeLookupDefine = \_mem _t -> SBEConcrete undefined
  , stackAlloca = \_mem _eltTp _n _a -> SBEConcrete undefined
  , stackPushFrame = \_mem -> SBEConcrete undefined
  , stackPopFrame = \_mem -> SBEConcrete undefined
  , writeAiger = \_f _t ->
                 error "Aiger creation not supported in concrete backend"
  }

liftSBEConcrete :: SBEConcrete a -> IO a
liftSBEConcrete = return . runConcrete
