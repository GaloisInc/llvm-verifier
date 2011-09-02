{- |
Module           : $Header$
Description      : A concrete backend
Stability        : provisional
Point-of-contact : atomb
-}

{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module LSS.SBEConcrete where

import           Data.Bits
import           LSS.SBEInterface
import qualified Text.LLVM.AST             as LLVM
import qualified Text.PrettyPrint.HughesPJ as PP
import qualified Verinf.Symbolic           as S

--------------------------------------------------------------------------------
-- Purely concrete backend

instance S.PrettyTerm Integer where
  prettyTermWithD = const PP.integer

newtype SBEConcrete a = SBEConcrete { runConcrete :: a }

type instance SBETerm SBEConcrete       = Integer
type instance SBEClosedTerm SBEConcrete = Integer
type instance SBEMemory SBEConcrete     = () -- TODO

concBool :: Bool -> SBEConcrete Integer
concBool = SBEConcrete . fromIntegral . fromEnum

sbeConcrete :: SBE SBEConcrete
sbeConcrete = SBE
  { termInt  = const (SBEConcrete . fromIntegral)
  , freshInt = nyi "freshInt "
  , termBool = concBool
  , termArray = nyi "termArray"
  , termDecomp = nyi "termDecomp"
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
  , applyConv        = nyi "applyConv"
  , applyBNot        = nyi "applyBNot"
  , termWidth        = nyi "termWidth"
  , closeTerm        = id
  , prettyTermD      = S.prettyTermD
  , memDump          = nyi "memDump"
  , memLoad          = nyi "memLoad"
  , memStore         = nyi "memStore"
  , memMerge         = nyi "memMerge"
  , memAddDefine     = nyi "memAddDefine"
  , memInitGlobal    = nyi "memInitGlobal"
  , codeBlockAddress = nyi "codeBlockAddress"
  , codeLookupDefine = nyi "codeLookupDefine"
  , stackAlloca      = nyi "stackAlloca"
  , stackPushFrame   = nyi "stackPushFrame"
  , stackPopFrame    = nyi "stackPopFrame"
  , heapAlloc        = nyi "heapAlloc"
  , memCopy          = nyi "memCopy"
  , writeAiger       = nyi "writeAiger"
  , evalAiger        = nyi "evalAiger"
  }
  where
    nyi :: forall a. String -> a
    nyi msg = error $ unwords ["SBEConcrete:", msg, "not yet supported"]

liftSBEConcrete :: SBEConcrete a -> IO a
liftSBEConcrete = return . runConcrete
