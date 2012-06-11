{- |
Module           : $Header$
Description      : A symbolic backend
Stability        : provisional
Point-of-contact : atomb
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module LSS.SBESymbolic
  ( module LSS.SBEInterface
  , sbeSymbolic
  )

where

import qualified Text.LLVM.AST as LLVM
import qualified Verinf.Symbolic as S
import LSS.SBEInterface

--------------------------------------------------------------------------------
-- Word-level symbolic backend

type instance SBETerm S.SymbolicMonad       = S.Node
type instance SBEClosedTerm S.SymbolicMonad = S.Node
type instance SBEMemory S.SymbolicMonad     = S.Node

-- | Symbolic interface with all operations at the word level.
sbeSymbolic :: SBE S.SymbolicMonad
sbeSymbolic = SBE
  { termInt  = \w v -> return . S.mkCInt (S.Wx w) . fromIntegral $ v
  , freshInt = nyi "freshInt"
  , termBool = return . S.mkCBool
  , termDouble = nyi "termDouble"
  , termFloat = nyi "termFloat"
  , termArray = nyi "termArray"
  , termDecomp = nyi "termDecomp"
  , applyIte = S.applyIte
  , applyICmp = \op -> case op of
                         LLVM.Ieq -> S.applyEq
                         _ -> error $
                              "unsupported comparison op: " ++
                              show op
  , applyBitwise = \op -> case op of
                            LLVM.And -> S.applyIAnd
                            LLVM.Or -> S.applyIOr
                            LLVM.Xor -> S.applyIXor
                            _ -> error $
                               "unsupported bitwise op: " ++
                               show op
  , applyArith = \op -> case op of
                          LLVM.Add  -> S.applyAdd
                          LLVM.Mul  -> S.applyMul
                          LLVM.Sub  -> S.applySub
                          LLVM.SDiv -> S.applySignedDiv
                          LLVM.SRem -> S.applySignedRem
                          LLVM.UDiv -> S.applyUnsignedDiv
                          LLVM.URem -> S.applyUnsignedRem
                          _ -> error $
                               "unsupported arithmetic op: " ++
                               show op
  , applyConv = nyi "applyConv"
  , applyBNot = nyi "applyBNot"
  , termWidth = nyi "termWidth"
  , closeTerm = id
  , prettyTermD = S.prettyTermD
  , asBool = nyi "asBool"
  , asUnsignedInteger = nyi "asUnsignedInteger"
  , memDump = nyi "memDump"
  , memLoad = nyi "memLoad "
  , memStore = nyi "memStore "
  , memMerge = nyi "memMerge "
  , memAddDefine = nyi "memAddDefine "
  , memInitGlobal = nyi "memInitGlobal"
  , memPushMergeFrame = nyi "memPushMergeFrame"
  , memPopMergeFrame = nyi "memPopMergeFrame"
  , codeBlockAddress = nyi "codeBlockAddress"
  , codeLookupSymbol = nyi "codeLookupSymbol"
  , stackAlloca = nyi "stackAlloca "
  , stackPushFrame = nyi "stackPushFrame "
  , stackPopFrame = nyi "stackPopFrame "
  , heapAlloc = nyi "heapAlloc"
  , memCopy = nyi "memCopy"
  , writeAiger = nyi "writeAiger"
  , evalAiger = nyi "evalAiger"
  }
  where
    nyi :: forall a. String -> a
    nyi msg = error $ unwords ["SBESymbolic:", msg, "not yet supported"]
