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
  , liftSBESymbolic
  )

where

import qualified Text.LLVM.AST as LLVM
import qualified Verinf.Symbolic as S
import LSS.SBEInterface

--------------------------------------------------------------------------------
-- Word-level symbolic backend

type instance SBETerm S.SymbolicMonad       = S.SymbolicTerm
type instance SBEClosedTerm S.SymbolicMonad = S.SymbolicTerm
type instance SBEMemory S.SymbolicMonad     = S.SymbolicTerm

-- | Symbolic interface with all operations at the word level.
sbeSymbolic :: SBE S.SymbolicMonad
sbeSymbolic = SBE
  { termInt  = \w v -> return . S.mkCInt (S.Wx w) . fromIntegral $ v
  , freshInt = nyi "freshInt"
  , termBool = return . S.mkCBool
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
  , memDump = nyi "memDump"
  , memLoad = nyi "memLoad "
  , memStore = nyi "memStore "
  , memMerge = nyi "memMerge "
  , memAddDefine = nyi "memAddDefine "
  , memInitGlobal = nyi "memInitGlobal"
  , codeBlockAddress = nyi "codeBlockAddress "
  , codeLookupDefine = nyi "codeLookupDefine "
  , stackAlloca = nyi "stackAlloca "
  , stackPushFrame = nyi "stackPushFrame "
  , stackPopFrame = nyi "stackPopFrame "
  , memCopy = nyi "memCopy"
  , writeAiger = nyi "writeAiger"
  , evalAiger = nyi "evalAiger"
  }
  where
    nyi :: forall a. String -> a
    nyi msg = error $ unwords ["SBESymbolic:", msg, "not yet supported"]

liftSBESymbolic :: S.SymbolicMonad a -> IO a
liftSBESymbolic = S.runSymbolic
