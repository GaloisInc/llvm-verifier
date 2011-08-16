{- |
Module           : $Header$
Description      : A symbolic backend
Stability        : provisional
Point-of-contact : atomb
-}

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

type instance SBETerm S.SymbolicMonad = S.SymbolicTerm
type instance SBEMemory S.SymbolicMonad = S.SymbolicTerm

-- | Symbolic interface with all operations at the word level.
sbeSymbolic :: SBE S.SymbolicMonad
sbeSymbolic = SBE
  { termInt  = \w v -> return . S.mkCInt (S.Wx w) . fromIntegral $ v
  , termBool = return . S.mkCBool
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
  , applyBAnd = S.applyBAnd
  , getBool = return . S.getBool
  , memLoad = \_mem _ptr -> return undefined
  , memStore = \_mem _val _ptr -> return undefined
  , memMerge = \_t _mem _mem' -> return undefined
  , memAddDefine = \_mem _sym _id -> return (undefined, undefined)
  , codeBlockAddress = \_mem _s _b -> return undefined
  , codeLookupDefine = \_mem _t -> return undefined
  , stackAlloca = \_mem _eltTp _n _a -> return undefined
  , stackPushFrame = \_mem -> return undefined
  , stackPopFrame = \_mem -> return undefined
  , writeAiger = \_f _t -> return undefined
  }

liftSBESymbolic :: S.SymbolicMonad a -> IO a
liftSBESymbolic = S.runSymbolic
