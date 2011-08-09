{- |
Module           : $Header$
Description      : A symbolic backend
Stability        : provisional
Point-of-contact : atomb
-}

{-# LANGUAGE TypeFamilies #-}

module LSS.SBESymbolic where

import qualified Text.LLVM.AST as LLVM
import qualified Verinf.Symbolic as S
import LSS.SBEInterface

--------------------------------------------------------------------------------
-- Symbolic backend

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
                          LLVM.Add -> S.applyAdd
                          LLVM.Mul -> S.applyMul
                          LLVM.Sub -> S.applySub
                          _ -> error $
                               "unsupported arithmetic op: " ++
                               show op
  , memInitMemory = return undefined
  , memAlloca = \_mem _eltType _len _a -> return undefined
  , memLoad = \_mem _ptr -> return undefined
  , memStore = \_mem _val _ptr -> return undefined
  , memAddDefine = \_mem _sym _id -> return (undefined, undefined)
  , memLookupDefine = \_mem _t -> return undefined
  , memBlockAddress = \_mem _s _b -> return undefined
  , memSelect = \_t _mem _mem' -> return undefined
  }

-- | Symbolic interface with all operations at the bit level.
-- TODO: can this be over S.SymbolicMonad? That enforces that the term
-- type is S.SymbolicTerm, which won't necessarily work here. Do we need
-- a newtype wrapper for bit-blasted symbolic terms?
sbeSymbolicBit :: SBE S.SymbolicMonad
sbeSymbolicBit = SBE
  { termInt  = undefined
  , termBool = undefined
  , applyIte = undefined
  , applyICmp = undefined
  , applyBitwise = undefined
  , applyArith = undefined
  , memInitMemory = return undefined
  , memAlloca = \_mem _eltType _len _a -> return undefined
  , memLoad = \_mem _ptr -> return undefined
  , memStore = \_mem _val _ptr -> return undefined
  , memAddDefine = \_mem _sym _id -> return (undefined, undefined)
  , memLookupDefine = \_mem _t -> return undefined
  , memBlockAddress = \_mem _s _b -> return undefined
  , memSelect = \_t _mem _mem' -> return undefined
  }

liftSBESymbolic :: S.SymbolicMonad a -> IO a
liftSBESymbolic = S.runSymbolic
