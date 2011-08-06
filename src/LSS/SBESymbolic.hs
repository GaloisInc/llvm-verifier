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

sbeSymbolic :: SBE S.SymbolicMonad
sbeSymbolic = SBE
  { termInt  = \w v -> return . S.mkCInt (S.Wx w) . fromIntegral $ v
  , termBool = return . S.mkCBool
  , applyIte = S.applyIte
  , applyEq = S.applyEq
  , applyINot = S.applyINot
  , applyIAnd = S.applyIAnd
  , applyIOr = S.applyIOr
  , applyIXor = S.applyIXor
  , applyShl = S.applyShl
  , applyShr = S.applyShr
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

liftSBESymbolic :: S.SymbolicMonad a -> IO a
liftSBESymbolic = S.runSymbolic
