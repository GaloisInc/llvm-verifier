{- |
Module           : $Header$
Description      : A symbolic backend
Stability        : provisional
Point-of-contact : atomb
-}

{-# LANGUAGE TypeFamilies #-}

module LSS.SBESymbolic where

import Control.Monad
import qualified Text.LLVM.AST as LLVM
import qualified Verinf.Symbolic as S
import qualified Verinf.Symbolic.Common as C
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

liftSBESymbolic :: S.SymbolicMonad a -> IO a
liftSBESymbolic = S.runSymbolic

newtype BitMonad a = BM { runBitMonad :: IO a }
type instance SBETerm BitMonad = S.LitResult S.Lit
type instance SBEMemory BitMonad = S.LitResult S.Lit

-- | Symbolic interface with all operations at the bit level.
-- TODO: This feels like it's breaking abstractions.
sbeSymbolicBit :: S.BitEngine S.Lit -> SBE BitMonad
sbeSymbolicBit be = SBE
  { termInt  = \w -> BM . return . S.concreteValueLit be . S.CInt (S.Wx w)
  , termBool = BM . return . S.concreteValueLit be . S.mkCBool
  , applyIte = undefined
  , applyICmp = undefined
  , applyBitwise = undefined
  , applyArith =
    \op a b -> case op of
                 LLVM.Add -> BM $ C.fromLsbfV `liftM`
                             S.beAddInt be (C.toLsbfV a) (C.toLsbfV b)
                 _ -> error $
                      "unsupported arithmetic op: " ++
                      show op
  , memInitMemory = BM $ return undefined
  , memAlloca = \_mem _eltType _len _a -> BM $ return undefined
  , memLoad = \_mem _ptr -> BM $ return undefined
  , memStore = \_mem _val _ptr -> BM $ return undefined
  , memAddDefine = \_mem _sym _id -> BM $ return (undefined, undefined)
  , memLookupDefine = \_mem _t -> BM $ return undefined
  , memBlockAddress = \_mem _s _b -> BM $ return undefined
  , memSelect = \_t _mem _mem' -> BM $ return undefined
  }

liftSBESymbolicBit :: BitMonad a -> IO a
liftSBESymbolicBit = runBitMonad
