{- |
Module           : $Header$
Description      : A symbolic backend
Stability        : provisional
Point-of-contact : atomb
-}

{-# LANGUAGE TypeFamilies #-}

module LSS.SBESymbolic where

import qualified Data.Vector.Storable as LV
import Control.Applicative
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
  , writeAiger = \_f _t -> return undefined
  }

liftSBESymbolic :: S.SymbolicMonad a -> IO a
liftSBESymbolic = S.runSymbolic

--------------------------------------------------------------------------------
-- Bit-level symbolic backend

-- BitMonad is just a wrapper to allow SBETerm and SBEMemory
-- associations.
newtype BitMonad a = BM { runBitMonad :: IO a }
type instance SBETerm BitMonad = S.LitResult S.Lit
type instance SBEMemory BitMonad = S.LitResult S.Lit

-- | Symbolic interface with all operations at the bit level.
sbeSymbolicBit :: S.BitEngine S.Lit -> SBE BitMonad
sbeSymbolicBit be = SBE
  { termInt  = \w -> BM . return . S.concreteValueLit be . S.CInt (S.Wx w)
  , termBool = BM . return . S.concreteValueLit be . S.mkCBool
  , applyIte = bitIte be
  , applyICmp = bitICmp be
  , applyBitwise = bitBitwise be
  , applyArith = bitArith be
  , memInitMemory = BM $ return undefined
  , memAlloca = \_mem _eltType _len _a -> BM $ return undefined
  , memLoad = \_mem _ptr -> BM $ return undefined
  , memStore = \_mem _val _ptr -> BM $ return undefined
  , memAddDefine = \_mem _sym _id -> BM $ return (undefined, undefined)
  , memLookupDefine = \_mem _t -> BM $ return undefined
  , memBlockAddress = \_mem _s _b -> BM $ return undefined
  , memSelect = \_t _mem _mem' -> BM $ return undefined
  , writeAiger = \f t -> case t of
                           S.LV ls -> BM $ S.beWriteAigerV be f ls
                           _ -> error "writeAiger called on non-scalar"
  }

bitIte :: S.BitEngine S.Lit
       -> S.LitResult S.Lit -> S.LitResult S.Lit -> S.LitResult S.Lit
       -> BitMonad (S.LitResult S.Lit)
bitIte be (S.LV c) (S.LV a) (S.LV b) = BM $ do
  let zero = LV.replicate (LV.length c) (S.beFalse be)
  cbit <- S.beEqVector be c zero
  S.LV <$> S.beIteInt be cbit (return b) (return a)
bitIte _ _ _ _ = error "if-then-else applied to non-scalar arguments"

bitICmp :: S.BitEngine S.Lit -> LLVM.ICmpOp
        -> S.LitResult S.Lit -> S.LitResult S.Lit
        -> BitMonad (S.LitResult S.Lit)
bitICmp be op (S.LV a) (S.LV b) = BM $ (S.LV . LV.singleton) <$> f be a b
  where f = case op of
              LLVM.Ieq -> S.beEqVector
              _ -> error $ "unsupported comparison op: " ++ show op
bitICmp _ op (S.LVN _) _ =
  error $ "comparison op " ++ show op ++ " applied to non-scalar"
bitICmp _ op _ (S.LVN _) =
  error $ "comparison op " ++ show op ++ " applied to non-scalar"

bitArith :: S.BitEngine S.Lit -> LLVM.ArithOp
         -> S.LitResult S.Lit -> S.LitResult S.Lit
         -> BitMonad (S.LitResult S.Lit)
bitArith be op (S.LV a) (S.LV b) = BM $ S.LV <$> f be a b
  where f = case op of
              LLVM.Add -> S.beAddInt
              LLVM.Mul -> S.beMulInt
              _ -> error $ "unsupported arithmetic op: " ++ show op
bitArith _ op (S.LVN _) _ =
  error $ "arithmetic op " ++ show op ++ " applied to non-scalar"
bitArith _ op _ (S.LVN _) =
  error $ "arithmetic op " ++ show op ++ " applied to non-scalar"

bitBitwise :: S.BitEngine S.Lit -> LLVM.BitOp
           -> S.LitResult S.Lit -> S.LitResult S.Lit
           -> BitMonad (S.LitResult S.Lit)
bitBitwise be op (S.LV a) (S.LV b) = BM $ S.LV <$> f be a b
  where f = case op of
              LLVM.And -> S.beAndInt
              LLVM.Or -> S.beOrInt
              _ -> error $ "unsupported arithmetic op: " ++ show op
bitBitwise _ op (S.LVN _) _ =
  error $ "bitwise op " ++ show op ++ " applied to non-scalar"
bitBitwise _ op _ (S.LVN _) =
  error $ "bitwise op " ++ show op ++ " applied to non-scalar"

liftSBESymbolicBit :: BitMonad a -> IO a
liftSBESymbolicBit = runBitMonad
