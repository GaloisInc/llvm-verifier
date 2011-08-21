{- |
Module           : $Header$
Description      : Info about LLVM memory
Stability        : provisional
Point-of-contact : jstanley
-}

module Data.LLVM.Memory where

import Data.Bits
import qualified Text.LLVM as LLVM

data LLVMContext = LLVMContext
  { llvmAddrWidthBits :: Int
  , llvmLookupAlias   :: LLVM.Ident -> LLVM.Type
  }

-- | Returns size of type in memory.
llvmPrimSizeOf :: LLVM.PrimType -> Integer
llvmPrimSizeOf LLVM.Label = error "internal: Cannot get size of label."
llvmPrimSizeOf LLVM.Void = error "internal: Cannot get size of void."
llvmPrimSizeOf (LLVM.Integer w) = (toInteger w + 7) `shiftR` 3
llvmPrimSizeOf (LLVM.FloatType LLVM.Float) = 4
llvmPrimSizeOf (LLVM.FloatType LLVM.Double) = 8
llvmPrimSizeOf (LLVM.FloatType LLVM.Fp128) = 16
llvmPrimSizeOf (LLVM.FloatType LLVM.X86_fp80) = 10
llvmPrimSizeOf (LLVM.FloatType LLVM.PPC_fp128) = 16
llvmPrimSizeOf LLVM.X86mmx = error "internal: X86MMX memory size is undefined."
llvmPrimSizeOf LLVM.Metadata = error "internal: Cannnot get size of metadata."

 -- | Returns number of bits for an LLVM.Type
llvmByteSizeOf :: LLVMContext -> LLVM.Type -> Integer
llvmByteSizeOf lc tp =
  case tp of
    LLVM.PrimType pt -> llvmPrimSizeOf pt
    LLVM.Alias a -> llvmByteSizeOf lc (llvmLookupAlias lc a)
    LLVM.Array l eTp -> toInteger l * llvmByteSizeOf lc eTp
    LLVM.FunTy _retTp _argTpl _ -> error "internal: Cannot get size of function type."
    LLVM.PtrTo _tp -> toInteger (llvmAddrWidthBits lc `shiftR` 3)
    --TODO: support alignment based on targetdata string in module.
    LLVM.Struct argTypes -> sum [ llvmByteSizeOf lc atp | atp <- argTypes ]
    LLVM.PackedStruct argTypes -> sum [ llvmByteSizeOf lc atp | atp <- argTypes ]
    LLVM.Vector l pt -> toInteger l * llvmPrimSizeOf pt
    LLVM.Opaque -> error "internal: Cannot get size of function type."

resolveType :: LLVMContext -> LLVM.Type -> LLVM.Type
resolveType lc (LLVM.Alias nm) = resolveType lc (llvmLookupAlias lc nm)
resolveType _ tp = tp
