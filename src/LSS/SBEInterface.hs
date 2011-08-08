{- |
Module           : $Header$
Description      : The interface to a symbolic backend
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE TypeFamilies #-}

module LSS.SBEInterface where

import qualified Text.LLVM.AST as LLVM

-- | SBETerm yields the type used to represent terms in particular SBE interface
-- implementation.
type family SBETerm (sbe :: * -> *)

-- | SBEMemory yields the type used to represent the memory in a particular SBE
-- interface implementation.
type family SBEMemory (sbe :: * -> *)

-- | Represents a partial result of trying to obtain a concrete value from
-- a symbolic term.
data PartialResult r
  = Result r -- ^ The result of the operation.
  | Indeterminate -- ^ The value of the operation could not be determined.
  | Invalid -- ^ The operation failed.

data SBE m = SBE
  { -- Constant terms
    termInt  :: Int -> Integer -> m (SBETerm m)
  --, termWord :: Int -> Integer -> m (SBETerm m)
  , termBool :: Bool   -> m (SBETerm m)
    -- Common operators
  , applyEq     :: SBETerm m -> SBETerm m -> m (SBETerm m)
  , applyIte    :: SBETerm m -> SBETerm m -> SBETerm m -> m (SBETerm m)
  --, applyBNot   :: SBETerm m -> m (SBETerm m)
  --, applyBAnd   :: SBETerm m -> SBETerm m -> m (SBETerm m)
  --, applyBOr    :: SBETerm m -> SBETerm m -> m (SBETerm m)
  --, applyBXor   :: SBETerm m -> SBETerm m -> m (SBETerm m)
  , applyINot   :: SBETerm m -> m (SBETerm m)
  , applyIAnd   :: SBETerm m -> SBETerm m -> m (SBETerm m)
  , applyIOr    :: SBETerm m -> SBETerm m -> m (SBETerm m)
  , applyIXor   :: SBETerm m -> SBETerm m -> m (SBETerm m)
  , applyShl    :: SBETerm m -> SBETerm m -> m (SBETerm m)
  , applyShr    :: SBETerm m -> SBETerm m -> m (SBETerm m)
    -- | @applyArith op a b@ performs LLVM arithmetic operation @op@
  , applyArith  :: LLVM.ArithOp -> SBETerm m -> SBETerm m -> m (SBETerm m)
    -- | @memInitMemory@ returns an initial heap with no values defined.
  , memInitMemory :: m (SBEMemory m)
    -- | @memAlloca h tp i align@ allocates memory on the stack for the given
    -- @i@ elements with the type @tp@ with an address aligned at a @2^align@
    -- byte boundary.
    -- TODO: Add support for malloc, new, etc.
  , memAlloca :: SBEMemory m
              -> LLVM.Type
              -> LLVM.Typed (SBETerm m)
              -> Int
              -> m (SBETerm m, SBEMemory m)
    -- | @memLoad h ptr@ returns the value in the given location in memory.
  , memLoad :: SBEMemory m
            -> LLVM.Typed (SBETerm m)
            -> m (SBETerm m)
    -- | @memStore h v ptr@ stores the value @v@ in the location @ptr@ in the
    -- heap @h@ and returns the modified heap.
  , memStore :: SBEMemory m
             -> LLVM.Typed (SBETerm m)
             -> SBETerm m
             -> m (SBEMemory m)
    -- | @memAddDefine mem d blocks@ adds a definition of @d@ with block
    -- identifiers @blocks@ to the memory @mem@ and returns a pointer to
    -- the definition, and updated memory.
    -- It is undefined to call this function with a symbol that has already
    -- been defined in the memory.
  , memAddDefine :: SBEMemory m
                 -> LLVM.Symbol
                 -> [LLVM.Ident]
                 -> m (SBETerm m, SBEMemory m)
    -- | @memLookupDefine ptr@ returns the symbol at the given address.
    -- Lookup may fail if the pointer does not point to a symbol, or if
    -- the pointer is a symbolic vlaue without a clear meaning.
    -- TODO: Consider moving this function to the symbolic simulator.
  , memLookupDefine :: SBEMemory m -> SBETerm m -> m (PartialResult LLVM.Symbol)
    -- | @memBlockAddress mem d l@ returns the address of basic block with
    -- label @l@ in definition @d@.
  , memBlockAddress :: SBEMemory m -> LLVM.Symbol -> LLVM.Ident -> m (SBETerm m)
    -- | @memSelect c t f@ returns a memory that corresponds to @t@ if @c@ is
    -- true and @f@ otherwise.  This function is useful in merging.
  , memSelect :: SBETerm m -> SBEMemory m -> SBEMemory m -> m (SBEMemory m)
  }

