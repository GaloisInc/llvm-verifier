{- |
Module           : $Header$
Description      : The interface to a symbolic backend
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}

module LSS.SBEInterface where

import           Text.PrettyPrint.HughesPJ
import qualified Text.LLVM.AST   as LLVM

-- | SBETerm yields the type used to represent terms in particular SBE interface
-- implementation.
type family SBETerm (sbe :: * -> *)

-- | SBEClosedTerm yields the newtype-wrapped, isomorphic-to-tuple type used to
-- represent SBE interface terms together with any SBE-specific state necessary
-- to perform certain operations (e.g. constant projection/injection) on those
-- terms.
type family SBEClosedTerm (sbe :: * -> *)

-- | SBEMemory yields the type used to represent the memory in a particular SBE
-- interface implementation.
type family SBEMemory (sbe :: * -> *)

-- | Represents a partial result of trying to obtain a concrete value from
-- a symbolic term.
data PartialResult r
  = Result r -- ^ The result of the operation.
  | Indeterminate -- ^ The value of the operation could not be determined.
  | Invalid -- ^ The operation failed.
  deriving Show

data SBE m = SBE
  {
    ----------------------------------------------------------------------------
    -- Term creation, operators

    -- | @termInt w n@ creates a term representing the constant @w@-bit
    -- value @n@
    termInt  :: Int -> Integer -> m (SBETerm m)
    -- | @termBool b@ creates a term representing the constant boolean
    -- (1-bit) value @b@
  , termBool :: Bool   -> m (SBETerm m)

    ----------------------------------------------------------------------------
    -- Term operator application

    -- | @applyIte a b c@ creates an if-then-else term
  , applyIte    :: SBETerm m -> SBETerm m -> SBETerm m -> m (SBETerm m)
    -- | @applyICmp op a b@ performs LLVM integer comparison @op@
  , applyICmp   :: LLVM.ICmpOp -> SBETerm m -> SBETerm m -> m (SBETerm m)
    -- | @applyBitwise op a b@ performs LLVM bitwise operation @op@
  , applyBitwise :: LLVM.BitOp -> SBETerm m -> SBETerm m -> m (SBETerm m)
    -- | @applyArith op a b@ performs LLVM arithmetic operation @op@
  , applyArith  :: LLVM.ArithOp -> SBETerm m -> SBETerm m -> m (SBETerm m)
    -- | @applyConv op v t@ performs LLVM conversion operation @op@
  , applyConv   :: LLVM.ConvOp -> SBETerm m -> LLVM.Type -> m (SBETerm m)
    ----------------------------------------------------------------------------
    -- Term miscellany

  , closeTerm   :: SBETerm m -> SBEClosedTerm m
  , prettyTermD :: SBETerm m -> Doc

    ----------------------------------------------------------------------------
    -- Memory model interface

    -- | @memDump h@ prints the contents of the memory model
  , memDump :: SBEMemory m -> m ()

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
    -- | @memMerge c t f@ returns a memory that corresponds to @t@ if @c@ is
    -- true and @f@ otherwise.
  , memMerge :: SBETerm m -> SBEMemory m -> SBEMemory m -> m (SBEMemory m)
    -- | @memAddDefine mem d blocks@ adds a definition of @d@ with block
    -- identifiers @blocks@ to the memory @mem@ and returns a pointer to
    -- the definition, and updated memory.
    -- It is undefined to call this function with a symbol that has already
    -- been defined in the memory.
  , memAddDefine :: SBEMemory m
                 -> LLVM.Symbol
                 -> [LLVM.Ident]
                 -> m (SBETerm m, SBEMemory m)
    -- | @codeBlockAddress mem d l@ returns the address of basic block with
    -- label @l@ in definition @d@.
  , codeBlockAddress :: SBEMemory m -> LLVM.Symbol -> LLVM.Ident -> m (SBETerm m)
    -- | @codeLookupDefine ptr@ returns the symbol at the given address.
    -- Lookup may fail if the pointer does not point to a symbol, or if
    -- the pointer is a symbolic vlaue without a clear meaning.
    -- TODO: Consider moving this function to the symbolic simulator.
  , codeLookupDefine :: SBEMemory m -> SBETerm m -> m (PartialResult LLVM.Symbol)
    -- | @stackAlloca h tp i align@ allocates memory on the stack for the given
    -- @i@ elements with the type @tp@ with an address aligned at a @2^align@
    -- byte boundary.
    -- TODO: Add support for malloc, new, etc.
  , stackAlloca :: SBEMemory m
                -> LLVM.Type
                -> LLVM.Typed (SBETerm m)
                -> Int
                -> m (SBETerm m, SBEMemory m)
    -- | @stackPushFrame mem@ returns the memory obtained by pushing a new
    -- stack frame to @mem@.
  , stackPushFrame :: SBEMemory m -> m (SBEMemory m)
    -- | @stackPushFrame mem@ returns the memory obtained by popping a new
    -- stack frame from @mem@.
  , stackPopFrame :: SBEMemory m -> m (SBEMemory m)

    ----------------------------------------------------------------------------
    -- Output functions

    -- | @writeAiger f t@ writes an AIG representation of @t@ into
    -- file @f@ in the Aiger format.
  , writeAiger :: String -> SBETerm m -> m ()
  }