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

data MemType = BitBlastBuddyAlloc | BitBlastDagBased deriving (Show)

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

-- | A result returned by an operation that is partial on symbolic operations.
-- The first element is the verification condition needed to show the result
-- is valie, result, while the second is the verification condition
-- needed to show the result is valid.
type SBEPartialResult m r  = (SBETerm m, r)

-- | Represents a partial result of trying to obtain a concrete value from
-- a symbolic term.
data LookupDefineResult
  = Result LLVM.Symbol -- ^ The definition associated with the address.
  | Indeterminate -- ^ The value of the operation could not be determined.
  | Invalid -- ^ The operation failed, because it had an invalid value.
  deriving Show

-- | Result returned by @stackAlloca@ (defined below).
data StackAllocaResult t m
  -- | @SAResult c p m@ is returned when allocation succeeded. @c@ is a symbolic
  -- path constraint that the allocation must satisfy for allocation to have
  -- succeeded, @m@ is the new memory state, and @p@ is a @ptr@ to the newly
  -- allocated space.  @c@ is false if the allocation failed due to
  -- insufficient space.
  = SAResult t t m
  -- | Returned if stackAlloca given a symbolic length and the implementation
  -- does not support this.
  | SASymbolicCountUnsupported

-- | Result returned by @heapAlloc@ (defined below). Currently
-- isomorphic to StackAllocResult, but that might change.
data HeapAllocResult t m
  -- | @HAResult c p m@ is returned when allocation succeeded. @c@
  -- is a symbolic path constraint that the allocation must satisfy
  -- for allocation to have succeeded, @m@ is the new memory state,
  -- and @p@ is a @ptr@ to the newly
  -- allocated space. @c@ is false if the allocation failed due to
  -- insufficient space.
  = HAResult t t m
  -- | Returned if heapAlloc given a symbolic length and the
  -- implementation does not support this.
  | HASymbolicCountUnsupported

data SBE m = SBE
  {
    ----------------------------------------------------------------------------
    -- Term creation, operators

    -- | @termInt w n@ creates a term representing the constant @w@-bit
    -- value @n@
    termInt  :: Int -> Integer -> m (SBETerm m)

    -- | @freshInt w@ creates a term representing a symbolic @w@-bit value
  , freshInt :: Int -> m (SBETerm m)

    -- | @termBool b@ creates a term representing the constant boolean
    -- (1-bit) value @b@
  , termBool :: Bool -> m (SBETerm m)

    -- | @termArray ts@ creates a term representing an array with element terms
    -- @ts@ (which must be nonempty).  A term list containing with
    -- heterogenously-sized terms is permitted.
  , termArray :: [SBETerm m] -> m (SBETerm m)

    -- | @termDecomp tys t@ decomposes the given term into @(length tys)@ terms,
    --  with each taking their type from the corresponding element of @tys@.
  , termDecomp :: [LLVM.Type] -> SBETerm m -> m [LLVM.Typed (SBETerm m)]

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
    -- | @applyBNot @a@ performs negation of a boolean term
  , applyBNot :: SBETerm m -> m (SBETerm m)

    ----------------------------------------------------------------------------
    -- Term miscellany

    -- | Yields the width of the given term in bits
  , termWidth   :: SBETerm m -> Integer
  , closeTerm   :: SBETerm m -> SBEClosedTerm m
  , prettyTermD :: SBETerm m -> Doc

    ----------------------------------------------------------------------------
    -- Memory model interface

    -- | @memDump h@ prints the contents of the memory model; the first
    -- parameter optionally constrains address ranges.
  , memDump :: SBEMemory m -> Maybe [(Integer, Integer)] -> m ()

    -- | @memLoad m p@ returns a pair @(v,c)@ where @v@ denotes the value at
    -- address @p@ in memory @m@, and @c@ denotes an additional path constraint
    -- that ensures the address @p@ is a valid memory location in @m@.
    -- In other words, @p@ is a valid memory location if @c@ is true.
  , memLoad :: SBEMemory m
            -> LLVM.Typed (SBETerm m)
            -> m (SBEPartialResult m (SBETerm m))
    -- | @memStore m v p@ returns a pair @(m',c)@ where @m'@ denotes the memory
    -- obtained by storing value @v@ at address @p@, and @c@ denotes an
    -- additional path constraint that ensures the address @p@ is a valid memory
    -- location in @m@.
  , memStore :: SBEMemory m
             -> LLVM.Typed (SBETerm m)
             -> SBETerm m
             -> m (SBEPartialResult m (SBEMemory m))
    -- | @memMerge c t f@ returns a memory that corresponds to @t@ if @c@ is
    -- true and @f@ otherwise.
  , memMerge :: SBETerm m -> SBEMemory m -> SBEMemory m -> m (SBEMemory m)
    -- | @memAddDefine mem d blocks@ adds a definition of @d@ with block
    -- labels @blocks@ to the memory @mem@ and returns a pointer to
    -- the definition, and updated memory if space is available.  If space
    -- is unavailable, then this returns nothing.
    -- It is undefined to call this function with a symbol that has already
    -- been defined in the memory.
  , memAddDefine :: SBEMemory m
                 -> LLVM.Symbol
                 -> [LLVM.BlockLabel]
                 -> m (Maybe (SBETerm m, SBEMemory m))
    -- | @memInitGlobal mem data@ attempts to write @data@ to a newly
    -- allocated region of memory in address space for globals.  If
    -- space is available, returns a pointer to the region
    -- and updated memory.  Otherwise returns @Nothing@.
  , memInitGlobal :: SBEMemory m
                  -> LLVM.Typed (SBETerm m)
                  -> m (Maybe (SBETerm m, SBEMemory m))
    -- | @codeBlockAddress mem d l@ returns the address of basic block with
    -- label @l@ in definition @d@.
  , codeBlockAddress :: SBEMemory m -> LLVM.Symbol -> LLVM.BlockLabel -> m (SBETerm m)
    -- | @codeLookupDefine ptr@ returns the symbol at the given address.
    -- Lookup may fail if the pointer does not point to a symbol, or if
    -- the pointer is a symbolic value without a clear meaning.
    -- TODO: Consider moving this function to the symbolic simulator.
  , codeLookupDefine :: SBEMemory m -> SBETerm m -> m LookupDefineResult
    -- | @stackAlloca h tp i align@ allocates memory on the stack for the given
    -- @i@ elements with the type @tp@ with an address aligned at a @2^align@
    -- byte boundary.
  , stackAlloca :: SBEMemory m
                -> LLVM.Type
                -> LLVM.Typed (SBETerm m)
                -> Int
                -> m (StackAllocaResult (SBETerm m) (SBEMemory m))
    -- | @stackPushFrame mem@ returns the memory obtained by pushing a new
    -- stack frame to @mem@.
  , stackPushFrame :: SBEMemory m -> m (SBEPartialResult m (SBEMemory m))
    -- | @stackPushFrame mem@ returns the memory obtained by popping a new
    -- stack frame from @mem@.
  , stackPopFrame :: SBEMemory m -> m (SBEMemory m)
    -- | @heapAlloc h tp i align@ allocates memory in the heap for the given
    -- @i@ elements with the type @tp@ with an address aligned at a @2^align@
    -- byte boundary.
  , heapAlloc :: SBEMemory m
              -> LLVM.Type
              -> LLVM.Typed (SBETerm m)
              -> Int
              -> m (HeapAllocResult (SBETerm m) (SBEMemory m))
    -- | @memcpy mem dst src len align@ copies @len@ bytes from @src@ to @dst@,
    -- both of which must be aligned according to @align@ and must refer to
    -- non-overlapping regions.
  , memCopy :: SBEMemory m
            -> SBETerm m -- ^ Destination pointer
            -> SBETerm m -- ^ Source pointer
            -> SBETerm m -- ^ Number of bytes to copy
            -> SBETerm m -- ^ Alignment in bytes
            -> m (SBEPartialResult m (SBEMemory m))

    ----------------------------------------------------------------------------
    -- Output functions

    -- | @writeAiger f ts@ writes an AIG reprsentation of (juxtaposed) @ts@ into
    -- file @f@ in the Aiger format.
  , writeAiger :: String -> [SBETerm m] -> m ()

    -- | @evalAiger inps t@ evaluates an AIG with the given concrete inputs;
    -- result is always a concrete term.
  , evalAiger :: [Bool] -> SBETerm m -> m (SBETerm m)
  }
