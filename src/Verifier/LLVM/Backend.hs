{- |
Module           : $Header$
Description      : The interface to a symbolic backend
License          : BSD3
Stability        : provisional
Point-of-contact : jhendrix
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
module Verifier.LLVM.Backend
  ( BitWidth
  , IntArithOp(..)
  , TypedExpr(..)
  , SBE(..)
  , SBETerm
  , SBEPred
  , SBEMemory
  , SBEPair(..)
  , AllocResult(..)
  , LookupSymbolResult
  , LookupSymbolError(..)
  , SBEPartialResult
  , termArray
  , termInt
  , termAdd
  , termMul
  , termZExt
  , termTruncScalar
  , AIG.SatResult(..)
  ) where

import Data.Kind (Type)
import qualified Data.Vector as V
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import qualified Data.AIG as AIG

import Verifier.LLVM.Codebase.AST

-- | SBETerm yields the type used to represent terms in particular SBE
-- interface implementation.
type family SBETerm (sbe :: Type -> Type)

-- | SBEPred yields the type used to represent a Boolean predicate associated to
-- a particular SBE interface implementation.
type family SBEPred (sbe :: Type -> Type)

-- | SBEMemory yields the type used to represent the memory in a particular SBE
-- interface implementation.
type family SBEMemory (sbe :: Type -> Type)

-- | A result returned by an operation that is partial on symbolic operations.
-- The first element is the verification condition needed to show the result
-- is valie, result, while the second is the verification condition
-- needed to show the result is valid.
type SBEPartialResult m r  = (SBEPred m, r)

-- | Represents an error obtained from trying to lookup a symbol.
data LookupSymbolError
  = Indeterminate -- ^ The value of the operation could not be determined.
  | Invalid -- ^ The operation failed, because it had an invalid value.
  deriving (Show)

type LookupSymbolResult = Either LookupSymbolError Symbol

-- | Result returned by @stackAlloca@ (defined below).
data AllocResult sbe
  -- | @SAResult c p m@ is returned when allocation succeeded. @c@ is a symbolic
  -- path constraint that the allocation must satisfy for allocation to have
  -- succeeded, @m@ is the new memory state, and @p@ is a @ptr@ to the newly
  -- allocated space.  @c@ is false if the allocation failed due to
  -- insufficient space.
  = AResult (SBEPred sbe) (SBETerm sbe) (SBEMemory sbe)
  -- | Return an error message if allocation failed.
  | AError String

data SBE m = SBE
  {
    ----------------------------------------------------------------------------
    -- Term creation, operators

    -- | @termBool b@ creates a term representing the constant boolean
    -- (1-bit) value @b@
    sbeTruePred :: SBEPred m
    -- | Return predicate indicating if two integer terms are equal.
  , applyIEq :: BitWidth -> SBETerm m -> SBETerm m -> m (SBEPred m)
    -- applyIEq sbe w x y = applyTypedExpr sbe (IntCmp Ieq Nothing w x y)
    -- | Return conjunction of two predicates.
  , applyAnd :: SBEPred m -> SBEPred m -> m (SBEPred m)
    -- applyAnd sbe x y = applyTypedExpr sbe (IntArith And Nothing 1 x y)
    -- | @applyBNot a@ performs negation of a boolean term
  , applyBNot :: SBEPred m -> m (SBEPred m)
    -- | @applyPredIte a b c@ creates an if-then-else term
  , applyPredIte :: SBEPred m -> SBEPred m -> SBEPred m -> m (SBEPred m)
    -- | @applyIte a b c@ creates an if-then-else term.  Or returns error
    -- if terms cannot be merged.
  , applyIte :: MemType
             -> SBEPred m
             -> SBETerm m
             -> SBETerm m
             -> m (Either String (SBETerm m))
    -- | Interpret the term as a concrete boolean if it can be.
  , asBool :: SBEPred m -> Maybe Bool

  , prettyPredD :: SBEPred m -> Doc

    -- | Evaluate a predicate for given input bits.
  , evalPred :: [Bool] -> SBEPred m -> m Bool

    -- | @freshInt w@ creates a term representing a symbolic @w@-bit value
  , freshInt :: BitWidth -> m (SBETerm m)

    -- | Simplify any conditionals according to a list of assumptions.
  , simplifyConds :: SBEPred m -> SBETerm m -> m (SBETerm m)

    ----------------------------------------------------------------------------
    -- Term operator application

  , typedExprEval :: forall v . TypedExpr v -> IO (ExprEvalFn v (SBETerm m))

    -- | Evaluate a typed expression.
  , applyTypedExpr :: TypedExpr (SBETerm m) -> m (SBETerm m)

    -- | Interpret the term as a concrete unsigned integer if it can
    -- be.  The first int is the bitwidth.  The term should be
    -- normalized to be properly identified as a concrete value.
  , asUnsignedInteger :: BitWidth -> SBETerm m -> Maybe Integer

    -- | Interpret the term as a concrete signed integer if it can be.
    -- The first int is the bitwidth.  The term should be normalized
    -- to be properly identified as a concrete value.
  , asSignedInteger :: BitWidth -> SBETerm m -> Maybe Integer

    -- | Interpret a pointer as an unsigned integer.
  , asConcretePtr :: SBETerm m -> Maybe Integer

  , prettyTermD :: SBETerm m -> Doc

    ----------------------------------------------------------------------------
    -- Memory model interface

    -- | @memDump h@ prints the contents of the memory model; the
    -- first parameter optionally constrains address ranges.
    --
    -- Each constraint @(a,b)@ stands for a closed-open range
    -- @{a,a+1,...,b-1}@, and multiple range constraints are ORed /
    -- memory that falls in *any* of the given ranges is printed. If
    -- the constraint is 'Nothing', then all memory is printed.
    --
    -- Note that some backends ignore the range constraints. Only the
    -- bitblast backend has been tested with range constraints at this
    -- time.
  , memDump :: SBEMemory m -> Maybe [(Integer, Integer)] -> m ()

    -- | @memLoad m tp p a@ returns a pair @(c,v)@ where @v@ denotes the value at
    -- address @p@ in memory @m@, and @c@ denotes an additional path constraint
    -- that ensures the address @p@ is a valid memory location in @m@.
    -- In other words, @p@ is a valid memory location if @c@ is true.
  , memLoad :: SBEMemory m
            -> MemType
            -> SBETerm m
            -> Alignment
            -> m (SBEPartialResult m (SBETerm m))
    -- | @memStore m p tp v a@ returns a pair @(c,m')@ where @m'@ denotes the memory
    -- obtained by storing value @v@ with type @tp@ at address @p@, and @c@ denotes an
    -- additional path constraint that ensures the address @p@ is a valid memory
    -- location in @m@.
  , memStore :: SBEMemory m
             -> SBETerm m -- Address to store value at. 
             -> MemType   -- Type of value
             -> SBETerm m -- Value to store
             -> Alignment
             -> m (SBEPartialResult m (SBEMemory m))
    -- | @memcpy mem dst src len align@ copies @len@ bytes from @src@ to @dst@,
    -- both of which must be aligned according to @align@ and must refer to
    -- non-overlapping regions.
  , memCopy :: SBEMemory m
            -> SBETerm m -- Destination pointer
            -> SBETerm m -- Source pointer
            -> BitWidth  -- Bitwidth for counting number of bits.
            -> SBETerm m -- Number of bytes to copy (should have
            -> SBETerm m -- Alignment in bytes (should have 32-bit bits)
            -> m (SBEPartialResult m (SBEMemory m))


    -- | @memAddDefine mem d blocks@ adds a definition of @d@ with block
    -- labels @blocks@ to the memory @mem@ and returns a pointer to
    -- the definition, the blocks, and updated memory if space is available.
    -- If space is unavailable, then this returns nothing.
    -- It is undefined to call this function with a symbol that has already
    -- been defined in the memory.
  , memAddDefine :: SBEMemory m
                 -> Symbol
                 -> [BlockLabel]
                 -> m (Maybe (SBETerm m, [SBETerm m], SBEMemory m))
    -- | @memInitGlobal mem tp data@ attempts to write @data@ to a newly
    -- allocated region of memory in address space for globals.  If
    -- space is available, returns a pointer to the region
    -- and updated memory.  Otherwise returns @Nothing@.
  , memInitGlobal :: SBEMemory m
                  -> MemType 
                  -> SBETerm m
                  -> m (Maybe (SBETerm m, SBEMemory m))

    -- | @codeLookupSymbol mem ptr@ returns the symbol at the given address
    -- in mem.  Lookup may fail if the pointer does not point to a symbol, or
    -- if the pointer is a symbolic value without a clear meaning.
  , codeLookupSymbol :: SBEMemory m -> SBETerm m -> m LookupSymbolResult

    -- | @stackAlloca h tp i align@ allocates memory on the stack for the given
    -- @i@ elements with the type @tp@ with an address aligned at a @2^align@
    -- byte boundary.
  , stackAlloc :: SBEMemory m -- Memory to allocate within.
               -> MemType     -- Type of elements to allocate.
               -> BitWidth    -- Width of count in bits. 
               -> SBETerm m   -- Count
               -> Alignment   -- Alignment required for allocation
               -> m (AllocResult m)
    -- | @stackPushFrame mem@ returns the memory obtained by pushing a new
    -- stack frame to @mem@.
  , stackPushFrame :: SBEMemory m -> m (SBEPartialResult m (SBEMemory m))
    -- | @stackPushFrame mem@ returns the memory obtained by popping a new
    -- stack frame from @mem@.
  , stackPopFrame :: SBEMemory m -> m (SBEMemory m)

    -- | @heapAlloc m tp iw i a@ allocates memory in the heap for @m@ for
    -- @i@ elements with the type @tp@ with an address aligned at a @2^align@
    -- byte boundary.
  , heapAlloc :: SBEMemory m -- Memory to allocate from.
              -> MemType     -- Type of value to allocate.
              -> BitWidth    -- Bitwidth of umber of elements to allocate.
              -> SBETerm m   -- Number of elements to allocate.
              -> Alignment   -- Alignment constraint.
              -> m (AllocResult m)

  , isAllocated :: SBEMemory m -- Memory to look in.
                -> SBETerm m   -- Pointer to look for.
                -> SBETerm m   -- Size of pointer to look for.
                -> m (SBEPred m)

    -- | @memBranch mem@ records that this memory is for a path that is
    -- about to branch.  This function should have no impact on the memory state,
    -- but allows the backend information about branches for optimization purposes.
    -- This call will be matched with a following call to @memBranchAbort@ or
    -- @memMerge@.
  , memBranch :: SBEMemory m -> m (SBEMemory m)
    -- | @memBranchAbort mem@ is called to indicate that the branch ended
    -- without a merge, because the other path failed.
  , memBranchAbort :: SBEMemory m -> m (SBEMemory m)
    -- | @memMerge c t f@ returns a memory that corresponds to @t@ if @c@ is
    -- true and @f@ otherwise.  The memory should have the same number of stack
    -- and merge frames.
  , memMerge :: SBEPred m -> SBEMemory m -> SBEMemory m -> m (SBEMemory m)

  -- | @predSAT t@ returns a 'AIG.SatResult' for the given
  -- predicate. If the current backend does not support SAT checking,
  -- returns 'Unknown' and prints a warning
  , termSAT :: SBEPred m -> m AIG.SatResult

    ----------------------------------------------------------------------------
    -- Output functions

    -- | @writeAiger f ts@ writes an AIG reprsentation of (juxtaposed) @ts@ into
    -- file @f@ in the Aiger format.
  , writeAiger :: FilePath -> [(MemType,SBETerm m)] -> m ()

    -- | @writeCnf f p@ writes a CNF representation of predicate @p@
    -- into file @f@.
  , writeCnf :: Maybe (FilePath -> SBEPred m -> m [Int])

    -- | @writeSmtLib isSmtLib2 f t@ writes an SMT-Lib representation
    -- of @t == 0@ into file @f@. If this is UNSAT, the expression is
    -- valid.
  , writeSmtLib :: Maybe (FilePath -> BitWidth -> SBETerm m -> m ())

    -- | @writeSAWCore f t@ writes a SAWCore representation of @t@ into
    -- file @f@, if this backend supports SAWCore output.
  , writeSAWCore :: Maybe (FilePath -> SBETerm m -> m ())

    -- | @evalAiger inps tp t@ evaluates an AIG with the given concrete inputs;
    -- result is always a concrete term.  The term @t@ has type @tp@.
  , evalAiger :: [Bool] -> MemType -> SBETerm m -> m (SBETerm m)

    -- | Run sbe computation in IO.
  , sbeRunIO :: forall v . m v -> IO v 
  }

-- | @termInt w n@ creates a term representing the constant @w@-bit
-- value @n@
termInt  :: SBE m -> BitWidth -> Integer -> m (SBETerm m)
termInt sbe w v = applyTypedExpr sbe (SValInteger w v)

-- | @termArray sbe tp v@ creates a term representing an array with element terms
-- @v@.  Each element must have type @tp@. 
termArray :: SBE m -> MemType -> V.Vector (SBETerm m) -> m (SBETerm m)
termArray sbe tp v = applyTypedExpr sbe (SValArray tp v)

termAdd :: SBE m -> BitWidth -> SBETerm m -> SBETerm m -> m (SBETerm m)
termAdd sbe w x y = applyTypedExpr sbe (IntArith (Add False False) Nothing w x y)

termMul :: SBE m -> BitWidth -> SBETerm m -> SBETerm m -> m (SBETerm m)
termMul sbe w x y = applyTypedExpr sbe (IntArith (Mul False False) Nothing w x y)

termZExt :: SBE sbe -> BitWidth -> SBETerm sbe -> BitWidth -> sbe (SBETerm sbe)
termZExt sbe iw x rw = applyTypedExpr sbe (ZExt Nothing iw x rw)

-- | Truncate a scalar to a smaller bitwidth.
termTruncScalar :: SBE m -> BitWidth -> SBETerm m -> BitWidth -> m (SBETerm m)
termTruncScalar sbe iw v rw = applyTypedExpr sbe (Trunc Nothing iw v rw)

-- | Represents some SBE backend and the initial memory.
data SBEPair where 
   SBEPair :: (Functor sbe, Ord (SBETerm sbe)) => SBE sbe -> SBEMemory sbe -> SBEPair
