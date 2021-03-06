-- | This module is for sketching out rough implementation decisions and spelling
-- out observations in how the LLVM symbolic simulator should model values and the
-- heap.  It may be deleted or moved as necessary as the symbolic simulator becomes
-- more mature.
module Memory where

-- Observations:
-- * LLVM code has instructions for bitcasting from any type to any other type
--   (either 'bitcast .. to' or a type specific instruction such as
--   'inttoptr ... to').  It may be reasonable for conversions to fail (such as
--   converting a symbolic integer to floating point), but even in that case we
--   need to print a useful error message.
-- * Pointers are going to be 32 or 64bit integers (may want to make this an
--   option). 16bit seems possible too, but may not be essential.
-- * Pointers may refer to basic block addresses (and consequently integers may
--   as well).  However, the only code that will use this (in a defined way) is
--   the 'indirectbr' instruction, which must also contain a list of possible
--   targets.  Rather than have special values for these basic block addresses,
--   it may be sufficient to simply maintain a mapping from (function,label)
--   pairs to an associated address, then add conditional checks in handling
--   basic blocks that checks for each potential target of 'indirectbr' whether
--   the address associated to the target matches the value given to 'indirectbr'.
-- * A pointer may point to a function as well, and be used in a 'call' or 'invoke'.
--   We probably want to require that a function pointer is ground, and maintain
--   a map from valid integers to the function associated with that integer.
-- * The memory will need to refer to the following types of values:
--   * Global data
--   * Functions
--   * Basic blocks within a function.
--   * Variables allocated on the stack (using 'alloca').
--   * Variables allocated on the hepa (using 'malloc', 'new' (C++), or an
--     OS-specific memory allocation routine (e.g., LocalAlloc).
-- 
-- Fundamentally, I think the heap should just be a map from integers to integers,
-- and we should rely on bitblasted bitpatterns to maintain distinctness (e.g.,
-- the heap could start at address 0x10000, while the stack is at address 0x20000).
-- The lookup function will take advantage of mismatches in the high-order bits of
-- allocations to differentiate between reads and writes.  We could start with a
-- slow linked list implementation, then look at more efficient modes.
--
-- As a cautionary note, this may run into problems if/when we switch to a word
-- level implementation, because the bit-patterns may not be as readily available.
-- Perhaps they could be generated on demand during the store or load instruction.

-- LLVM First class types are the only type that can be produced by instructions.
-- They include the following:
--   integer, floating point, pointer, vector, structure, array, label, metadata.
-- * In addition to the variables below, all types have an 'undef' value and a
--   a 'trap value' (representing a undefined operation that may have side effects).

-- Array
--   Values
--   * Constants
--   Operations
--   * 'extractvalue', 'inservalue', 
-- Floating point
--   Values
--   * Constants with one the following subtypes (see [1] for more details)
--     float	 32-bit floating point value
--     double	 64-bit floating point value
--     fp128	 128-bit floating point value (112-bit mantissa)
--     x86_fp80	 80-bit floating point value (X87)
--     ppc_fp128 128-bit floating point value (two 64-bits)
--   * Constants formed from following operations:
--     'fadd', 'fdiv', 'fmul', 'frem', 'fsub'
--   * 'fptrunc', 'fpext'
--   * 'uitofp', 'sitofp' | Convert integer to floating point, may fail if
--       integer is symbolic.
-- Function
--   Values
--   * zero?
--   Operations
--   * 'call', 'invoke'
-- Integer | iX (where X is the bit width).
--   A simple type that specifies an arbitrary bit width for the integer type.
--   Values:
--   * 'true' and 'false' for i1.
--   * Integer constants
--   * Integer variables
--   * Binary operations applied to two integers:
--     'add', 'mul', 'sdiv', 'srem 'sub', 'udiv', 'urem'
--   * Bitwise inary operations applied to two integers:
--     'shl', 'lshr', 'ashr', 'and', 'or', 'xor', 
--   * 'sext', 'trunc', 'zext'
--   * 'fptoui', 'fptosi' | Convert floating point to integer.
-- Label
--   A pointer to a basic block.  It is referenced in branch instructions.
--   Values
--   * Labels with a block.
--   Operations
--   * Branch to specific blocks.
-- Metadata
--   Values
--   * Structure like constants
--   Operations
--   * ??? Note sure
-- Opague
--   Structure types that do not have a body specified (such as a C forward
--   declared struct).
--   * Just used for pointer conversions (afaik).
-- Pointer | <type> *
--   Specifies code and data in memory.  May have an operational address space
--     attribute.
--   Values
--   * null
--   * Basic block Labels "i8* blockaddress(@fn, %label)"
--   * Global variables and function addresses.
--   * Stack and heap allocations.
--   Operations
--   * 'alloca', 'load', 'store'
--   * 'cmpxchg', 'atomicrmw', 'getelementptr'
--   * 'ptotoint' | Convert a pointer to an integer type.
--   * 'inttoptr' | Convert integer to pointer type.
-- Structure
--   Values
--   * Constants
--   Operations
--   * 'extractvalue', 'inservalue', 
-- Vector | <# elements> x <elementtype>
--   A non-empty vector of elements (used with SIMD instructions).
--   Values
--   * Constants
--   * Pointwise arithmetic and bitwise binary operations on the underlying
--     type (e.g., 'add').
--   Operations
--   * 'extractelement', 'insertelement', 'shufflevector'
-- X86mmx
--   Values
--   * Zero (possibly?)
--   Operations?? Unclear to me.
-- 
-- [1] http://llvm.org/docs/LangRef.html

{-
data PartialResult r
  = Result r -- ^ The result of the operation.
  | Indeterminate -- ^ The value of the operation could not be determined.
  | Invalid -- ^ The operation failed.

type family SBEHeap m
type family SBETerm m

data SymMemory m = SymMemory m {
    smAlloca :: SBEHeap m -> LLVM.Type -> m (SBETerm m, SBEHeap m)
    -- | @smLoad tp valuw@ loads 
  , smLoad :: SBEHeap m -> LLVM.Type -> SBETerm m -> m (SBETerm m)
    -- | @smStore value pointer@ stores value in address denoted by pointer.
  , smStore :: v -> v -> IO ()
    -- | @smLookupDefine addr@ returns symbolic definition associated with
    -- address.  Lookup may fail if the 
  , smLookupDefine :: v -> IO (PartialResult SymDefine)
    -- | @smBlockAddress d l@ returns address of basic block with label @l@ in
    -- definition @d@.
  , smBlockAddress :: LLVM.Symbol -> LLVM.Ident -> IO v 
  }
