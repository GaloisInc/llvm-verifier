{- |
Module           : $Header$
Description      : The interface to a symbolic backend
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE TypeFamilies #-}

module LSS.SBEInterface where

import qualified Text.LLVM.AST as LLVM

-- | SBETerm is a function over types that yields the term type associated with
-- a particular SBE interface implementation
type family SBETerm (sbe :: * -> *)

-- | SBEMemory yields the type used to represent the memory in a particular SBE
-- interface implementation.
type family SBEMemory (sbe :: * -> *)

-- | SBEMonad is a function over types that yields the base monad type
-- associated with a particular SBE interface implementation
type family SBEMonad sbe :: * -> *
type instance SBEMonad (SBE m) = m

-- | Represents a partial result of trying to obtain a concrete value from
-- a symbolic term.
data PartialResult r
  = Result r -- ^ The result of the operation.
  | Indeterminate -- ^ The value of the operation could not be determined.
  | Invalid -- ^ The operation failed.

data SBE m = SBE
  { falseTerm   :: m (SBETerm m)
  , termInteger :: Integer -> m (SBETerm m)
  , applyAdd    :: SBETerm m -> SBETerm m -> m (SBETerm m)
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
  }

--------------------------------------------------------------------------------
-- SBE implementations

newtype SBEStub a = SBEStub { runStub :: a }
type instance SBETerm SBEStub = Int

data SBEStubMemoryOne = UndefinedMemoryOne
type instance SBEMemory SBEStub = SBEStubMemoryOne

sbeStub :: SBE SBEStub
sbeStub = SBE
  { falseTerm   = SBEStub 0
  , termInteger = SBEStub . fromIntegral
  , applyAdd    = \x y -> SBEStub (x + y)
  , memInitMemory = SBEStub undefined
  , memAlloca = \_mem _eltType _len _a -> SBEStub undefined
  , memLoad = \_mem _ptr -> SBEStub undefined
  , memStore = \_mem _val _ptr -> SBEStub undefined
  , memAddDefine = \_mem _sym _id -> SBEStub (undefined, undefined)
  , memLookupDefine = \_mem _t -> SBEStub undefined
  , memBlockAddress = \_mem _s _b -> SBEStub undefined
  }

liftStubToIO :: SBEStub a -> IO a
liftStubToIO = return . runStub

newtype SBEStubTwo a = SBEStubTwo { runStubTwo :: a }
type instance SBETerm SBEStubTwo = Integer

data SBEStubMemoryTwo = UndefinedMemoryTwo

type instance SBEMemory SBEStubTwo = SBEStubMemoryTwo

sbeStubTwo :: SBE SBEStubTwo
sbeStubTwo = SBE
  { falseTerm   = SBEStubTwo 0
  , termInteger = SBEStubTwo . fromIntegral
  , applyAdd    = \x y -> SBEStubTwo (x + y)
  , memInitMemory = SBEStubTwo undefined
  , memAlloca = \_mem _eltType _len _a -> SBEStubTwo undefined
  , memLoad = \_mem _ptr -> SBEStubTwo undefined
  , memStore = \_mem _val _ptr -> SBEStubTwo undefined
  , memAddDefine = \_mem _sym _id -> SBEStubTwo (undefined, undefined)
  , memLookupDefine = \_mem _t -> SBEStubTwo undefined
  , memBlockAddress = \_mem _s _b -> SBEStubTwo undefined
  }

liftStubTwoToIO :: SBEStubTwo a -> IO a
liftStubTwoToIO = return . runStubTwo
