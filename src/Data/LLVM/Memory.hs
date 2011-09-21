{- |
Module           : $Header$
Description      : Info about LLVM memory
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.LLVM.Memory
  ( Addr
  , MemGeom(..)
  , defaultMemGeom
  , resolveType
  )
where

import           Data.LLVM.TargetData
import qualified Text.LLVM            as L

type Addr = Integer

resolveType :: LLVMContext -> L.Type -> L.Type
resolveType lc (L.Alias nm) = resolveType lc (llvmLookupAlias lc nm)
resolveType _ tp = tp

data MemGeom = MemGeom {
        mgStack :: (Addr, Addr)
      , mgCode :: (Addr, Addr)
      , mgData :: (Addr, Addr)
      , mgHeap :: (Addr, Addr)
      }

-- We make a keep it simple concession and divide up the address space as
-- follows:
--
-- Top  1/4: Stack
-- Next 1/8: Code
-- Next 1/8: Data
-- Last 1/2: Heap
--
-- One advantage of this is that it's easy to tell the segment to which a
-- pointer belongs simply by inspecting its address.
--
-- TODO: Allow user overrides of memory geom
defaultMemGeom :: LLVMContext -> MemGeom
defaultMemGeom lc =
  MemGeom (stackStart, stackEnd)
          (codeStart,  codeEnd)
          (dataStart,  dataEnd)
          (heapStart,  heapEnd)
  where
    w           = llvmAddrWidthBits lc
    addrSpace   = 2 ^ w - 1
    stackStart  = 0
    stackEnd    = addrSpace `div` 4
    codeStart   = stackEnd + 1
    codeEnd     = codeStart + addrSpace `div` 8
    dataStart   = codeEnd + 1
    dataEnd     = dataStart + addrSpace `div` 8
    heapStart   = dataEnd + 1
    heapEnd     = addrSpace
