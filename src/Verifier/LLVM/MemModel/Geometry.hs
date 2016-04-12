------------------------------------------------------------------------
-- |
-- Module           : Verifier.LLVM.MemModel.Geometry
-- Copyright        : (c) Galois, Inc 2011-2013
-- Maintainer       : Joe Hendrix <jhendrix@galois.com>
-- Stability        : provisional
-- License          : BSD3
-- 
-- This module provides a data structure for storing memory geometry
-- information when using concrete addresses.
------------------------------------------------------------------------
module Verifier.LLVM.MemModel.Geometry
  ( MemGeom(..)
  , Addr
  , defaultMemGeom
  ) where

import Verifier.LLVM.Codebase.DataLayout

-- | Represents ranges of addresses for stack, code, data, and heap.
data MemGeom = MemGeom {
        mgStack :: (Addr, Addr)
      , mgCode :: (Addr, Addr)
      , mgData :: (Addr, Addr)
      , mgHeap :: (Addr, Addr)
      }

-- | Addresses are just integers.
type Addr = Integer


-- | The default memory layout makes the following simple division:
-- 
-- Top  1/4: Stack
-- Next 1/8: Code
-- Next 1/8: Data
-- Last 1/2: Heap
defaultMemGeom :: DataLayout  -> MemGeom
defaultMemGeom dl
    | w < 16 =  error "Pointers must be at least 16bits to get sufficient memory size."
    | otherwise = 
        MemGeom (stackStart, stackEnd)
                (codeStart,  codeEnd)
                (dataStart,  dataEnd)
                (heapStart,  heapEnd)
  where
    w = ptrBitwidth dl
    stackStart  = 4096 -- Start at first page rather than null
    codeStart   = 2 ^ w `div` 4
    dataStart   = codeStart + 2 ^ w `div` 8
    heapStart   = dataStart + 2 ^ w `div` 8

    stackEnd    = codeStart - 1
    codeEnd     = dataStart - 1
    dataEnd     = heapStart - 1
    heapEnd     = 2 ^ w - 1
