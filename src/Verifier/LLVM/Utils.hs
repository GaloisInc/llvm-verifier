{- |
Module           : $Header$
Description      : Utility functions for execution of LLVM Symbolic programs
Stability        : provisional
Point-of-contact : jhendrix
-}
module Verifier.LLVM.Utils 
  ( -- * Arithmetic utilities
    isPow2
  , lg
  , nextPow2
  , nextMultiple
    -- * LLVM pretty AST constants.
  , intn
  , i1, i8, i16, i32, i64
  , i8p, i16p, i32p, i64p
  , voidPtr
  , voidTy
  , strTy
    -- * Pretty print utilities.
  , BitWidth
  , ppIntType
  , ppPtrType
  , ppIntVector
  , ppTypeVector
  ) where

import Data.Bits (Bits(..))
import Data.Int (Int32)
import qualified Text.LLVM     as L
import Text.LLVM.AST
import Text.PrettyPrint.HughesPJ

-- | Returns true if number is a power of two.
isPow2 :: (Bits a, Num a) => a -> Bool
isPow2 x = x .&. (x-1) == 0

-- q| Returns floor of log base 2.
lg :: (Bits a, Num a) => a -> Int
lg i0 = go 0 (i0 `shiftR` 1)
  where go r 0 = r
        go r n = go (r+1) (n `shiftR` 1)

-- | Returns smallest power of two not smaller than value.
nextPow2 :: (Ord a, Bits a, Integral a) => a -> a
nextPow2 x = 1 `shiftL` (lg (x-1) + 1)

-- | @nextMultiple x y@ computes the next multiple m of x s.t. m >= y.  E.g.,
-- nextMultiple 4 8 = 8 since 8 is a multiple of 8; nextMultiple 4 7 = 8;
-- nextMultiple 8 6 = 8.
nextMultiple :: Integral a => a -> a -> a
nextMultiple x y = ((y + x - 1) `div` x) * x

intn :: Int32 -> Type
intn = L.iT

i1, i8, i16, i32, i64 :: Type
i1     = intn 1
i8     = intn 8
i16    = intn 16
i32    = intn 32
i64    = intn 64

i8p, i16p, i32p, i64p :: Type
i8p    = PtrTo i8
i16p   = PtrTo i16
i32p   = PtrTo i32
i64p   = PtrTo i64

voidTy, strTy, voidPtr :: Type
voidTy = PrimType Void
strTy  = i8p
voidPtr = PtrTo voidTy

type BitWidth = Int 

-- | Pretty print int type with width.
ppIntType :: BitWidth -> Doc
ppIntType i = char 'i' <> integer (toInteger i)

-- | Pretty print pointer type.
ppPtrType :: L.Type -> Doc
ppPtrType tp = L.ppType tp <> char '*'

ppVector :: Int -> Doc -> Doc
ppVector n e = L.angles (int n <+> char 'x' <+> e)

ppIntVector :: Int -> BitWidth -> Doc
ppIntVector n w = ppVector n (ppIntType w)

ppTypeVector :: Int -> L.Type -> Doc
ppTypeVector n w = ppVector n (L.ppType w)