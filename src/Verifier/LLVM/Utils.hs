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
  , lgCeil
  , nextPow2
  , nextMultiple
  , nextPow2Multiple
    -- * LLVM pretty AST constants.
  , intn
    -- * Pretty print utilities.
  , BitWidth
  , ppIntType
  , ppPtrType
  , ppArrayType
  , ppVectorType
  , ppIntVector
  ) where

import Data.Bits (Bits(..))
import Data.Int (Int32)
import qualified Text.LLVM     as L
import Text.LLVM.AST hiding (angles)
import Text.PrettyPrint.Leijen hiding ((<$>))

-- | Returns true if number is a power of two.
isPow2 :: (Bits a, Num a) => a -> Bool
isPow2 x = x .&. (x-1) == 0

-- | Returns floor of log base 2.
lg :: (Bits a, Num a, Ord a) => a -> Int
lg i0 | i0 > 0 = go 0 (i0 `shiftR` 1)
      | otherwise = error "lg given number that is not positive."
  where go r 0 = r
        go r n = go (r+1) (n `shiftR` 1)

-- | Returns ceil of log base 2.
lgCeil :: (Bits a, Num a, Ord a) => a -> Int
lgCeil 1 = 0
lgCeil i | i > 1 = 1 + lg (i-1)
         | otherwise = error "lgCeil given number that is not positive."

-- | Returns smallest power of two not smaller than value.
nextPow2 :: (Ord a, Bits a, Integral a) => a -> a
nextPow2 x = 1 `shiftL` lgCeil x

-- | @nextMultiple x y@ computes the next multiple m of x s.t. m >= y.  E.g.,
-- nextMultiple 4 8 = 8 since 8 is a multiple of 8; nextMultiple 4 7 = 8;
-- nextMultiple 8 6 = 8.
nextMultiple :: Integral a => a -> a -> a
nextMultiple x y = ((y + x - 1) `div` x) * x

-- | @nextPow2Multiple x n@ returns the smallest multiple of @2^n@ 
-- not less than @x@.
nextPow2Multiple :: (Bits a, Integral a) => a -> Int -> a
nextPow2Multiple x n | x >= 0 && n >= 0 = ((x+2^n -1) `shiftR` n) `shiftL` n
                     | otherwise = error "nextPow2Multiple given negative value."

intn :: Int32 -> Type
intn = L.iT

type BitWidth = Int 

-- | Pretty print int type with width.
ppIntType :: BitWidth -> Doc
ppIntType i = char 'i' <> integer (toInteger i)

-- | Pretty print pointer type.
ppPtrType :: Doc -> Doc
ppPtrType tp = tp <> char '*'

ppArrayType :: Int -> Doc -> Doc
ppArrayType n e = brackets (int n <+> char 'x' <+> e)

ppVectorType :: Int -> Doc -> Doc
ppVectorType n e = angles (int n <+> char 'x' <+> e)

ppIntVector :: Int -> BitWidth -> Doc
ppIntVector n w = ppVectorType n (ppIntType w)