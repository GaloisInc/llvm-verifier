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
    -- * Pretty print utilities.
  , BitWidth
  , ppIntType
  , ppPtrType
  , ppArrayType
  , ppVectorType
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

ppTypeVector :: Int -> L.Type -> Doc
ppTypeVector n e = ppVectorType n (L.ppType e)