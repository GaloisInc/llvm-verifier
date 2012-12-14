module LSS.LLVMUtils where

import           Control.Applicative
import           Data.Bits
import           Data.Char
import           Data.Int
import           Data.List
import           Text.LLVM.AST
import           Text.LLVM     ((=:))
import qualified Text.LLVM     as L

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

charArrTy :: Int32 -> L.Type
charArrTy len = L.Array len i8

int32const :: Int32 -> Typed L.Value
int32const x = i32 =: L.ValInteger (fromIntegral x)

int64const :: Int64 -> Typed L.Value
int64const x = i64 =: L.ValInteger (fromIntegral x)

-- | Null-terminated LLVM string value
cstring :: String -> Typed L.Value
cstring str =
  charArrTy (fromIntegral $ length str + 1) =: L.ValString (str ++ [chr 0])

typedAs :: Typed a -> b -> Typed b
typedAs tv x = const x <$> tv

-- | @nextMultiple x y@ computes the next multiple m of x s.t. m >= y.  E.g.,
-- nextMultiple 4 8 = 8 since 8 is a multiple of 8; nextMultiple 4 7 = 8;
-- nextMultiple 8 6 = 8.
nextMultiple :: Integral a => a -> a -> a
nextMultiple x y = ((y + x - 1) `div` x) * x

isPow2 :: (Bits a, Num a) => a -> Bool
isPow2 x = x .&. (x-1) == 0

nextPow2 :: (Ord a, Bits a, Integral a) => a -> a
nextPow2 x = 2 ^ (lg x + 1)

lg :: (Ord a, Bits a, Num a) => a -> a
lg = genericLength . takeWhile (>0) . drop 1 . iterate (`shiftR` 1)

isIntegerType :: Type -> Bool
isIntegerType (PrimType t) = isInteger t
isIntegerType _ = False
