module LSS.LLVMUtils where

import Data.Int
import Text.LLVM.AST

intn :: Int32 -> Type
intn n = PrimType (Integer n)

i8, i16, i32, i64 :: Type
i8     = intn 8
i16    = intn 16
i32    = intn 32
i64    = intn 64

i8p, i16p, i32p, i64p :: Type
i8p    = PtrTo i8
i16p   = PtrTo i16
i32p   = PtrTo i32
i64p   = PtrTo i64

voidTy, strTy :: Type
voidTy = PrimType Void
strTy  = i8p
