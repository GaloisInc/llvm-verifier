-- FieldAdd contains a small example program with a field
-- addition operation.
module FieldAdd where

import Data.Int
import Text.PrettyPrint.HughesPJ
import Text.LLVM
import qualified Data.LLVM.CFG as LLVM

import Data.LLVM.Symbolic.AST
import Data.LLVM.Symbolic.Translation

intType :: Int32 -> Type
intType = PrimType . Integer

i384 = intType 384

overflowAddType :: Int32 -> Type
overflowAddType w = Struct [intType w, intType 1]

-- | Declares uadd.with.overflow intrinsic with given width.
uadd_with_overflow :: Int32 -> LLVM Symbol
uadd_with_overflow w = do
 let sym = Symbol ("llvm.uadd.with.overflow.i" ++ show w)
 declare (overflowAddType w) sym [intType w, intType w]
 return sym

-- | Declares usub.with.overflow intrinsic with given width.
usub_with_overflow :: Int32 -> LLVM Symbol
usub_with_overflow w = do
 let sym = Symbol ("llvm.usub.with.overflow.i" ++ show w)
 declare (overflowAddType w) sym [intType w, intType w]
 return sym


fieldModule = snd $ runLLVM $ do
  uadd384 <- uadd_with_overflow 384
  usub384 <- usub_with_overflow 384
  
  let extractPair p = do
        x <- extractValue p 0
        y <- extractValue p 1
        return (x,y)
  define emptyFunAttrs i384 (Symbol "field_add") (i384,i384) $ \x y -> do
    let negP = i384 -: (2 ^ 128 + 2 ^ 96 - 2 ^ 32 + 1 :: Integer)
    (sum1, c1) <- extractPair =<< call (overflowAddType 384) uadd384 [x, y]
    (sum2, c2) <- extractPair =<< call (overflowAddType 384) uadd384 [sum1, negP]
    c <- c1 `bor` c2
    r <- select c sum2 sum1
    ret r
   
-- let negP = (2 ^ 128 + 2 ^ 96 - 2 ^ 32 + 1)

test = do
  putStrLn $ render $ ppModule fieldModule
  let [fAdd] = modDefines fieldModule
  let cfg = LLVM.buildCFG (defBody fAdd)
  let lti = LTI { ltiPostDominators = \_ -> []
                , ltiIsImmediatePostDominator = \_ _ -> False
                , ltiNewPostDominators = \_ _ -> []
                }
  putStrLn $ render $ ppSymDefine $ liftDefine lti fAdd
