{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Verifier.LLVM.SAWBackend where

import Data.LLVM.Memory
import Data.LLVM.TargetData
import Verifier.LLVM.Backend


import Verifier.SAW
import Verifier.SAW.ParserUtils
import Verifier.SAW.Prelude

data SAWBackend s a = SAWBackend { runSAWBackend :: IO a }
  deriving (Functor)

data SAWMemory = SAWMemory ()

type instance SBETerm (SAWBackend s) = SharedTerm s
type instance SBEMemory (SAWBackend s) = SAWMemory

$(runDecWriter $ do
    prelude <- decModule [|preludeModule|] preludeModule
    llvm <- mkDecModule [prelude] "llvmModule" "saw/LLVM.saw"
    decSharedModuleFns "LLVM" (dmModule llvm)    
 )

lift2 :: (x -> y -> IO r) -> x -> y -> SAWBackend s r
lift2 fn x y = SAWBackend (fn x y)

createSAWBackend :: LLVMContext
                 -> MemGeom
                 -> IO (SBE (SAWBackend s), SAWMemory)
createSAWBackend _lc _mg = do
  sc <- mkSharedContext llvmModule
  let ?sc = sc
  t <- scApplyPreludeTrue sc
  f <- scApplyPreludeFalse sc
  integerToSigned <- scApplyPreludeIntegerToSigned sc
  let nyi nm = error $ "Not yet implemented: " ++ show nm
  let sbeBool b = if b then t else f
      sbeInt w v = do
        wt <- scNat (toInteger w)
        integerToSigned wt =<< scInteger v
  let sbe = SBE { termBool = SAWBackend . return . sbeBool
                , termInt = lift2 sbeInt
                , freshInt = nyi "freshInt"
                , termDouble = nyi "termDouble"
                , termFloat  = nyi "termFloat"
                , termArray  = nyi "termArray"
                , termStruct = nyi "termStruct"
                , termDecomp = nyi "termDecomp"
                , applyIte   = nyi "applyIte"
                , applyBNot = nyi "applyBNot"
                , applyUAddWithOverflow = nyi "applyUAddWithOverflow"

                , applyTypedExpr  = nyi "applyTypedExpr"

                , closeTerm = nyi "closeTerm"
                , prettyTermD = nyi "prettyTermD"
                , asBool = nyi "asBool"
                , asUnsignedInteger = nyi "asUnsignedInteger"
                , memDump = nyi "memDump"
                , memLoad = nyi "memLoad"
                , memStore = nyi "memStore"
                , memAddDefine = nyi "memAddDefine"
                , memInitGlobal = nyi "memInitGlobal"
                , codeBlockAddress = nyi "codeBlockAddress"
                , codeLookupSymbol = nyi "codeLookupSymbol"
                , stackAlloca = nyi "stackAlloca"
                , stackPushFrame = nyi "stackPushFrame"
                , stackPopFrame = nyi "stackPopFrame"
                , heapAlloc = nyi "heapAlloc"
                , memCopy = nyi "memCopy"
                , memPushMergeFrame = SAWBackend . return
                , memPopMergeFrame = SAWBackend . return
                , memMerge = nyi "memMerge"
                , writeAiger = nyi "writeAiger"
                , writeCnf = nyi "writeCnf"
                , evalAiger = nyi "evalAiger"
                , sbeRunIO = runSAWBackend
                }
  let mem = nyi "mem"
  return (sbe,mem)