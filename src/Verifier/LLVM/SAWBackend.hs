{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
module Verifier.LLVM.SAWBackend where

import Data.LLVM.Memory
import Data.LLVM.TargetData
import Verifier.LLVM.Backend


data SAWBackend a = SAWBackend { runSAWBackend :: IO a }
  deriving (Functor)

data SAWMemory = SAWMemory ()

type instance SBEMemory SAWBackend = SAWMemory

createSAWBackend :: LLVMContext
                 -> MemGeom
                 -> IO (SBE SAWBackend, SAWMemory)
createSAWBackend _lc _mg = do
  let nyi nm = error $ "Not yet implemented: " ++ show nm
  let sbe = SBE { termBool = nyi "termBool"
                , termInt = nyi "termInt"
                , freshInt = nyi "freshInt"
                , termDouble = nyi "termDouble"
                , termFloat = nyi "termFloat"
                , termArray = nyi "termArray"
                , termDecomp = nyi "termDecomp"
                , applyIte = nyi "applyIte"
                , applyICmp = nyi "applyICmp"
                , applyBitwise = nyi "applyBitwise"
                , applyArith = nyi "applyArith"
                , applyConv = nyi "applyConv"
                , applyBNot = nyi "applyBNot"
                , termWidth = nyi "termWidth"
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