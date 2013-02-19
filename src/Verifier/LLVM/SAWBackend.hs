{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
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
type instance SBEPred (SAWBackend s) = SharedTerm s
type instance SBEMemory (SAWBackend s) = SAWMemory

$(runDecWriter $ do
    prelude <- importExp [|preludeModule|] preludeModule
    llvm <- mkDecModule [prelude] "llvmModule" "saw/LLVM.sawcore"
    decSharedModuleFns "LLVM" (decVal llvm)
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
  let nyi nm = error $ "Not yet implemented: " ++ show nm
  let sbeInt w v = do
        wt <- scNat (toInteger w)
        scBitvector wt v
  let sbe = SBE { sbeTruePred = t
                , applyIEq = nyi "applyIEq"
                , applyAnd = nyi "applyAnd"
                , applyBNot = nyi "applyBNot"
                , applyPredIte = nyi "applyPredIte"
                , applyIte = nyi "applyIte"
                , asBool = nyi "asBool"
                , evalPred = nyi "evalPred"

                , termInt = lift2 sbeInt
                , freshInt = nyi "freshInt"
                , termDouble = nyi "termDouble"
                , termFloat  = nyi "termFloat"
                , termArray  = nyi "termArray"
                , termStruct = nyi "termStruct"
                , termDecomp = nyi "termDecomp"
                , applyUAddWithOverflow = nyi "applyUAddWithOverflow"

                , applyTypedExpr  = nyi "applyTypedExpr"

                , closeTerm = nyi "closeTerm"
                , prettyTermD = nyi "prettyTermD"
                , prettyPredD = nyi "prettyPredD"
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