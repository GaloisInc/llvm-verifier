{-# LANGUAGE TemplateHaskell #-}

{- |
Module           : $Header$
Description      : The symbolic simulator for LLVM-Symbolic programs
License          : BSD3
Stability        : provisional
Point-of-contact : jhendrix
-}
module Verifier.LLVM.Backend.SAWImport where

import Verifier.SAW.ParserUtils as SAW

$(defineModuleFromFileWithFns
  "llvmModule" "scLoadLLVMModule" "saw/LLVM.sawcore")
