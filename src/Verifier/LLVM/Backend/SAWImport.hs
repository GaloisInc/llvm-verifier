{-# LANGUAGE TemplateHaskell #-}

{- |
Module           : $Header$
Description      : The symbolic simulator for LLVM-Symbolic programs
License          : Free for non-commercial use. See LICENSE.
Stability        : provisional
Point-of-contact : jhendrix
-}
module Verifier.LLVM.Backend.SAWImport where

import Verifier.SAW as SAW
import Verifier.SAW.ParserUtils as SAW

$(runDecWriter $ do
    prelude <- defineImport [|preludeModule|] preludeModule
    llvm <- defineModuleFromFile [prelude] "llvmModule" "saw/LLVM.sawcore"
    declareDefTermF prelude "ite"
    declareSharedModuleFns "LLVM" (decVal llvm)
 )
