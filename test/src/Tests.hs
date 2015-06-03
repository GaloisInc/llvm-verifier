{-# LANGUAGE Rank2Types #-}

{- |
Module           : $Header$
Description      : Symbolic execution tests
License          : Free for non-commercial use. See LICENSE.
Stability        : provisional
Point-of-contact : atomb
-}

module Main where

import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.Ingredients
import Test.Tasty.Runners.AntXML
import Data.Proxy

import Tests.AES
import Tests.Aggregates
import Tests.Errors
import Tests.IO
import Tests.BitMemModel
import Tests.MemModel
import Tests.PrimOps
import Tests.Symbolic
import Tests.Common

main :: IO ()
main = defaultMainWithIngredients ingrs tests

ingrs :: [Ingredient]
ingrs =
   [ includingOptions [ Option (Proxy :: Proxy VerbosityOption)
                      ]
   , antXMLRunner
   ]
   ++
   defaultIngredients

tests :: TestTree
tests =
   testGroup "LLVM"
   [ testGroup "PrimOps"     primOpTests
   , testGroup "Aggregates"  aggTests
   , testGroup "Symbolic"    symTests
   , testGroup "I/O"         ioTests
   , testGroup "AES"         aesTests
   , testGroup "BitMemModel" bitMemModelTests
   , testGroup "MemModel"    memModelTests
   , testGroup "Errors"      errorTests
   ]
