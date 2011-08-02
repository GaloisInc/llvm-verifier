{-# LANGUAGE DoRec                       #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module TestModules
  ( FieldAdd.fieldModule
  , factorial
  , exh
  , multiexit
  , testTranslate
  , simple
  , trivial0
  , trivial1
  )

where

import FieldAdd
import Text.LLVM
import Data.LLVM.Symbolic.Translation

trivial0 :: Module
trivial0 = snd $ runLLVM $ do
  define emptyFunAttrs (iT 32) "int32_add" (iT 32, iT 32) $ \x y ->
    ret =<< add x y

trivial1 :: Module
trivial1 = snd $ runLLVM $ do
  define emptyFunAttrs (iT 32) "int32_add" (iT 32, iT 32) $ \x y -> do
    "entry"
    r0 <- alloca (iT 32) Nothing (Just 4)
    r1 <- alloca (iT 32) Nothing (Just 4)
    store x r0
    store y r1
    r2 <- load r0
    r3 <- load r1
    r4 <- add r2 r3
    ret r4

simple :: Module
simple = snd $ runLLVM $ do
  define emptyFunAttrs (iT 32) "simple" (iT 1, iT 32) $ \c0 x -> do
    "init"
    br c0 "then" "else"

    "then"
    r1 <- add x (int 42)
    jump "exit"

    "else"
    r2 <- add x (int 99)
    jump "exit"

    "exit"
    r3 <- phi (iT 32) [r1 `from` "then", r2 `from` "else"]
    ret r3


factorial :: Module
factorial = snd $ runLLVM $ do
  define emptyFunAttrs (iT 32) "factorial" (iT 32) $ \ x -> do
    "entry"
    jump "test"

    rec "test"
        i   <- phi (iT 32) [x     `from` "entry", i'   `from` "incr"]
        acc <- phi (iT 32) [int 1 `from` "entry", acc' `from` "incr"]

        b   <- icmp Iule i (int 1)
        br b "exit" "incr"

        "incr"
        acc' <- mul acc i
        i'   <- sub i (int 1)
        jump "test"

    "exit"
    ret acc

multiexit :: Module
multiexit = snd $ runLLVM $ do
  define emptyFunAttrs voidT "multiexit" (iT 32) $ \x -> do
    "0"
    b0 <- call (iT 1) (Symbol "b0") [x]
    br b0 "2" "4"
    "2"
    _ <- call (iT 32) (Symbol "f0") []
    jump "10"
    "4"
    b1 <- call (iT 1) (Symbol "b1") [x]
    br b1 "6" "8"
    "6"
    _ <- call (iT 32) (Symbol "f1") []
    call_ voidT (Symbol "exit") [Typed (iT 32) (ValInteger 1)]
    unreachable
    "8"
    _ <- call (iT 32) (Symbol "f2") []
    jump "10"
    "10"
    retVoid

exh :: Module
exh = snd $ runLLVM $ do
  define emptyFunAttrs voidT "exh" (iT 32) $ \x -> do
    "entry"
    jump "loop_header"

    "loop_header"
    b0 <- call (iT 1) (Symbol "b0") [x]
    br b0 "proc" "body"

    "proc"
    call_ voidT (Symbol "proc") []
    retVoid

    "body"
    invoke voidT (Symbol "foo") [] "loop_header" "handler"

    "handler"
    call_ voidT (Symbol "handler") []
    unreachable

