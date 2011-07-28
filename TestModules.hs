{-# LANGUAGE DoRec                       #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module TestModules
  ( FieldAdd.fieldModule
  , factorial
  , exh
  , multiexit
  , testTranslate
  )

where

import FieldAdd
import Text.LLVM
import Data.LLVM.Symbolic.Translation

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

