{- |
Module           : $Header$
Description      : Standard overrides for intrinsics, symbolic functions, and
                   tricky library functions.
Stability        : provisional
Point-of-contact : atomb
-}

{-# LANGUAGE OverloadedStrings #-}

module LSS.Overrides
  ( registerStandardOverrides )
where

import Control.Monad.Trans
import Text.LLVM.AST

import LSS.Execution.Common
import LSS.SBEInterface
import LSS.Simulator

i8, i32, void, str :: Type
i8 = PrimType (Integer 8)
i32 = PrimType (Integer 32)
void = PrimType Void
str = PtrTo i8

registerStandardOverrides :: (Functor m, Monad m, MonadIO m) =>
                             Simulator sbe m ()
registerStandardOverrides = do
  -- TODO: stub! Should be replaced with something useful.
  registerOverride "exit" void [i32] False $
    Override $ \_sym _rty _args -> dbugM "TODO: Exit!" >> return Nothing
  -- TODO: causes printing, but not in the same format as real printf
  registerOverride "printf" i32 [str] True $
    Override $ \_sym _rty args ->
      do mapM_ (dbugTerm "Printf") (map typedValue args)
         r <- withSBE $ \sbe -> termInt sbe 32 0
         return (Just r)
