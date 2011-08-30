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
import LSS.Simulator

registerStandardOverrides :: (Functor m, Monad m, MonadIO m) =>
                             Simulator sbe m ()
registerStandardOverrides = do
  -- TODO: stub! Should be replaced with something useful.
  registerOverride "exit" (PrimType Void) [PrimType (Integer 32)] False
    (Override $ \_sym _rty _args -> dbugM "Exit!" >> return ())
