{- |
Module           : $Header$
Description      : Utility functions for execution of LLVM Symbolic programs
Stability        : provisional
Point-of-contact : jhendrix
-}

module Verifier.LLVM.Simulator.SimUtils
  ( module Verinf.Utils.LogMonad
  , banners
  , banners'
  , dbugM
  , dbugM'
  ) where

import Control.Monad.Trans
import Verinf.Utils.LogMonad

dbugM :: MonadIO m => String -> m ()
dbugM = liftIO . putStrLn

dbugM' :: (LogMonad m, MonadIO m) => Int -> String -> m ()
dbugM' lvl = whenVerbosity (>=lvl) . dbugM

banners :: MonadIO m => String -> m ()
banners msg = do
  dbugM $ replicate 80 '-'
  dbugM msg
  dbugM $ replicate 80 '-'

banners' :: (LogMonad m, MonadIO m) => Int -> String -> m ()
banners' lvl = whenVerbosity (>=lvl) . banners