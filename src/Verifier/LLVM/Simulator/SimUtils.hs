{- |
Module           : $Header$
Description      : Utility functions for execution of LLVM Symbolic programs
License          : Free for non-commercial use. See LICENSE.
Stability        : provisional
Point-of-contact : jhendrix
-}

module Verifier.LLVM.Simulator.SimUtils
  ( banners
  , dbugM
  ) where

import Control.Monad.Trans

dbugM :: MonadIO m => String -> m ()
dbugM = liftIO . putStrLn

banners :: MonadIO m => String -> m ()
banners msg = do
  dbugM $ replicate 80 '-'
  dbugM msg
  dbugM $ replicate 80 '-'
