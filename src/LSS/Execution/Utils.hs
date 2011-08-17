{- |
Module           : $Header$
Description      : Utility functions for execution of LLVM Symbolic programs
Stability        : provisional
Point-of-contact : jstanley
-}

module LSS.Execution.Utils
  ( module Verinf.Utils.LogMonad
  , banners
  , dbugM
  , dbugM'
  , headf
  , safeHead
  )

where

import Data.Maybe          (listToMaybe)
import Control.Monad.Trans
import Verinf.Utils.LogMonad


headf :: [a] -> (a -> a) -> [a]
headf [] _     = error "headf: empty list"
headf (x:xs) f = f x : xs

dbugM :: MonadIO m => String -> m ()
dbugM = liftIO . putStrLn

dbugM' :: (LogMonad m, MonadIO m) => Int -> String -> m ()
dbugM' lvl = whenVerbosity (>=lvl) . dbugM

banners :: MonadIO m => String -> m ()
banners msg = do
  dbugM $ replicate 80 '-'
  dbugM msg
  dbugM $ replicate 80 '-'

safeHead :: [a] -> Maybe a
safeHead = listToMaybe

