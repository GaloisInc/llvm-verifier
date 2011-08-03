{- |
Module           : $Header$
Description      : Utility functions for execution of LLVM Symbolic programs
Stability        : provisional
Point-of-contact : jstanley
-}

module LSS.Execution.Utils where

import Control.Monad.Trans

headf :: [a] -> (a -> a) -> [a]
headf [] _     = error "headf: empty list"
headf (x:xs) f = f x : xs

dbugM :: MonadIO m => String -> m ()
dbugM = liftIO . putStrLn