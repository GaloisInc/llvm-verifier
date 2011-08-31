{- |
Module           : $Header$
Description      : Printf-style formatting for symbolic terms
Stability        : provisional
Point-of-contact : atomb

Based on suggestions from emertens.
-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module LSS.Printf where

import Text.Printf

data Printer = Printer (forall a. PrintfType a => a)
data Arg = forall a. PrintfArg a => Arg a

symPrintf :: String -> [Arg] -> String
symPrintf s args =
  case foldl f (Printer (printf s)) args of
    Printer res -> res
  where f :: Printer -> Arg -> Printer
        f p a = case p of
                  Printer p' -> case a of
                                 Arg a' -> Printer (p' a')

