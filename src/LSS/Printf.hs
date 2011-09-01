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

-- | Replace some % directives with %s in a format string. The
-- provided list of bools should contain as many elements as there are
-- arguments to printf. True means replace with %s.
formatAsStrings :: String -> [Bool] -> String
formatAsStrings "" _ = ""
formatAsStrings ('%' : '%' : rest) repls =
  '%' : '%' : formatAsStrings rest repls
formatAsStrings ('%' : rest) (True : bs) =
  '%' : 's' : formatAsStrings rest' bs
    where rest' = dropSpecifier rest
          dropSpecifier = tail . dropWhile (not . (`elem` endChars))
          endChars = "diouxXDOUeEfFgGaAcCsSpn"
formatAsStrings ('%' : rest) (False : bs) =
  '%' : formatAsStrings rest bs
formatAsStrings (c : rest) bs = c : formatAsStrings rest bs

symPrintf :: String -> [Arg] -> String
symPrintf s args =
  case foldl f (Printer (printf s)) args of
    Printer res -> res
  where f :: Printer -> Arg -> Printer
        f p a = case p of
                  Printer p' -> case a of
                                  Arg a' -> Printer (p' a')

