{- |
Module           : $Header$
Description      : Printf-style formatting for symbolic terms
Stability        : provisional
Point-of-contact : atomb

Based on suggestions from emertens.
-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Verifier.LLVM.Printf where

import Text.Printf

data Printer = Printer (forall a. PrintfType a => a)
data Arg = forall a. PrintfArg a => Arg a

-- | Replace some % directives with %s in a format string. The
-- provided list of bools should contain as many elements as there are
-- arguments to printf. True means replace with %s.
formatAsStrings :: [Bool] -> String -> String
formatAsStrings _ "" = ""
formatAsStrings repls ('%' : '%' : rest) =
  '%' : '%' : formatAsStrings repls rest
formatAsStrings (True : bs) ('%' : rest) =
  '%' : 's' : formatAsStrings bs rest'
    where rest' = dropSpecifier rest
          dropSpecifier = tail . dropWhile (not . (`elem` endChars))
          endChars = "diouxXDOUeEfFgGaAcCsSpn"
formatAsStrings (False : bs) ('%' : rest) =
  '%' : formatAsStrings bs rest
formatAsStrings bs (c : rest) = c : formatAsStrings bs rest

replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace from to str = go from to [] str
  where go (f:fs) ts a (s:ss) | f == s = go fs ts (f : a) ss
                              | otherwise = reverse a ++ (s : go (f:fs) ts [] ss)
        go [] ts _ ss = ts ++ go from to [] ss
        go _ _ _ [] = []

fixFormat :: Int -> [Bool] -> String -> String
fixFormat ptrWidth symArgs =
  replace "%p" ("0x%0" ++ show ptrWidth ++ "x") .
  formatAsStrings symArgs

symPrintf :: String -> [Arg] -> String
symPrintf s args =
  case foldl f (Printer (printf s)) args of
    Printer res -> res
  where f :: Printer -> Arg -> Printer
        f (Printer p') (Arg a') = Printer (p' a')
        