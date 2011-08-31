{- |
Module           : $Header$
Description      : Printf-style formatting for symbolic terms
Stability        : provisional
Point-of-contact : atomb
-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module LSS.Printf where

import Data.Int
import Text.Printf

import Verinf.Symbolic.Common (ConstantProjection(..), CValue(..))

data Printer = Printer (forall a. PrintfType a => a)
data Arg = forall a. PrintfArg a => Arg a

symPrintf :: String -> [Arg] -> String
symPrintf s args =
  case foldl f (Printer (printf s)) args of
    Printer res -> res
  where f :: Printer -> Arg -> Printer
        f p a = case p of
                  Printer p -> case a of
                                 Arg a -> Printer (p a)

termToArg :: (ConstantProjection a) => a -> Arg
termToArg a =
  case termConst a of
    (Just (CInt 8 n))  -> Arg (fromInteger n :: Int8)
    (Just (CInt 16 n)) -> Arg (fromInteger n :: Int16)
    (Just (CInt 32 n)) -> Arg (fromInteger n :: Int32)
    (Just (CInt 64 n)) -> Arg (fromInteger n :: Int64)
    _ -> error "failed to convert term to printf argument"
