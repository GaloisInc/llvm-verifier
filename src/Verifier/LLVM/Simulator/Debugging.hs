{- |
Module           : $Header$
Description      : Debugger implementation for LSS
Stability        : provisional
Point-of-contact : acfoltzer

Debugger for the LLVM Symbolic Simulator. This module provides
implementations of the 'SEH' event handlers. Commands and their
semantics are loosely based on gdb.

-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE ViewPatterns     #-}

module Verifier.LLVM.Simulator.Debugging (sanityChecks) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans
import Control.Lens hiding (createInstance)

import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tuple.Curry
import Data.Word (Word16)

import System.Console.Haskeline
import System.Console.Haskeline.Completion
import System.Console.Haskeline.History
import System.Exit

import Text.PrettyPrint

import qualified Text.LLVM.AST as L

import Verifier.LLVM.Simulator
import Verifier.LLVM.Simulator.Common

#if __GLASGOW_HASKELL__ < 706
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.Read as R
readEither :: Read a => String -> Either String a
readEither s =
  case [ x | (x,"") <- R.readPrec_to_S read' R.minPrec s ] of
    [x] -> Right x
    []  -> Left "Prelude.read: no parse"
    _   -> Left "Prelude.read: ambiguous parse"
 where
  read' =
    do x <- R.readPrec
       R.lift P.skipSpaces
       return x

-- | Parse a string using the 'Read' instance.
-- Succeeds if there is exactly one valid result.
readMaybe :: Read a => String -> Maybe a
readMaybe s = case readEither s of
                Left _  -> Nothing
                Right a -> Just a
#else
import Text.Read (readMaybe)
#endif

-- | Add a breakpoint to @main@ in the current codebase
breakOnMain :: (Functor m, Monad m) => Simulator sbe m ()
breakOnMain = addBreakpoint (L.Symbol "main") BreakEntry



-- NB: Currently only valid for SBEBitBlast mems
sanityChecks ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => SEH sbe m
sanityChecks = SEH
  {
    onPreStep         = \_ -> return ()
  , onPostStep        = \_ -> return ()
  , onMkGlobTerm      = \_ -> return ()
  , onPostOverrideReg = return ()

  , onPreGlobInit = \_ _ -> return ()

{-
  , onPreGlobInit = \g (Typed ty gdata) -> do
      CE.assert (L.globalType g == ty) $ return ()
      sz  <- withLC (`llvmStoreSizeOf` ty)
      szt <- withSBE' $ \sbe -> termWidth sbe gdata
      when (szt `shiftR` 3 /= sz) $ do
        dbugM $ "onPreGlobInit assert failure on " ++ show (L.ppSymbol $ L.globalSym g)
                ++ " (size check)"
        CE.assert False $ return ()
-}

  , onPostGlobInit = \_g _ -> do
      {-
      Just mem       <- getMem
      sz        <- withLC (`llvmStoreSizeOf` ty)
      addrWidth <- withLC llvmAddrWidthBits
      -- Read back and check
      gstart <- withSBE $ \sbe -> termInt sbe addrWidth (bmDataAddr mem - sz)
      (cond, gdata') <- withSBE $ \sbe -> memLoad sbe mem (Typed (L.PtrTo ty) gstart)
      processMemCond cond
      eq <- uval =<< (Typed i1 <$> withSBE (\sbe -> applyICmp sbe L.Ieq gdata gdata'))
      when (eq /= 1) $ do
        dbugM $ "onPostGlobInit assert failure on " ++ show (L.ppSymbol $ L.globalSym g)
                ++ " (read-back) "
        CE.assert False $ return ()
        -}
      return ()
  }
  {-
  where
    uval (typedValue -> v) =
      fromJust <$> withSBE' (\sbe -> snd $ asUnsignedInteger sbe v)
-}
