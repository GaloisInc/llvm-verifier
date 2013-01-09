{- |
Module           : $Header$
Description      : A symbolic backend
Stability        : provisional
Point-of-contact : atomb
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module LSS.SBESymbolic
  ( module Verifier.LLVM.Backend
  , sbeSymbolic
  ) where

import Control.Monad.Trans
  
import qualified Verinf.Symbolic as S

import Verifier.LLVM.Backend

--------------------------------------------------------------------------------
-- Word-level symbolic backend

newtype SymbolicIO v = SymbolicIO { liftSymbolicIO :: IO v }
  deriving (Monad, MonadIO, Functor)
  
type instance SBETerm SymbolicIO       = S.DagTerm
type instance SBEClosedTerm SymbolicIO = S.DagTerm
type instance SBEMemory SymbolicIO     = S.DagTerm

-- | Symbolic interface with all operations at the word level.
sbeSymbolic :: SBE SymbolicIO
sbeSymbolic = SBE
  { termInt  = \w v -> return . S.mkCInt (S.Wx w) . fromIntegral $ v
  , freshInt = nyi "freshInt"
  , termBool = return . S.mkCBool
  , termDouble = nyi "termDouble"
  , termFloat = nyi "termFloat"
  , termArray = nyi "termArray"
  , termDecomp = nyi "termDecomp"
  , applyIte = nyi "applyIte"
  , applyICmp = nyi "applyICmp"
  , applyBitwise = nyi "applyBitwise"
  , applyArith = nyi "applyArith"
  , applyConv = nyi "applyConv"
  , applyBNot = nyi "applyBNot"
  , termWidth = nyi "termWidth"
  , closeTerm = id
  , prettyTermD = S.prettyTermD
  , asBool = nyi "asBool"
  , asUnsignedInteger = nyi "asUnsignedInteger"
  , memDump = nyi "memDump"
  , memLoad = nyi "memLoad "
  , memStore = nyi "memStore "
  , memMerge = nyi "memMerge "
  , memAddDefine = nyi "memAddDefine "
  , memInitGlobal = nyi "memInitGlobal"
  , memPushMergeFrame = nyi "memPushMergeFrame"
  , memPopMergeFrame = nyi "memPopMergeFrame"
  , codeBlockAddress = nyi "codeBlockAddress"
  , codeLookupSymbol = nyi "codeLookupSymbol"
  , stackAlloca = nyi "stackAlloca "
  , stackPushFrame = nyi "stackPushFrame "
  , stackPopFrame = nyi "stackPopFrame "
  , heapAlloc = nyi "heapAlloc"
  , memCopy = nyi "memCopy"
  , termSAT = nyi "termSAT"
  , writeAiger = nyi "writeAiger"
  , evalAiger = nyi "evalAiger"
  , writeCnf = nyi "writeCnf"
  , sbeRunIO = liftSymbolicIO
  }
  where
    nyi :: forall a. String -> a
    nyi msg = error $ unwords ["SBESymbolic:", msg, "not yet supported"]
