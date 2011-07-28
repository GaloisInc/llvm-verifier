{- |
Module           : $Header$
Description      : Collection of LLVM Symbolic code
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE EmptyDataDecls              #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module LSS.Execution.Codebase
  ( Codebase(..)
  , loadCodebase
  , dumpSymDefine
  )

where

import           Control.Monad.Trans
import qualified Data.Map                       as M
import qualified Text.LLVM                      as LLVM

import           Data.LLVM.CFG
import           Data.LLVM.Symbolic.AST
import           Data.LLVM.Symbolic.Translation


{-

TODO:

- Let's assume for the moment that we can be given a single .bc input,
  effectively; whether or not we invoke the llvm linker ourselves in order to do
  this is something that we can resolve later.  It seems there's no notion of
  module-scoped identifiers, but there is mention in the docos that "multiple
  modules can be merged together using the llvm linker" which is how I assume
  we'll avoid name collisions in the single, flat, global namespace.

- Parse .bc obtaining LLVM.Module
- foreach define d in module
  - build cfg for d                                   | ...
  - build LTI dominator info for translation step     | ... moved to Translation module
  - translate d to d_sym
  - register name of d_sym in global function name map

- write callDefine as the entry point for a toplevel function (implicitly main
  via the lss executable, probably; maybe 'callToplevel' is a better name, and
  it should assert that the frame stack is empty etc.).  There'll likely be one
  step that needs to occur before or as a part of the execution of the first
  "init" basic block in the invoked function that won't occur elsewhere, and
  that is the binding of actual params passed to callDefine and formal params in
  the call frame environment of the newly-invoked function

-}

type GlobalNameMap = M.Map LLVM.Symbol SymDefine

data Codebase = Codebase {
    cbGlobalNameMap :: GlobalNameMap
  }

-- For now, only take a single bytecode file argument and assume that the world
-- is linked together a priori.
loadCodebase :: FilePath -> IO Codebase
loadCodebase bcFile = do
  mdl <- parseByteCode bcFile
  let xlt d = M.insert (LLVM.defName d) (liftDefine d)
      cb    = Codebase
              { cbGlobalNameMap = foldr xlt M.empty (LLVM.modDefines mdl) }
  return cb

lookupDefine :: Codebase -> LLVM.Symbol -> SymDefine
lookupDefine cb s = case M.lookup s (cbGlobalNameMap cb) of
  Nothing -> error $ "Failed to locate " ++ show s ++ " in code base."
  Just sd -> sd

-- STUB TODO
parseByteCode :: FilePath -> IO LLVM.Module
parseByteCode _ = return $ snd $ LLVM.runLLVM $ do
  LLVM.define LLVM.emptyFunAttrs (LLVM.iT 32) "int32_add" (LLVM.iT 32, LLVM.iT 32) $ \x y -> do
    "entry"
    r0 <- LLVM.alloca (LLVM.iT 32) Nothing (Just 4)
    r1 <- LLVM.alloca (LLVM.iT 32) Nothing (Just 4)
    LLVM.store x r0
    LLVM.store y r1
    r2 <- LLVM.load r0
    r3 <- LLVM.load r1
    r4 <- LLVM.add r2 r3
    LLVM.ret r4

dumpSymDefine :: MonadIO m => m Codebase -> String -> m ()
dumpSymDefine getCB sym = getCB >>= \cb ->
  liftIO $ putStrLn $ show $ ppSymDefine (lookupDefine cb (LLVM.Symbol sym))

