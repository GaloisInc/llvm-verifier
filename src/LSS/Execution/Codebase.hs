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
  , lookupDefine
  , dumpSymDefine
  )

where

import           Control.Monad.Trans
import qualified Data.Map                       as M
import qualified Text.LLVM                      as LLVM

-- import           Data.LLVM.CFG
import           Data.LLVM.Symbolic.AST
import           Data.LLVM.Symbolic.Translation

-- NB: We assume for the moment that we can be given a single .bc input (whether
-- or not we invoke the llvm linker ourselves in order to do this is something
-- that we can resolve later).

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

lookupDefine :: LLVM.Symbol -> Codebase -> SymDefine
lookupDefine s cb = case M.lookup s (cbGlobalNameMap cb) of
  Nothing -> error $ "Failed to locate " ++ show s ++ " in code base."
  Just sd -> sd

-- STUB TODO
parseByteCode :: FilePath -> IO LLVM.Module
parseByteCode _ = return $ snd $ LLVM.runLLVM $ do
  LLVM.define LLVM.emptyFunAttrs (LLVM.iT 32) "int32_add" (LLVM.iT 32, LLVM.iT 32) $ \x y ->
    LLVM.ret =<< LLVM.add x y

dumpSymDefine :: MonadIO m => m Codebase -> String -> m ()
dumpSymDefine getCB sym = getCB >>= \cb ->
  liftIO $ putStrLn $ show $ ppSymDefine (lookupDefine (LLVM.Symbol sym) cb)

