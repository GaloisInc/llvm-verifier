{- |
Module           : $Header$
Description      : Collection of LLVM Symbolic code
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module LSS.Execution.Codebase
  ( Codebase(..)
  , dumpSymDefine
  , loadCodebase
  , lookupAlias
  , lookupAlias'
  , lookupDefine
  , lookupSym
  )

where

import           Control.Monad
import           Control.Monad.Trans
import           Data.LLVM.TargetData
import           Data.LLVM.Symbolic.AST
import           Data.LLVM.Symbolic.Translation
import           Text.PrettyPrint.HughesPJ
import qualified Control.Exception              as CE
import qualified Data.ByteString                as BS
import qualified Data.LLVM.BitCode              as BC
import qualified Data.Map                       as M
import qualified Text.LLVM                      as LLVM

-- NB: We assume for the moment that we can be given a single .bc input (whether
-- or not we invoke the llvm linker ourselves in order to do this is something
-- that we can resolve later).

type GlobalNameMap = M.Map LLVM.Symbol (Either LLVM.Global SymDefine)
type TypeAliasMap  = M.Map LLVM.Ident LLVM.Type

data Codebase = Codebase {
    cbGlobalNameMap :: GlobalNameMap
  , cbTypeAliasMap  :: TypeAliasMap
  , cbLLVMCtx       :: LLVMContext
  , origModule      :: LLVM.Module
  }

-- For now, only take a single bytecode file argument and assume that the world
-- is linked together a priori.
loadCodebase :: FilePath -> IO Codebase
loadCodebase bcFile = do
  eab <- parse bcFile `CE.catch` \(e :: CE.SomeException) -> err (show e)
  case eab of
    Left msg  -> err (BC.formatError msg)
    Right mdl -> do

      let ins0 d = M.insert (LLVM.defName d) (Right $ liftDefine d)
          ins1 g = M.insert (LLVM.globalSym g) (Left g)
          ins2 d = M.insert (LLVM.typeName d) (LLVM.typeValue d)
          m1     = foldr ins0 M.empty (LLVM.modDefines mdl)
          m2     = foldr ins1 m1 (LLVM.modGlobals mdl)
          m3     = foldr ins2 M.empty (LLVM.modTypes mdl)
          cb     = Codebase { cbGlobalNameMap = m2
                            , cbTypeAliasMap  = m3
                            , cbLLVMCtx       = buildLLVMContext
                                                  (`lookupAlias` cb)
                                                  (LLVM.modDataLayout mdl)
                            , origModule      = mdl
                            }

      when (null $ LLVM.modDataLayout mdl) $
        putStrLn "Warning: No target data layout found; will use defaults."

--       putStrLn $ "Target data layout: " ++ show (LLVM.modDataLayout mdl)
--       putStrLn $ "LLVMCtx:" ++ show (cbLLVMCtx cb)

      return cb
  where
    parse = BS.readFile >=> BC.parseBitCode
    err msg = error $ "Bitcode parsing of " ++ bcFile ++ " failed:\n"
              ++ show (nest 2 (vcat $ map text $ lines msg))

lookupDefine :: LLVM.Symbol -> Codebase -> Maybe SymDefine
lookupDefine sym cb = case M.lookup sym (cbGlobalNameMap cb) of
  Just (Right sd) -> Just sd
  _               -> Nothing

-- | Returns the global variable or symbolic definition associated with the
-- given symbol.
lookupSym :: LLVM.Symbol -> Codebase -> Maybe (Either LLVM.Global SymDefine)
lookupSym sym = M.lookup sym . cbGlobalNameMap

lookupAlias :: LLVM.Ident -> Codebase -> LLVM.Type
lookupAlias ident cb = case lookupAlias' ident cb of
  Nothing -> error $ "Failed to locate type alias named "
                     ++ show (LLVM.ppIdent ident) ++ " in code base"
  Just t  -> t

lookupAlias' :: LLVM.Ident -> Codebase -> Maybe (LLVM.Type)
lookupAlias' ident cb = M.lookup ident (cbTypeAliasMap cb)

dumpSymDefine :: MonadIO m => m Codebase -> String -> m ()
dumpSymDefine getCB sym = getCB >>= \cb ->
  liftIO $ putStrLn $ show $ (ppSymDefine `fmap` lookupDefine (LLVM.Symbol sym) cb)
