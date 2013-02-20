{- |
Module           : $Header$
Description      : Collection of LLVM Symbolic code
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Verifier.LLVM.Codebase
  ( Codebase(..)
  , dumpSymDefine
  , loadCodebase
  , lookupAlias
  , lookupAlias'
  , lookupDefine
  , lookupSym
  , TypeAliasMap
  )

where

import           Control.Monad
import           Control.Monad.Trans
import           Text.PrettyPrint.HughesPJ
import qualified Control.Exception              as CE
import qualified Data.ByteString                as BS
import qualified Data.LLVM.BitCode              as BC
import qualified Data.Map                       as M
import qualified Text.LLVM                      as LLVM

import           Verifier.LLVM.LLVMContext
import           Verifier.LLVM.AST
import           Verifier.LLVM.Translation

-- NB: We assume for the moment that we can be given a single .bc input (whether
-- or not we invoke the llvm linker ourselves in order to do this is something
-- that we can resolve later).

type GlobalNameMap = M.Map LLVM.Symbol (Either LLVM.Global SymDefine)

data Codebase = Codebase {
    cbGlobalNameMap :: GlobalNameMap
  , cbLLVMCtx       :: LLVMContext
  , cbDeclareMap    :: M.Map LLVM.Symbol LLVM.Declare   
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
      let ins2 d = M.insert (LLVM.typeName d) (LLVM.typeValue d)
          tam     = foldr ins2 M.empty (LLVM.modTypes mdl)
          lc = buildLLVMContext tam (LLVM.modDataLayout mdl)
      let ins0 m d = do
            let (wl,sd) = liftDefine lc d
            forM_ wl $ \w -> do
              putStrLn $ "Warning while reading bitcode in " ++ bcFile ++ ":\n"      
                ++ show (nest 2 w)
            return $ M.insert (LLVM.defName d) (Right sd) m
      m1 <- foldM ins0 M.empty (LLVM.modDefines mdl)
      let ins1 g = M.insert (LLVM.globalSym g) (Left g)
          m2     = foldr ins1 m1 (LLVM.modGlobals mdl)
          declareFromDefine d = LLVM.Declare { LLVM.decName = LLVM.defName d
                                             , LLVM.decArgs = map LLVM.typedType (LLVM.defArgs d)
                                             , LLVM.decVarArgs = LLVM.defVarArgs d
                                             , LLVM.decRetType = LLVM.defRetType d
                                             }
          cb     = Codebase { cbGlobalNameMap = m2
                            , cbDeclareMap = M.fromList $
                               [ (LLVM.defName d, declareFromDefine d)
                                 | d <- LLVM.modDefines mdl ]        
                               ++ [ (LLVM.decName d, d) | d <- LLVM.modDeclares mdl ]            
                            , cbLLVMCtx       = lc
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
lookupAlias ident cb = llvmLookupAlias (cbLLVMCtx cb) ident

lookupAlias' :: LLVM.Ident -> Codebase -> Maybe (LLVM.Type)
lookupAlias' ident cb = llvmLookupAlias' (cbLLVMCtx cb) ident

dumpSymDefine :: MonadIO m => m Codebase -> String -> m ()
dumpSymDefine getCB sym = getCB >>= \cb ->
  liftIO $ putStrLn $ show $ (ppSymDefine `fmap` lookupDefine (LLVM.Symbol sym) cb)