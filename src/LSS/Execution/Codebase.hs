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
  , loadCodebase
  , lookupDefine
  , lookupDefine'
  , lookupGlobal
  , lookupGlobal'
  , dumpSymDefine
  )

where

import qualified Control.Exception              as CE
import           Control.Monad
import           Control.Monad.Trans
import           Data.LLVM.Symbolic.AST
import           Data.LLVM.Symbolic.Translation
import           Text.PrettyPrint.HughesPJ
import qualified Data.ByteString                as BS
import qualified Data.LLVM.BitCode              as BC
import qualified Data.Map                       as M
import qualified Text.LLVM                      as LLVM

-- NB: We assume for the moment that we can be given a single .bc input (whether
-- or not we invoke the llvm linker ourselves in order to do this is something
-- that we can resolve later).

type GlobalNameMap = M.Map LLVM.Symbol (Either LLVM.Global SymDefine)

data Codebase = Codebase {
    cbGlobalNameMap :: GlobalNameMap
  }

-- For now, only take a single bytecode file argument and assume that the world
-- is linked together a priori.
loadCodebase :: FilePath -> IO Codebase
loadCodebase bcFile = do
  eab <- parse bcFile `CE.catch` \(e :: CE.SomeException) -> err (show e)
  case eab of
    Left msg  -> err msg
    Right mdl -> do
      let ins0 d = M.insert (LLVM.defName d) (Right $ liftDefine d)
          ins1 g = M.insert (LLVM.globalSym g) (Left g)
          m1     = foldr ins0 M.empty (LLVM.modDefines mdl)
          m2     = foldr ins1 m1 (LLVM.modGlobals mdl)
          cb     = Codebase { cbGlobalNameMap = m2 }
--       putStrLn $ "mdl = \n" ++ show (LLVM.ppModule mdl)
--       putStrLn $ "globals = \n" ++ show (map LLVM.ppGlobal (LLVM.modGlobals mdl))
--       putStrLn $ "xlated = \n" ++ (unlines $ map (show . ppSymDefine) (M.elems (cbGlobalNameMap cb)))
--       putStrLn "--"
      return cb
  where
    parse   = BS.readFile >=> BC.parseBitCode
    err msg = error $ "Bitcode parsing of " ++ bcFile ++ " failed:\n"
              ++ show (nest 2 (vcat $ map text $ lines msg))

lookupDefine :: LLVM.Symbol -> Codebase -> SymDefine
lookupDefine sym cb = case lookupDefine' sym cb of
  Nothing -> error $ "Failed to locate define " ++ show sym ++ " in code base."
  Just sd -> sd

lookupDefine' :: LLVM.Symbol -> Codebase -> Maybe SymDefine
lookupDefine' sym cb = case M.lookup sym (cbGlobalNameMap cb) of
  Just (Right sd) -> Just sd
  _               -> Nothing

lookupGlobal :: LLVM.Symbol -> Codebase -> LLVM.Global
lookupGlobal sym cb = case lookupGlobal' sym cb of
  Nothing -> error $ "Failed to locate global " ++ show sym ++ " in code base."
  Just g  -> g

lookupGlobal' :: LLVM.Symbol -> Codebase -> Maybe LLVM.Global
lookupGlobal' sym cb = case M.lookup sym (cbGlobalNameMap cb) of
  Just (Left g) -> Just g
  _             -> Nothing

dumpSymDefine :: MonadIO m => m Codebase -> String -> m ()
dumpSymDefine getCB sym = getCB >>= \cb ->
  liftIO $ putStrLn $ show $ ppSymDefine (lookupDefine (LLVM.Symbol sym) cb)
