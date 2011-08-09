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

type GlobalNameMap = M.Map LLVM.Symbol SymDefine

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
      let xlt d = M.insert (LLVM.defName d) (liftDefine d)
          cb    = Codebase
                  { cbGlobalNameMap = foldr xlt M.empty (LLVM.modDefines mdl) }
      return cb
  where
    parse   = BS.readFile >=> BC.parseBitCode
    err msg = error $ "Bitcode parsing of " ++ bcFile ++ " failed:\n"
              ++ show (nest 2 (vcat $ map text $ lines msg))

lookupDefine :: LLVM.Symbol -> Codebase -> SymDefine
lookupDefine s cb = case M.lookup s (cbGlobalNameMap cb) of
  Nothing -> error $ "Failed to locate " ++ show s ++ " in code base."
  Just sd -> sd

-- testMdl :: IO LLVM.Module
-- testMdl = return $ snd $ LLVM.runLLVM $ do
--   let i32 = LLVM.iT 32
--   LLVM.define LLVM.emptyFunAttrs i32 "int32_add" (i32, i32) $ \x y -> do
--     LLVM.ret =<< LLVM.add x y

--   LLVM.define LLVM.emptyFunAttrs i32 "int32_mul" (i32, i32) $ \x y -> do
--     LLVM.ret =<< LLVM.mul x y

--   LLVM.define LLVM.emptyFunAttrs i32 "int32_square" i32 $ \x -> do
--     LLVM.ret =<< LLVM.call i32 (LLVM.Symbol "int32_mul") [x, x]

--   LLVM.define LLVM.emptyFunAttrs i32 "int32_muladd" (i32, i32) $ \x y -> do
--     r0 <- LLVM.call i32 (LLVM.Symbol "int32_add") [x, y]
--     r1 <- LLVM.call i32 (LLVM.Symbol "int32_square") [r0]
--     LLVM.ret r1

dumpSymDefine :: MonadIO m => m Codebase -> String -> m ()
dumpSymDefine getCB sym = getCB >>= \cb ->
  liftIO $ putStrLn $ show $ ppSymDefine (lookupDefine (LLVM.Symbol sym) cb)
