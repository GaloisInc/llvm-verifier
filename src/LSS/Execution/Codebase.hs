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
import           Data.LLVM.Symbolic.AST
import           Data.LLVM.Symbolic.Translation
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
{-
define i32 @int32_add(i32 %x, i32 %y) nounwind readnone ssp {
  %1 = add nsw i32 %y, %x
  ret i32 %1
}
define i32 @int32_mul(i32 %x, i32 %y) nounwind readnone ssp {
  %1 = mul nsw i32 %y, %x
  ret i32 %1
}
define i32 @int32_square(i32 %x) nounwind ssp {
  %1 = call i32 @int32_mul(i32 %x, i32 %x)
  ret i32 %1
}
define i32 @int32_muladd(i32 %x, i32 %y) nounwind ssp {
  %1 = call i32 @int32_add(i32 %x, i32 %y)
  %2 = call i32 @int32_square(i32 %1)
  ret i32 %2
}
-}
parseByteCode :: FilePath -> IO LLVM.Module
parseByteCode _ = return $ snd $ LLVM.runLLVM $ do
  let i32 = LLVM.iT 32

  LLVM.define LLVM.emptyFunAttrs i32 "int32_add" (i32, i32) $ \x y -> do
    LLVM.ret =<< LLVM.add x y

  LLVM.define LLVM.emptyFunAttrs i32 "int32_mul" (i32, i32) $ \x y -> do
    LLVM.ret =<< LLVM.mul x y

  LLVM.define LLVM.emptyFunAttrs i32 "int32_square" i32 $ \x -> do
    LLVM.ret =<< LLVM.call i32 (LLVM.Symbol "int32_mul") [x, x]

  LLVM.define LLVM.emptyFunAttrs i32 "int32_muladd" (i32, i32) $ \x y -> do
    r0 <- LLVM.call i32 (LLVM.Symbol "int32_add") [x, y]
    r1 <- LLVM.call i32 (LLVM.Symbol "int32_square") [r0]
    LLVM.ret r1

dumpSymDefine :: MonadIO m => m Codebase -> String -> m ()
dumpSymDefine getCB sym = getCB >>= \cb ->
  liftIO $ putStrLn $ show $ ppSymDefine (lookupDefine (LLVM.Symbol sym) cb)
