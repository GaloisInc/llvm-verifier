-- Dead simple llvm-dis/parser/AST translation inspection utility

{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Exception as CE
import           Control.Monad
import           System.Environment
import           System.Process
import           System.Directory
import           System.FilePath
import           Text.PrettyPrint.HughesPJ
import qualified Text.LLVM                      as LLVM

import           Verifier.LLVM.AST
import           Verifier.LLVM.Codebase
import           Verifier.LLVM.Simulator.SimUtils
import           Verifier.LLVM.Translation

main :: IO ()
main = do
  files <- getArgs
  when (length files /= 1) $ error "Usage: bcdump <LLVM bytecode file>"
  let bcFile  = head files
      tmpll   = bcFile <.> "tmp" <.> "ll"
      err msg = error $ "Bitcode parsing of " ++ bcFile ++ " failed:\n"
                        ++ show (nest 2 (vcat $ map text $ lines msg))

  mllvmDisExe <- findExecutable "llvm-dis"
  case mllvmDisExe of
    Nothing         -> putStrLn $ "Warning: no llvm-dis in path, skipping."
    Just llvmDisExe -> do
      (_,_,_,ph) <- createProcess $ proc llvmDisExe ["-o=" ++ tmpll, bcFile]
      _ <- waitForProcess ph
      dis <- readFile tmpll
      banners $ "llvm-dis module"
      putStrLn dis
      removeFile tmpll

  cb <- loadCodebase bcFile `CE.catch` \(e :: CE.SomeException) -> err (show e)
  let mdl = origModule cb
  banners $ "llvm-pretty module"
  putStrLn $ show (LLVM.ppModule mdl)
  putStrLn ""
  sdl <- forM (LLVM.modDefines mdl) $ \d -> do
    let ?lc = cbLLVMCtx cb
    case liftDefine d of
      Left ed -> do putStrLn $ show ed
                    return empty
      Right (warnings,sd) -> do
        mapM_ (putStrLn . show) warnings
        return $ ppSymDefine sd
  banners $ "translated module"
  putStr $ unlines $ fmap show sdl