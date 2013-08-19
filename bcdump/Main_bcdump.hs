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
import           Text.PrettyPrint.Leijen hiding ((<$>))
import qualified Text.LLVM                      as LLVM

import Verifier.LLVM.Backend.SAW
import Verifier.LLVM.Codebase
import Verifier.LLVM.Simulator.SimUtils

import Verinf.Symbolic (createBitEngine)

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

  mdl <- loadModule bcFile `CE.catch` \(e :: CE.SomeException) -> err (show e)
  let dl = parseDataLayout $ LLVM.modDataLayout mdl
  be <- createBitEngine
  (sbe, _) <- createSAWBackend be dl
  (cbWarnings, cb) <- mkCodebase sbe dl mdl
  mapM_ (\m -> print $ text "Warning:" <+> m) cbWarnings

  banners $ "llvm-pretty module"
  putStrLn $ show (LLVM.ppModule mdl)
  putStrLn ""
  let sdl = fmap ppSymDefine $ cbDefs cb
  banners $ "translated module"
  putStr $ unlines $ fmap show sdl