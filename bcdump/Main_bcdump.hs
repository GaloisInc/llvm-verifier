-- Dead simple llvm-dis/parser/AST translation inspection utility

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Exception as CE
import           Control.Monad
import           System.Environment
import           System.Process
import           System.Directory
import           System.FilePath
import           LSS.Execution.Utils
import           Data.LLVM.Symbolic.AST
import           Data.LLVM.Symbolic.Translation
import           Text.PrettyPrint.HughesPJ
import qualified Data.LLVM.BitCode              as BC
import qualified Data.ByteString    as BS
import qualified Text.LLVM                      as LLVM

main :: IO ()
main = do
  files <- getArgs
  when (length files /= 1) $ error "Usage: bcdump <LLVM bytecode file>"
  let bcFile  = head files
      tmpll   = bcFile <.> "tmp" <.> "ll"
      parse   = BS.readFile >=> BC.parseBitCode
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

  eab <- parse bcFile `CE.catch` \(e :: CE.SomeException) -> err (show e)
  case eab of
    Left msg  -> err (BC.formatError msg)
    Right mdl -> do
      banners $ "llvm-pretty module"
      putStrLn $ show (LLVM.ppModule mdl)
      putStrLn ""
      banners $ "translated module"
      putStr
        $ unlines
        $ map (show . ppSymDefine)
        $ map liftDefine
        $ LLVM.modDefines mdl
