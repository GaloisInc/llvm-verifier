{- |
Module           : $Header$
Description      : The command line driver for the LLVM symbolic simulator
Stability        : provisional
Point-of-contact : jstanley
-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns               #-}

module Main where

import           Control.Applicative             hiding (many)
import           Control.Monad
import           Data.Char
import           System.Console.CmdArgs.Implicit hiding (args, setVerbosity, verbosity)
import           System.Environment              (getArgs)
import           System.Exit
import           Text.ParserCombinators.Parsec
import           Verinf.Symbolic                 (createBitEngine)
import qualified System.Console.CmdArgs.Implicit as Args
import qualified Text.LLVM                       as L

import           LSSImpl

import           Verifier.LLVM.Backend (prettyTermD)
import           Verifier.LLVM.BitBlastBackend (createBuddyAll, createDagAll)
import           Verifier.LLVM.SAWBackend (createSAWBackend)

import           Verifier.LLVM.Codebase (loadCodebase, origModule, cbLLVMCtx)
import           Verifier.LLVM.LLVMContext (defaultMemGeom)
import           Verifier.LLVM.Translation  (ppSymDefine, liftDefine)

main :: IO ()
main = do
  numArgs <- length <$> getArgs
  when (numArgs == 0) $ do
    putStrLn $ "lss: No command line options given. Try --help for more information."
    exitFailure

  args <- cmdArgs $ LSS
            { dbug    = def &= help "Debug verbosity level (1-7)"
--             , stack   = def &= opt "8" &= help "Stack size in megabytes (default: 8)"
            , argv     = def &= typ "\"arg1 arg2 ...\""
                             &= help "Space-delimited arguments to main()"
            , backend  = def &= typ "[bitblast|dag|saw]"
                             &= help "Symbolic backend to use (default: bitblast)"
            , errpaths = def &= help "Dump error path details upon program completion (potentially very verbose)."
            , xlate    = def &= help "Prints the symbolic AST translation to stdout, and then terminates."
            , mname    = def &= typ "Fully-linked .bc containing main()"
                             &= Args.args
            }
            &= summary ("LLVM Symbolic Simulator (lss) 0.2a Aug 2012. "
                        ++ "Copyright 2011-2012 Galois, Inc. All rights reserved.")

  let eatWS (' ':cs) = eatWS cs
      eatWS cs       = cs

  backEnd <- case eatWS <$> backend args of
    Just "bitblast" -> return BitBlastBuddyAlloc
    Just "dag"      -> return BitBlastDagBased
    Just "saw"      -> return SAWBackendType
    Nothing         -> return BitBlastBuddyAlloc
    _               -> do
      putStrLn "Invalid backend specified.  Please choose 'bitblast', 'dag', or 'saw'."
      exitFailure

  bcFile <- case mname args of
    Nothing -> do putStrLn $ "lss: No LLVM bitcode file provided. "
                             ++ "Try --help for more information."
                  exitFailure
    Just nm -> return nm

  let p     = many $ between spaces spaces $ many1 $ satisfy $ not . isSpace
      argv' = case runParser p () "argv values" (argv args) of
                Left _e -> error "Unable to parse command-line arguments (argv)."
                Right x -> x

  cb <- loadCodebase bcFile
  when (xlate args) $ do
    let xlateDefine d = ppSymDefine sd
          where (_,sd) = liftDefine (cbLLVMCtx cb) d
    -- Dump the translated module and exit
    let via s f = mapM_ (putStrLn . show  . f) (s $ origModule cb)
    ((:[]) . L.modDataLayout) `via` L.ppDataLayout
    L.modTypes                `via` L.ppTypeDecl
    L.modGlobals              `via` L.ppGlobal
    L.modDeclares             `via` L.ppDeclare
    L.modDefines              `via` xlateDefine
    exitWith ExitSuccess

  be <- createBitEngine
  let lc = cbLLVMCtx cb
      mg = defaultMemGeom lc
      processRslt sbe execRslt = do
        case execRslt of
            NoMainRV _eps _mm -> do
              unless (dbug args == 0) $
                putStrLn "Obtained no return value from main()."
              _ <- exitWith ExitSuccess
              return ()
            SymRV _eps _mm rv -> do
              unless (dbug args == 0) $
                putStrLn "Obtained symbolic return value from main():"
              putStrLn $ show $ prettyTermD sbe rv
              _ <- exitWith ExitSuccess
              return ()
            ConcRV _eps _mm (fromIntegral -> rv) -> do
              unless (dbug args == 0) $
                putStrLn $ "Obtained concrete return value from main(): " ++ show rv
              _ <- exitWith (if rv == 0 then ExitSuccess else ExitFailure rv)
              return ()
  case backEnd of
    BitBlastDagBased -> do
      (sbe, mem) <- createDagAll be lc mg --first (sbeBitBlast lc be) <$> createDagMemModel lc be mg
      processRslt sbe =<< lssImpl sbe mem cb argv' args
    BitBlastBuddyAlloc -> do
       (sbe, mem) <- createBuddyAll be lc mg
       processRslt sbe =<< lssImpl sbe mem cb argv' args
    SAWBackendType -> do
      (sbe,mem) <- createSAWBackend lc mg    
      processRslt sbe =<< lssImpl sbe mem cb argv' args
