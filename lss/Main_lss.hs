{- |
Module           : $Header$
Description      : The command line driver for the LLVM symbolic simulator
Stability        : provisional
Point-of-contact : jstanley
-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE CPP                        #-}

module Main where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative             hiding (many)
#endif
import           Control.Monad
import qualified Data.ABC as ABC

import           Data.Char
import           System.Console.CmdArgs.Implicit hiding (args, setVerbosity, verbosity)
import           System.Environment              (getArgs)
import           System.Exit
import           Text.ParserCombinators.Parsec
import qualified System.Console.CmdArgs.Implicit as Args
import qualified Text.LLVM                       as L
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           LSSImpl

import Verifier.LLVM.Backend (prettyTermD, SBEPair(..))
import qualified Verifier.LLVM.Backend.BitBlast as BB


import Verifier.LLVM.Backend.SAW (createSAWBackend)
import Verifier.LLVM.Codebase
import Verifier.LLVM.MemModel.Geometry (defaultMemGeom)

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
            , startDebugger = def &= help "Break and enter the LSS debugger when running main"
            , satBranches   = def &= help "With a supported symbolic backend, always check satisfiability of symbolic path assertions at branches"
            }
            &= summary ("LLVM Symbolic Simulator (lss) 0.2d July 2013. "
                        ++ "Copyright 2011-2013 Galois, Inc. All rights reserved.")

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

  mdl <-
    case mname args of
      Nothing -> do putStrLn $ "lss: No LLVM bitcode file provided. "
                               ++ "Try --help for more information."
                    exitFailure
      Just nm -> loadModule nm

  let dl = parseDataLayout $ L.modDataLayout mdl

  SBEPair sbe mem <- 
    case backEnd of
      BitBlastDagBased -> do
        ABC.SomeGraph g <- ABC.newGraph ABC.giaNetwork
        BB.createDagAll g dl (defaultMemGeom dl)
      BitBlastBuddyAlloc -> do
        ABC.SomeGraph g <- ABC.newGraph ABC.giaNetwork
        return (BB.createBuddyAll g dl (defaultMemGeom dl))
      SAWBackendType -> do
        uncurry SBEPair <$> createSAWBackend ABC.giaNetwork dl
  (cbWarnings,cb) <- mkCodebase sbe dl mdl
  mapM_ (\m -> print $ text "Warning:" <+> m) cbWarnings
  -- Print out translation when just asked to translate.
  when (xlate args) $ do
    let ?lc = cbLLVMContext cb
    -- Dump the translated module and exit
    let via s f = mapM_ (putStrLn . show  . f) (s mdl)
    ((:[]) . L.modDataLayout) `via` L.ppDataLayout
    L.modTypes                `via` L.ppTypeDecl
    L.modGlobals              `via` L.ppGlobal
    L.modDeclares             `via` L.ppDeclare
    mapM_ (print . ppSymDefine) (cbDefs cb)
    exitWith ExitSuccess

  let p     = many $ between spaces spaces $ many1 $ satisfy $ not . isSpace
      argv' = case runParser p () "argv values" (argv args) of
                Left _e -> error "Unable to parse command-line arguments (argv)."
                Right x -> x

  execRslt <- lssImpl sbe mem cb argv' args
  case execRslt of
    NoMainRV _eps _mm -> do
      unless (dbug args == 0) $
        putStrLn "Obtained no return value from main()."
      void $ exitWith ExitSuccess
    SymRV _eps _mm rv -> do
      unless (dbug args == 0) $
         putStrLn "Obtained symbolic return value from main():"
      putStrLn $ show $ prettyTermD sbe rv
      void $ exitWith ExitSuccess
    ConcRV _eps _mm (fromIntegral -> rv) -> do
      unless (dbug args == 0) $
        putStrLn $ "Obtained concrete return value from main(): " ++ show rv
      void $ exitWith (if rv == 0 then ExitSuccess else ExitFailure rv)
