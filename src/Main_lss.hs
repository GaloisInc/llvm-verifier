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
import           Data.LLVM.Symbolic.AST
import           Data.LLVM.Symbolic.Translation  (liftDefine)
import           LSS.Execution.Codebase
import           LSS.Execution.Utils
import           LSS.SBEInterface
import           LSSImpl
import           System.Console.CmdArgs.Implicit hiding (args, setVerbosity, verbosity)
import           System.Environment              (getArgs)
import           System.Exit
import           Text.ParserCombinators.Parsec
import qualified System.Console.CmdArgs.Implicit as Args
import qualified Text.LLVM                       as L

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
            , memtype  = def &= typ "[bitblast|dagbased]"
                             &= help "Memory model to use (default: bitblast)"
            , errpaths = def &= help "Dump error path details upon program completion (potentially very verbose)."
            , xlate    = def &= help "Prints the symbolic AST translation to stdout, and then terminates."
            , mname    = def &= typ "Fully-linked .bc containing main()"
                             &= Args.args
            }
            &= summary ("LLVM Symbolic Simulator (lss) 0.1a Sep 2011. "
                        ++ "Copyright 2011 Galois, Inc. All rights reserved.")

  let eatWS (' ':cs) = eatWS cs
      eatWS cs       = cs

  mem <- case eatWS <$> memtype args of
           Just "dagbased" -> return DagBased
           Just "bitblast" -> return BitBlast
           Nothing         -> return BitBlast
           _               -> do
             putStrLn "Invalid memory model specified.  Please choose 'bitblast' or 'dagbased'."
             exitFailure

  bcFile <- case mname args of
    Nothing -> do putStrLn $ "lss: No LLVM bitcode file provided. "
                             ++ "Try --help for more information."
                  exitFailure
    Just nm -> return nm

  let p     = many $ between spaces spaces $ many1 $ satisfy $ not . isSpace
      argv' = case runParser p () "argv values" (argv args) of
                Left _e -> error "Unable to parse command-line arguments (argv)."
                Right x -> "lss" : x

  cb <- loadCodebase bcFile

  when (xlate args) $ do
    -- Dump the translated module and exit
    let via s f = mapM_ (putStrLn . show  . f) (s $ origModule cb)
    ((:[]) . L.modDataLayout) `via` L.ppDataLayout
    L.modTypes                `via` L.ppTypeDecl
    L.modGlobals              `via` L.ppGlobal
    L.modDeclares             `via` L.ppDeclare
    L.modDefines              `via` (ppSymDefine . liftDefine)
    exitWith ExitSuccess

  lssImpl cb argv' mem args $ \sbe execRslt -> do
    case execRslt of
        NoMainRV _eps _mm -> do
          unless (dbug args == 0) $ dbugM "Obtained no return value from main()."
          exitWith ExitSuccess >> return ()
        SymRV _eps _mm rv -> do
          unless (dbug args == 0) $ dbugM "Obtained symbolic return value from main():"
          dbugM $ show $ prettyTermD sbe rv
          exitWith ExitSuccess >> return ()
        ConcRV _eps _mm (fromIntegral -> rv) -> do
          unless (dbug args == 0) $ dbugM $ "Obtained concrete return value from main(): " ++ show rv
          exitWith (if rv == 0 then ExitSuccess else ExitFailure rv) >> return ()

