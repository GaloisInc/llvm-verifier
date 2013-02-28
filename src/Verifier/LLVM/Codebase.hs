{- |
Module           : $Header$
Description      : Collection of LLVM Symbolic code
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Verifier.LLVM.Codebase
  ( Global(..)
  , Codebase(..)
  , cbDefs
  , cbUndefinedFns
  , dumpSymDefine
  , loadCodebase
  , lookupDefine
  , lookupSym
  , lookupFunctionType
  )

where

import           Control.Applicative
import Control.Lens
import           Control.Monad
import Control.Monad.State
import           Text.PrettyPrint.HughesPJ
import qualified Control.Exception              as CE
import qualified Data.ByteString                as BS
import qualified Data.LLVM.BitCode              as BC
import qualified Data.Map                       as M
import qualified Text.LLVM                      as LLVM
import qualified Text.LLVM                      as L

import           Verifier.LLVM.LLVMContext
import           Verifier.LLVM.AST
import           Verifier.LLVM.Translation

-- NB: We assume for the moment that we can be given a single .bc input (whether
-- or not we invoke the llvm linker ourselves in order to do this is something
-- that we can resolve later).

data Global = Global { globalSym :: !L.Symbol
                     , globalType :: !MemType
                     , globalValue :: TypedSymValue
                     }

type GlobalNameMap = M.Map L.Symbol (Either Global SymDefine)

data Codebase = Codebase {
    cbGlobalNameMap :: GlobalNameMap
  , cbLLVMCtx       :: LLVMContext
  , cbFunctionTypes :: M.Map L.Symbol FunDecl
  , origModule      :: LLVM.Module
  }


cbGlobalNameMapLens :: Simple Lens Codebase GlobalNameMap
cbGlobalNameMapLens = lens cbGlobalNameMap sfn
  where sfn v m = v { cbGlobalNameMap = m }

-- | Returns definitions in codebase.
cbDefs :: Codebase -> [SymDefine]
cbDefs = toListOf (folded . _Right) . cbGlobalNameMap

-- | Return all functions that are declared, but not defined.
cbUndefinedFns :: Codebase -> [(L.Symbol,FunDecl)]
cbUndefinedFns cb =
  toListOf (folded . filtered (not . (cbGlobalNameMap cb ^.) . contains . fst))
           (M.toList (cbFunctionTypes cb))


cbGlobalNameLens :: L.Symbol -> Simple Lens Codebase (Maybe (Either Global SymDefine))
cbGlobalNameLens sym = cbGlobalNameMapLens . at sym

cbFunctionTypeLens :: L.Symbol -> Simple Lens Codebase (Maybe FunDecl)
cbFunctionTypeLens sym = lens cbFunctionTypes sfn . at sym
  where sfn v m = v { cbFunctionTypes = m }

lookupFunctionType :: L.Symbol -> Codebase -> Maybe FunDecl
lookupFunctionType sym = view (cbFunctionTypeLens sym)

-- | Returns the global variable or symbolic definition associated with the
-- given symbol.
lookupSym :: LLVM.Symbol -> Codebase -> Maybe (Either Global SymDefine)
lookupSym sym = view (cbGlobalNameLens sym)

lookupDefine :: LLVM.Symbol -> Codebase -> Maybe SymDefine
lookupDefine sym = (^? _Right) <=< lookupSym sym
                         
-- For now, only take a single bytecode file argument and assume that the world
-- is linked together a priori.
loadCodebase :: FilePath -> IO Codebase
loadCodebase bcFile = do
  eab <- parse bcFile `CE.catch` \(e :: CE.SomeException) -> err (show e)
  case eab of
    Left msg  -> err (BC.formatError msg)
    Right mdl -> do
      let warn msg = putStrLn $ show $ text "Warning:" <+> msg
      when (null $ LLVM.modDataLayout mdl) $
        warn $ text "No target data layout found; will use defaults."
      let (err0,lc) = buildLLVMContext (L.modTypes mdl) (LLVM.modDataLayout mdl)
      mapM_ warn err0
      let cb0 = Codebase { cbGlobalNameMap = M.empty
                         , cbLLVMCtx = lc
                         , cbFunctionTypes = M.empty
                         , origModule = mdl
                         }
      let go = do
            let ?lc = lc
            -- Add definitions
            forM_ (L.modDefines mdl) $ \d ->
              case liftDefine d of
                Left emsg -> liftIO $ warn emsg
                Right (msgs,sd) -> do
                  mapM_ (liftIO . warn) msgs
                  let nm = sdName sd
                  let sdType = FunDecl (sdRetType sd) (snd <$> sdArgs sd) False
                  modify $ (cbGlobalNameLens   nm ?~ Right sd)
                         . (cbFunctionTypeLens nm ?~ sdType)
             -- Add declarations
            forM_ (L.modDeclares mdl) $ \d -> do
              let mtp = FunDecl <$> liftRetType (L.decRetType d)
                                <*> mapM liftMemType (L.decArgs d)
                                <*> pure (L.decVarArgs d)
              case mtp of
                Nothing -> liftIO $ warn $ text "Skipping import of" <+> L.ppSymbol (L.decName d)
                           <> text "; Unsupported type."
                Just tp -> modify $ cbFunctionTypeLens (L.decName d) `set` (Just tp)
             -- Add globals
            forM_ (L.modGlobals mdl) $ \lg -> do
              let sym = L.globalSym lg
              let mg = do tp <- liftMemType (L.globalType lg)
                          v <- liftValue tp (L.globalValue lg)
                          return Global { globalSym = sym
                                        , globalType = tp
                                        , globalValue = v
                                        }
              case mg of
                Nothing -> liftIO $ warn $ text "Skipping definition of" <+> L.ppSymbol sym
                           <> text "; Unsupported type."
                Just g -> modify $ cbGlobalNameLens sym ?~ Left g
      execStateT go cb0
  where
    parse = BS.readFile >=> BC.parseBitCode
    err msg = error $ "Bitcode parsing of " ++ bcFile ++ " failed:\n"
              ++ show (nest 2 (vcat $ map text $ lines msg))

dumpSymDefine :: MonadIO m => m Codebase -> String -> m ()
dumpSymDefine getCB sym = getCB >>= \cb ->
  liftIO $ putStrLn $ show $ (ppSymDefine `fmap` lookupDefine (LLVM.Symbol sym) cb)