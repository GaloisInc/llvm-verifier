{- |
Module           : $Header$
Description      : Collection of LLVM Symbolic code
License          : BSD3
Stability        : provisional
Point-of-contact : jhendrix
-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Verifier.LLVM.Codebase
  ( Global(..)
  , GlobalNameMap
  , Codebase
  , mkCodebase
  , cbLLVMContext
  , cbDataLayout
  , cbGlobalNameMap
  , cbGlobalDeps
  , cbFunctionType
  , cbFunctionTypes
  , cbDefs
  , cbUndefinedFns
  , lookupDefine
  , lookupSym
  , lookupFunctionType
    -- * Translation utilities.
  , liftStringValue
    -- * Loading utilities.
  , loadModule
  , loadCodebase
    -- * LLVMContext re-exports.
  , LLVMContext
  , compatMemTypeLists
  , compatRetTypes
    -- * AST re-exports.
  , module Verifier.LLVM.Codebase.AST
  )

where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Prelude ()
import Prelude.Compat hiding ( mapM, mapM_, (<>) )

import qualified Control.Exception              as CE
import qualified Data.ByteString                as BS
import qualified Data.Foldable                  as F
import qualified Data.LLVM.BitCode              as BC
import qualified Data.Map                       as M
import qualified Text.LLVM                      as LLVM
import qualified Text.LLVM                      as L

import Verifier.LLVM.Backend
import Verifier.LLVM.Codebase.AST
import Verifier.LLVM.Codebase.LLVMContext
import Verifier.LLVM.Codebase.Translation

-- NB: We assume for the moment that we can be given a single .bc input (whether
-- or not we invoke the llvm linker ourselves in order to do this is something
-- that we can resolve later).

data Global t = Global { globalSym :: !L.Symbol
                       , globalType :: !MemType
                       , globalValue :: SymValue t
                       }

type GlobalNameMap t = M.Map L.Symbol (Either (Global t) (SymDefine t))

data Codebase sbe = Codebase {
    cbLLVMContext :: LLVMContext
  , _cbGlobalNameMap :: GlobalNameMap (SBETerm sbe)
  , _cbFunctionTypes :: M.Map L.Symbol FunDecl
  }

cbDataLayout :: Codebase sbe -> DataLayout
cbDataLayout = llvmDataLayout . cbLLVMContext

cbGlobalNameMap :: Simple Lens (Codebase sbe) (GlobalNameMap (SBETerm sbe))
cbGlobalNameMap = lens _cbGlobalNameMap sfn
  where sfn v m = v { _cbGlobalNameMap = m }

cbFunctionTypes :: Simple Lens (Codebase sbe) (M.Map L.Symbol FunDecl)
cbFunctionTypes = lens _cbFunctionTypes sfn
  where sfn v m = v { _cbFunctionTypes = m }

cbFunctionType :: L.Symbol -> Lens' (Codebase sbe) (Maybe FunDecl)
cbFunctionType sym = cbFunctionTypes . at sym

lookupFunctionType :: L.Symbol -> Codebase sbe -> Maybe FunDecl
lookupFunctionType sym = view (cbFunctionType sym)

-- | Returns definitions in codebase.
cbDefs :: Codebase sbe -> [SymDefine (SBETerm sbe)]
cbDefs = toListOf (folded . _Right) . view cbGlobalNameMap

-- | Return all functions that are declared, but not defined.
cbUndefinedFns :: Codebase sbe -> [(L.Symbol,FunDecl)]
cbUndefinedFns cb =
  toListOf (folded . filtered (\(k,_) -> not (M.member k (cb^.cbGlobalNameMap))))
           (M.toList (cb^.cbFunctionTypes))

type EitherGlobal sbe = Either (Global (SBETerm sbe)) (SymDefine (SBETerm sbe))

cbGlobalName :: L.Symbol
             -> Lens' (Codebase sbe) (Maybe (EitherGlobal sbe))
cbGlobalName sym = cbGlobalNameMap . at sym

cbGlobalDeps :: Codebase sbe -> Global (SBETerm sbe) -> [L.Symbol]
cbGlobalDeps cb = globalRefs . globalValue
  where globalRefs (SValIdent _) = []
        globalRefs (SValSymbol sym) =
          case M.lookup sym nms of
            Just (Left _) -> [sym]
            _ -> []
        globalRefs (SValExpr e _) = F.concatMap globalRefs e
        nms = cb^.cbGlobalNameMap

-- | Returns the global variable or symbolic definition associated with the
-- given symbol.
lookupSym :: LLVM.Symbol
          -> Codebase sbe
          -> Maybe (EitherGlobal sbe)
lookupSym sym cb = cb^.cbGlobalName sym

lookupDefine :: LLVM.Symbol -> Codebase sbe -> Maybe (SymDefine (SBETerm sbe))
lookupDefine sym cb = cb^.cbGlobalName sym >>= (^? _Right)

-- | Create codebase from backend, parsed datalayout and module.
-- Return warnings during loading.
mkCodebase :: SBE sbe
           -> DataLayout
           -> L.Module
           -> IO ([Doc], Codebase sbe)
mkCodebase sbe dl mdl = do
  let (err0, lc) = mkLLVMContext dl (L.modTypes mdl)

  let cb0 = Codebase { cbLLVMContext = lc
                     , _cbGlobalNameMap = M.empty
                     , _cbFunctionTypes = M.empty
                     }
  fmap (over _1 reverse) $ flip execStateT ([],cb0) $ do
    let warn :: MonadState ([Doc],a) m => Doc -> m ()
        warn msg = _1 %= (msg:)
    when (null $ LLVM.modDataLayout mdl) $
      warn $ text "No target data layout found; will use defaults."
    mapM_ warn err0
    let ?lc = lc
    let ?sbe = sbe
     -- Add definitions
    forM_ (L.modDefines mdl) $ \d -> do
      md <- liftIO $ liftDefine d
      case md of
        Left emsg -> warn emsg
        Right (msgs,sd) -> do
          mapM_ warn msgs
          let nm = sdName sd
          _2 . cbGlobalNameMap . at nm ?= Right sd
          _2 . cbFunctionType  nm ?= FunDecl (sdRetType sd) (snd <$> sdArgs sd) False
    -- Add declarations
    forM_ (L.modDeclares mdl) $ \d -> do
      let mtp = FunDecl <$> liftRetType (L.decRetType d)
                        <*> mapM liftMemType (L.decArgs d)
                        <*> pure (L.decVarArgs d)
      case mtp of
        Nothing -> warn $ text "Skipping import of" <+> ppSymbol (L.decName d)
                       <> text "; Unsupported type."
        Just tp -> _2 . cbFunctionType (L.decName d) ?= tp
    -- Add globals
    forM_ (L.modGlobals mdl) $ \lg -> do
      let sym = L.globalSym lg
      mg <- liftIO $ runLiftAttempt $ do
        tp <- liftMemType' (L.globalType lg)
        case L.globalValue lg of
          Nothing -> failAttempt $ unwords ["mkCodebase: global has null value", show lg]
          Just gv -> Global sym tp <$> liftValue tp gv
      case mg of
        Left{} -> warn $ text "Skipping definition of" <+> ppSymbol sym
                      <> text "; Unsupported type."
        Right g -> _2 . cbGlobalNameMap . at sym ?= Left g

------------------------------------------------------------------------
-- LLVM helper utilities.

-- | Load a module, calling fail if parsing or loading fails.
loadModule :: FilePath -> IO L.Module
loadModule bcFile = do
  eab <- parse bcFile `CE.catch` \(e :: CE.SomeException) -> err (show e)
  case eab of
    Left msg  -> err (BC.formatError msg)
    Right mdl -> return mdl
 where
    parse = BS.readFile >=> BC.parseBitCode
    err msg = fail $ "Bitcode parsing of " ++ bcFile ++ " failed:\n"
              ++ show (nest 2 (vcat $ map text $ lines msg))

-- | Load module and return codebase with given backend.
loadCodebase :: SBE sbe -> FilePath -> IO (Codebase sbe)
loadCodebase sbe bcFile = do
  mdl <- loadModule bcFile
  let dl = parseDataLayout $ L.modDataLayout mdl
  (warnings,cb) <- mkCodebase sbe dl mdl
  mapM_ (\m -> putStrLn $ show $ text "Warning:" <+> m) warnings 
  return cb
