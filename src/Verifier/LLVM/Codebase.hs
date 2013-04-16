{- |
Module           : $Header$
Description      : Collection of LLVM Symbolic code
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Verifier.LLVM.Codebase
  ( module Verifier.LLVM.LLVMContext
  , Global(..)
  , Codebase
  , cbLLVMContext
  , cbDataLayout
  , cbGlobalNameMap
  , cbFunctionType
  , cbFunctionTypes
  , cbDefs
  , cbUndefinedFns
  , dumpSymDefine
  , loadModule
  , mkCodebase
  , loadCodebase
  , lookupDefine
  , lookupSym
  , lookupFunctionType
  , liftStringValue
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

data Global t = Global { globalSym :: !L.Symbol
                       , globalType :: !MemType
                       , globalValue :: SymValue t
                       }

type GlobalNameMap t = M.Map L.Symbol (Either (Global t) (SymDefine t))

data Codebase sbe = Codebase {
    cbDataLayout :: DataLayout
  , cbAliasMap :: AliasMap
  , _cbGlobalNameMap :: GlobalNameMap (SBETerm sbe)
  , _cbFunctionTypes :: M.Map L.Symbol FunDecl
  }

-- | Return llvm context within codebase.
cbLLVMContext :: Codebase sbe -> LLVMContext
cbLLVMContext cb = mkLLVMContext (cbDataLayout cb) (cbAliasMap cb)

cbGlobalNameMap :: Simple Lens (Codebase sbe) (GlobalNameMap (SBETerm sbe))
cbGlobalNameMap = lens _cbGlobalNameMap sfn
  where sfn v m = v { _cbGlobalNameMap = m }

cbFunctionTypes :: Simple Lens (Codebase sbe) (M.Map L.Symbol FunDecl)
cbFunctionTypes = lens _cbFunctionTypes sfn
  where sfn v m = v { _cbFunctionTypes = m }

cbFunctionType :: L.Symbol -> Simple Lens (Codebase sbe) (Maybe FunDecl)
cbFunctionType sym = cbFunctionTypes . at sym

lookupFunctionType :: L.Symbol -> Codebase sbe -> Maybe FunDecl
lookupFunctionType sym = view (cbFunctionType sym)

-- | Returns definitions in codebase.
cbDefs :: Codebase sbe -> [SymDefine (SBETerm sbe)]
cbDefs = toListOf (folded . _Right) . view cbGlobalNameMap

-- | Return all functions that are declared, but not defined.
cbUndefinedFns :: Codebase sbe -> [(L.Symbol,FunDecl)]
cbUndefinedFns cb =
  toListOf (folded . filtered (not . ((cb^.cbGlobalNameMap) ^.) . contains . fst))
           (M.toList (cb^.cbFunctionTypes))

type EitherGlobal sbe = Either (Global (SBETerm sbe)) (SymDefine (SBETerm sbe))

cbGlobalName :: L.Symbol
             -> Simple Lens (Codebase sbe) (Maybe (EitherGlobal sbe))
cbGlobalName sym = cbGlobalNameMap . at sym

-- | Returns the global variable or symbolic definition associated with the
-- given symbol.
lookupSym :: LLVM.Symbol
          -> Codebase sbe
          -> Maybe (EitherGlobal sbe)
lookupSym sym cb = cb^.cbGlobalName sym

lookupDefine :: LLVM.Symbol -> Codebase sbe -> Maybe (SymDefine (SBETerm sbe))
lookupDefine sym = lookupSym sym >=> (^? _Right)

loadModule :: FilePath -> IO L.Module
loadModule bcFile = do
  eab <- parse bcFile `CE.catch` \(e :: CE.SomeException) -> err (show e)
  case eab of
    Left msg  -> err (BC.formatError msg)
    Right mdl -> return mdl
 where
    parse = BS.readFile >=> BC.parseBitCode
    err msg = error $ "Bitcode parsing of " ++ bcFile ++ " failed:\n"
              ++ show (nest 2 (vcat $ map text $ lines msg))

mkCodebase :: SBE sbe
           -> DataLayout
           -> L.Module
           -> IO (Codebase sbe)
mkCodebase sbe dl mdl = do
  when (null $ LLVM.modDataLayout mdl) $
    warn $ text "No target data layout found; will use defaults."
  mapM_ warn err0
  execStateT go cb0
 where warn msg = putStrLn $ show $ text "Warning:" <+> msg
       (err0, am) = mkAliasMap dl (L.modTypes mdl)
       cb0 = Codebase { cbDataLayout = dl
                      , cbAliasMap = am
                      , _cbGlobalNameMap = M.empty
                      , _cbFunctionTypes = M.empty
                      }
       go = do
         let ?lc = mkLLVMContext dl am
         let ?sbe = sbe
         -- Add definitions
         forM_ (L.modDefines mdl) $ \d -> do
           md <- liftIO $ liftDefine d
           case md of
             Left emsg -> liftIO $ warn emsg
             Right (msgs,sd) -> do
               mapM_ (liftIO . warn) msgs
               let nm = sdName sd
               cbGlobalNameMap . at nm ?= Right sd
               cbFunctionType  nm ?= FunDecl (sdRetType sd) (snd <$> sdArgs sd) False
         -- Add declarations
         forM_ (L.modDeclares mdl) $ \d -> do
           let mtp = FunDecl <$> liftRetType (L.decRetType d)
                             <*> mapM liftMemType (L.decArgs d)
                             <*> pure (L.decVarArgs d)
           case mtp of
             Nothing -> liftIO $ warn $ text "Skipping import of" <+> L.ppSymbol (L.decName d)
                          <> text "; Unsupported type."
             Just tp -> cbFunctionType (L.decName d) ?= tp
         -- Add globals
         forM_ (L.modGlobals mdl) $ \lg -> do
           let sym = L.globalSym lg
           mg <- liftIO $ runLiftAttempt $ do
             tp <- liftMemType' (L.globalType lg)
             Global sym tp <$> liftValue tp (L.globalValue lg)
           case mg of
             Nothing -> liftIO $ warn $ text "Skipping definition of" <+> L.ppSymbol sym
                          <> text "; Unsupported type."
             Just g -> modify $ cbGlobalNameMap . at sym ?~ Left g

-- For now, only take a single bytecode file argument and assume that the world
-- is linked together a priori.
loadCodebase :: SBE sbe -> FilePath -> IO (Codebase sbe)
loadCodebase sbe bcFile = do
  mdl <- loadModule bcFile
  let dl = parseDataLayout $ L.modDataLayout mdl
  mkCodebase sbe dl mdl

dumpSymDefine :: MonadIO m => m (Codebase sbe) -> String -> m ()
dumpSymDefine getCB sym = getCB >>= \cb ->
  liftIO $ putStrLn $ show $ (ppSymDefine `fmap` lookupDefine (LLVM.Symbol sym) cb)
