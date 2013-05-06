{- |
Module           : $Header$
Description      : Provides information about types in an LLVM Module.
Stability        : provisional
Point-of-contact : jhendrix
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
module Verifier.LLVM.LLVMContext
  ( module Verifier.LLVM.DataLayout
  , AliasMap
  , mkAliasMap
  , LLVMContext
  , mkLLVMContext
  , llvmDataLayout
  , llvmAliasMap
  , liftMemType
  , liftRetType
  , asMemType
  , asRetType
  , Addr
  , MemGeom(..)
  , defaultMemGeom
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.State (State, runState, MonadState(..), modify)
import           Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Text.LLVM         as L
import Text.PrettyPrint.Leijen hiding ((<$>))


import Verifier.LLVM.DataLayout

instance (?lc :: LLVMContext) => Eq FunDecl where
  FunDecl xr xa xv == FunDecl yr ya yv = (xr,xa,xv) == (yr,ya,yv)

instance (?lc :: LLVMContext) => Eq SymType where
  MemType x == MemType y = x == y
  Alias i == y = maybe False (== y) (lookupAlias i)
  x == Alias i = maybe False (x ==) (lookupAlias i)
  FunType x == FunType y = x == y
  UnsupportedType{} == UnsupportedType{} = True
  VoidType == VoidType = True
  _ == _ = False

instance (?lc :: LLVMContext) => Eq MemType where
  IntType x == IntType y = x == y
  FloatType == FloatType = True
  DoubleType == DoubleType = True
  PtrType x == PtrType y = x == y
  ArrayType xn xt == ArrayType yn yt = (xn,xt) == (yn,yt)
  VecType   xn xt == VecType   yn yt = (xn,xt) == (yn,yt)
  StructType x == StructType y = x == y
  _ == _ = False

instance (?lc :: LLVMContext) => Eq StructInfo where
  x == y = (siIsPacked x, siFieldTypes x)
        == (siIsPacked y, siFieldTypes y)

data IdentStatus
  = Resolved SymType
  | Active
  | Pending L.Type

data TCState = TCS { tcsDataLayout :: DataLayout
                   , tcsMap :: Map L.Ident IdentStatus 
                     -- | Set of types encountered that are not supported by
                     -- the 
                   , tcsUnsupported :: Set L.Type
                   , tcsUnresolvable :: Set L.Ident
                   }

runTC :: DataLayout
      -> Map L.Ident IdentStatus
      -> TC a
      -> ([Doc], a)
runTC pdl initMap m = over _1 tcsErrors . view swapped $ runState m tcs0
  where tcs0 = TCS { tcsDataLayout = pdl
                   , tcsMap =  initMap
                   , tcsUnsupported = Set.empty
                   , tcsUnresolvable = Set.empty
                   }

tcsErrors :: TCState -> [Doc]
tcsErrors tcs = (ppUnsupported <$> Set.toList (tcsUnsupported tcs))
             ++ (ppUnresolvable <$> Set.toList (tcsUnresolvable tcs))
  where ppUnsupported tp = text "Unsupported type:" <+> text (show (L.ppType tp))
        ppUnresolvable i = text "Could not resolve identifier:" <+> text (show (L.ppIdent i))
 
-- | Type lifter contains types that could not be parsed.
type TC = State TCState

recordUnsupported :: L.Type -> TC ()
recordUnsupported tp = modify fn
  where fn tcs = tcs { tcsUnsupported = Set.insert tp (tcsUnsupported tcs) }

-- | Returns the type bound to an identifier.
tcIdent :: L.Ident -> TC SymType
tcIdent i = do
  im <- tcsMap <$> get
  let retUnsupported = tp <$ modify fn
        where tp = UnsupportedType (L.Alias i)
              fn tcs = tcs { tcsUnresolvable = Set.insert i (tcsUnresolvable tcs) }
  case Map.lookup i im of
    Nothing -> retUnsupported
    Just (Resolved tp) -> return tp
    Just Active -> retUnsupported
    Just (Pending tp) -> do
        modify (ins Active)
        stp <- tcType tp
        stp <$ modify (ins (Resolved stp))
      where ins v tcs = tcs { tcsMap = Map.insert i v (tcsMap tcs) }

resolveMemType :: SymType -> TC (Maybe MemType)
resolveMemType = resolve
  where resolve (MemType mt) = return (Just mt)
        resolve (Alias i) = resolve =<< tcIdent i
        resolve FunType{} = return Nothing
        resolve UnsupportedType{} = return Nothing
        resolve VoidType = return Nothing

resolveRetType :: SymType -> TC (Maybe RetType)
resolveRetType = resolve
  where resolve (MemType mt) = return (Just (Just mt))
        resolve (Alias i) = resolve =<< tcIdent i
        resolve VoidType = return (Just Nothing)
        resolve _ = return Nothing

tcMemType :: L.Type -> TC (Maybe MemType)
tcMemType tp = resolveMemType =<< tcType tp

tcType :: L.Type -> TC SymType
tcType tp0 = do
  let badType = UnsupportedType tp0 <$ recordUnsupported tp0
  let maybeApp :: (a -> MemType) -> TC (Maybe a) -> TC SymType
      maybeApp f mmr = maybe badType (return . MemType . f) =<< mmr
  case tp0 of
    L.PrimType pt ->
      case pt of
        L.Void -> return VoidType
        L.Integer w -> return $ MemType $ IntType (fromIntegral w)
        L.FloatType ft -> do
          case ft of
            L.Float -> return $ MemType FloatType
            L.Double -> return $ MemType DoubleType
            _ -> badType
        _ -> badType
    L.Alias i -> return (Alias i)
    L.Array n etp -> maybeApp (ArrayType (fromIntegral n)) $ tcMemType etp
    L.FunTy res args va -> do
      mrt <- resolveRetType =<< tcType res
      margs <- mapM tcMemType args
      maybe badType (return . FunType) $
        FunDecl <$> mrt <*> sequence margs <*> pure va
    L.PtrTo tp ->  (MemType . PtrType) <$> tcType tp
    L.Struct tpl       -> maybeApp StructType $ tcStruct False tpl
    L.PackedStruct tpl -> maybeApp StructType $ tcStruct True  tpl
    L.Vector n etp -> maybeApp (VecType (fromIntegral n)) $ tcMemType etp
    L.Opaque -> badType

-- | Constructs a function for obtaining target-specific size/alignment
-- information about structs.  The function produced corresponds to the
-- StructLayout object constructor in TargetData.cpp.
tcStruct :: Bool -> [L.Type] -> TC (Maybe StructInfo)
tcStruct packed fldTys = do
  pdl <- tcsDataLayout <$> get
  fmap (mkStructInfo pdl packed) . sequence <$> mapM tcMemType fldTys

type AliasMap = Map L.Ident SymType

data LLVMContext = LLVMContext
  { llvmDataLayout :: DataLayout
  , llvmAliasMap  :: AliasMap
  }

lookupAlias :: (?lc :: LLVMContext) => L.Ident -> Maybe SymType
lookupAlias i = llvmAliasMap ?lc ^. at i

asMemType :: AliasMap -> SymType -> Maybe MemType
asMemType _ (MemType mt) = return mt
asMemType m (Alias i) = asMemType m =<< (m ^. at i)
asMemType _ _ = Nothing

asRetType :: AliasMap -> SymType -> Maybe RetType
asRetType _ (MemType mt) = Just (Just mt)
asRetType _ VoidType = Just Nothing
asRetType m (Alias i) = asRetType m =<< (m ^. at i)
asRetType _ _ = Nothing

-- | Returns the type alias map for the given types.
mkAliasMap :: DataLayout -> [L.TypeDecl]  -> ([Doc], AliasMap)
mkAliasMap dl decls = runTC dl (Pending <$> Map.fromList tps) $ do
    Map.fromList <$> traverse (_2 tcType) tps
  where tps = [ (L.typeName d, L.typeValue d) | d <- decls ]

-- | Returns an LLVM context and types that are not supported by symbolic simulator.
mkLLVMContext :: DataLayout -> AliasMap -> LLVMContext
mkLLVMContext = LLVMContext

liftType :: (?lc :: LLVMContext) => L.Type -> Maybe SymType
liftType tp | null edocs = Just stp
            | otherwise = Nothing
  where m0 = Resolved <$> llvmAliasMap ?lc
        (edocs,stp) = runTC (llvmDataLayout ?lc) m0 $ tcType tp

liftMemType :: (?lc :: LLVMContext) => L.Type -> Maybe MemType
liftMemType tp = asMemType (llvmAliasMap ?lc) =<< liftType tp

liftRetType :: (?lc :: LLVMContext) => L.Type -> Maybe RetType
liftRetType tp = asRetType (llvmAliasMap ?lc) =<< liftType tp

-- Memery Geometry

type Addr = Integer

data MemGeom = MemGeom {
        mgStack :: (Addr, Addr)
      , mgCode :: (Addr, Addr)
      , mgData :: (Addr, Addr)
      , mgHeap :: (Addr, Addr)
      }

-- We make a keep it simple concession and divide up the address space as
-- follows:
--
-- Top  1/4: Stack
-- Next 1/8: Code
-- Next 1/8: Data
-- Last 1/2: Heap
--
-- One advantage of this is that it's easy to tell the segment to which a
-- pointer belongs simply by inspecting its address.
--
-- TODO: Allow user overrides of memory geom
defaultMemGeom :: DataLayout  -> MemGeom
defaultMemGeom dl
    | w < 16 =  error "Pointers must be at least 16bits to get sufficient memory size."
    | otherwise = 
        MemGeom (stackStart, stackEnd)
                (codeStart,  codeEnd)
                (dataStart,  dataEnd)
                (heapStart,  heapEnd)
  where
    w = ptrBitwidth dl
    stackStart  = 4096 -- Start at first page rather than null
    codeStart   = 2 ^ w `div` 4
    dataStart   = codeStart + 2 ^ w `div` 8
    heapStart   = dataStart + 2 ^ w `div` 8

    stackEnd    = codeStart - 1
    codeEnd     = dataStart - 1
    dataEnd     = heapStart - 1
    heapEnd     = 2 ^ w - 1