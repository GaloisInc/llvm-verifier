{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{- |
Module           : $Header$
Description      : Symbolic execution tests
License          : BSD3
Stability        : provisional
Point-of-contact : acfoltzer

This module defines the translation from LLVM IR to Symbolic IR.

Translation into symbolic IR requires post-dominator information about the
LLVM IR.  This information is analyzed and generated during translation.

In addition to branches, call and phi non-terminal instructions require
special support:

[Phi Statements]
  The value of a Phi statement in LLVM depends on which previous block was
  executed.  Since phi statements must appear at the top of the block, we can
  move phi statements to execute during the transition from the previous
  block to the new block.
-}
module Verifier.LLVM.Codebase.Translation
  ( liftDefine
  , LiftAttempt
  , runLiftAttempt
  , failAttempt
  , liftMemType'
  , liftValue
  , liftStringValue
  ) where

import Control.Exception (try, catch, Exception, throwIO)
import Control.Lens hiding (op)
import Control.Monad.State.Strict hiding ( fail )
import qualified Data.Foldable as F
import qualified Data.LLVM.CFG              as CFG
import Data.Map                   (Map)
import qualified Data.Map                   as Map
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Vector                as V
import Data.Word (Word8)
import qualified Text.LLVM                  as L
import qualified Text.LLVM.PP               as L
import Text.LLVM.AST              (Stmt'(..), Typed (..))
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Prelude ()
import Prelude.Compat hiding ( mapM_, (<>), fail )

import Verifier.LLVM.Backend
import Verifier.LLVM.Codebase.AST
import Verifier.LLVM.Codebase.LLVMContext

-- Utility {{{1

-- | This function is called whenever lifting fails due to an internal error.
liftError :: Doc -> a
liftError d = error (show d)

-- LLVMTranslationInfo {{{1
-- | Information about basic block and control-flow graph used during
-- code generation.
newtype LLVMTranslationInfo = LTI CFG.CFG

-- | Build the translation info using the given CFG's dominator analysis
mkLTI :: CFG.CFG -> LLVMTranslationInfo
mkLTI = LTI

blockIsDummyExit :: L.BlockLabel -> Bool
blockIsDummyExit (L.Named (L.Ident nm)) = nm == CFG.dummyExitName
blockIsDummyExit _ = False

-- | Returns basic blocks excluding dummy exit block.
ltiBlocks :: LLVMTranslationInfo -> [CFG.BB]
ltiBlocks (LTI cfg) = [ bb
                      | bb <- CFG.allBBs cfg
                      , let Just lab = L.bbLabel bb
                      , not (blockIsDummyExit (snd lab))
                      ]

-- | @ltiImmediatePostDominator lti bb@ returns the immediate post dominator
-- of @bb@ or @Nothing@ if it has no post-dominator.
ltiImmediatePostDominator :: LLVMTranslationInfo
                          -> L.BlockLabel
                          -> Maybe L.BlockLabel
ltiImmediatePostDominator (LTI cfg) bb =
  case CFG.ipdom cfg (CFG.asId cfg bb) of
    Nothing    -> Nothing
    Just apdId | blockIsDummyExit n -> Nothing
               | otherwise -> Just n
      where n = CFG.asName cfg apdId

-- Block generator Monad {{{1

type TranslationWarning = Doc

data BlockGeneratorState t =
     BlockGeneratorState { _bgBlocks :: [SymBlock t]
                         , _bgRevWarnings :: [TranslationWarning]
                         }

bgBlocks :: Simple Lens (BlockGeneratorState t) [SymBlock t]
bgBlocks = lens _bgBlocks (\s v -> s { _bgBlocks = v })

bgRevWarnings :: Simple Lens (BlockGeneratorState t) [TranslationWarning]
bgRevWarnings = lens _bgRevWarnings (\s v -> s { _bgRevWarnings = v })

--type BlockGenerator sbe a = StateT (BlockGeneratorState (SBETerm sbe)) IO a

runBlockGenerator :: (Functor f, Monad f) =>
                     StateT (BlockGeneratorState t) f a
                  -> f ([TranslationWarning], [SymBlock t])
runBlockGenerator m = final <$> execStateT m s0
  where s0 = BlockGeneratorState { _bgBlocks = []
                                 , _bgRevWarnings = []
                                 }
        final s1 = (reverse (_bgRevWarnings s1), _bgBlocks s1)

mkSymBlock :: SymBlockID -> [SymStmt t] -> SymBlock t
mkSymBlock sbid stmts = SymBlock { sbId = sbid, sbStmts = V.fromList stmts }

addWarning :: MonadState (BlockGeneratorState t) m =>
              TranslationWarning -> m ()
addWarning d = bgRevWarnings %= (d:)

-- Phi instruction parsing {{{1


-- | Maps source target pairs to either an unsupported stmt or list of
-- assignments.
type PhiMap t = Map (L.BlockLabel, L.BlockLabel)
                    (Either L.Stmt [(L.Ident, MemType, SymValue t)])


blockPhiMap' :: (?lc::LLVMContext, ?sbe :: SBE sbe)
             => [CFG.BB] -> IO (PhiMap (SBETerm sbe))
blockPhiMap' blocks = execStateT (traverse go blocks) Map.empty
  where go :: (?lc::LLVMContext, ?sbe :: SBE sbe)
           => CFG.BB -> StateT (PhiMap (SBETerm sbe)) IO ()
        go bb@(L.BasicBlock { L.bbLabel = Nothing }) =
          error $ unwords ["blockPhiMap': basic block missing label: ", show bb]
        go (L.BasicBlock { L.bbLabel = Just (_,tgt), L.bbStmts = sl }) =
          mapM_ (parseInstr tgt) (fmap (fmap snd) sl)
        parseInstr tgt stmt@(L.Result r (L.Phi tp vals) _) = do
          forM_ vals $ \(v,src) -> do
            mentry <- runLiftAttempt $ do
              mtp <- liftMemType' tp
              val <- liftValue mtp v
              return (r,mtp,val)
            modify (updateInstr stmt src tgt mentry)
        parseInstr _ _ = return ()
        updateInstr stmt src tgt mr m =
          case mr of
            Left{} -> Map.insert (src,tgt) (Left stmt) m
            Right entry ->
              case fromMaybe (Right []) $ Map.lookup (src,tgt) m of
                Left{}  -> m
                Right l -> Map.insert (src,tgt) (Right (entry:l)) m

-- | Computation that attempts to lift LLVM values to symbolic representation.
-- This runs in IO, because symbolic backends may need to do IO.
newtype LiftAttempt a = LiftAttempt { unLiftAttempt :: IO a }
  deriving (Functor, Applicative, MonadIO)

instance Monad LiftAttempt where
  return = LiftAttempt . return
  LiftAttempt m >>= f = LiftAttempt (m >>= unLiftAttempt . f)

failAttempt :: forall a. String -> LiftAttempt a
failAttempt = LiftAttempt . throwIO . LiftAttemptError

newtype LiftAttemptError = LiftAttemptError { unLiftAttemptError :: String }
  deriving Show

instance Exception LiftAttemptError

errorStack :: String -> LiftAttempt a -> LiftAttempt a
errorStack prefix (LiftAttempt m) = LiftAttempt $
  m `catch` \(LiftAttemptError e) -> throwIO (LiftAttemptError (prefix ++ e))

runLiftAttempt :: MonadIO m => LiftAttempt a -> m (Either String a)
runLiftAttempt m =
  liftIO (either (Left . unLiftAttemptError) Right <$> try (unLiftAttempt m))

unsupportedStmt :: (MonadState (BlockGeneratorState t) m)
                => L.Stmt
                -> String
                -> m (SymStmt t)
unsupportedStmt stmt detailMsg = do
  let base = text "Unsupported instruction: " <+> text (show (L.ppLLVM (L.ppStmt stmt)))
      detail | null detailMsg = base
             | otherwise = base <$$> indent 2 (text detailMsg)
  addWarning detail
  return (BadSymStmt stmt)

trySymStmt :: (MonadState (BlockGeneratorState t) m, MonadIO m) =>
              L.Stmt -> LiftAttempt (SymStmt t) -> m (SymStmt t)
trySymStmt stmt m = do
  mr <- runLiftAttempt m
  case mr of
    Left msg -> unsupportedStmt stmt msg
    Right s -> return s

-- Lift operations

liftTypedValue :: (?lc :: LLVMContext, ?sbe :: SBE sbe)
               => L.Typed L.Value
               -> LiftAttempt (SymValue (SBETerm sbe))
liftTypedValue (L.Typed tp v) = flip liftValue v =<< liftMemType' tp

mkSValExpr :: (?sbe :: SBE sbe, MonadIO m)
           => TypedExpr (SymValue (SBETerm sbe))
           -> m (SymValue (SBETerm sbe))
mkSValExpr expr = liftIO $ SValExpr expr <$> typedExprEval ?sbe expr

liftStringValue :: (?sbe :: SBE sbe) => [Word8] -> IO (SymValue (SBETerm sbe))
liftStringValue s = mkSValExpr . SValArray tp =<< traverse toChar (V.fromList s)
 where tp = IntType 8
       toChar c = mkSValExpr (SValInteger 8 (toInteger c))

liftValue :: (?lc :: LLVMContext, ?sbe :: SBE sbe)
          => MemType -- ^ Expected type of value.
          -> L.Value -- ^ LLVM Value to lift.
          -> LiftAttempt (SymValue (SBETerm sbe))
liftValue (IntType w) (L.ValInteger x) =
  mkSValExpr $ SValInteger w x
liftValue (IntType 1) (L.ValBool x) =
  mkSValExpr $ SValInteger 1 (if x then 1 else 0)
liftValue FloatType  (L.ValFloat x) =
  mkSValExpr $ SValFloat x
liftValue DoubleType (L.ValDouble d) =
  mkSValExpr $ SValDouble d
liftValue _ (L.ValIdent i) =
  return $ SValIdent i
liftValue _ (L.ValSymbol symval) =
  return $ SValSymbol symval
liftValue (PtrType etp) L.ValNull =
  mkSValExpr $ SValNull etp
liftValue (IntType w) L.ValNull =
  -- TODO: this seems odd, but it does show up in LLVM bitcode!
  mkSValExpr $ SValInteger w 0
liftValue (ArrayType len etp) (L.ValArray _ el0)
    | fromIntegral len == V.length el =
       mkSValExpr . SValArray etp =<< traverse (liftValue etp) el
  where el = V.fromList el0
liftValue (VecType len etp) (L.ValVector _ el0)
    | fromIntegral len == V.length el =
      mkSValExpr . SValVector etp =<< traverse (liftValue etp) el
  where el = V.fromList el0
liftValue (StructType si) (L.ValStruct fldvs) =
    mkSValExpr . SValStruct si =<< traverse liftTypedValue (V.fromList fldvs)
liftValue (StructType si) (L.ValPackedStruct fldvs) =
    mkSValExpr . SValStruct si =<< traverse liftTypedValue (V.fromList fldvs)
liftValue (ArrayType len (IntType 8)) (L.ValString str) = do
  unless (fromIntegral len == length str) $ failAttempt "Incompatible types"
  liftIO $ liftStringValue str
liftValue rtp (L.ValConstExpr ce)  =
  case ce of
    L.ConstGEP _ (Just _) _ _ ->
      failAttempt "NYI: Constant GEP with `inrange`"
    L.ConstGEP inbounds Nothing _ (base:il) -> do
      mkSValExpr . snd =<< liftGEP inbounds base il
    L.ConstConv op (L.Typed itp0 t) _tp1 -> do
      itp <- liftMemType' itp0
      v <- liftValue itp t
      case (op,itp,rtp) of
        (L.PtrToInt, PtrType ptrType, IntType w) -> do
          mkSValExpr (PtrToInt Nothing ptrType v w)
        (L.IntToPtr, IntType w, PtrType ptrType) -> do
          mkSValExpr (IntToPtr Nothing w v ptrType)
        (L.BitCast,_,_) -> liftBitcast itp v rtp
        _ -> failAttempt "Could not interpret constant expression."
    _ -> failAttempt "Could not interpret constant expression."
liftValue tp L.ValUndef = zeroValue tp
liftValue _  L.ValLabel{} = failAttempt "Could not interpret label."
liftValue tp L.ValZeroInit = zeroValue tp
liftValue _  L.ValAsm{} = failAttempt "Could not interpret asm."
liftValue _  L.ValMd{} = failAttempt "Could not interpret metadata."
liftValue tp v =
    failAttempt $ concat [ "Could not interpret LLVM value "
                         , show v
                         , " of type "
                         , show (ppMemType tp)
                         ]

-- | Lift a bitcast expression.
liftBitcast :: (Monad m, ?lc :: LLVMContext)
            => MemType -- ^ Input argument type
            -> a -- ^ Input argument expression.
            -> MemType -- ^ Result argument type
            -> m a
liftBitcast PtrType{} v PtrType{} = return v
liftBitcast itp v rtp | itp `compatMemTypes` rtp = return v
liftBitcast itp _ rtp =
    error $ "Symbolic simulator does not support bitcast between types " ++
           show (ppMemType itp) ++ " and " ++ show (ppMemType rtp)

zeroValue :: (?sbe :: SBE sbe) => MemType -> LiftAttempt (SymValue (SBETerm sbe))
zeroValue tp = mkSValExpr =<< zeroExpr tp

zeroExpr :: (?sbe :: SBE sbe) => MemType -> LiftAttempt (TypedExpr (SymValue (SBETerm sbe)))
zeroExpr tp0 =
  case tp0 of
    IntType w  -> return $ SValInteger (fromIntegral w) 0
    FloatType  -> return $ SValFloat 0
    DoubleType -> return $ SValDouble 0
    PtrType tp -> return $ SValNull tp
    ArrayType n tp -> SValArray tp . V.replicate (fromIntegral n) <$> zeroValue tp
    VecType n tp  -> SValVector tp . V.replicate (fromIntegral n) <$> zeroValue tp
    StructType si -> SValStruct si <$> traverse zeroValue (siFieldTypes si)

liftGEP :: (?lc :: LLVMContext, ?sbe :: SBE sbe)
        => Bool
        -> L.Typed L.Value
        -> [L.Typed L.Value]
        -> LiftAttempt (MemType, TypedExpr (SymValue (SBETerm sbe)))
liftGEP _inbounds (Typed initType0 initValue) args0 = do
     rtp <- liftMemType' initType0
     let fn (L.Typed tp v) = (,v) <$> liftMemType' tp
     expr0 <- mkSValExpr (SValInteger aw 0)
     go expr0 rtp =<< traverse fn args0
  where gepFailure msg = failAttempt $ "Could not parse GEP Value: " ++ msg
        pdl = llvmDataLayout ?lc
        aw :: BitWidth
        aw = ptrBitwidth pdl
        mn = Nothing

        go args tp [] = do
          initType <- liftMemType' initType0
          sv <- liftValue initType initValue
          return (tp, PtrAdd sv args)
        go args (ArrayType _ etp) r = goArray args etp r
        go args (PtrType tp) r = do
          mtp <- maybe (failAttempt "failed to convert pointer type") return $
                 asMemType tp
          goArray args mtp r
        go args (StructType si) r = goStruct args si r
        go _ tp _ = gepFailure $ "go with weird type: " ++ show (ppMemType tp)

        mergeAdd (SValExpr (SValInteger _ 0) _) y = return y
        mergeAdd x (SValExpr (SValInteger _ 0) _) = return x
        mergeAdd (SValExpr (SValInteger _ i) _) (SValExpr (SValInteger _ j) _) =
          mkSValExpr (SValInteger aw (i+j))
        mergeAdd x y = mkSValExpr (IntArith (Add False False) mn aw x y)

        goArray args etp ((IntType w, v0) : r)= do
          v1 <- liftValue (IntType w) v0
          let v2 | aw == w   = return v1
                 | aw >  w   = mkSValExpr $ SExt  mn w v1 aw
                 | otherwise = mkSValExpr $ Trunc mn w v1 aw
          let sz = toInteger $ memTypeSize pdl etp
          sz' <- mkSValExpr $ SValInteger aw sz
          v3 <- mkSValExpr . IntArith (Mul False False)  mn aw sz' =<< v2
          args' <- mergeAdd args v3
          go args' etp r
        goArray _ _ tps = gepFailure $ "goArray with weird types: " ++ show (map (ppMemType . fst) tps)

        goStruct args si  ((IntType 32, L.ValInteger i) : r) = do
          fi <- maybe (failAttempt "failed to get struct field info") return $
                siFieldInfo si (fromIntegral i)
          val <- mkSValExpr (SValInteger aw (toInteger (fiOffset fi)))
          args' <- mergeAdd args val
          go args' (fiType fi) r
        goStruct _ _ tps = gepFailure $ "goStruct with weird types: " ++ show (map (ppMemType . fst) tps)

-- | Lift a maybe alignment constraint to the alignment for the type.
liftAlign :: (?lc :: LLVMContext) => MemType -> Maybe L.Align -> Alignment
liftAlign _ (Just a) | a /= 0 = fromIntegral a
liftAlign tp _ = memTypeAlign (llvmDataLayout ?lc) tp

liftMemType' :: (?lc :: LLVMContext, ?sbe :: SBE sbe)
             => L.Type
             -> LiftAttempt MemType
liftMemType' tp =
  maybe (failAttempt ("failed to lift MemType: " ++ show (L.ppType tp))) return $
  liftMemType tp

liftStmt ::
  (?lc :: LLVMContext, ?sbe :: SBE sbe) =>
  L.Stmt ->
  LiftAttempt (SymStmt (SBETerm sbe))
liftStmt stmt =
  errorStack (show stmt ++ " > ") $
  case stmt of
    Effect (L.Call _ _ (L.ValAsm{}) _) _ ->
      -- TODO: it would be good to emit a warning here, but we can't in
      -- this monad
      return (BadSymStmt stmt)
    Effect (L.Call _b tp v tpvl) _ -> do
      mtp <- liftMemType' tp
      sv <- liftValue mtp v
      svl <- traverse liftArgValue tpvl
      return $ Call sv svl Nothing
    -- We don't care if it's atomic, since the symbolic simulator is
    -- effectively single-threaded
    Effect (L.Store (L.Typed tp0 v) addr _ a) _ -> do
      tp <- liftMemType' tp0
      tptr <- liftValue tp v
      taddr <- liftTypedValue addr
      return $ Store tp tptr taddr (liftAlign tp a)
    Effect{} ->
      failAttempt $ "can't translate effect: " ++ show (L.ppLLVM (L.ppStmt stmt))
    Result _ (L.Call _ _ (L.ValAsm{}) _) _ ->
      -- TODO: it would be good to emit a warning here, but we can't in
      -- this monad
      return (BadSymStmt stmt)
    Result reg (L.Call _b tp v tpvl) _ -> do
      liftMemType' tp >>= \case
        mtp@(PtrType (FunType (fdRetType -> Just rty))) -> do
          sv <- liftValue mtp v
          svl <- traverse liftArgValue tpvl
          return $ Call sv svl (Just (rty, reg))
        _ -> failAttempt $ "Call not to PtrType: " <> show (L.ppType tp)
    Result r app _ -> do
      -- Return an assignemnt statement for the value.
      let retExpr tp v = return $ Assign r tp v
      let retTExpr tp v = Assign r tp <$> mkSValExpr v
      let retIntArith op tp0 u v = do
            tp <- liftMemType' tp0
            x <- liftValue tp u
            y <- liftValue tp v
            case tp of
              IntType w -> retTExpr tp (IntArith op Nothing w x y)
              VecType n (IntType w) -> retTExpr tp (IntArith op (Just n) w x y)
              _ -> failAttempt "Could not parse argument type."
      case app of
        L.Arith llvmOp (L.Typed tp u) v -> do
           op <- case llvmOp of
                   L.Add nuw nsw -> return (Add nuw nsw)
                   L.Sub nuw nsw -> return (Sub nuw nsw)
                   L.Mul nuw nsw -> return (Mul nuw nsw)
                   L.UDiv exact  -> return (UDiv exact)
                   L.SDiv exact  -> return (SDiv exact)
                   L.URem        -> return URem
                   L.SRem        -> return SRem
                   _ -> failAttempt "Floating point operations not supported."
           retIntArith op tp u v
        L.Bit llvmOp (L.Typed tp u) v   -> retIntArith op tp u v
          where op = case llvmOp of
                       L.Shl nuw nsw -> Shl nuw nsw
                       L.Lshr exact -> Lshr exact
                       L.Ashr exact -> Ashr exact
                       L.And -> And
                       L.Or  -> Or
                       L.Xor -> Xor
        L.Conv op (L.Typed itp0 e) rtp0 -> do
          itp <- liftMemType' itp0
          rtp <- liftMemType' rtp0
          sv <- liftValue itp e
          let intConv cond fn =
                case (itp, rtp) of
                  (IntType iw, IntType rw) | cond iw rw ->
                    retTExpr rtp (fn Nothing iw sv rw)
                  (VecType n (IntType iw), VecType nr (IntType rw)) | n == nr && cond iw rw ->
                    retTExpr rtp (fn (Just n) iw sv rw)
                  _ -> failAttempt "Could not parse conversion types."
          case op of
            L.Trunc -> intConv (\iw rw -> iw > rw) Trunc
            L.ZExt -> intConv (\iw rw -> iw < rw) ZExt
            L.SExt -> intConv (\iw rw -> iw < rw) SExt
            L.PtrToInt ->
              case (itp, rtp) of
                (PtrType ptr, IntType w) ->
                  retTExpr rtp (PtrToInt Nothing ptr sv w)
                ( VecType n (PtrType ptr), VecType nr (IntType w)) | n == nr ->
                  retTExpr rtp (PtrToInt (Just n) ptr sv w)
                _ -> failAttempt "Could not parse conversion types."
            L.IntToPtr ->
              case (itp, rtp) of
                ( IntType w, PtrType ptr) ->
                  retTExpr rtp (IntToPtr Nothing w sv ptr)
                ( VecType n (IntType w), VecType nr (PtrType ptr)) | n == nr ->
                  retTExpr rtp (IntToPtr (Just n) w sv ptr)
                _ -> failAttempt "Could not parse conversion types."
            L.BitCast -> do
              retExpr rtp =<< liftBitcast itp sv rtp
            _ -> failAttempt $ "Unsupported conversion operator: " ++ show (L.ppConvOp op)
        L.Alloca tp0 msz a -> do
          tp <- liftMemType' tp0
          ssz <- case msz of
                   Nothing -> return Nothing
                   Just (L.Typed szTp0 sz) ->
                     liftMemType' szTp0 >>= \case
                       IntType w -> do v <- liftValue (IntType w) sz
                                       return (Just (w,v))
                       badTy -> failAttempt $ "Allocation size is not IntType: " <> show (ppMemType badTy)
          return $ Alloca r tp ssz (liftAlign tp a)
        L.Load (L.Typed tp0 ptr) _ malign -> do
          tp <- liftMemType' tp0
          case tp of
            PtrType etp0 -> do
              etp <- maybe (failAttempt "") return $ asMemType etp0
              v <- liftValue tp ptr
              return $ Load r v etp (liftAlign etp malign)
            _ -> failAttempt $ "Unsupported type to load: " ++ show (ppMemType tp)
        L.ICmp op (L.Typed tp0 u) v -> do
          tp <- liftMemType' tp0
          x <- liftValue tp u
          y <- liftValue tp v
          case tp of
            IntType w ->
              retTExpr (IntType 1) (ICmp op Nothing (Left w) x y)
            VecType n (IntType w) ->
              retTExpr (VecType n (IntType 1)) (ICmp op (Just n) (Left w) x y)
            PtrType ptp ->
              retTExpr (IntType 1) (ICmp op Nothing (Right ptp) x y)
            VecType n (PtrType ptp) ->
              retTExpr (VecType n (IntType 1)) (ICmp op (Just n) (Right ptp) x y)

            _ -> failAttempt "Could not parse icmp argument type."
        L.GEP ib tp tpvl     -> uncurry retTExpr =<< liftGEP ib tp tpvl
        L.Select (L.Typed tpc0 c') (L.Typed tpv0 v1') v2' -> do
          tpc <- liftMemType' tpc0
          tpv <- liftMemType' tpv0
          c  <- liftValue tpc c'
          v1 <- liftValue tpv v1'
          v2 <- liftValue tpv v2'
          case (tpc, tpv) of
            (IntType w,_) | w == 1 ->
              retTExpr tpv $ Select Nothing c tpv v1 v2
            (VecType n (IntType w), VecType nr tpe) | w == 1 && n == nr ->
              retTExpr tpv $ Select (Just n) c tpe v1 v2
            _  -> failAttempt "Could not parse select intruction."
        L.ExtractValue (L.Typed vtp v) il -> do
            vmtp <- liftMemType' vtp
            go vmtp il =<< liftValue vmtp v
          where go tp [] sv = retExpr tp sv
                go (StructType si) (i0 : is) sv =
                    case fiType <$> siFieldInfo si i of
                      Nothing -> failAttempt "Illegal index"
                      Just tp -> go tp is =<< mkSValExpr (GetStructField si sv i)
                  where i = fromIntegral i0
                go (ArrayType n tp) (i : is) sv
                    | 0 <= i && i < fromIntegral n = go tp is =<< mkSValExpr expr
                    | otherwise = failAttempt "Illegal index"
                  where expr = GetConstArrayElt (fromIntegral n) tp sv (fromIntegral i)
                go _ _ _ = failAttempt "non-composite type in extractvalue"
        -- TODO: it would be good to issue a warning in the following,
        -- but the monad doesn't currently allow it
        L.InsertValue _ _ _ -> return (BadSymStmt stmt)
        _ -> failAttempt $ "Unsupported instruction: " ++ show (L.ppLLVM (L.ppInstr app))

liftArgValue :: (?lc :: LLVMContext, ?sbe :: SBE sbe)
             => L.Typed L.Value -> LiftAttempt (MemType, SymValue (SBETerm sbe))
liftArgValue (Typed tp val) = do
  mtp <- liftMemType' tp
  (mtp,) <$> liftValue mtp val


-- Lift LLVM basic block to symbolic block {{{1
--
-- Invariants assumed by block:
-- * When jumping from the current block to a new block,
--   * The current block must ensure that the correct post-dominator merge frames are added.
--   * The current block must set the phi value registers.
liftBB :: (?lc::LLVMContext, ?sbe::SBE sbe,
           MonadState (BlockGeneratorState (SBETerm sbe)) m, MonadIO m) =>
          LLVMTranslationInfo
       -> PhiMap (SBETerm sbe)
       -> CFG.BB
       -> m ()
liftBB lti phiMap bb = do
    symBlocks <- impl (fmap (fmap snd) (L.bbStmts bb)) []
    bgBlocks %= (symBlocks ++)
  where llvmId = CFG.blockName bb
        -- Block for post dominator
        pd = flip symBlockID 0 <$> ltiImmediatePostDominator lti llvmId

        blockName :: Int -> SymBlockID
        blockName = symBlockID llvmId

                    {-
        emptyBlock :: L.BlockLabel
                   -> State (Seq.Seq (SymBlock (SBETerm sbe))) SymBlockID
                   -}
        emptyBlock tgt = do
          let mkNewBlock stmts = do
                l <- get
                let n = Seq.length l + 1
                let jumpStmt = Jump (symBlockID tgt 0)
                put $ l Seq.|> mkSymBlock (blockName n) (reverse (jumpStmt : stmts))
                return (blockName n)
          case Map.lookup (llvmId, tgt) phiMap of
            Nothing -> return (symBlockID tgt 0)
            Just (Left stmt) -> mkNewBlock [BadSymStmt stmt]
            Just (Right pairs) -> mkNewBlock ((\(r,tp,v) -> Assign r tp v) <$> pairs)
        -- | Sequentially process statements.
        {-
        impl :: (?lc :: LLVMContext, ?sbe :: SBE sbe)
             => [L.Stmt] -- ^ Remaining statements
             -> [SymStmt (SBETerm sbe)] -- ^ Previously generated statements in reverse order.
             -> BlockGenerator sbe [SymBlock (SBETerm sbe)]
        -}
        impl [] il = liftError $
                         text "Missing terminal instruction in block" <+>
                         text "after generating the following statements:" <$$>
                         (nest 2 . vcat $ ppStmt <$> il)
        impl [stmt@(Effect (L.Ret tpv) _)] il = do
          symStmt <- trySymStmt stmt $ do
            Ret . Just <$> liftTypedValue tpv
          return [ mkSymBlock (blockName 0) (reverse (symStmt:il)) ]

        impl [Effect L.RetVoid _] il =
          return [ mkSymBlock (blockName 0) (reverse (Ret Nothing:il)) ]

        impl [Effect (L.Jump tgt) _] il = do
          let il' = case Map.lookup (llvmId, tgt) phiMap of
                      Nothing -> []
                      Just (Left stmt) -> [BadSymStmt stmt]
                      Just (Right pairs) -> (\(r,tp,v) -> Assign r tp v) <$> pairs
              jumpStmt = Jump (symBlockID tgt 0)
              block = mkSymBlock (blockName 0) (reverse (jumpStmt : (il' ++ il)))
          return [ block ]

        impl [stmt@(Effect (L.Br (L.Typed tp v) tgt1 tgt2) _)] il = do
          mres <- runLiftAttempt $ liftMemType' tp >>= \case
            IntType 1 -> liftValue (IntType 1) v
            badTy -> failAttempt $ "Branch cond is not IntType 1: " <> show (ppMemType badTy)
          case mres of
            Left{} -> do
              ss <- unsupportedStmt stmt "Unparsable condition."
              return [ mkSymBlock (blockName 0) (reverse (ss:il)) ]
            Right tc -> do
              let ((b1,b2), rest) =
                    runState (both emptyBlock (tgt1, tgt2)) Seq.empty
              let branchStmt = Br tc b1 b2 pd
              return $ mkSymBlock (blockName 0) (reverse (branchStmt:il))
                         : F.toList rest
        impl [stmt@(Effect (L.Switch (Typed tp v) def cases) _)] il = do
          mr <- runLiftAttempt $
                liftMemType' tp >>= \case
                  IntType w -> do tsv <- liftValue (IntType w) v
                                  return (w, tsv)
                  badTy -> failAttempt $ "Switch value is not IntType: " <> show (ppMemType badTy)
          case mr of
            Left{} -> do
              symStmt <- unsupportedStmt stmt "Unparsable switch statement."
              return [ mkSymBlock (blockName 0) $ reverse (symStmt:il) ]
            Right (w, tsv) -> return (initBlock : F.toList caseBlocks)
              where (consts, targets) = unzip cases
                    ((caseBlockIds,defBlock), caseBlocks) = flip runState Seq.empty $ do
                      (,) <$> traverse emptyBlock targets <*> emptyBlock def
                    symCases = Map.fromList $ zip consts caseBlockIds
                    switchStmt = Switch w tsv defBlock symCases pd
                    initBlock  = mkSymBlock (blockName 0) (reverse (switchStmt:il))

        impl [Effect L.Unreachable _] il = do
          return [ mkSymBlock (blockName 0) (reverse (Unreachable : il)) ]

        impl [stmt@(Effect L.Unwind _)] il = do
          ss <- unsupportedStmt stmt ""
          return [ mkSymBlock (blockName 0) (reverse (ss : il)) ]

        impl [stmt@(Effect (L.Resume _) _)] il = do
          ss <- unsupportedStmt stmt ""
          return [ mkSymBlock (blockName 0) (reverse (ss : il)) ]

        -- Treat @invoke@ like @call@ followed by @jmp@. TODO: let's
        -- support unwinding eventually.
        impl [Effect (L.Invoke fn args res next _unwind) mds] il = do
          addWarning "Treating `invoke` instruction as a `call`"
          impl [ Effect (L.Call False fn args res) mds
               , Effect (L.Jump next) []
               ] il

        impl [Result reg (L.Invoke fn args res next _unwind) mds] il = do
          addWarning "Treating `invoke` instruction as a `call`"
          impl [ Result reg (L.Call False fn args res) mds
               , Effect (L.Jump next) []
               ] il

        -- Skip certain intrinsics
        impl (Effect (L.Call _ _ (L.ValSymbol v) _) _:r) il
          | v `elem` [ "llvm.dbg.declare", "llvm.dbg.value"]
          = impl r il

        -- Skip landing pads. TODO: implement fully.
        impl (Result _ (L.LandingPad _ _ _ _) _:r) il = do
          addWarning "Ignoring `landingpad` instruction"
          impl r il

        -- Phi statements are handled by initial blocks.
        impl (Result _ L.Phi{} _:r) il = impl r il
        impl (Effect (L.Comment _) _:r) il = impl r il
        impl (stmt:rest) il = do
          s' <- trySymStmt stmt $ liftStmt stmt
          impl rest (s' : il)

-- Lift LLVM definition to symbolic definition {{{1
{-
liftDefine :: forall sbe . (?lc :: LLVMContext, ?sbe :: SBE sbe)
           => L.Define
           -> IO (Either Doc ([TranslationWarning], SymDefine (SBETerm sbe)))
-}
liftDefine d
    | L.defVarArgs d =
       return $ Left (text "Unsupported var args function" <+> symd <> char '.')
    | otherwise =
       case mfd of
         Just (FunDecl rtp args _) -> do
              phiMap <- blockPhiMap' blocks
              (warnings,symBlocks) <- runBlockGenerator (mapM_ (liftBB lti phiMap) blocks)
              let sd = SymDefine { sdName = L.defName d
                                 , sdArgs = zip (L.typedValue <$> L.defArgs d) args
                                 , sdRetType = rtp
                                 , sdEntry = symBlockID initBlockLabel 0
                                 , sdBody = Map.fromList
                                              [ (sbId b,b) | b <- symBlocks ]
                                 }
              return $ Right (warnings, sd)
            where cfg            = CFG.buildCFG (L.defBody d)
                  lti            = mkLTI cfg
                  blocks         = ltiBlocks lti
                  initBlock      = CFG.bbById cfg (CFG.entryId cfg)
                  initBlockLabel = CFG.blockName initBlock
         Nothing -> return $ Left (text "Unsupported type for function" <+> symd <> char '.')
  where mfd = FunDecl <$> liftRetType (L.defRetType d)
                      <*> traverse liftMemType (L.typedType <$> L.defArgs d)
                      <*> pure False
        symd = ppSymbol (L.defName d)
