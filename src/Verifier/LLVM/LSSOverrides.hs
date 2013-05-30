{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Verifier.LLVM.LSSOverrides
  ( registerLSSOverrides
  ) where

import Control.Applicative
import qualified Control.Exception         as CE
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Class
import qualified Data.Map                  as M
import Data.String
import System.IO
import Text.PrettyPrint.Leijen (nest)

import Verifier.LLVM.AST
import Verifier.LLVM.Backend
import Verifier.LLVM.Codebase
import Verifier.LLVM.Simulator.Internals
import Verifier.LLVM.Simulator.SimUtils

-- | Attempts to read an array of boolean values from a pointer with the given number
-- of elements.
getEvalInputs :: (Functor m, MonadIO m, Functor sbe)
               => String -- ^ Name of function calling this for error purposes.
              -> SBETerm sbe -- ^ Pointer to input values (should be an i8p).
              -> SBETerm sbe -- ^ Size of array of inputs (should be an int32).
              -> Simulator sbe m [Bool]
getEvalInputs nm p sz = do
  sbe <- gets symBE
  case asUnsignedInteger sbe 32 sz of
    Just csz -> do
      a <- load (ArrayType (fromInteger csz) i8) p 0
      elems <- readArrayElements (fromInteger csz) i8 a 
      case traverse (asUnsignedInteger sbe 8) elems of
        Just ints -> return $ (/= 0) <$> ints
        Nothing -> errorPath $ nm ++ ": symbolc inputs not supported."
    Nothing -> errorPath $ nm ++ ": symbolic size not supported."

-- | Return list of elements in an array.
readArrayElements ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => Int         -- ^ Count
  -> MemType     -- ^ Element type
  -> SBETerm sbe -- ^ Array Value
  -> Simulator sbe m [SBETerm sbe]
readArrayElements c tp a = go c []
  where go 0 r = return r
        go i r = do
          sbe <- gets symBE
          v <- liftSBE $ applyTypedExpr sbe (GetConstArrayElt c tp a (i-1))
          go (i-1) (v:r)

checkAigFile ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => String -> Simulator sbe m ()
checkAigFile filename = do
  eab <- liftIO $ CE.try (openFile filename WriteMode)
  case eab of
    Left (e :: CE.SomeException) -> errorPath $ "checkAigFile: " ++ show e
    Right h                       -> liftIO $ hClose h


overrideEntry :: Functor m
              => Symbol
              -> MemType
              -> [MemType]
              -> ([(MemType, SBETerm sbe)] -> Simulator sbe m (SBETerm sbe))
              -> OverrideEntry sbe m
overrideEntry nm rtp tps h = (nm, funDecl rtp tps, override h)

voidOverrideEntry :: Functor m
                  => Symbol
                  -> [MemType]
                  -> VoidOverrideHandler sbe m
                  -> OverrideEntry sbe m
voidOverrideEntry nm tps h = (nm, voidFunDecl tps, voidOverride h)

type StdOvdEntry sbe m =
  ( Functor sbe
  , Functor m
  , MonadIO m
  )
  => OverrideEntry sbe m

overrideByName :: StdOvdEntry sbe m
overrideByName = do
  voidOverrideEntry "lss_override_function_by_name" [strTy, strTy] $ \args ->
    case args of
      [(PtrType{}, fromNamePtr), (PtrType{}, toNamePtr)] -> do
        src <- fromString <$> loadString "lss_override_function_by_name from" fromNamePtr
        tgt <- fromString <$> loadString "lss_override_function_by_name to" toNamePtr
        src `userRedirectTo` tgt
      _ -> wrongArguments "lss_override_function_by_name"

overrideByAddr :: StdOvdEntry sbe m
overrideByAddr = do
  voidOverrideEntry "lss_override_function_by_addr" [strTy, strTy] $ \args ->
    case args of
      [(PtrType{}, fromPtr), (PtrType{}, toPtr)] -> do
        syms <- both resolveFunPtrTerm (fromPtr, toPtr)
        case syms of
          (LookupResult src, LookupResult tgt) -> src `userRedirectTo` tgt
          _ -> errorPath "overrideByAddr: Failed to resolve function pointer"
      _ -> wrongArguments "lss_override_function_by_addr"

overrideIntrinsic :: StdOvdEntry sbe m
overrideIntrinsic = do
  voidOverrideEntry "lss_override_llvm_intrinsic" [strTy, strTy] $ \args ->
    case args of
      [(PtrType{}, nmPtr), (PtrType{}, fp)] -> do
        nm  <- fromString <$> loadString "lss_override_llvm_intrinsic" nmPtr
        msym <- resolveFunPtrTerm fp
        case msym of
          LookupResult sym -> nm `userRedirectTo` sym
          _ -> errorPath "overrideIntrinsic: Failed to resolve function pointer"
      _ -> wrongArguments "lss_override_llvm_intrinsic"


overrideResetByAddr :: StdOvdEntry sbe m
overrideResetByAddr =
  voidOverrideEntry "lss_override_reset_by_addr" [strTy] $ \args ->
    case args of
      [(PtrType{},fp)] -> do
        msym <- resolveFunPtrTerm fp
        case msym of
          LookupResult sym -> sym `userRedirectTo` sym
          _ -> errorPath "overrideResetByAddr: Failed to resolve function pointer"
      _ -> wrongArguments "lss_override_reset_by_addr"

overrideResetByName :: StdOvdEntry sbe m
overrideResetByName = 
  voidOverrideEntry "lss_override_reset_by_name" [strTy] $ \args ->
    case args of
      [(PtrType{}, fnNamePtr)] -> do
        fnSym <- fromString <$> loadString "lss_override_reset_by_name" fnNamePtr
        fnSym `userRedirectTo` fnSym
      _ -> wrongArguments "lss_override_reset_by_name"

-- TODO (?): We may want to avoid retraction of overridden intrinsics, since
-- presumably the user always wants the overridden version.
overrideResetAll :: StdOvdEntry sbe m
overrideResetAll =
  voidOverrideEntry "lss_override_reset_all" [] $ \_ -> do
    ovds <- use fnOverrides
    forM_ (M.assocs ovds) $ \(sym, (_, userOvd)) ->
      when userOvd $ do
        fnOverrides . at sym .= Nothing

userRedirectTo :: MonadIO m => Symbol -> Symbol -> Simulator sbe m ()
userRedirectTo src tgt = do
  cb <- gets codebase
  let nameOf = show . ppSymbol 
  --TODO: Add better error messages.
  case (cb^.cbFunctionType src, cb^.cbFunctionType tgt) of
    (Nothing,_) -> error $ "Could not find symbol " ++ nameOf src ++ "."
    (_,Nothing) -> error $ "Could not find symbol " ++ nameOf tgt ++ "."  
    (Just fd, Just td) -> do
      checkTypeCompat src fd (show (ppSymbol tgt)) td
      fnOverrides . at src ?= (Redirect tgt, True)

printSymbolic :: StdOvdEntry sbe m
printSymbolic = do
  voidOverrideEntry "lss_print_symbolic" [i8p] $ \args ->
    case args of
      [(_,ptr)] -> do
        v <- load i8 ptr 0
        d <- withSBE' $ \sbe -> prettyTermD sbe v
        liftIO $ print d
      _ -> wrongArguments "lss_print_symbolic"


abortHandler :: StdOvdEntry sbe m
abortHandler = voidOverrideEntry "lss_abort" [strTy] $ \args ->
  case args of
    [(_,tv)] -> do
      msg <- loadString "abort message" tv
      errorPath $ "lss_abort(): " ++ msg
    _ -> errorPath "Incorrect number of parameters passed to lss_abort()"

showMemOverride :: StdOvdEntry sbe m
showMemOverride = voidOverrideEntry "lss_show_mem" [] $ \_ -> do
  unlessQuiet $ dumpMem 1 "lss_show_mem()"

showPathOverride :: StdOvdEntry sbe m
showPathOverride = voidOverrideEntry "lss_show_path" [] $ \_ -> do
  Just p <- preuse currentPathOfState
  sbe <- gets symBE
  unlessQuiet $ dbugM $ show $ nest 2 $ ppPath sbe p

userSetVerbosityOverride :: StdOvdEntry sbe m
userSetVerbosityOverride = voidOverrideEntry "lss_set_verbosity" [i32] $ \args ->
  case args of
    [(_,v)] -> do
      sbe <- gets symBE
      case asUnsignedInteger sbe 32 v of
        Nothing  -> errorPath "symbolic verbosity is illegal"
        Just v'' -> setVerbosity (fromIntegral v'')
    _ -> errorPath "Incorrect number of parameters passed to lss_set_verbosity"

freshInt' :: BitWidth -> StdOvdEntry sbe m
freshInt' n = do
  let nm = fromString $ "lss_fresh_uint" ++ show n
      itp = IntType n
  overrideEntry nm itp [itp] $ \_ -> withSBE (flip freshInt n)

-- | @freshIntArray n@ returns an override that yields an array of integers,
-- each with with @n@ bits.
freshIntArray :: BitWidth -> StdOvdEntry sbe m
freshIntArray n = do
  let nm = fromString $ "lss_fresh_array_uint" ++ show n
      itp = IntType n
      ptp = PtrType (MemType itp)
  overrideEntry nm ptp [i32, itp, ptp] $ \args ->
    case args of
      [(_, sizeTm), _, _] -> do
        sbe <- gets symBE
        case asUnsignedInteger sbe 32 sizeTm of
          Just size -> do
            let sz = fromIntegral size
                ty = ArrayType (fromIntegral size) itp
            arrPtr <- alloca itp 32 sizeTm 0
            elts <- replicateM sz (withSBE $ flip freshInt n)
            arrTm <- liftSBE $ termArray sbe itp elts
            arrPtr <$ store ty arrTm arrPtr 0
          Nothing -> errorPath "lss_fresh_array_uint called with symbolic size"
      _ -> wrongArguments "lss_fresh_array_uint"

addAigOutput :: BitWidth -> StdOvdEntry sbe m
addAigOutput n = do
  let nm = fromString $ "lss_aiger_add_output_uint" ++ show n
  let tp = IntType n
  voidOverrideEntry nm [tp] $ \args ->
    case args of
      [(_,t)] -> aigOutputs %= ((tp,t):)
      _   -> wrongArguments (show nm)

addAigArrayOutput :: BitWidth -- ^ Width of array that target points to.
                  -> StdOvdEntry sbe m
addAigArrayOutput n = do
  let nm = fromString $ "lss_aiger_add_output_array_uint" ++ show n
  let itp = IntType n
  let ptp = PtrType (MemType itp)
  voidOverrideEntry nm [ptp, i32] $ \args ->
    case args of
      [(_,tptr), (_, sizeTm)] -> do
        sbe <- gets symBE
        case asUnsignedInteger sbe 32 sizeTm of
          Just size -> do
            let tp = ArrayType (fromInteger size) itp
            arrTm <- load tp tptr 0
            aigOutputs %= ((tp,arrTm):)
          Nothing -> errorPath "lss_aiger_add_output_array called with symbolic size"
      _ -> wrongArguments "lss_aiger_add_output_array"

writeIntAiger :: BitWidth -> StdOvdEntry sbe m
writeIntAiger n = do
  let nm = fromString $ "lss_write_aiger_uint" ++ show n
  let itp = IntType n
  voidOverrideEntry nm [itp, strTy] $ \args ->
    case args of
      [(_,t), (_,fptr)] -> do
        file <- loadString "lss_write_aiger_uint file" fptr
        checkAigFile file
        sbe <- gets symBE
        liftSBE (writeAiger sbe file [(itp,t)])
      _ -> wrongArguments "lss_write_aiger_uint"

writeIntArrayAiger :: BitWidth -> StdOvdEntry sbe m
writeIntArrayAiger n = do
  let nm = fromString $ "lss_write_aiger_array_uint" ++ show n
  let itp = IntType n
  let ptp = PtrType (MemType itp)
  voidOverrideEntry nm [ptp, i32, strTy] $ \args ->
    case args of
      [(_, tptr), (_, sizeTm), (_, fptr)] -> do
        sbe <- gets symBE
        case asUnsignedInteger sbe 32 sizeTm of
          Just size -> do
            file <- loadString "lss_write_aiger_array_uint" fptr
            checkAigFile file
            let tp = ArrayType (fromInteger size) itp
            arrTm <- load tp tptr 0
            liftSBE $ writeAiger sbe file [(tp,arrTm)]
          Nothing ->
            errorPath "lss_write_aiger_array_uint called with symbolic size"
      _ -> wrongArguments "lss_write_aiger_array_uint"

writeCollectedAigerOutputs :: StdOvdEntry sbe m
writeCollectedAigerOutputs =
  voidOverrideEntry "lss_write_aiger" [strTy] $ \args ->
    case args of
    [(_,fptr)] -> do
      outputTerms <- reverse <$> use aigOutputs
      if null outputTerms then
        errorPath "lss_write_aiger: no AIG outputs have been collected"
      else do
        file <- loadString "lss_write_aiger file" fptr
        withSBE $ \s -> writeAiger s file outputTerms
        aigOutputs .= []
    _ -> wrongArguments "lss_write_aiger"

-- | Eval aiger override the argument is the type of the first argument.
evalAigerOverride :: BitWidth -> StdOvdEntry m sbe
evalAigerOverride n = do
  let nm = fromString $ "lss_eval_aiger_uint" ++ show n
  let itp = IntType n
  overrideEntry nm itp [itp, i8p, i32] $ \args ->
    case args of
      [(_,tm), (_, p), (_,szTm)] -> do
        bools <- getEvalInputs "lss_eval_aiger" p szTm
        sbe <- gets symBE
        liftSBE $ evalAiger sbe bools itp tm
      _ -> wrongArguments "lss_eval_aiger"

evalAigerArray :: BitWidth -- ^ Width of array elements.
               -> StdOvdEntry sbe m
evalAigerArray n = do
  let nm = fromString $ "lss_eval_aiger_array_uint" ++ show n
  let itp = IntType n
  let ptp = PtrType (MemType itp)
  voidOverrideEntry nm [ptp, ptp, i32, i8p, i32] $ \args ->
    case args of
      [ (_, sym)
       ,(_, dst)
       ,(_, szTm)
       ,(_,  input)
       ,(_, inputSz)
       ] -> do
        sbe <- gets symBE
        case asUnsignedInteger sbe 32 szTm of
          Just sz -> do
            bools <- getEvalInputs "lss_eval_aiger_array" input inputSz
            let tp = ArrayType (fromInteger sz) itp
            tm <- load tp sym 0
            res <- liftSBE $ evalAiger sbe bools tp tm
            store tp dst res 0
          _ -> errorPath "lss_eval_aiger_array: symbolic sizes not supported"
      _ -> wrongArguments "lss_eval_aiger_array"


-- CNF {{{

writeCNF :: StdOvdEntry sbe m
writeCNF =
  voidOverrideEntry "lss_write_cnf" [i32, strTy] $ \args ->
    case args of
      [(IntType w, t), (PtrType{},fptr)] | w == 32 -> do
        file <- loadString "lss_write_cnf" fptr
        void $ withSBE $ \s -> writeCnf s file 32 t
      _ -> wrongArguments "lss_write_cnf"

-- SMTLIB2 {{{

ptrToSMTLIB2 :: MemType
ptrToSMTLIB2 = PtrType VoidType -- (UnsupportedType L.Opague)

lss_SMTLIB2_create :: StdOvdEntry sbe m
lss_SMTLIB2_create =
  overrideEntry "lss_SMTLIB2_create" ptrToSMTLIB2 [strTy] $ \args -> do
    let [path] = args
    undefined path

lss_SMTLIB2_assert_nonzero_uint8 :: StdOvdEntry sbe m
lss_SMTLIB2_assert_nonzero_uint8 =
  voidOverrideEntry "lss_SMTLIB2_assert_nonzero_uint8" [ptrToSMTLIB2, i8] $ \args ->
    undefined args
 
lss_SMTLIB2_check_sat :: StdOvdEntry sbe m
lss_SMTLIB2_check_sat =
  voidOverrideEntry "lss_SMTLIB2_check_sat" [ptrToSMTLIB2] $ \args ->
    undefined args

lss_SMTLIB2_close :: StdOvdEntry sbe m
lss_SMTLIB2_close =
  voidOverrideEntry "lss_SMTLIB2_close" [ptrToSMTLIB2] $ \args ->
    undefined args

registerLSSOverrides :: (Functor m, MonadIO m, Functor sbe) => Simulator sbe m ()
registerLSSOverrides = do
  let groundOverrides =
        [ abortHandler
        , printSymbolic
        , overrideByAddr
        , overrideByName
        , overrideIntrinsic
        , overrideResetByAddr
        , overrideResetByName
        , overrideResetAll
        , showPathOverride
        , showMemOverride
        , userSetVerbosityOverride
        , writeCollectedAigerOutputs
        , writeCNF
          -- * SMTLIB2 functions
        , lss_SMTLIB2_create
        , lss_SMTLIB2_assert_nonzero_uint8
        , lss_SMTLIB2_check_sat
        , lss_SMTLIB2_close
        ]
      polyOverrides =
        [ freshInt'
        , freshIntArray
        , addAigOutput
        , addAigArrayOutput
        , writeIntAiger
        , writeIntArrayAiger
        , evalAigerOverride
        , evalAigerArray
        ]
      polySizes = [8, 16, 32, 64]
  registerOverrides $
      groundOverrides
      ++ [ f i | f <- polyOverrides, i <- polySizes ]

