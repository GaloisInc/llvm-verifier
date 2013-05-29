{-# LANGUAGE DoAndIfThenElse #-}
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

-- | Eval aiger override the argument is the type of the first argument.
evalAigerOverride :: MemType -> StdOvd m sbe
evalAigerOverride tp =
  override $ \args ->
    case args of
      [(_,tm), (PtrType{}, p), (IntType 32,szTm)] -> do
        bools <- getEvalInputs "lss_eval_aiger" p szTm
        sbe <- gets symBE
        liftSBE $ evalAiger sbe bools tp tm
      _ -> wrongArguments "lss_eval_aiger"

evalAigerArray :: MemType -- Type of first argument pointer.
               -> StdOvd sbe m
evalAigerArray ty = voidOverride $ \args ->
    case args of
      [ (PtrType{}, sym)
       ,(PtrType{}, dst)
       ,(IntType szw, szTm)
       ,(PtrType{},  input)
       ,(IntType 32, inputSz)
       ] -> do
        sbe <- gets symBE
        case asUnsignedInteger sbe szw szTm of
          Just sz -> do
            bools <- getEvalInputs "lss_eval_aiger_array" input inputSz
            let tp = ArrayType (fromInteger sz) ty
            tm <- load tp sym 0
            res <- liftSBE $ evalAiger sbe bools tp tm
            store tp dst res 0
          _ -> errorPath "lss_eval_aiger_array: symbolic sizes not supported"
      _ -> wrongArguments "lss_eval_aiger_array"


overrideByName :: StdOvd sbe m
overrideByName = voidOverride $ \args ->
  case args of
    [(PtrType{}, fromNamePtr), (PtrType{}, toNamePtr)] -> do
      src <- fromString <$> loadString "lss_override_function_by_name from" fromNamePtr
      tgt <- fromString <$> loadString "lss_override_function_by_name to" toNamePtr
      src `userRedirectTo` tgt
    _ -> wrongArguments "lss_override_function_by_name"

overrideByAddr :: StdOvd sbe m
overrideByAddr = voidOverride $ \args ->
  case args of
    [(PtrType{}, fromPtr), (PtrType{}, toPtr)] -> do
      syms <- both resolveFunPtrTerm (fromPtr, toPtr)
      case syms of
        (LookupResult src, LookupResult tgt) -> src `userRedirectTo` tgt
        _ -> errorPath "overrideByAddr: Failed to resolve function pointer"
    _ -> wrongArguments "lss_override_function_by_addr"

overrideIntrinsic :: StdOvd sbe m
overrideIntrinsic = voidOverride $ \args ->
  case args of
    [(PtrType{}, nmPtr), (PtrType{}, fp)] -> do
      nm  <- fromString <$> loadString "lss_override_llvm_intrinsic" nmPtr
      msym <- resolveFunPtrTerm fp
      case msym of
        LookupResult sym -> nm `userRedirectTo` sym
        _ -> errorPath "overrideIntrinsic: Failed to resolve function pointer"
    _ -> wrongArguments "lss_override_llvm_intrinsic"


overrideResetByAddr :: StdOvd sbe m
overrideResetByAddr = voidOverride $ \args ->
  case args of
    [(PtrType{},fp)] -> do
      msym <- resolveFunPtrTerm fp
      case msym of
        LookupResult sym -> sym `userRedirectTo` sym
        _ -> errorPath "overrideResetByAddr: Failed to resolve function pointer"
    _ -> wrongArguments "lss_override_reset_by_addr"

overrideResetByName :: StdOvd sbe m
overrideResetByName = voidOverride $ \args ->
  case args of
    [(PtrType{}, fnNamePtr)] -> do
      fnSym <- fromString <$> loadString "lss_override_reset_by_name" fnNamePtr
      fnSym `userRedirectTo` fnSym
    _ -> wrongArguments "lss_override_reset_by_name"

-- TODO (?): We may want to avoid retraction of overridden intrinsics, since
-- presumably the user always wants the overridden version.
overrideResetAll :: StdOvd sbe m
overrideResetAll = voidOverride $ \args ->
  case args of
    [] -> do ovds <- use fnOverrides
             forM_ (M.assocs ovds) $ \(sym, (_, userOvd)) ->
               when userOvd $ do
                 fnOverrides . at sym .= Nothing
    _ -> wrongArguments "lss_override_reset_all"

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

printSymbolic :: StdOvd sbe m
printSymbolic = voidOverride $ \args ->
  case args of
    [(_,ptr)] -> do
      v <- load i8 ptr 0
      d <- withSBE' $ \sbe -> prettyTermD sbe v
      liftIO $ print d
    _ -> wrongArguments "lss_print_symbolic"

abortHandler :: StdOvd sbe m
abortHandler = voidOverride $ \args -> do
  case args of
    [(_,tv)] -> do
      msg <- loadString "abort message" tv
      errorPath $ "lss_abort(): " ++ msg
    _ -> errorPath "Incorrect number of parameters passed to lss_abort()"

showPathOverride :: StdOvd sbe m
showPathOverride = voidOverride $ \_args -> do
  Just p <- preuse currentPathOfState
  sbe <- gets symBE
  unlessQuiet $ dbugM $ show $ nest 2 $ ppPath sbe p

showMemOverride :: StdOvd sbe m
showMemOverride = voidOverride $ \_args -> do
  unlessQuiet $ dumpMem 1 "lss_show_mem()"

userSetVerbosityOverride :: StdOvd sbe m
userSetVerbosityOverride = voidOverride $ \args ->
  case args of
    [(_,v)] -> do
      sbe <- gets symBE
      case asUnsignedInteger sbe 32 v of
        Nothing  -> errorPath "symbolic verbosity is illegal"
        Just v'' -> setVerbosity (fromIntegral v'')
    _ -> errorPath "Incorrect number of parameters passed to lss_set_verbosity"

freshInt' :: Int -> StdOvd sbe m
freshInt' n = override $ \_ -> withSBE (flip freshInt n)

-- | @freshIntArray n@ returns an override that yields an array of integers,
-- each with with @n@ bits.
freshIntArray :: Int -> StdOvd sbe m
freshIntArray n = override $ \args ->
  case args of
    [(_, sizeTm), _, _] -> do
      sbe <- gets symBE
      case asUnsignedInteger sbe 32 sizeTm of
        Just size -> do
          let sz = fromIntegral size
              ety = IntType n
              ty = ArrayType (fromIntegral size) ety
          arrPtr <- alloca ety 32 sizeTm 0
          elts <- replicateM sz (withSBE $ flip freshInt n)
          arrTm <- liftSBE $ termArray sbe ety elts
          arrPtr <$ store ty arrTm arrPtr 0
        Nothing -> errorPath "lss_fresh_array_uint called with symbolic size"
    _ -> wrongArguments "lss_fresh_array_uint"

addAigOutput :: MemType -> StdOvd sbe m
addAigOutput tp = voidOverride $ \args ->
  case args of
    [(_,t)] -> aigOutputs %= ((tp,t):)
    _   -> wrongArguments "lss_aiger_add_output"

addAigArrayOutput :: MemType -- ^ Type of value target points to.
                  -> StdOvd sbe m
addAigArrayOutput tgtTy = voidOverride $ \args ->
  case args of
    [(_,tptr), (_, sizeTm)] -> do
      sbe <- gets symBE
      case asUnsignedInteger sbe 32 sizeTm of
        Just size -> do
          let tp = ArrayType (fromInteger size) tgtTy
          arrTm <- load tp tptr 0
          aigOutputs %= ((tp,arrTm):)
        Nothing -> errorPath "lss_aiger_add_output_array called with symbolic size"
    _ -> wrongArguments "lss_aiger_add_output_array"

writeCNF :: StdOvd sbe m
writeCNF = voidOverride $ \args ->
  case args of
    [(IntType w, t), (PtrType{},fptr)] | w == 32 -> do
      file <- loadString "lss_write_cnf" fptr
      void $ withSBE $ \s -> writeCnf s file 32 t
    _ -> wrongArguments "lss_write_cnf"

writeIntAiger :: MemType -> StdOvd sbe m
writeIntAiger itp = voidOverride $ \args ->
  case args of
    [(_,t), (_,fptr)] -> do
      file <- loadString "lss_write_aiger_uint file" fptr
      checkAigFile file
      sbe <- gets symBE
      liftSBE (writeAiger sbe file [(itp,t)])
    _ -> wrongArguments "lss_write_aiger_uint"

writeIntArrayAiger :: MemType -> StdOvd sbe m
writeIntArrayAiger ety = voidOverride $ \args ->
  case args of
    [(PtrType{}, tptr), (IntType sizeW, sizeTm), (PtrType{},fptr)] -> do
      sbe <- gets symBE
      case asUnsignedInteger sbe sizeW sizeTm of
        Just size -> do
          file <- loadString "lss_write_aiger_array_uint" fptr
          checkAigFile file
          let tp = ArrayType (fromInteger size) ety
          arrTm <- load tp tptr 0
          liftSBE $ writeAiger sbe file [(tp,arrTm)]
        Nothing ->
          errorPath "lss_write_aiger_array_uint called with symbolic size"
    _ -> wrongArguments "lss_write_aiger_array_uint"

writeCollectedAigerOutputs :: StdOvd sbe m
writeCollectedAigerOutputs = voidOverride $ \args ->
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

registerLSSOverrides :: (Functor m, MonadIO m, Functor sbe) => Simulator sbe m ()
registerLSSOverrides = registerOverrides
  [ ("lss_abort",          voidFunDecl [strTy], abortHandler)
  , ("lss_print_symbolic", voidFunDecl [i8p],   printSymbolic)
  , ("lss_fresh_uint8",    funDecl  i8 [ i8],   freshInt'  8)
  , ("lss_fresh_uint16",   funDecl i16 [i16],   freshInt' 16)
  , ("lss_fresh_uint32",   funDecl i32 [i32],   freshInt' 32)
  , ("lss_fresh_uint64",   funDecl i64 [i64],   freshInt' 64)
  , ("lss_fresh_array_uint8",  funDecl  i8p [i32,  i8,  i8p], freshIntArray 8)
  , ("lss_fresh_array_uint16", funDecl i16p [i32, i16, i16p], freshIntArray 16)
  , ("lss_fresh_array_uint32", funDecl i32p [i32, i32, i32p], freshIntArray 32)
  , ("lss_fresh_array_uint64", funDecl i64p [i32, i64, i64p], freshIntArray 64)
  , ("lss_aiger_add_output_uint8",  voidFunDecl [ i8], addAigOutput i8)
  , ("lss_aiger_add_output_uint16", voidFunDecl [i16], addAigOutput i16)
  , ("lss_aiger_add_output_uint32", voidFunDecl [i32], addAigOutput i32) 
  , ("lss_aiger_add_output_uint64", voidFunDecl [i64], addAigOutput i64)
  , ("lss_aiger_add_output_array_uint8" , voidFunDecl [ i8p, i32], addAigArrayOutput i8)
  , ("lss_aiger_add_output_array_uint16", voidFunDecl [i16p, i32], addAigArrayOutput i16)
  , ("lss_aiger_add_output_array_uint32", voidFunDecl [i32p, i32], addAigArrayOutput i32)
  , ("lss_aiger_add_output_array_uint64", voidFunDecl [i64p, i32], addAigArrayOutput i64)
  , ("lss_write_aiger",        voidFunDecl [strTy], writeCollectedAigerOutputs)
  , ("lss_write_aiger_uint8",  voidFunDecl [ i8, strTy], writeIntAiger  i8)
  , ("lss_write_aiger_uint16", voidFunDecl [i16, strTy], writeIntAiger i16)
  , ("lss_write_aiger_uint32", voidFunDecl [i32, strTy], writeIntAiger i32)
  , ("lss_write_aiger_uint64", voidFunDecl [i64, strTy], writeIntAiger i64)
  , ("lss_write_aiger_array_uint8",  voidFunDecl [i8p,  i32, strTy], writeIntArrayAiger i8)
  , ("lss_write_aiger_array_uint16", voidFunDecl [i16p, i32, strTy], writeIntArrayAiger i16)
  , ("lss_write_aiger_array_uint32", voidFunDecl [i32p, i32, strTy], writeIntArrayAiger i32)
  , ("lss_write_aiger_array_uint64", voidFunDecl [i64p, i32, strTy], writeIntArrayAiger i64)
  , ("lss_eval_aiger_uint8",  funDecl  i8 [ i8, i8p, i32], evalAigerOverride  i8)
  , ("lss_eval_aiger_uint16", funDecl i16 [i16, i8p, i32], evalAigerOverride i16)
  , ("lss_eval_aiger_uint32", funDecl i32 [i32, i8p, i32], evalAigerOverride i32)
  , ("lss_eval_aiger_uint64", funDecl i64 [i64, i8p, i32], evalAigerOverride i64)
  , ("lss_eval_aiger_array_uint8",  voidFunDecl [i8p,  i8p,  i32, i8p, i32], evalAigerArray i8)
  , ("lss_eval_aiger_array_uint16", voidFunDecl [i16p, i16p, i32, i8p, i32], evalAigerArray i16)
  , ("lss_eval_aiger_array_uint32", voidFunDecl [i32p, i32p, i32, i8p, i32], evalAigerArray i32)
  , ("lss_eval_aiger_array_uint64", voidFunDecl [i64p, i64p, i32, i8p, i32], evalAigerArray i64)
  , ("lss_write_cnf", voidFunDecl [i32, strTy], writeCNF)
  , ("lss_override_function_by_name", voidFunDecl [strTy, strTy], overrideByName)
  , ("lss_override_function_by_addr", voidFunDecl [strTy, strTy], overrideByAddr)
  , ("lss_override_llvm_intrinsic",   voidFunDecl [strTy, strTy], overrideIntrinsic)
  , ("lss_override_reset_by_name",    voidFunDecl [strTy], overrideResetByName)
  , ("lss_override_reset_by_addr",    voidFunDecl [strTy], overrideResetByAddr)
  , ("lss_override_reset_all", voidFunDecl [], overrideResetAll)
  , ("lss_show_path", voidFunDecl [], showPathOverride)
  , ("lss_show_mem",  voidFunDecl [], showMemOverride)
  , ("lss_set_verbosity", voidFunDecl [i32], userSetVerbosityOverride)
  ]