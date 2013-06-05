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
import Data.IORef
import Data.Map (Map)
import qualified Data.Map                  as Map
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

lss_override_function_by_name :: StdOvdEntry sbe m
lss_override_function_by_name = do
  let nm = "lss_override_function_by_name"
  voidOverrideEntry (fromString nm) [strTy, strTy] $ \args ->
    case args of
      [(_, fromNamePtr), (_, toNamePtr)] -> do
        src <- fromString <$> loadString (nm ++ " from") fromNamePtr
        tgt <- fromString <$> loadString (nm ++ " to") toNamePtr
        src `userRedirectTo` tgt
      _ -> wrongArguments nm

lss_override_function_by_addr :: StdOvdEntry sbe m
lss_override_function_by_addr = do
  voidOverrideEntry "lss_override_function_by_addr" [strTy, strTy] $ \args ->
    case args of
      [(PtrType{}, fromPtr), (PtrType{}, toPtr)] -> do
        syms <- both resolveFunPtrTerm (fromPtr, toPtr)
        case syms of
          (LookupResult src, LookupResult tgt) -> src `userRedirectTo` tgt
          _ -> errorPath "lss_override_function_by_addr: Failed to resolve function pointer"
      _ -> wrongArguments "lss_override_function_by_addr"

lss_override_llvm_intrinsic :: StdOvdEntry sbe m
lss_override_llvm_intrinsic = do
  voidOverrideEntry "lss_override_llvm_intrinsic" [strTy, strTy] $ \args ->
    case args of
      [(PtrType{}, nmPtr), (PtrType{}, fp)] -> do
        nm  <- fromString <$> loadString "lss_override_llvm_intrinsic" nmPtr
        msym <- resolveFunPtrTerm fp
        case msym of
          LookupResult sym -> nm `userRedirectTo` sym
          _ -> errorPath "lss_override_llvm_intrinsic: Failed to resolve function pointer"
      _ -> wrongArguments "lss_override_llvm_intrinsic"


lss_override_reset_by_addr :: StdOvdEntry sbe m
lss_override_reset_by_addr =
  voidOverrideEntry "lss_override_reset_by_addr" [strTy] $ \args ->
    case args of
      [(PtrType{},fp)] -> do
        msym <- resolveFunPtrTerm fp
        case msym of
          LookupResult sym -> sym `userRedirectTo` sym
          _ -> errorPath "lss_override_reset_by_addr: Failed to resolve function pointer"
      _ -> wrongArguments "lss_override_reset_by_addr"

lss_override_reset_by_name :: StdOvdEntry sbe m
lss_override_reset_by_name = 
  voidOverrideEntry "lss_override_reset_by_name" [strTy] $ \args ->
    case args of
      [(PtrType{}, fnNamePtr)] -> do
        fnSym <- fromString <$> loadString "lss_override_reset_by_name" fnNamePtr
        fnSym `userRedirectTo` fnSym
      _ -> wrongArguments "lss_override_reset_by_name"

-- TODO (?): We may want to avoid retraction of overridden intrinsics, since
-- presumably the user always wants the overridden version.
lss_override_reset_all :: StdOvdEntry sbe m
lss_override_reset_all =
  voidOverrideEntry "lss_override_reset_all" [] $ \_ -> do
    ovds <- use fnOverrides
    forM_ (Map.assocs ovds) $ \(sym, (_, userOvd)) ->
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

lss_print_symbolic :: StdOvdEntry sbe m
lss_print_symbolic = do
  voidOverrideEntry "lss_print_symbolic" [i8p] $ \args ->
    case args of
      [(_,ptr)] -> do
        v <- load i8 ptr 0
        d <- withSBE' $ \sbe -> prettyTermD sbe v
        liftIO $ print d
      _ -> wrongArguments "lss_print_symbolic"

lss_abort :: StdOvdEntry sbe m
lss_abort = voidOverrideEntry "lss_abort" [strTy] $ \args ->
  case args of
    [(_,tv)] -> do
      msg <- loadString "abort message" tv
      errorPath $ "lss_abort(): " ++ msg
    _ -> errorPath "Incorrect number of parameters passed to lss_abort()"

lss_show_mem :: StdOvdEntry sbe m
lss_show_mem = voidOverrideEntry "lss_show_mem" [] $ \_ -> do
  unlessQuiet $ dumpMem 1 "lss_show_mem()"

lss_show_path :: StdOvdEntry sbe m
lss_show_path = voidOverrideEntry "lss_show_path" [] $ \_ -> do
  Just p <- preuse currentPathOfState
  sbe <- gets symBE
  unlessQuiet $ dbugM $ show $ nest 2 $ ppPath sbe p

lss_set_verbosity :: StdOvdEntry sbe m
lss_set_verbosity = voidOverrideEntry "lss_set_verbosity" [i32] $ \args ->
  case args of
    [(_,v)] -> do
      sbe <- gets symBE
      case asUnsignedInteger sbe 32 v of
        Nothing  -> errorPath "symbolic verbosity is illegal"
        Just v'' -> setVerbosity (fromIntegral v'')
    _ -> errorPath "Incorrect number of parameters passed to lss_set_verbosity"

lss_fresh_uint :: BitWidth -> StdOvdEntry sbe m
lss_fresh_uint n = do
  let nm = fromString $ "lss_fresh_uint" ++ show n
      itp = IntType n
  overrideEntry nm itp [itp] $ \_ -> withSBE (flip freshInt n)

-- | @lss_fresh_array_uint n@ returns an override that yields an array of integers,
-- each with with @n@ bits.
lss_fresh_array_uint :: BitWidth -> StdOvdEntry sbe m
lss_fresh_array_uint n = do
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

lss_aiger_add_output_uint :: BitWidth -> StdOvdEntry sbe m
lss_aiger_add_output_uint n = do
  let nm = fromString $ "lss_aiger_add_output_uint" ++ show n
  let tp = IntType n
  voidOverrideEntry nm [tp] $ \args ->
    case args of
      [(_,t)] -> aigOutputs %= ((tp,t):)
      _   -> wrongArguments (show nm)

lss_aiger_add_output_array_uint :: BitWidth -- ^ Width of array that target points to.
                  -> StdOvdEntry sbe m
lss_aiger_add_output_array_uint n = do
  let nm = "lss_aiger_add_output_array_uint" ++ show n
  let itp = IntType n
  let ptp = PtrType (MemType itp)
  voidOverrideEntry (fromString nm) [ptp, i32] $ \args ->
    case args of
      [(_,tptr), (_, sizeTm)] -> do
        sbe <- gets symBE
        case asUnsignedInteger sbe 32 sizeTm of
          Just size -> do
            let tp = ArrayType (fromInteger size) itp
            arrTm <- load tp tptr 0
            aigOutputs %= ((tp,arrTm):)
          Nothing -> errorPath $ nm ++ " called with symbolic size."
      _ -> wrongArguments nm

lss_write_aiger_uint :: BitWidth -> StdOvdEntry sbe m
lss_write_aiger_uint n = do
  let nm = "lss_write_aiger_uint" ++ show n
  let itp = IntType n
  voidOverrideEntry (fromString nm) [itp, strTy] $ \args ->
    case args of
      [(_,t), (_,fptr)] -> do
        file <- loadString (nm ++ " file") fptr
        checkAigFile file
        sbe <- gets symBE
        liftSBE (writeAiger sbe file [(itp,t)])
      _ -> wrongArguments nm

lss_write_aiger_array_uint :: BitWidth -> StdOvdEntry sbe m
lss_write_aiger_array_uint n = do
  let nm = "lss_write_aiger_array_uint" ++ show n
  let itp = IntType n
  let ptp = PtrType (MemType itp)
  voidOverrideEntry (fromString nm) [ptp, i32, strTy] $ \args ->
    case args of
      [(_, tptr), (_, sizeTm), (_, fptr)] -> do
        sbe <- gets symBE
        case asUnsignedInteger sbe 32 sizeTm of
          Just size -> do
            file <- loadString nm fptr
            checkAigFile file
            let tp = ArrayType (fromInteger size) itp
            arrTm <- load tp tptr 0
            liftSBE $ writeAiger sbe file [(tp,arrTm)]
          Nothing ->
            errorPath $ nm ++ " called with symbolic size"
      _ -> wrongArguments nm

lss_write_aiger :: StdOvdEntry sbe m
lss_write_aiger =
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
lss_eval_aiger_uint :: BitWidth -> StdOvdEntry m sbe
lss_eval_aiger_uint n = do
  let nm = "lss_eval_aiger_uint" ++ show n
  let itp = IntType n
  overrideEntry (fromString nm) itp [itp, i8p, i32] $ \args ->
    case args of
      [(_,tm), (_, p), (_,szTm)] -> do
        bools <- getEvalInputs nm p szTm
        sbe <- gets symBE
        liftSBE $ evalAiger sbe bools itp tm
      _ -> wrongArguments nm

lss_eval_aiger_array_uint :: BitWidth -- ^ Width of array elements.
               -> StdOvdEntry sbe m
lss_eval_aiger_array_uint n = do
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

lss_write_cnf :: StdOvdEntry sbe m
lss_write_cnf =
  voidOverrideEntry "lss_write_cnf" [i32, strTy] $ \args ->
    case args of
      [(IntType w, t), (PtrType{},fptr)] | w == 32 -> do
        file <- loadString "lss_write_cnf" fptr
        void $ withSBE $ \s -> writeCnf s file 32 t
      _ -> wrongArguments "lss_write_cnf"

------------------------------------------------------------------------
-- SMTLIB common

malloc1 ::
  ( MonadIO m
  , Functor m
  , Functor sbe
  )
  => MemType
  -> Simulator sbe m (SBETerm sbe)
malloc1 tp = do
  sbe <- gets symBE
  cnt1 <- liftSBE $ termInt sbe 32 1
  malloc tp 32 cnt1

-- | Delete key from map stored in ref.
insertIntoRef :: (MonadIO m, Ord k) => IORef (Map k v) -> k -> v -> Simulator sbe m ()
insertIntoRef r k v = liftIO $ do
  m <- readIORef r
  writeIORef r $! Map.insert k v m

-- | Delete key from map stored in ref.
removeFromRef :: (Functor sbe, Functor m, MonadIO m, Ord k)
              => IORef (Map k v) -> String -> k -> Simulator sbe m ()
removeFromRef r nm k = do
  m <- liftIO $ readIORef r
  when (Map.notMember k m) $ do
    errorPath $ nm ++ ": Could not resolve script pointer."
  liftIO $ writeIORef r $! Map.delete k m

assertUint8IsNonzero :: (Monad m) => SBETerm sbe -> Simulator sbe m (SBEPred sbe)
assertUint8IsNonzero v = do
  sbe <- gets symBE
  zero <- liftSBE $ termInt sbe 8 0
  isZero <- liftSBE $ applyIEq sbe 8 v zero
  liftSBE $ applyBNot sbe isZero

------------------------------------------------------------------------
-- SMTLIB1

ptrToSMTLIB1 :: MemType
ptrToSMTLIB1 = PtrType VoidType -- (UnsupportedType L.Opague)


registerSMTLIB1Overrides ::
  forall sbe m . (Functor m, MonadIO m, Functor sbe,  Ord (SBETerm sbe))
    => Simulator sbe m ()
registerSMTLIB1Overrides = do
  scriptsRef <- liftIO $ newIORef (Map.empty :: Map.Map (SBETerm sbe) (SMTLIB1Script sbe))
  let getScript :: String -> SBETerm sbe -> Simulator sbe m (SMTLIB1Script sbe)
      getScript nm p = do
        scriptsMap <- liftIO $ readIORef scriptsRef
        case Map.lookup p scriptsMap of
          Nothing -> errorPath $ nm ++ ": Could not resolve script pointer."
          Just s -> return s
  registerOverrides
    [ overrideEntry "lss_SMTLIB1_create" ptrToSMTLIB1 [strTy] $ \args -> do
        let [(_,namePtr)] = args
        -- Get name
        name <- loadString "lss_SMTLIB1_write" namePtr
        -- Create variable for pointer.
        rslt <- malloc1 ptrToSMTLIB1
        -- Update scriptsRef to store new variable.
        sbe <- gets symBE
        case createSMTLIB1Script sbe of
          Nothing -> errorPath "lss_SMTLIB1_create: Backend does not support SMTLIB generation."
          Just creator -> do
            script <- liftSBE $ creator name
            insertIntoRef scriptsRef rslt script
            -- Return pointer
            return rslt
    , voidOverrideEntry "lss_SMTLIB1_assumption_nonzero_uint8" [ptrToSMTLIB1, i8] $ \args -> do
        let [(_,sp),(_,v)] = args
        s <- getScript "lss_SMTLIB1_assumption_nonzero_uint8" sp
        nz <- assertUint8IsNonzero v
        liftSBE $ addSMTLIB1Assumption s nz
    , voidOverrideEntry "lss_SMTLIB1_formula_nonzero_uint8" [ptrToSMTLIB1, i8] $ \args -> do
        let [(_,sp),(_,v)] = args
        s <- getScript "lss_SMTLIB1_formula_nonzero_uint8" sp
        nz <- assertUint8IsNonzero v
        liftSBE $ addSMTLIB1Formula s nz
    , voidOverrideEntry "lss_SMTLIB1_write" [ptrToSMTLIB1, strTy] $ \args -> do
        let [(_,sp),(_,pathPtr)] = args
        path <- loadString "lss_SMTLIB1_write" pathPtr
        s <- getScript "lss_SMTLIB1_write" sp
        liftIO $ writeSMTLIB1ToFile s path
    , voidOverrideEntry "lss_SMTLIB1_free" [ptrToSMTLIB1] $ \args -> do
        let [(_,sp)] = args
        removeFromRef scriptsRef "lss_SMTLIB1_free" sp
    ]

-- SMTLIB2 {{{

ptrToSMTLIB2 :: MemType
ptrToSMTLIB2 = PtrType VoidType -- (UnsupportedType L.Opague)

registerSMTLIB2Overrides ::
  forall sbe m . (Functor m, MonadIO m, Functor sbe,  Ord (SBETerm sbe))
    => Simulator sbe m ()
registerSMTLIB2Overrides = do
  scriptsRef <- liftIO $ newIORef (Map.empty :: Map.Map (SBETerm sbe) (SMTLIB2Script sbe))
  let getScript :: String -> SBETerm sbe -> Simulator sbe m (SMTLIB2Script sbe)
      getScript nm p = do
        scriptsMap <- liftIO $ readIORef scriptsRef
        case Map.lookup p scriptsMap of
          Nothing -> errorPath $ nm ++ ": Could not resolve pointer to script."
          Just s -> return s
  registerOverrides
    [ overrideEntry "lss_SMTLIB2_create" ptrToSMTLIB2 [] $ \_ -> do
        -- Create variable for pointer.
        rslt <- malloc1 ptrToSMTLIB2
        -- Update scriptsRef to store new variable.
        sbe <- gets symBE
        case createSMTLIB2Script sbe of
          Nothing -> errorPath "lss_SMTLIB2_create: Backend does not support SMTLIB generation."
          Just creator -> do
            script <- liftSBE creator
            insertIntoRef scriptsRef rslt script
            -- Return pointer
            return rslt
    , voidOverrideEntry "lss_SMTLIB2_assert_nonzero_uint8" [ptrToSMTLIB2, i8] $ \args -> do
        let [(_,sp),(_,v)] = args
        s <- getScript "lss_SMTLIB2_assert_nonzero_uint8" sp
        nz <- assertUint8IsNonzero v
        liftSBE $ addSMTLIB2Assert s nz
    , voidOverrideEntry "lss_SMTLIB2_check_sat" [ptrToSMTLIB2] $ \args -> do
        let [(_,sp)] = args
        s <- getScript "lss_SMTLIB2_check_sat" sp
        liftSBE $ addSMTLIB2CheckSat s
    , voidOverrideEntry "lss_SMTLIB2_write" [ptrToSMTLIB2, strTy] $ \args -> do
        let [(_,sp),(_,pathPtr)] = args
        path <- loadString "lss_SMTLIB2_write" pathPtr
        s <- getScript "lss_SMTLIB2_write" sp
        liftIO $ writeSMTLIB2ToFile s path
    , voidOverrideEntry "lss_SMTLIB2_free" [ptrToSMTLIB2] $ \args -> do
        let [(_,sp)] = args
        removeFromRef scriptsRef "lss_SMTLIB2_free" sp
    ]

registerLSSOverrides :: (Functor m, MonadIO m, Functor sbe, Ord (SBETerm sbe))
                     => Simulator sbe m ()
registerLSSOverrides = do
  let groundOverrides =
        [ lss_write_aiger
        , lss_write_cnf
          -- Override support
        , lss_override_function_by_addr
        , lss_override_function_by_name
        , lss_override_llvm_intrinsic
        , lss_override_reset_by_addr
        , lss_override_reset_by_name
        , lss_override_reset_all
          -- Debugging suport
        , lss_print_symbolic
        , lss_abort
        , lss_show_path
        , lss_show_mem
        , lss_set_verbosity
        ]
      polyOverrides =
        [ lss_fresh_uint
        , lss_fresh_array_uint
        , lss_aiger_add_output_uint
        , lss_aiger_add_output_array_uint
        , lss_write_aiger_uint
        , lss_write_aiger_array_uint
        , lss_eval_aiger_uint
        , lss_eval_aiger_array_uint
        ]
      polySizes = [8, 16, 32, 64]
  registerOverrides $
    groundOverrides
    ++ [ f i | f <- polyOverrides, i <- polySizes ]
  registerSMTLIB1Overrides
  registerSMTLIB2Overrides
