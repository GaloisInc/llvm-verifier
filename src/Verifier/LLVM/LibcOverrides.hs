{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Verifier.LLVM.LibcOverrides
  ( registerLibcOverrides
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Data.String
import qualified Data.Vector as V
import Numeric                   (showHex)

import Verifier.LLVM.AST
import Verifier.LLVM.Backend
import Verifier.LLVM.Simulator.Internals

allocaOvd :: BitWidth -> StdOvdEntry sbe m
allocaOvd aw = do
  let sizeT = IntType aw
  overrideEntry "alloca" i8p [sizeT] $ \args ->
    case args of
      [(_,sizeTm)] -> alloca i8 aw sizeTm 0
      _ -> wrongArguments "alloca"

mallocOvd :: SymType -> BitWidth -> StdOvdEntry m sbe
mallocOvd ptp aw = do
  overrideEntry "malloc" (PtrType ptp) [IntType aw] $ \args ->
    case args of
      [(_,sizeTm)] -> malloc i8 aw sizeTm
      _ -> wrongArguments "malloc"

data PrintfFlags = PrintfFlags {
    zeroPad :: Bool
  }

printfToString :: forall sbe m . (Functor sbe, Functor m, MonadIO m)
               => String -> [(MemType,SBETerm sbe)] -> Simulator sbe m String
printfToString fmt args = do
    let vargs = V.fromList args

    let valueAt :: Int -> Simulator sbe m (MemType,SBETerm sbe)
        valueAt p = maybe (errorPath msg) return (vargs V.!? p)
          where msg = "Could not get argument at position " ++ show p
    sbe <- gets symBE
    let badArg p = errorPath $ "printf given bad argument at position " ++ show p
    let pr :: ((MemType, SBETerm sbe) -> Maybe String)
           -> Int -> Simulator sbe m String
        pr f p = maybe (badArg p) return . f =<< valueAt p
    let fmtSigned (IntType w, v) = Just $
          case asSignedInteger sbe w v of
            Just cv  -> show cv
            Nothing -> show (prettyTermD sbe v)
        fmtSigned _ = Nothing
    let fmtUnsigned (IntType w, v) = Just $
          case asUnsignedInteger sbe w v of
            Just cv -> show cv
            Nothing -> show (prettyTermD sbe v)
        fmtUnsigned _ = Nothing
    let fmtPointer (PtrType{}, v) = Just $
          case asConcretePtr sbe v of
            Just cv  -> "0x" ++ showHex cv ""
            Nothing -> show (prettyTermD sbe v)
        fmtPointer _ = Nothing
    let printString p = do
          mv <- valueAt p
          case mv of
            (PtrType{}, v) -> loadString "printToString" v
            _ -> badArg p
    let procString ('%':r) p rs = procArg r defaultFlags p rs
          where defaultFlags = PrintfFlags { zeroPad = False }
        procString (c:r) p rs = procString r p (c:rs)
        procString [] _ rs = return (reverse rs)

        procArg ('d':r) _ p rs = procRest r (p+1) rs =<< pr fmtSigned p
        procArg ('i':r) _ p rs = procRest r (p+1) rs =<< pr fmtSigned p
        procArg ('p':r) _ p rs = procRest r (p+1) rs =<< pr fmtPointer p
        procArg ('u':r) _ p rs = procRest r (p+1) rs =<< pr fmtUnsigned p
        procArg ('s':r) _ p rs = procRest r (p+1) rs =<< printString p
        procArg r       _ _ _  = errorPath $ "Unsupported format string " ++ show r

        procRest r p rs s = procString r p (reverse s ++ rs)
    procString fmt 0 []

printf :: StdOvdEntry sbe m
printf =
  varArgsOverrideEntry "printf" i32 [strTy] $ \args ->
    case args of
      ((_,fmtPtr) : rest) -> do
        fmtStr <- loadString "printf format string" fmtPtr
        --isSym <- withSBE' isSymbolic
        --ptrWidth <- withLC llvmAddrWidthBits
        --let fmtStr' = fixFormat (ptrWidth `div` 4) (map isSym rest) fmtStr
        --resString <- symPrintf fmtStr' <$> mapM termToArg rest
        resString <- printfToString fmtStr rest
        unlessQuiet $ liftIO $ putStr resString
        withSBE $ \s -> termInt s 32 (toInteger (length resString))
      _ -> wrongArguments "printf"

handle_assert :: String -> StdOvdEntry sbe m
handle_assert nm = do
  voidOverrideEntry (fromString nm) [i8p, i8p, i32, i8p] $ \args -> do
    case args of
      [(_,v1), (_,v2), (_,v3), (_,v4)] -> do
          fname     <- loadString "assert function" v1
          file      <- loadString "assert filename" v2
          sbe <- gets symBE
          let Just line = asSignedInteger sbe 32 v3
          err       <- loadString "assert error message" v4
          errorPath $ unwords [ nm
                              , file ++ ":" ++ show line ++ ":" ++ fname ++ ":"
                              , err
                              ]
      _ -> wrongArguments nm

free :: StdOvdEntry sbe m
free = do
  voidOverrideEntry "free" [i8p] $ \_ -> return ()

memset_chk :: BitWidth -> StdOvdEntry sbe m
memset_chk aw = do
  let nm = "__memset_chk"
  let sizeT = IntType aw
  overrideEntry (fromString nm) i8p [i8p, i32, sizeT, sizeT] $ \args -> do
    case args of
      [(_,dst), (_,val0), (_, len), (_, _dstlen)] -> do
        sbe <- gets symBE
        -- Truncate down to unsigned integer.
        val <- liftSBE $ termTruncScalar sbe 32 val0 8
        memset nm dst val aw len
        return dst
      _ -> wrongArguments nm

registerLibcOverrides :: (Functor m, MonadIO m, Functor sbe) => Simulator sbe m ()
registerLibcOverrides = do
  aw <- ptrBitwidth <$> getDL
  -- Register malloc
  tryFindFunDecl "malloc" $ \d -> do
    case fdRetType d of
      Just (PtrType ptp) -> do
        registerOverrideEntry (mallocOvd ptp aw)
      _ -> return ()
  registerOverrides
    [ handle_assert "__assert_fail"
    , handle_assert "__assert_rtn"
    --, ("exit", voidTy, [i32], False, exitHandler)
    , allocaOvd aw
    , free
    , memset_chk aw
    , printf
    ]