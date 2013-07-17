{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Verifier.LLVM.LibcOverrides
  ( registerLibcOverrides
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Data.Char
import Data.List (stripPrefix)
import Data.String
import qualified Data.Vector as V
import Numeric                   (showHex, showOct)

import Verifier.LLVM.AST
import Verifier.LLVM.Backend
import Verifier.LLVM.Simulator.Internals
import Verifier.LLVM.Simulator.SimUtils

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

data Justification = LeftJustify | RightJustify

data PrintfFlags = PrintfFlags {
    printfJustify :: Justification
  , printfForceSign :: Bool
  , printfBlankBeforeUnsigned :: Bool
  , printfShowBasePrefix :: Bool
  , printfZeroPad :: Bool
    -- | printfMinWidth is @Left n@ if an argument is used for the
    -- width, and @Right w@ is the min width is set zero.
  , printfMinWidth :: Either Int Integer
  
  , printfPrecision :: Either Int Integer
  , lengthMod :: String
  }

defaultFlags :: PrintfFlags
defaultFlags = PrintfFlags { printfJustify = RightJustify
                           , printfForceSign = False
                           , printfBlankBeforeUnsigned = False
                           , printfShowBasePrefix = False
                           , printfZeroPad = False
                           , printfMinWidth = Right 0
                           , printfPrecision = Right 0
                           , lengthMod = "" 
                           }

-- | Attempt to format a term as an unsigned number with a given format term.
fmtUnsigned' :: SBE sbe -> (Integer -> String) -> (MemType,SBETerm sbe) -> Maybe String
fmtUnsigned' sbe cf (IntType w, v) = Just $
  case asUnsignedInteger sbe w v of
    Just cv -> cf cv
    Nothing -> show (prettyTermD sbe v)
fmtUnsigned' _ _ _ = Nothing

printfToString :: forall sbe m . (Functor sbe, Functor m, MonadIO m)
               => String -> [(MemType,SBETerm sbe)] -> Simulator sbe m String
printfToString fmt args = do
    let vargs = V.fromList args

    let valueAt :: Int -> Simulator sbe m (MemType,SBETerm sbe)
        valueAt p = maybe (errorPath msg) return (vargs V.!? p)
          where msg = "Could not get argument at position " ++ show p
    sbe <- gets symBE
    let badArg p = do
          whenVerbosity (>0) $ 
            dbugM $ "printf omitting bad argument at position " ++ show p
          return ""
                   
    let -- @pr f i@ prints argument at index i using the formating
        -- function f.
        pr :: ((MemType, SBETerm sbe) -> Maybe String)
           -> PrintfFlags
           -> Int
           -> Simulator sbe m String
        pr f _ p = maybe (badArg p) return . f =<< valueAt p

    let fmtSigned (IntType w, v) = Just $
          case asSignedInteger sbe w v of
            Just cv  -> show cv
            Nothing -> show (prettyTermD sbe v)
        fmtSigned _ = Nothing

    let fmtUnsigned = fmtUnsigned' sbe show
    let fmtOctal    = fmtUnsigned' sbe (\v -> showOct v "")
    let fmtHexLower = fmtUnsigned' sbe (\v -> showHex v "")
    let fmtHexUpper = fmtUnsigned' sbe (\v -> toUpper <$> showHex v "")

    let fmtChar     = fmtUnsigned' sbe (\v -> toEnum (fromInteger v) : []) 

    let fmtPointer (PtrType{}, v) = Just $
          case asConcretePtr sbe v of
            Just cv  -> "0x" ++ showHex cv ""
            Nothing -> show (prettyTermD sbe v)
        fmtPointer _ = Nothing

    let printString fl p = do
          case lengthMod fl of
            "" -> do
              mv <- valueAt p
              case mv of
                (PtrType{}, v) -> loadString "printToString" v
                _ -> badArg p
            "l" -> do
              dbugM "lss does not yet support printing wide characters."
              return ""
            lm -> do
              whenVerbosity (> 0) $
                dbugM $ "printf skipping argument with unsupported length modifier "
                  ++ show lm
              return ""

    let procString ('%':r) p rs = procFlags r defaultFlags p rs
        procString (c:r) p rs = procString r p (c:rs)
        procString [] _ rs = return (reverse rs)


        procFlags ('-':r) fl =
          procFlags r fl { printfJustify = LeftJustify }
        procFlags ('+':r) fl =
          procFlags r fl { printfForceSign = True }
        procFlags (' ':r) fl =
          procFlags r fl { printfBlankBeforeUnsigned = True }
        procFlags ('#':r) fl =
          procFlags r fl { printfShowBasePrefix = True }
        procFlags ('0':r) fl =
          procFlags r fl { printfZeroPad = True }
        procFlags r fl =
          procWidth r fl

        readInt :: String -> (Integer,String)
        readInt r = (read digits, r')
          where (digits,r') = span isDigit r
 
        -- Read width specifier.
        procWidth r@(c:_) fl p | isDigit c = procPrecision r' fl' p
          where (n,r') = readInt r
                fl' = fl { printfMinWidth = Right n }
        procWidth ('*':r) fl p = procPrecision r fl' (p+1)
          where fl' = fl { printfMinWidth = Left p }
        procWidth r fl p       = procPrecision r fl p

        -- Read precision
        procPrecision r@(c:_) fl p | isDigit c = procPrefix r' fl' p
          where (n,r') = readInt r
                fl' = fl { printfMinWidth = Right n }
        procPrecision ('*':r) fl p = procPrefix r fl' (p+1)
          where fl' = fl { printfMinWidth = Left p }
        procPrecision r fl p       = procPrefix r fl p

        -- Read prefix followed by type
        prefixes = [ "h", "hh", "l", "ll", "j", "z", "t", "L"]
        procPrefix r fl = firstPrefix prefixes
          where firstPrefix (pre:rest) =
                  case stripPrefix pre r of
                    Nothing -> firstPrefix rest
                    Just r' -> procType r' fl'
                      where fl' = fl { lengthMod = pre }
                firstPrefix [] = procType r fl

        floatingPointUnsupported _ _ = do
          whenVerbosity (> 0) $ dbugM "printf skipping floating point argument."
          return ""
           
        printCharCount _ _ = do
          whenVerbosity (> 0) $ dbugM "printf does not yet support %n parameters."
          return ""

        typePrinters :: [(Char, PrintfFlags -> Int -> Simulator sbe m String)]
        typePrinters
          = [ ('d', pr fmtSigned)
            , ('i', pr fmtSigned)
            , ('u', pr fmtUnsigned)
            , ('o', pr fmtOctal)
            , ('x', pr fmtHexLower)
            , ('X', pr fmtHexUpper)

            , ('f', floatingPointUnsupported)
            , ('F', floatingPointUnsupported)
            , ('e', floatingPointUnsupported)
            , ('E', floatingPointUnsupported)
            , ('g', floatingPointUnsupported)
            , ('G', floatingPointUnsupported)
            , ('a', floatingPointUnsupported)
            , ('A', floatingPointUnsupported)

            , ('c', pr fmtChar)
            , ('C', pr fmtChar)
            , ('s', printString)
            , ('p', pr fmtPointer)
            , ('n', printCharCount)
            , ('%', \_ _ -> return "%")
            ]
 
        procType :: String -- ^ String so far.
                 -> PrintfFlags -- ^ Flags if any
                 -> Int -- ^ Current argument position.
                 -> String -- ^ String to be printed.
                 -> Simulator sbe m String
        procType (c:r) fl p rs = findFirstMatch typePrinters
          where findFirstMatch ((nm,fn):otherPrinters)
                  | nm == c = procRest r (p+1) rs =<< fn fl p
                  | otherwise = findFirstMatch otherPrinters
                findFirstMatch [] = badArg p
        procType [] _ p rs = procRest [] (p+1) rs =<< badArg p

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