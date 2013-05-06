{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
module Verifier.LLVM.LLVMIntrinsics 
  ( registerLLVMIntrinsicOverrides
  ) where

import Control.Applicative
import Control.Lens hiding (act,from)
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Data.String
import Text.PrettyPrint.Leijen hiding ((<$>), align, line)

import Verifier.LLVM.Backend
import Verifier.LLVM.DataLayout
import Verifier.LLVM.Simulator.Internals


uaddWithOverflowIntrinsic :: BitWidth -> StdOvd m sbe
uaddWithOverflowIntrinsic w = Override $ \_ mregs args ->
  case (mregs, args) of
    (Just _, [(_,x), (_,y)]) -> do
      sbe <- gets symBE
      fmap Just $ liftSBE $ applyTypedExpr sbe (UAddWithOverflow w x y)
    _ -> wrongArguments "llvm.uadd.with.overflow"

memcpyIntrinsic :: BitWidth -> StdOvd m sbe
memcpyIntrinsic w = voidOverride $ \args -> do
  case args of
    [(_,dst), (_,src), (_,len), (_,align), _] -> do
      Just m <- preuse currentPathMem
      (c,m') <- withSBE $ \sbe -> memCopy sbe m dst src w len align
      currentPathMem .= m'
      sbe <- gets symBE
      let pts = map (prettyTermD sbe) [dst,src,len]
      let fr = "memcopy operation was not valid: (dst,src,len) = "
                ++ show (parens $ hcat $ punctuate comma $ pts)
      processMemCond fr c
    _ -> wrongArguments "llvm.memcpy"

memsetIntrinsic :: BitWidth -> StdOvd m sbe
memsetIntrinsic lenWidth = voidOverride $ \args -> do
  case args of
    [(_,dst0), (_,val), (_,len), _, _] -> do
      sbe <- gets symBE
      case asUnsignedInteger sbe lenWidth len of
        Nothing -> errorPath "LSS does not support llvm.memset with symbolic lengths."
        Just n0 -> go n0 dst0
          where go 0 _ = return ()
                go i dst = do
                  store i8 val dst 0
                  go (i-1) =<< ptrInc dst
    _ -> wrongArguments "llvm.memset"

objectsizeIntrinsic :: BitWidth -> StdOvd m sbe
objectsizeIntrinsic w = Override $ \_ _ args ->
  case args of
    [_, (_,maxOrMin)] -> do
      sbe <- gets symBE
      case asUnsignedInteger sbe 1 maxOrMin of
        Nothing -> errorPath $ "llvm.objectsize expects concrete 2nd parameter"
        Just v  -> Just <$> liftSBE (termInt sbe w tv)
          where tv = if v == 0 then -1 else 0
    _ -> wrongArguments "llvm.objectsize"

registerLLVMIntrinsicOverrides :: (Functor sbe, Functor m, MonadIO m) => Simulator sbe m ()
registerLLVMIntrinsicOverrides = do
  let override_uadd_with_overflow w = do
        let nm = fromString $ "llvm.uadd.with.overflow.i" ++ show w
        tryRegisterOverride nm $ \_ -> do
          return $ uaddWithOverflowIntrinsic w
  mapM_ override_uadd_with_overflow [16, 32, 64]
  registerOverrides 
    [ ("llvm.memcpy.p0i8.p0i8.i32", voidFunDecl [i8p, i8p, i32, i32, i1], memcpyIntrinsic 32)
    , ("llvm.memcpy.p0i8.p0i8.i64", voidFunDecl [i8p, i8p, i64, i32, i1], memcpyIntrinsic 64)
    , ("llvm.memset.p0i8.i32", voidFunDecl [i8p, i8, i32, i32, i1], memsetIntrinsic 32)
    , ("llvm.memset.p0i8.i64", voidFunDecl [i8p, i8, i64, i32, i1], memsetIntrinsic 64)
    , ("llvm.objectsize.i32", funDecl i32 [i8p, i1], objectsizeIntrinsic 32)
    , ("llvm.objectsize.i64", funDecl i64 [i8p, i1], objectsizeIntrinsic 64)
    -- Do nothing.
    , ("llvm.lifetime.start", voidFunDecl [i64, i8p], voidOverride (\_ -> return ()))
    , ("llvm.lifetime.end",   voidFunDecl [i64, i8p], voidOverride (\_ -> return ()))
    ]
