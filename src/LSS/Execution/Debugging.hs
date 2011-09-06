{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE ViewPatterns     #-}

module LSS.Execution.Debugging where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Data.Bits
import           Data.LLVM.TargetData
import           Data.Maybe
import           LSS.Execution.Common
import           LSS.LLVMUtils
import           LSS.SBEInterface
import           LSS.SBEBitBlast
import           LSS.Simulator
import           Text.LLVM              (Typed(..), typedValue)
import           Verinf.Symbolic.Common (ConstantProjection(..), Lit)
import qualified Control.Exception      as CE
import qualified Text.LLVM              as L

-- NB: Currently only valid for SBEBitBlast mems
sanityChecks ::
  ( Functor m
  , MonadIO m
  , ConstantProjection (SBEClosedTerm sbe)
  , SBEMemory sbe ~ BitMemory Lit
  )
  => SEH sbe m
sanityChecks = SEH
  {
    onPreStep         = \_ -> return ()
  , onPostStep        = \_ -> return ()
  , onMkGlobTerm      = \_ -> return ()
  , onPostOverrideReg = return ()

  , onPreGlobInit = \g (Typed ty gdata) -> do
      CE.assert (L.globalType g == ty) $ return ()
      sz  <- withLC (`llvmStoreSizeOf` ty)
      szt <- withSBE' $ \sbe -> termWidth sbe gdata
      when (szt `shiftR` 3 /= sz) $ do
        dbugM $ "onPreGlobInit assert failure on " ++ show (L.ppSymbol $ L.globalSym g)
                ++ " (size check)"
        CE.assert False $ return ()

  , onPostGlobInit = \g (Typed ty gdata) -> do
      mem       <- getMem
      sz        <- withLC (`llvmStoreSizeOf` ty)
      addrWidth <- withLC llvmAddrWidthBits
      -- Read back and check
      gstart <- withSBE $ \sbe -> termInt sbe addrWidth (bmDataAddr mem - sz)
      (cond, gdata') <- withSBE $ \sbe -> memLoad sbe mem (Typed (L.PtrTo ty) gstart)
      eq <- uval =<< (Typed i1 <$> withSBE (\sbe -> applyICmp sbe L.Ieq gdata gdata'))
      when (eq /= 1) $ do
        dbugM $ "onPostGlobInit assert failure on " ++ show (L.ppSymbol $ L.globalSym g)
                ++ " (read-back) "
        CE.assert False $ return ()
  }
  where
    uval (typedValue -> v) =
      fromJust <$> withSBE' (\sbe -> getUVal $ closeTerm sbe v)


