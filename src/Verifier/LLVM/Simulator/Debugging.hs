{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE ViewPatterns     #-}

module Verifier.LLVM.Simulator.Debugging where
import           Control.Monad.Trans
import           Verifier.LLVM.Simulator.Common

-- NB: Currently only valid for SBEBitBlast mems
sanityChecks ::
  ( Functor m
  , MonadIO m
  , Functor sbe
  )
  => SEH sbe m
sanityChecks = SEH
  {
    onPreStep         = \_ -> return ()
  , onPostStep        = \_ -> return ()
  , onMkGlobTerm      = \_ -> return ()
  , onPostOverrideReg = return ()

  , onPreGlobInit = \_ _ -> return ()

{-
  , onPreGlobInit = \g (Typed ty gdata) -> do
      CE.assert (L.globalType g == ty) $ return ()
      sz  <- withLC (`llvmStoreSizeOf` ty)
      szt <- withSBE' $ \sbe -> termWidth sbe gdata
      when (szt `shiftR` 3 /= sz) $ do
        dbugM $ "onPreGlobInit assert failure on " ++ show (L.ppSymbol $ L.globalSym g)
                ++ " (size check)"
        CE.assert False $ return ()
-}

  , onPostGlobInit = \_g _ -> do
      {-
      Just mem       <- getMem
      sz        <- withLC (`llvmStoreSizeOf` ty)
      addrWidth <- withLC llvmAddrWidthBits
      -- Read back and check
      gstart <- withSBE $ \sbe -> termInt sbe addrWidth (bmDataAddr mem - sz)
      (cond, gdata') <- withSBE $ \sbe -> memLoad sbe mem (Typed (L.PtrTo ty) gstart)
      processMemCond cond
      eq <- uval =<< (Typed i1 <$> withSBE (\sbe -> applyICmp sbe L.Ieq gdata gdata'))
      when (eq /= 1) $ do
        dbugM $ "onPostGlobInit assert failure on " ++ show (L.ppSymbol $ L.globalSym g)
                ++ " (read-back) "
        CE.assert False $ return ()
        -}
      return ()
  }
  {-
  where
    uval (typedValue -> v) =
      fromJust <$> withSBE' (\sbe -> snd $ asUnsignedInteger sbe v)
-}
