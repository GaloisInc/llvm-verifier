{- |
Module           : $Header$
Description      : Error path and error handling tests
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}

module Tests.Errors (errorTests) where

import           Control.Monad
import           LSS.LLVMUtils
import           LSS.SBEBitBlast
import           LSS.Simulator
import           LSSImpl
import           System.FilePath
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Tests.Common
import           Text.LLVM              ((=:))
import           Verinf.Symbolic.Common (ConstantProjection(..))
import qualified Text.LLVM              as L

errorTests :: [(Args, Property)]
errorTests =
  [
    test 1 False "test-error-paths-all" $ let v = 1 in psk v $ do
      let chk sbe (memType, mem) execRslt = do
            dbugM $ "runAllMemModelTestLSS on mem" ++ show memType
            -- any way to do something ~ (memDump sbe mem Nothing) here?
            case execRslt of
              NoMainRV eps _ -> return (length eps == 2)
              _              -> return False
      runAllMemModelTestLSS Nothing (ctestCB "test-error-paths-all.bc") [] chk
  ]

--------------------------------------------------------------------------------
-- Scratch

_nowarn :: a
_nowarn = undefined main

main :: IO ()
main = runTests errorTests
