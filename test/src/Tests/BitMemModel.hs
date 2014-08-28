{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
module Tests.BitMemModel (bitMemModelTests) where

import Data.Bits
import qualified Data.Vector.Storable as LV
import Test.QuickCheck
import Test.QuickCheck.Monadic

-- import Verinf.Symbolic (createBitEngine)
-- import Verinf.Symbolic.Lit
-- import Verinf.Symbolic.Lit.Functional

-- import Verifier.LLVM.Backend.BitBlast

import qualified Data.AIG as AIG
import           Data.AIG (IsAIG)
import qualified Data.ABC as ABC
import Verifier.LLVM.Backend.BitBlastNew


import Verifier.LLVM.Codebase.DataLayout

mmTest :: String
       -> Int
       -> (forall mem g l s. (IsAIG l g, Eq (l s))
            => DataLayout
            -> g s
            -> SBE (BitIO mem (l s))
            -> BitBlastMemModel mem (l s)
            -> mem
            -> PropertyM IO ()) 
       -> (Args, Property)
mmTest testName n fn = 
  ( stdArgs {maxSuccess = n}
  , label testName $ monadicIO $ do
      let dl = defaultDataLayout
      let mg = MemGeom { 
                   mgStack = (0x10,0x0)
                 , mgCode = (0x0,0x0)
                 , mgData = (0x0, 0x0)
                 , mgHeap = (0x0,0x0)
                 }
      (AIG.SomeGraph g) <- run $ AIG.newGraph ABC.giaNetwork
      (mm, mem) <- run $ createDagMemModel dl g mg
      fn dl g (sbeBitBlast g (error "No CNF writer function defined!") dl mm) mm mem
   )


-- | Generate bit vector from integer.
bfi :: Int -> Integer -> [Bool]
bfi w v = AIG.bvToList $ AIG.generate_lsb0 w (testBit v)

bitMemModelTests :: [(Args, Property)]
bitMemModelTests =
  [ mmTest "symbolicTests" 1 $ \dl g sbe@SBE { .. } MemModel { .. } m0 -> do
      let ptrWidth = ptrBitwidth dl
      let bytes = AIG.bvFromInteger g 8 7
      let tTrue = sbeTruePred
      cnt <- runSBE $ freshInt 1
      -- Try allocating symbolic ammount.
      AResult c0 ptr m1 <- run $ mmStackAlloc m0 1 cnt 0
      assert (c0 == tTrue)
      -- Try filling up rest of memory and testing stack allocation
      -- failed.
      do fill <- runSBE $ termInt sbe 4 0x0F
         AResult c0' _ _ <- run $ mmStackAlloc m1 1 fill 1
         assert =<< runSBE (evalPred [False] c0')
      -- Test store to concrete address succeeds.
      (c1, m2) <- run $ mmStore m1 ptr bytes 0
      do assert =<< runSBE (evalPred [True] c1)
      -- Test symbolic load succeeds under appropiate conditions.
      do cntExt <- runSBE $ applyTypedExpr (SExt Nothing 1 cnt ptrWidth)
         rptr <- runSBE $ applyTypedExpr (PtrAdd ptr cntExt)
         (c2, _) <- run $ mmLoad m2 rptr 1 0 
         assert =<< runSBE (evalPred [False] c2)
  , mmTest "mergeTest" 1 $ \_lc g sbe@SBE { .. } MemModel { .. } m0 -> do
      let tTrue = sbeTruePred
      -- Allocate space on stack.
      cnt <- runSBE $ termInt sbe 8 1
      m0' <- run $ mmRecordBranch m0
      AResult c0 ptr m1 <- run $ mmStackAlloc m0' 1 cnt 0
      assert (c0 == tTrue)
      -- Store bytes
      let lvi = AIG.bvFromInteger g
      (ct, mt) <- run $ mmStore m1 ptr (lvi 8 1) 0
      assert =<< runSBE (evalPred [] ct)
      (cf, mf) <- run $ mmStore m1 ptr (lvi 8 0) 0
      assert =<< runSBE (evalPred [] cf)
      -- Merge
      cond0 <- runSBE $ freshInt 1
      cond <- runSBE $ applyIEq 1 cond0 =<< termInt sbe 1 1
      m2 <- run $ mmMux cond mt mf
      -- Check result of merge
      (c2, v) <- run $ mmLoad m2 ptr 1 0 
      assert (c2 == tTrue)
      v1 <- run $ AIG.evaluate (AIG.Network g (AIG.bvToList v)) [False]
      v2 <- run $ AIG.evaluate (AIG.Network g (AIG.bvToList v)) [True]
      assert (v1 == bfi 8 0)
      assert (v2 == bfi 8 1)
  ] 
 where runSBE = run . liftSBEBitBlast
