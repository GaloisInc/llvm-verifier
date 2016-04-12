{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module           : $Header$
Description      : Symbolic execution tests
License          : BSD3
Stability        : provisional
Point-of-contact : atomb
-}
module Tests.BitMemModel (bitMemModelTests) where

import Data.Bits

import qualified Data.Vector as V

import qualified Data.AIG as AIG
import           Data.AIG (IsAIG)
import qualified Data.ABC as ABC
import Verifier.LLVM.Backend.BitBlast

import Test.Tasty
import Test.Tasty.HUnit as HU

import Verifier.LLVM.Codebase.DataLayout

mmTest :: String
       -> Bool -- Diable buddy model?
       -> (forall mem g l s. (IsAIG l g, Eq (l s))
            => DataLayout
            -> g s
            -> SBE (BitIO mem (l s))
            -> BitBlastMemModel mem (l s)
            -> mem
            -> IO ())
       -> TestTree
mmTest testName disableBuddy fn =
  let dl = defaultDataLayout
      mg = MemGeom {
                   mgStack = (0x10,0x0)
                 , mgCode = (0x0,0x0)
                 , mgData = (0x0, 0x0)
                 , mgHeap = (0x0,0x0)
                 }
   in testGroup testName $
      [ HU.testCase "dag model" $ do
           (AIG.SomeGraph g) <- AIG.newGraph ABC.giaNetwork
           (mm, mem) <- createDagMemModel dl g mg
           fn dl g (sbeBitBlast g dl mm) mm mem
      ] ++
      if disableBuddy then [] else
      [ HU.testCase "buddy model" $ do
           (AIG.SomeGraph g) <- AIG.newGraph ABC.giaNetwork
           let mem = buddyInitMemory mg
               mm  = buddyMemModel dl g
           fn dl g (sbeBitBlast g dl mm) mm mem
      ]

-- | Generate bit vector from integer.
bfi :: Int -> Integer -> [Bool]
bfi w v = AIG.bvToList $ AIG.generate_lsb0 w (testBit v)

bitMemModelTests :: [TestTree]
bitMemModelTests =
  [ mmTest "symbolicTests" True $ \dl g sbe@SBE { .. } MemModel { .. } m0 -> do
      let ptrWidth = ptrBitwidth dl
      let bytes = V.fromList [AIG.bvFromInteger g 8 7]
      let tTrue = sbeTruePred
      cnt <- runSBE $ freshInt 1
      -- Try allocating symbolic ammount.
      AResult c0 ptr m1 <- mmStackAlloc m0 1 cnt 0
      HU.assertBool "failed to stack allocate" (c0 == tTrue)
      -- Try filling up rest of memory and testing stack allocation
      -- failed.
      do fill <- runSBE $ termInt sbe 4 0x0F
         AResult c0' _ _ <- mmStackAlloc m1 1 fill 1
         HU.assertBool "failed to fill stack allocate" =<< runSBE (evalPred [False] c0')
      -- Test store to concrete address succeeds.
      (c1, m2) <- mmStore m1 ptr bytes 0
      do HU.assertBool "failed to store" =<< runSBE (evalPred [True] c1)
      -- Test symbolic load succeeds under appropiate conditions.
      do cntExt <- runSBE $ applyTypedExpr (SExt Nothing 1 cnt ptrWidth)
         rptr <- runSBE $ applyTypedExpr (PtrAdd ptr cntExt)
         (c2, _) <- mmLoad m2 rptr 1 0
         HU.assertBool "failed to load" =<< runSBE (evalPred [False] c2)
  , mmTest "mergeTest" False $ \_lc g sbe@SBE { .. } MemModel { .. } m0 -> do
      let tTrue = sbeTruePred
      -- Allocate space on stack.
      cnt <- runSBE $ termInt sbe 8 1
      m0' <- mmRecordBranch m0
      AResult c0 ptr m1 <- mmStackAlloc m0' 1 cnt 0
      HU.assertBool "failed to stack allocate" (c0 == tTrue)
      -- Store bytes
      let lvi w x = V.fromList $ [AIG.bvFromInteger g w x]
      (ct, mt) <- mmStore m1 ptr (lvi 8 1) 0
      HU.assertBool "failed to store" =<< runSBE (evalPred [] ct)
      (cf, mf) <- mmStore m1 ptr (lvi 8 0) 0
      HU.assertBool "failed to store, 2" =<< runSBE (evalPred [] cf)
      -- Merge
      cond0 <- runSBE $ freshInt 1
      cond <- runSBE $ applyIEq 1 cond0 =<< termInt sbe 1 1
      m2 <- mmMux cond mt mf
      -- Check result of merge
      (c2, bytes) <- mmLoad m2 ptr 1 0
      HU.assertEqual "bytes loaded" (V.length bytes) 1
      let v = bytes V.! 0
      HU.assertBool "failed to load" (c2 == tTrue)
      v1 <- AIG.evaluate (AIG.Network g (AIG.bvToList v)) [False]
      v2 <- AIG.evaluate (AIG.Network g (AIG.bvToList v)) [True]
      HU.assertEqual "unexpected value for v1" (bfi 8 0) v1
      HU.assertEqual "unexpected value for v2" (bfi 8 1) v2
  ]
 where runSBE = liftSBEBitBlast
