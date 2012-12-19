{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
module Tests.MemModel where

import Data.Bits
import Data.Int
import qualified Data.Vector.Storable as LV
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Data.LLVM.Memory
import Data.LLVM.TargetData
import Verifier.LLVM.BitBlastBackend
import qualified Text.LLVM.AST as LLVM
import Verinf.Symbolic (createBitEngine)
import Verinf.Symbolic.Lit
import Verinf.Symbolic.Lit.Functional

mmTest :: String
       -> Int
       -> (forall mem l . (Eq l, LV.Storable l)
            => LLVMContext
            -> BitEngine l
            -> SBE (BitIO mem l)
            -> BitBlastMemModel mem l
            -> mem
            -> PropertyM IO ()) 
       -> (Args, Property)
mmTest testName n fn = 
  ( stdArgs {maxSuccess = n}
  , label testName $ monadicIO $ do
      let lc  = buildLLVMContext
              (error "LLVM Context has no ident -> type relation defined")
              []
      let mg = MemGeom { 
                   mgStack = (0x10,0x0)
                 , mgCode = (0x0,0x0)
                 , mgData = (0x0, 0x0)
                 , mgHeap = (0x0,0x0)
                 }
      be <- run $ createBitEngine
      (mm, mem) <- run $ createDagMemModel lc be mg
      fn lc be (sbeBitBlast lc be mm) mm mem
      run $ beFree be)


intType :: Int32 -> LLVM.Type
intType w = LLVM.PrimType (LLVM.Integer w)

-- | Generate bit vector from integer.
bfi :: Int -> Integer -> LV.Vector Bool
bfi w v = LV.generate w (testBit v)

memModelTests :: [(Args, Property)]
memModelTests =
  [ mmTest "symbolicTests" 1 $ \lc be SBE { .. } MemModel { .. } m0 -> do
      let ptrWidth = llvmAddrWidthBits lc
      let ?be = be
      let bytes = lVectorFromInt 8 7
      let assertEval inputs expected t = do
            r <- runSBE $ evalAiger inputs t
            assert (r == expected)
      tTrue <- runSBE $ termBool True
      tFalse <- runSBE $ termBool False
      cnt <- runSBE $ freshInt 1
      -- Try allocating symbolic ammount.
      SAResult c0 ptr m1 <- run $ mmStackAlloca m0 1 cnt 0
      assert (c0 == tTrue)
      -- Try filling up rest of memory and testing stack allocation
      -- failed.
      do fill <- runSBE $ termInt 4 0x0F
         SAResult c0' _ _ <- run $ mmStackAlloca m1 1 fill 1
         assertEval [False] tTrue c0'
         assertEval [True] tFalse c0'
      -- Test store to concrete address succeeds.
      (c1, m2) <- run $ mmStore m1 bytes ptr
      do assertEval [False] tFalse c1
         assertEval [True] tTrue  c1
      -- Test symbolic load succeeds under appropiate conditions.
      do cntExt <- runSBE $ applySExt 1 ptrWidth cnt
         rptr <- runSBE $ applyArith (LLVM.Sub False False) ptrWidth ptr cntExt
         (c2, _) <- run $ mmLoad m2 rptr 1 
         assertEval [False] tTrue c2
         assertEval [True] tFalse c2
  , mmTest "mergeTest" 1 $ \_lc be SBE { .. } MemModel { .. } m0 -> do
      let ?be = be
      let assertEval inputs expected t = do
            r <- runSBE $ evalAiger inputs t
            assert (r == expected)
      tTrue <- runSBE $ termBool True
      -- Allocate space on stack.
      cnt <- runSBE $ termInt 8 1
      SAResult c0 ptr m1 <- run $ mmStackAlloca m0 1 cnt 0
      assert (c0 == tTrue)
      -- Store bytes
      let lvi = lVectorFromInt
      (ct, mt) <- run $ mmStore m1 (lvi 8 1) ptr
      assertEval [] tTrue ct
      (cf, mf) <- run $ mmStore m1 (lvi 8 0) ptr
      assertEval [] tTrue cf
      -- Merge
      cond <- runSBE $ freshInt 1
      m2 <- run $ mmMux cond mt mf
      -- Check result of merge
      (c2, v) <- run $ mmLoad m2 ptr 1 
      assert (c2 == tTrue)
      v1 <- run $ lEvalAig (LV.fromList [False]) v
      v2 <- run $ lEvalAig (LV.fromList [True ]) v
      assert (v1 == bfi 8 0)
      assert (v2 == bfi 8 1)
  ] 
 where runSBE = run . liftSBEBitBlast
