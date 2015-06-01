{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tests.MemModel
  ( TestCase
  , runTestCase
  , memModelTests
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Lens
import qualified Data.Vector as V
import System.Random

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Verifier.LLVM.MemModel.Common

newtype CheckedNum a = CN { unCN :: a }
  deriving (Random, Show)

instance (Bounded a,Integral a) => Num (CheckedNum a) where
  CN x + CN y
    | y > 0 && x > maxBound - y = CN maxBound
    | y < 0 && x < minBound - y = CN minBound
    | otherwise = CN (x + y)
  CN x - CN y
    | y < 0 && x > maxBound + y = CN maxBound
    | y > 0 && x < minBound + y = CN minBound
    | otherwise = CN (x - y)
  CN x * CN y = fromInteger (toInteger x * toInteger y)
  abs (CN x) = fromInteger (abs (toInteger x))
  signum (CN x) = CN (signum x)
  fromInteger x = CN r
    where minB :: Bounded a => a -> a
          minB _ = minBound
          maxB :: Bounded a => a -> a
          maxB _ = maxBound
          r = fromInteger (max (toInteger (minB r))
                               (min (toInteger (maxB r)) x))

-- | Make a random value greater than the input, but not too large.
genBoundedSucc :: Size -> Gen Size
genBoundedSucc l = (l+) <$> choose (0, min (maxBound - l) ms)
  where ms = 20 -- Maximum size of range for quickcheck purposes.

shrinkNonzeroSize :: Size -> [Size]
shrinkNonzeroSize s = filter (/= 0) (shrinkIntegral s)

shrinkNonemptyList :: (a -> [a]) -> [a] -> [[a]]
shrinkNonemptyList f l = filter (not . null) (shrinkList f l)


addrNearby :: Addr -> Size -> Gen Addr
addrNearby l s = unCN <$> choose (CN l - CN s, CN l + CN s)

instance Arbitrary Range where
  arbitrary = do
    l <- arbitrary
    R l <$> genBoundedSucc l

-- | @rangeNearby l s@ returns a range starting in set [l-s, l+s),
-- and with length up to 2*s.
rangeNearby :: Size -> Size -> Gen Range
rangeNearby l s = do
  o <- addrNearby l s
  R o <$> (unCN <$> choose (CN o, CN o + 2*CN s))

instance Arbitrary Type where
  arbitrary = frequency [ (4, arbBV)
                        , (1, return floatType)
                        , (1, return doubleType)
                        , (1, arbArray)
                        , (1, arbStruct) ]
    where arbBV     = bitvectorType <$> choose (1,4)
          arbArray  = arrayType <$> choose (1,4) <*> arbitrary
          arbStruct = do
             n <- choose (1,4)
             let fldFn = (,) <$> arbitrary <*> choose (0,4)
             mkStruct <$> V.replicateM n fldFn
  shrink t =
    case typeF t of
      Bitvector s -> bitvectorType <$> shrinkNonzeroSize s
      Float -> []
      Double -> []
      Array n v -> arrayType <$> shrinkNonzeroSize n <*> shrink v
      Struct v -> do
        let fldPair f = (f^.fieldVal, fieldPad f)
            fldFn (tp,p) = (,) <$> shrink tp <*> shrinkIntegral p
        mkStruct . V.fromList <$> shrinkNonemptyList fldFn (V.toList (fldPair <$> v))

instance Arbitrary BasePreference where
  arbitrary = do
    i <- choose (0,2)
    return $ case (i::Int) of
               0 -> FixedLoad
               1 -> FixedStore
               _ -> NeitherFixed

printEqn :: (Show a, Testable prop) => String -> a -> prop -> Property
printEqn nm x = counterexample (nm ++ " = " ++ show x)

forArbitraryVar :: (Arbitrary a, Show a, Testable prop) =>
                   String -> (a -> prop) -> Gen Property
forArbitraryVar nm prop = do
  x <- arbitrary
  return $ shrinking shrink x $ \y ->
    printEqn nm y (prop y)

-- | Generates a named variable that will be printed if the test case fails.
forVar :: (Show a, Testable prop) =>
          String -> Gen a -> (a -> prop) -> Gen Property
forVar nm gen prop = gen >>= \v -> return (printEqn nm v (prop v))

type TestCase = (String, Property)

runTestCase :: TestCase -> IO ()
runTestCase (nm,p) = do
  putStr ("Running " ++ nm ++ "... ")
  quickCheckWith stdArgs p

mkTestCase :: Testable prop => String -> prop -> TestCase
mkTestCase nm p = (nm, property p)

rangeLoadTestCase :: Testable prop =>
                     String
                  -> (Size
                      -> Type
                      -> Range
                      -> (ValueCtor (RangeLoad Addr) -> Property)
                      -> prop)
                  -> TestCase
rangeLoadTestCase nm fn = mkTestCase nm $
  forArbitraryVar "l" $ \l ->
  forArbitraryVar "tp" $ \tp ->
  forVar "s" (rangeNearby l (typeSize tp)) $ \s ->
  fn l tp s (checkRangeValueLoad l tp s)

testMaybeEq :: (Eq v, Show v, Testable prop)
            => String
            -> Maybe v
            -> v
            -> prop
            -> Property
testMaybeEq nm Nothing _ prop =
  counterexample ("Missing " ++ show nm) prop
testMaybeEq nm (Just v) expected prop
  | v == expected = property prop
  | otherwise = printEqn nm v False

-- | Checks that the a value created has the correct type,
-- all imports are in increasing order, and have correctly
-- recorded whether they are in the store range.
checkRangeValueLoad :: Addr -> Type -> Range -> ValueCtor (RangeLoad Addr)
                    -> Property
checkRangeValueLoad lo ltp s v =
    testMaybeEq "Return type" (typeOfValue (Just . rangeLoadType) v) ltp $
    checkReads lo (valueImports v)
  where le = typeEnd lo ltp
        checkReads po [] = po <= le
        checkReads po (OutOfRange o tp:r) =
            po <= o && isDisjoint (R o e) s && checkReads e r
          where e = typeEnd o tp
        checkReads po (InRange o tp:r) =
            (po <= rStart s + o) && e <= _rEnd s && checkReads e r
          where e = rStart s + typeEnd o tp

testRangeLoad :: TestCase
testRangeLoad = rangeLoadTestCase "rangeLoad" $ \l tp s p ->
  p $ adjustOffset fromIntegral fromIntegral <$> rangeLoad l tp s

testFixedOffsetRangeLoad :: TestCase
testFixedOffsetRangeLoad =
  rangeLoadTestCase "fixedOffsetRangeLoad" $ \l tp s p -> do
    let ec = evalContext l tp s
    p $ eval ec (fixedOffsetRangeLoad l tp (rStart s))

testFixedSizeRangeLoad :: TestCase
testFixedSizeRangeLoad =
  rangeLoadTestCase "fixedSizeRangeLoad" $ \l tp s p -> do
    forArbitraryVar "pref" $ \pref -> do
    let ec = evalContext l tp s
    p $ fmap (fromInteger . evalV ec) <$> eval ec (fixedSizeRangeLoad pref tp (rSize s))

testSymbolicRangeLoad :: TestCase
testSymbolicRangeLoad =
  rangeLoadTestCase "symbolicRangeLoad" $ \l tp s p ->
    forArbitraryVar "pref" $ \pref -> do
    let ec = evalContext l tp s
    p $ fmap (fromInteger . evalV ec) <$> eval ec (symbolicRangeLoad pref tp)

valueLoadTestCase :: Testable prop =>
                     String
                  -> (Addr
                      -> Type
                      -> Addr
                      -> Type
                      -> (ValueCtor (ValueLoad Addr) -> Property)
                      -> prop)
                  -> TestCase
valueLoadTestCase nm f = mkTestCase nm $
  forArbitraryVar "lo" $ \lo ->
  forArbitraryVar "ltp" $ \ltp ->
  forArbitraryVar "so" $ \so ->
  forArbitraryVar "stp" $ \stp -> do
  f lo ltp so stp $ \v ->
    counterexample ("result = " ++ show v) $
    property $ checkValueLoad lo ltp so stp v

checkValueLoad :: Addr -> Type -> Addr -> Type -> ValueCtor (ValueLoad Addr)
               -> Bool
checkValueLoad _ ltp _ _ v =
    Just ltp == typeOfValue valueLoadType v

testValueLoad :: TestCase
testValueLoad = valueLoadTestCase "valueLoad" $ \l ltp s stp p ->
  p $ valueLoad l ltp s (Var stp)

testSymbolicValueLoad :: TestCase
testSymbolicValueLoad = valueLoadTestCase "symbolicValueLoad" $ \l ltp s stp p ->
  forArbitraryVar "pref" $ \pref -> do
  let ec = evalContext l ltp (R s (s + typeSize stp))
  let v = eval ec (symbolicValueLoad pref ltp (Var stp))
  p (fmap (fromInteger . evalV ec) <$> v)


testCases :: [TestCase]
testCases =
  [ testRangeLoad
  , testFixedOffsetRangeLoad
  , testFixedSizeRangeLoad
  , testSymbolicRangeLoad

  , testValueLoad
  , testSymbolicValueLoad
  ]

memModelTests :: [TestTree]
memModelTests = fn <$> testCases
 where fn (nm,p) = testProperty nm p
