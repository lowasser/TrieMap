{-# LANGUAGE ScopedTypeVariables #-}
module Data.TrieMap.TrieKey.Tests (tests) where

import Control.Monad.Option

import Data.TrieMap.Sized
import Data.TrieMap.TrieKey

import qualified Data.Map as M

import Data.TrieMap.TrieKey.Tests.Utils

import qualified Data.TrieMap.TrieKey.Subset.Tests as SubsetTests
import qualified Data.TrieMap.TrieKey.Buildable.Tests as BuildTests
import qualified Data.TrieMap.TrieKey.SetOp.Tests as SetOpTests
import qualified Data.TrieMap.TrieKey.Projection.Tests as ProjTests

import Test.QuickCheck
import Test.QuickCheck.Function
import Prelude hiding (foldr, foldl)

testSize :: forall k . (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
testSize test _ = testQuery (test ++ "/TrieKey/Size")
  (sizeM :: TMap k Int -> Int) M.size

testIndex :: forall k . (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
testIndex test _ = printTestCase (test ++ "/TrieKey/Index") $ \ (NonNegative i) (NonEmpty (xs :: [(k, Int)])) ->
  let tm = fromModel xs :: TMap k Int; m = fromModel xs; i' = i `rem` M.size m in
  case indexM' tm i' of
    (_, Assoc k1 a1, hole) -> let m = fromModel xs in case (M.elemAt i' m, M.deleteAt i' m) of
      ((k2, a2), m') -> k1 == k2 && a1 == a2 && toModel (clearM hole) == toModel m'

testAlter :: forall k . (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
testAlter test _ = property $ \ (k :: k) r0 -> let
  g Nothing = r0
  g (Just a) = if odd a then Just (a * a) else Nothing
  f = fmap (Assoc k) . g . fmap getValue
  in testOp (test ++ "/TrieKey/Alter") (alterM f k) (M.alter g k)

testLookup :: forall k . (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
testLookup test _ = property $ \ (k :: k) -> testQuery (test ++ "/TrieKey/Lookup")
  (fmap getValue . liftOption . lookupMC k) (M.lookup k)

testSearchLookup :: forall k . (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
testSearchLookup test _ = printTestCase (test ++ "/TrieKey/Search-Lookup") $ \ k (xs :: [(k, Int)]) -> 
  let m = fromModel xs :: TMap k Int in liftOption (lookupMC k m) == searchMC k m (const Nothing) (const . Just)

testSearchAssignClear :: forall k . (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
testSearchAssignClear test _ = printTestCase (test ++ "/TrieKey/Search-Assign-Clear") $ \ k (xs :: [(k, Int)]) ->
  toModel (searchMC k (fromModel xs :: TMap k Int) clearM assignM) == toModel (fromModel xs :: TMap k Int)

testExtractHole :: forall k . (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
testExtractHole test _ = testQuery (test ++ "/TrieKey/ExtractHole")
  extractHoleTMap (extractHoleList . (toModel :: M.Map k a -> [(k, a)]))

extractHoleTMap :: TrieKey k => TMap k a -> [(k, a, [(k, a)])]
extractHoleTMap m = do
  (Assoc k a, hole) <- extractHoleM m
  return (k, a, toModel (clearM hole))

extractHoleList :: [(k, a)] -> [(k, a, [(k, a)])]
extractHoleList [] = []
extractHoleList [(k, a)] = [(k, a, [])]
extractHoleList (x@(k,a):xs) = (k, a, xs) : [(k', a', x:xs') | (k', a', xs') <- extractHoleList xs]

testFoldr :: forall k . (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
testFoldr test _ = testQuery (test ++ "/Foldable/Foldr")
  (\ (tm :: TMap k Int) -> foldr (\ (Assoc k a) xs -> (k, a):xs) [] tm)
  M.assocs
  
testFoldl :: forall k . (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
testFoldl test _ = testQuery (test ++ "/Foldable/Foldl")
  (\ (tm :: TMap k Int) -> foldl (\ xs (Assoc k a) -> (k, a):xs) [] tm)
  (M.foldlWithKey (\ xs k a -> (k, a):xs) [])

testFoldMap :: forall k . (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
testFoldMap test _ = testQuery (test ++ "/Foldable/Foldl")
  (\ (tm :: TMap k Int) -> foldMap (\ (Assoc k a) -> [(k, a)]) tm)
  M.assocs

testInsertWith :: forall k . (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
testInsertWith test _ = property $ \ (k :: k) (fun :: Fun (Int, Int) Int) (a :: Int) -> testOp (test ++ "/TrieKey/InsertWith")
  (insertWithM (\ (Assoc _ b) -> Assoc k $ apply fun (a, b)) k (Assoc k a) :: TOp k Int)
  (M.insertWith (curry $ apply fun) k a)

testSplit :: forall k . (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
testSplit test _ = printTestCase (test ++ "/TrieKey/SplitLookup") $ \ k (xs :: [(k, Int)]) ->
  case M.splitLookup k (fromModel xs) of
    (lM, xM, rM) -> case searchMC k (fromModel xs :: TMap k Int) (\ hole -> (beforeM hole, Nothing, afterM hole))
	(\ (Assoc _ a) hole -> (beforeM hole, Just a, afterM hole)) of
      (lT, xT, rT) -> conjoin [
	printTestCase "Exact" $ expect xM xT,
	printTestCase "Less" $ expect (toModel lM) (toModel lT),
	printTestCase "Greater" $ expect (toModel rM) (toModel rT)]

testBeforeWith :: forall k . (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
testBeforeWith test _ = printTestCase (test ++ "/TrieKey/BeforeWith") $ \ k a0 xs -> let a = Assoc k a0 in
  let hole = searchMC k (fromModel xs :: TMap k Int) id (const id) in
    toModel (insertWithM id k a (beforeM hole)) == toModel (beforeWithM a hole)

testAfterWith :: forall k . (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
testAfterWith test _ = printTestCase (test ++ "/TrieKey/AfterWith") $ \ k a0 xs -> let a = Assoc k a0 in
  let hole = searchMC k (fromModel xs :: TMap k Int) id (const id) in
    toModel (insertWithM id k a (afterM hole)) == toModel (afterWithM a hole)

testSimple :: forall k . (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
testSimple test _ = testQuery (test ++ "/TrieKey/Simple")
  (\ (m :: TMap k Int) -> do
      Assoc k a <- getSimpleM m
      return (k, a))
  (\ m -> case M.assocs m of
      [] -> Null
      [(k, a)] -> Singleton (k, a)
      _	-> NonSimple)

expect :: (Eq a, Show a) => a -> a -> Property
expect expected actual = printTestCase ("Expected: " ++ show expected ++ "\nActual: " ++ show actual) (expected == actual)

tests :: (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
tests test k = conjoin [t test k | t <- 
  [SubsetTests.tests, BuildTests.tests, SetOpTests.tests, ProjTests.tests, testSearchAssignClear, testSplit,
    testSearchLookup, testLookup, testAlter, testSize, testExtractHole, testIndex, testInsertWith, testBeforeWith,
    testAfterWith, testSimple, testFoldr, testFoldl, testFoldMap]]