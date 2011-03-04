{-# LANGUAGE ScopedTypeVariables #-}
module Data.TrieMap.TrieKey.Tests (tests) where

import Control.Monad.Option
import Control.Monad.Ends

import Data.TrieMap.Sized
import Data.TrieMap.TrieKey

import qualified Data.Map as M

import Data.TrieMap.TrieKey.Tests.Utils

import qualified Data.TrieMap.TrieKey.Subset.Tests as SubsetTests
import qualified Data.TrieMap.TrieKey.Buildable.Tests as BuildTests
import qualified Data.TrieMap.TrieKey.SetOp.Tests as SetOpTests
import qualified Data.TrieMap.TrieKey.Projection.Tests as ProjTests

import Test.QuickCheck

testSize :: forall k . (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
testSize test _ = testQuery (test ++ "/TrieKey/Size")
  (sizeM :: TMap k Int -> Int) M.size

testAlter :: forall k . (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
testAlter test _ = property $ \ (k :: k) -> testOp (test ++ "/TrieKey/Alter")
  (alterM f k) (M.alter g k)
  where	g Nothing = Just 42
	g (Just a) = if odd a then Just (a * a) else Nothing
	f Nothing = Nothing
	f (Just (Assoc k a)) = fmap (Assoc k) (g (Just a))

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

tests :: (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
tests test k = conjoin [t test k | t <- 
  [SubsetTests.tests, BuildTests.tests, SetOpTests.tests, ProjTests.tests, testSearchAssignClear,
    testSearchLookup, testLookup, testAlter, testSize, testExtractHole]]