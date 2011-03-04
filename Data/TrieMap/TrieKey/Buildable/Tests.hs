{-# LANGUAGE ScopedTypeVariables #-}
module Data.TrieMap.TrieKey.Buildable.Tests (tests) where

import Data.TrieMap.Sized
import Data.TrieMap.TrieKey

import qualified Data.Map as M
import Data.List (sortBy)
import Data.Ord

import Data.TrieMap.TrieKey.Tests.Utils
import Test.QuickCheck

testUFold :: forall k . (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
testUFold test _ = testOp (test ++ "/Buildable/UFold") (id :: TOp k Int) id

testAFold :: forall k . (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
testAFold test _ = printTestCase (test ++ "/Buildable/AFold") $ \ (Ordered (xs :: [(k, Int)])) ->
  toModel (runFoldl (aFold f) [(k, Assoc k a) | (k, a) <- xs] :: TrieMap k (Assoc k Int)) == 
  toModel (M.fromAscListWith (+) xs)
  where f (Assoc k a1) (Assoc _ a2) = Assoc k (a1 + a2)

newtype DistinctAsc k a = DA [(k, a)] deriving (Show)

daify :: Ord k => [(k, a)] -> [(k, a)]
daify xs = distinctify (sortBy (comparing fst) xs) where
  distinctify (x@(k1,_):xs@((k2,_):_))
    | k1 == k2	= distinctify xs
    | otherwise	= x:distinctify xs
  distinctify xs = xs

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (DistinctAsc k a) where
  arbitrary = fmap (DA . daify) arbitrary
  shrink (DA xs) = fmap (DA . daify) (shrink xs)

testDAFold :: forall k . (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
testDAFold test _ = printTestCase (test ++ "/Buildable/DAFold") $ \ (DA xs) ->
  toModel (fromModel xs :: TMap k Int) == toModel (M.fromDistinctAscList xs)

tests :: (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
tests test k = conjoin [t test k | t <- [testUFold, testAFold, testDAFold]]