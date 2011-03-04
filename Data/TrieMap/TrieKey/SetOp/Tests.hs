{-# LANGUAGE ScopedTypeVariables #-}
module Data.TrieMap.TrieKey.SetOp.Tests (tests) where

import Data.TrieMap.TrieKey

import qualified Data.Map as M

import Data.TrieMap.TrieKey.Tests.Utils
import Test.QuickCheck

testUnion :: forall k . (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
testUnion test _ = printTestCase (test ++ "/SetOp/Union") $ \ xs1 xs2 ->
  toModel (union f (fromModel xs1 :: TMap k Int) (fromModel xs2)) == toModel (M.unionWith (+) (fromModel xs1) (fromModel xs2))
  where f (Assoc k a1) (Assoc _ a2) = Just $ Assoc k (a1 + a2)

testIsect :: forall k . (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
testIsect test _ = printTestCase (test ++ "/SetOp/Isect") $ \ xs1 xs2 ->
  toModel (isect f (fromModel xs1 :: TMap k Int) (fromModel xs2 :: TMap k Int)) == 
    toModel (M.intersectionWith (*) (fromModel xs1) (fromModel xs2))
  where f (Assoc k a1) (Assoc _ a2) = Just $ Assoc k (a1 * a2)

testDiff :: forall k . (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
testDiff test _ = printTestCase (test ++ "/SetOp/Diff") $ \ xs1 xs2 ->
  toModel (diff f (fromModel xs1 :: TMap k Int) (fromModel xs2 :: TMap k Int)) == 
    toModel (M.differenceWith g (fromModel xs1) (fromModel xs2))
  where f (Assoc k a1) (Assoc _ a2) = fmap (Assoc k) (g a1 a2)
	g a1 a2 = if even (a1 + a2) then Just (a1 + a2) else Nothing

tests :: (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
tests test k = conjoin [testUnion test k, testIsect test k, testDiff test k]