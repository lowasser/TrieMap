{-# LANGUAGE ScopedTypeVariables #-}
module Data.TrieMap.TrieKey.Buildable.Tests (tests) where

import Data.TrieMap.Sized
import Data.TrieMap.TrieKey

import qualified Data.Map as M

import Data.TrieMap.TrieKey.Tests.Utils
import Test.QuickCheck

testUFold :: forall k . (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
testUFold test _ = testOp (test ++ "/Buildable/UFold") (id :: TOp k Int) id

testAFold :: forall k . (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
testAFold test _ = printTestCase (test ++ "/Buildable/AFold") $ \ (Ordered (xs :: [(k, Int)])) ->
  toModel (runFoldl (aFold f) [(k, Assoc k a) | (k, a) <- xs] :: TrieMap k (Assoc k Int)) == 
  toModel (M.fromAscListWith (+) xs)
  where f (Assoc k a1) (Assoc _ a2) = Assoc k (a1 + a2)

tests :: (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
tests test k = conjoin [testUFold test k, testAFold test k]