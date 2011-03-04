{-# LANGUAGE ScopedTypeVariables, UnboxedTuples #-}
module Data.TrieMap.TrieKey.Projection.Tests (tests) where

import Data.TrieMap.Sized
import Data.TrieMap.TrieKey
import qualified Data.Map as M

import Data.TrieMap.TrieKey.Tests.Utils
import Test.QuickCheck

testMapMaybe :: forall k . (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
testMapMaybe test _ = testOp (test ++ "/Projection/MapMaybe")
  (mapMaybe (\ x@(Assoc (_ :: k) a) -> if even a then Just x else Nothing)) (M.filter even)

testMapEither :: forall k . (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
testMapEither test _ = testOp (test ++ "/Projection/MapEither")
  (\ m -> case mapEither (\ x@(Assoc (_ :: k) a) -> if even a then (# Just x, Nothing #) else (# Nothing, Just x #)) m of
    (# mL, _ #) -> mL) (fst .  M.partition even)

tests :: (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
tests test k = testMapMaybe test k .&&. testMapEither test k