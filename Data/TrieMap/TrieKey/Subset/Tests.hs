{-# LANGUAGE ScopedTypeVariables, ImplicitParams #-}
module Data.TrieMap.TrieKey.Subset.Tests where

import Data.TrieMap.TrieKey

import qualified Data.Map as M

import Data.TrieMap.TrieKey.Tests.Utils
import Test.QuickCheck

tests :: forall k . (TrieKey k, Arbitrary k, Show k) => String -> k -> Property
tests test _ = printTestCase (test ++ "/Subset") $ \ x1 x2 ->
  ((fromModel x1 :: TMap k Int) <<=? (fromModel x2 :: TMap k Int)) ==
    M.isSubmapOfBy ?le (fromModel x1) (fromModel x2)
  where ?le = \ _ _ -> True