{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, TypeSynonymInstances #-}
module Data.TrieMap.TrieKey.Tests.Utils where

import Data.TrieMap.Sized
import Data.TrieMap.TrieKey
import qualified Data.Map as M
import qualified Data.Foldable as Fold

import Test.QuickCheck

type TMap k a = TrieMap k (Assoc k a)
type TQuery k a r = TMap k a -> r
type TOp k a = TMap k a -> TMap k a
type MQuery k a r = M.Map k a -> r
type MOp k a = M.Map k a -> M.Map k a

class Model f m | f -> m where
  toModel :: f -> m
  fromModel :: m -> f

instance TrieKey k => Model (TMap k a) [(k, a)] where
  toModel tm = [(k, a) | Assoc k a <- Fold.toList tm]
  fromModel xs = runFoldl (uFold const) [(k, Assoc k a) | (k, a) <- xs]

instance Ord k => Model (M.Map k a) [(k, a)] where
  toModel m = M.assocs m
  fromModel xs = M.fromList xs

testQuery :: (TrieKey k, Arbitrary k, Show k, Eq r) => 
  String -> TQuery k Int r -> MQuery k Int r -> Property
testQuery test queryTrie queryMap = printTestCase test $ \ xs ->
  queryTrie (fromModel xs) == queryMap (fromModel xs)

testOp :: (TrieKey k, Arbitrary k, Show k) =>
  String -> TOp k Int -> MOp k Int -> Property
testOp test opTrie opMap = printTestCase test $ \ xs ->
  toModel (opTrie (fromModel xs)) == toModel (opMap (fromModel xs))