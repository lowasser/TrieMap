{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Data.TrieMap.UnionMap.Zippable () where

import Data.TrieMap.UnionMap.Base

import Prelude hiding ((^))

instance (Zippable (TrieMap k1), Zippable (TrieMap k2), TrieKey k1, TrieKey k2) => Zippable (TrieMap (Either k1 k2)) where
  empty = Empty
  
  clear hole = case hView hole of
	  Hole1 h1 m2	-> clearM h1 ^ m2
	  Hole2 m1 h2	-> m1 ^ clearM h2
  assign v hole = case hView hole of
	  Hole1 h1 m2	-> Just (assign v h1) ^ m2
	  Hole2 m1 h2	-> m1 ^ Just (assign v h2)