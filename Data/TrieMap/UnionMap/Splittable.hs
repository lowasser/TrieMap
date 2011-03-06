{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Data.TrieMap.UnionMap.Splittable () where

import Data.TrieMap.UnionMap.Base
import Prelude hiding ((^))

instance (Splittable (TrieMap k1), Splittable (TrieMap k2), TrieKey k1, TrieKey k2) =>
    Splittable (TrieMap (Either k1 k2)) where
  
  before hole = case hView hole of
	  Hole1 h1 __	-> beforeG h1 ^ Nothing
	  Hole2 m1 h2	-> m1 ^ beforeG h2
  beforeWith a hole = case hView hole of
	  Hole1 h1 __	-> MapL (beforeWith a h1)
	  Hole2 m1 h2	-> m1 ^ Just (beforeWith a h2)

  after hole = case hView hole of
	  Hole1 h1 m2	-> afterG h1 ^ m2
	  Hole2 __ h2	-> Nothing ^ afterG h2
  afterWith a hole = case hView hole of
	  Hole1 h1 m2	-> Just (afterWith a h1) ^ m2
	  Hole2 __ h2	-> MapR (afterWith a h2)