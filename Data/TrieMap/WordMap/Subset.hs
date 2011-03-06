{-# LANGUAGE FlexibleInstances #-}
module Data.TrieMap.WordMap.Subset () where

import Control.Monad
import Control.Monad.Option

import Data.TrieMap.TrieKey

import Data.Word

import Data.TrieMap.WordMap.Base
import Data.TrieMap.WordMap.Searchable ()

import Prelude hiding (lookup)

instance Subset SNode where
  (<=?) = subMap where
    t1 `subMap` t2 = case (node t1, node t2) of
      (Bin p1 m1 l1 r1, Bin p2 m2 l2 r2)
	| shorter m1 m2 -> False
	| shorter m2 m1	-> match p1 p2 m2 && (if mask0 p1 m2 then t1 `subMap` l2
							    else t1 `subMap` r2)
	| otherwise	-> (p1==p2) && l1 `subMap` l2 && r1 `subMap` r2
      (Bin{}, _)	-> False
      (Tip k x, _)	-> isSome (mfilter (x <?=) (lookup k t2))
      (Nil, _)		-> True

instance Subset (TrieMap Word) where
  WordMap m1 <=? WordMap m2 = m1 <=? m2