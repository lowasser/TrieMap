{-# LANGUAGE TypeSynonymInstances #-}
module Data.TrieMap.OrdMap.Subset () where

import Data.TrieMap.OrdMap.Base
import Data.TrieMap.OrdMap.Traversable ()
import Data.TrieMap.OrdMap.Splittable ()
import Data.TrieMap.OrdMap.Searchable ()

import Data.Functor.Immoral

instance Ord k => Subset (SNode k) where
  t1 <=? t2 = castMap t1 `subMap` castMap t2 where
    t1 `subMap` t2 = case ((node :: SNode k (Elem a) -> Node k (Elem a)) t1, node t2) of
      (Tip, _)	-> True
      (_, Tip)	-> False
      (Bin kx x l r, _) -> case splitLookup kx t2 of
	(_, Nothing, _)	-> False
	(tl, Just y, tr) -> x <=? y && l `subMap` tl && r `subMap` tr

instance Ord k => Subset (OrdMap k) where
  OrdMap m1 <=? OrdMap m2 = m1 <=? m2