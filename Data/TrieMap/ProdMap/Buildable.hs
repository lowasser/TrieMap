{-# LANGUAGE RecordWildCards, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
module Data.TrieMap.ProdMap.Buildable () where

import Data.TrieMap.ProdMap.Base
import Data.TrieMap.ProdMap.Searchable ()

instance (Eq k1, Searchable (TrieMap k1) k1, Buildable (TrieMap k1) k1, Buildable (TrieMap k2) k2, TrieKey k2) => 
    Buildable (TrieMap (k1, k2)) (k1, k2) where
  type UStack (TrieMap (k1, k2)) = TrieMap (k1, k2)
  uFold = defaultUFold
  type AStack (TrieMap (k1, k2)) = Stack k1 k2 (DAMStack k1) (AMStack k2)
  aFold f = prodFold daFold (aFold f)
  type DAStack (TrieMap (k1, k2)) = Stack k1 k2 (DAMStack k1) (DAMStack k2)
  daFold = prodFold daFold daFold

prodFold :: Eq k1 => FromList z1 k1 (TrieMap k2 a) -> FromList z2 k2 a -> 
  FromList (Stack k1 k2 z1 z2) (k1, k2) a
prodFold Foldl{snoc = snoc1, begin = begin1, zero = zero1, done = done1}
	    Foldl{snoc = snoc2, begin = begin2, done = done2}
  = Foldl{zero = PMap zero1, ..}
  where	snoc (First k1 stk2) (k1', k2') a
	  | k1' == k1	= First k1 (snoc2 stk2 k2' a)
	snoc (Stack k1 stk1 stk2) (k1', k2') a
	  | k1' == k1	= Stack k1 stk1 (snoc2 stk2 k2' a)
	snoc stk (k1, k2) a = Stack k1 (collapse stk) (begin2 k2 a)
	
	collapse (First k1 stk2) = begin1 k1 (done2 stk2)
	collapse (Stack k1 stk1 stk2) = snoc1 stk1 k1 (done2 stk2)
	
	begin (k1, k2) a = First k1 (begin2 k2 a)
	
	done = PMap . done1 . collapse

data Stack k1 k2 z1 z2 a = First k1 (z2 a) | Stack k1 (z1 (TrieMap k2 a)) (z2 a)