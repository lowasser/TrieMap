{-# LANGUAGE TypeFamilies, UnboxedTuples #-}

module Data.TrieMap.Key () where

import Control.Applicative

import Data.TrieMap.Class
import Data.TrieMap.TrieKey
import Data.TrieMap.Representation.Class
import Data.TrieMap.Modifiers

import Data.TrieMap.ProdMap()
import Data.TrieMap.UnionMap()
import Data.TrieMap.IntMap()
import Data.TrieMap.OrdMap()
import Data.TrieMap.RadixTrie()

instance TKey k => TrieKey (Key k) where
	Key k1 =? Key k2 = toRep k1 =? toRep k2
	Key k1 `cmp` Key k2 = toRep k1 `cmp` toRep k2
  
	newtype TrieMap (Key k) a = KeyMap (TrieMap (Rep k) a)
	newtype Hole (Key k) a = KeyHole (Hole (Rep k) a)
	
	emptyM = KeyMap emptyM
	singletonM (Key k) a = KeyMap (singletonM (toRep k) a)
	getSimpleM (KeyMap m) = getSimpleM m
	sizeM (KeyMap m) = sizeM m
	lookupM (Key k) (KeyMap m) = lookupM (toRep k) m
	traverseM f (KeyMap m) = KeyMap <$> traverseM f m
	foldrM f (KeyMap m) = foldrM f m
	foldlM f (KeyMap m) = foldlM f m
	fmapM f (KeyMap m) = KeyMap (fmapM f m)
	mapMaybeM f (KeyMap m) = KeyMap (mapMaybeM f m)
	mapEitherM f (KeyMap m) = both KeyMap KeyMap (mapEitherM f) m
	unionM f (KeyMap m1) (KeyMap m2) = KeyMap (unionM f m1 m2)
	isectM f (KeyMap m1) (KeyMap m2) = KeyMap (isectM f m1 m2)
	diffM f (KeyMap m1) (KeyMap m2) = KeyMap (diffM f m1 m2)
	isSubmapM (<=) (KeyMap m1) (KeyMap m2) = isSubmapM (<=) m1 m2

	singleHoleM (Key k) = KeyHole (singleHoleM (toRep k))
	beforeM a (KeyHole hole) = KeyMap (beforeM a hole)
	afterM a (KeyHole hole) = KeyMap (afterM a hole)
	searchM (Key k) (KeyMap m) = onSnd KeyHole (searchM (toRep k)) m
	indexM i (KeyMap m) = case indexM i m of
		(# i', v, hole #) -> (# i', v, KeyHole hole #)
	extractHoleM (KeyMap m) = do
		(v, hole) <- extractHoleM m
		return (v, KeyHole hole)
	assignM v (KeyHole hole) = KeyMap (assignM v hole)
	
	unifyM (Key k1) a1 (Key k2) a2 = either (Left . KeyHole) (Right . KeyMap) (unifyM (toRep k1) a1 (toRep k2) a2)