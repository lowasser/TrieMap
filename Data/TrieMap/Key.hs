{-# LANGUAGE TypeFamilies, MagicHash, CPP #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.TrieMap.Key () where

import Data.Functor

import Data.TrieMap.Class
import Data.TrieMap.TrieKey
import Data.TrieMap.Sized
import Data.TrieMap.Representation.Class
import Data.TrieMap.Modifiers

keyMap :: (TKey k, Sized a) => TrieMap (Rep k) a -> TrieMap (Key k) a
keyMap m = KeyMap (sizeM m) m

#define KMAP(m) KeyMap{tMap = m}

-- | @'TrieMap' ('Key' k) a@ is a wrapper around a @TrieMap (Rep k) a@.
instance TKey k => TrieKey (Key k) where
	{-# SPECIALIZE instance (Repr k, TrieKey (Rep k)) => TrieKey (Key k) #-}
	Key k1 =? Key k2 = toRep k1 =? toRep k2
	Key k1 `cmp` Key k2 = toRep k1 `cmp` toRep k2
  
	data TrieMap (Key k) a = KeyMap {sz :: !Int, tMap :: !(TrieMap (Rep k) a)}
	newtype Hole (Key k) a = KeyHole (Hole (Rep k) a)
	
	emptyM = KeyMap 0 emptyM
	singletonM (Key k) a = KeyMap (getSize a) (singletonM (toRep k) a)
	getSimpleM KMAP(m) = getSimpleM m
	sizeM = sz
	lookupM (Key k) KMAP(m) = lookupM (toRep k) m
	insertWithM f (Key k) a KMAP(m) = keyMap (insertWithM f (toRep k) a m)
	traverseM f KMAP(m) = keyMap <$> traverseM f m
	foldrM f KMAP(m) = foldrM f m
	foldlM f KMAP(m) = foldlM f m
	fmapM f KMAP(m) = keyMap (fmapM f m)
	mapMaybeM f KMAP(m) = keyMap (mapMaybeM f m)
	mapEitherM f KMAP(m) = both keyMap keyMap (mapEitherM f) m
	unionM f KMAP(m1) KMAP(m2) = keyMap (unionM f m1 m2)
	isectM f KMAP(m1) KMAP(m2) = keyMap (isectM f m1 m2)
	diffM f KMAP(m1) KMAP(m2) = keyMap (diffM f m1 m2)
	isSubmapM (<=) KMAP(m1) KMAP(m2) = isSubmapM (<=) m1 m2

	singleHoleM (Key k) = KeyHole (singleHoleM (toRep k))
	beforeM (KeyHole hole) = keyMap (beforeM hole)
	beforeWithM a (KeyHole hole) = keyMap (beforeWithM a hole)
	afterM (KeyHole hole) = keyMap (afterM hole)
	afterWithM a (KeyHole hole) = keyMap (afterWithM a hole)
	searchM (Key k) KMAP(m) = onSnd KeyHole (searchM (toRep k)) m
	indexM i KMAP(m) = onThird KeyHole (indexM i) m
	extractHoleM KMAP(m) = fmap KeyHole <$> extractHoleM m
	assignM v (KeyHole hole) = keyMap (assignM v hole)
	
	unifyM (Key k1) a1 (Key k2) a2 = keyMap <$> unifyM (toRep k1) a1 (toRep k2) a2