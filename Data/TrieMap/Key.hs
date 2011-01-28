{-# LANGUAGE TypeFamilies, MagicHash #-}

module Data.TrieMap.Key () where

import Data.Functor

import Data.TrieMap.Class
import Data.TrieMap.TrieKey
import Data.TrieMap.Sized
import Data.TrieMap.Representation.Class
import Data.TrieMap.Modifiers

import GHC.Exts

keyMap :: (TKey k, Sized a) => TrieMap (Rep k) a -> TrieMap (Key k) a
keyMap m = KeyMap (sizeM m) m

-- | @'TrieMap' ('Key' k) a@ is a wrapper around a @TrieMap (Rep k) a@.
instance TKey k => TrieKey (Key k) where
	{-# SPECIALIZE instance (Repr k, TrieKey (Rep k)) => TrieKey (Key k) #-}
	Key k1 =? Key k2 = toRep k1 =? toRep k2
	Key k1 `cmp` Key k2 = toRep k1 `cmp` toRep k2
  
	data TrieMap (Key k) a = KeyMap Int# !(TrieMap (Rep k) a)
	newtype Hole (Key k) a = KeyHole (Hole (Rep k) a)
	
	emptyM = KeyMap 0# emptyM
	singletonM (Key k) a = KeyMap (getSize# a) (singletonM (toRep k) a)
	getSimpleM (KeyMap _ m) = getSimpleM m
	sizeM (KeyMap sz# _) = sz#
	lookupM (Key k) (KeyMap _ m) = lookupM (toRep k) m
	insertWithM f (Key k) a (KeyMap _ m) = keyMap (insertWithM f (toRep k) a m)
	traverseM f (KeyMap _ m) = keyMap <$> traverseM f m
	foldrM f (KeyMap _ m) = foldrM f m
	foldlM f (KeyMap _ m) = foldlM f m
	fmapM f (KeyMap _ m) = keyMap (fmapM f m)
	mapMaybeM f (KeyMap _ m) = keyMap (mapMaybeM f m)
	mapEitherM f (KeyMap _ m) = both keyMap keyMap (mapEitherM f) m
	unionM f (KeyMap _ m1) (KeyMap _ m2) = keyMap (unionM f m1 m2)
	isectM f (KeyMap _ m1) (KeyMap _ m2) = keyMap (isectM f m1 m2)
	diffM f (KeyMap _ m1) (KeyMap _ m2) = keyMap (diffM f m1 m2)
	isSubmapM (<=) (KeyMap _ m1) (KeyMap _ m2) = isSubmapM (<=) m1 m2

	singleHoleM (Key k) = KeyHole (singleHoleM (toRep k))
	beforeM (KeyHole hole) = keyMap (beforeM hole)
	beforeWithM a (KeyHole hole) = keyMap (beforeWithM a hole)
	afterM (KeyHole hole) = keyMap (afterM hole)
	afterWithM a (KeyHole hole) = keyMap (afterWithM a hole)
	searchM (Key k) (KeyMap _ m) = onSnd KeyHole (searchM (toRep k)) m
	indexM i (KeyMap _ m) = onThird KeyHole (indexM i) m
	extractHoleM (KeyMap _ m) = fmap KeyHole <$> extractHoleM m
	assignM v (KeyHole hole) = keyMap (assignM v hole)
	
	unifyM (Key k1) a1 (Key k2) a2 = keyMap <$> unifyM (toRep k1) a1 (toRep k2) a2