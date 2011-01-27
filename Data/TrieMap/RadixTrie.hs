{-# LANGUAGE BangPatterns, UnboxedTuples, TypeFamilies, MagicHash, FlexibleInstances #-}

module Data.TrieMap.RadixTrie () where

import Data.TrieMap.TrieKey
import Data.TrieMap.Sized

import Data.Functor
import Control.Monad

import Foreign.Storable

import Data.Monoid
import Data.Ord
import Data.Foldable (foldr, foldl)
import Data.Vector.Generic hiding (Vector, cmp, foldl, foldr)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import Data.Traversable
import Data.Word

import Data.TrieMap.RadixTrie.Slice
import Data.TrieMap.RadixTrie.Edge

import Prelude hiding (length, and, zip, zipWith, foldr, foldl)

-- | @'TrieMap' ('Vector' k) a@ is a traditional radix trie.
instance TrieKey k => TrieKey (Vector k) where
	ks =? ls	= length ks == length ls && and (zipWith (=?) ks ls)
	ks `cmp` ls	= V.foldr (\ (k, l) z -> (k `cmp` l) `mappend` z) (comparing length ks ls) (zip ks ls)

	newtype TrieMap (Vector k) a = Radix (MEdge Vector k a)
	newtype Hole (Vector k) a = Hole (EdgeLoc Vector k a)
	
	emptyM = Radix Nothing
	singletonM ks a = Radix (Just (singletonEdge (v2S ks) a))
	getSimpleM (Radix Nothing)	= Null
	getSimpleM (Radix (Just e))	= getSimpleEdge e
	sizeM (Radix m) = getSize# m
	lookupM ks (Radix m) = m >>= lookupEdge ks

	fmapM f (Radix m) = Radix (mapEdge f <$> m)
	mapMaybeM f (Radix m) = Radix (m >>= mapMaybeEdge f)
	mapEitherM f (Radix e) = both Radix Radix (mapEitherMaybe (mapEitherEdge f)) e
	traverseM f (Radix m) = Radix <$> traverse (traverseEdge f) m

	foldrM f (Radix m) z = foldr (foldrEdge f) z m
	foldlM f (Radix m) z = foldl (foldlEdge f) z m

	unionM f (Radix m1) (Radix m2) = Radix (unionMaybe (unionEdge f) m1 m2)
	isectM f (Radix m1) (Radix m2) = Radix (isectMaybe (isectEdge f) m1 m2)
	diffM f (Radix m1) (Radix m2) = Radix (diffMaybe (diffEdge f) m1 m2)
	
	isSubmapM (<=) (Radix m1) (Radix m2) = subMaybe (isSubEdge (<=)) m1 m2

	singleHoleM ks = Hole (singleLoc (v2S ks))
	searchM ks (Radix (Just e)) = case searchEdge (v2S ks) e Root of
		(a, loc) -> (# a, Hole loc #)
	searchM ks _ = (# Nothing, singleHoleM ks #)
	indexM i (Radix (Just e)) = onThird Hole (indexEdge i e) Root
	indexM _ (Radix Nothing) = indexFail ()

	clearM (Hole loc) = Radix (clearEdge loc)
	assignM a (Hole loc) = Radix (assignEdge a loc)
	
	extractHoleM (Radix (Just e)) = do
		(a, loc) <- extractEdgeLoc e Root
		return (a, Hole loc)
	extractHoleM _ = mzero
	
	beforeM (Hole loc) = Radix (beforeEdge Nothing loc)
	beforeWithM a (Hole loc) = Radix (beforeEdge (Just a) loc)
	afterM (Hole loc) = Radix (afterEdge Nothing loc)
	afterWithM a (Hole loc) = Radix (afterEdge (Just a) loc)
	
	unifyM ks1 a1 ks2 a2 = fmap (Radix . Just) (unifyEdge (v2S ks1) a1 (v2S ks2) a2)

type WordVec = S.Vector Word

vZipWith :: (Storable a, Storable b) => (a -> b -> c) -> S.Vector a -> S.Vector b -> Vector c
vZipWith f xs ys = V.zipWith f (convert xs) (convert ys)

-- | @'TrieMap' ('S.Vector' Word) a@ is a traditional radix trie specialized for word arrays.
instance TrieKey (S.Vector Word) where
	ks =? ls	= length ks == length ls && and (vZipWith (=?) ks ls)
	ks `cmp` ls	= V.foldr (\ (k, l) z -> (k `cmp` l) `mappend` z) (comparing length ks ls) (vZipWith (,) ks ls)

	newtype TrieMap WordVec a = WRadix (MEdge S.Vector Word a)
	newtype Hole WordVec a = WHole (EdgeLoc S.Vector Word a)
	
	emptyM = WRadix Nothing
	singletonM ks a = WRadix (Just (singletonEdge (v2S ks) a))
	getSimpleM (WRadix Nothing)	= Null
	getSimpleM (WRadix (Just e))	= getSimpleEdge e
	sizeM (WRadix m) = getSize# m
	lookupM ks (WRadix m) = m >>= lookupEdge ks

	fmapM f (WRadix m) = WRadix (mapEdge f <$> m)
	mapMaybeM f (WRadix m) = WRadix (m >>= mapMaybeEdge f)
	mapEitherM f (WRadix e) = both WRadix WRadix (mapEitherMaybe (mapEitherEdge f)) e
	traverseM f (WRadix m) = WRadix <$> traverse (traverseEdge f) m

	foldrM f (WRadix m) z = foldr (foldrEdge f) z m
	foldlM f (WRadix m) z = foldl (foldlEdge f) z m

	unionM f (WRadix m1) (WRadix m2) = WRadix (unionMaybe (unionEdge f) m1 m2)
	isectM f (WRadix m1) (WRadix m2) = WRadix (isectMaybe (isectEdge f) m1 m2)
	diffM f (WRadix m1) (WRadix m2) = WRadix (diffMaybe (diffEdge f) m1 m2)
	
	isSubmapM (<=) (WRadix m1) (WRadix m2) = subMaybe (isSubEdge (<=)) m1 m2

	singleHoleM ks = WHole (singleLoc (v2S ks))
	searchM ks (WRadix (Just e)) = case searchEdge (v2S ks) e Root of
		(a, loc) -> (# a, WHole loc #)
	searchM ks _ = (# Nothing, singleHoleM ks #)
	indexM i (WRadix (Just e)) = case indexEdge i e Root of
		(# i', a, loc #) -> (# i', a, WHole loc #)
	indexM _ (WRadix Nothing) = indexFail ()

	clearM (WHole loc) = WRadix (clearEdge loc)
	assignM a (WHole loc) = WRadix (assignEdge a loc)
	
	extractHoleM (WRadix (Just e)) = do
		(a, loc) <- extractEdgeLoc e Root
		return (a, WHole loc)
	extractHoleM _ = mzero

	beforeM (WHole loc) = WRadix (beforeEdge Nothing loc)
	beforeWithM a (WHole loc) = WRadix (beforeEdge (Just a) loc)
	afterM (WHole loc) = WRadix (afterEdge Nothing loc)
	afterWithM a (WHole loc) = WRadix (afterEdge (Just a) loc)
	
	unifyM ks1 a1 ks2 a2 = fmap (WRadix . Just) (unifyEdge (v2S ks1) a1 (v2S ks2) a2)