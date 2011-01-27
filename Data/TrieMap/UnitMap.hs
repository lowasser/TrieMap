{-# LANGUAGE TypeFamilies, UnboxedTuples, MagicHash #-}

module Data.TrieMap.UnitMap where

import Data.TrieMap.TrieKey
import Data.TrieMap.Sized

import Control.Applicative
import Control.Monad

import Data.Foldable
import Data.Traversable
import Data.Maybe

import Prelude hiding (foldr, foldl)

-- | @'TrieMap' () a@ is implemented as @'Maybe' a@.
instance TrieKey () where
	_ =? _ = True
	_ `cmp` _ = EQ
  
	newtype TrieMap () a = Unit {getUnit :: Maybe a}
	data Hole () a = Hole
	
	emptyM = Unit Nothing
	singletonM _ = Unit . Just
	getSimpleM (Unit m) = maybe Null Singleton m
	sizeM (Unit (Just a)) = getSize# a
	sizeM _ = 0#
	lookupM _ (Unit m) = m
	traverseM f (Unit m) = Unit <$> traverse f m
	foldrM f (Unit m) z = foldr f z m
	foldlM f (Unit m) z = foldl f z m
	fmapM f (Unit m) = Unit (f <$> m)
	mapMaybeM f (Unit m) = Unit (m >>= f)
	mapEitherM f (Unit a) = both Unit Unit (mapEitherMaybe f) a
	unionM f (Unit m1) (Unit m2) = Unit (unionMaybe f m1 m2)
	isectM f (Unit m1) (Unit m2) = Unit (isectMaybe f m1 m2)
	diffM f (Unit m1) (Unit m2) = Unit (diffMaybe f m1 m2)
	isSubmapM (<=) (Unit m1) (Unit m2) = subMaybe (<=) m1 m2
	fromListM _ [] = Unit Nothing
	fromListM f ((_, v):xs) = Unit $ Just (foldl (\ v' -> f v' . snd) v xs)
	
	singleHoleM _ = Hole
	beforeM a _ = Unit a
	afterM a _ = Unit a
	searchM _ (Unit m) = (# m, Hole #)

	indexM i (Unit (Just v)) = (# i, v, Hole #)
	indexM _ _ = indexFail ()
	
	unifyM _ _ _ _ = Left Hole
	
	extractHoleM (Unit (Just v)) = return (v, Hole)
	extractHoleM _ = mzero
	
	assignM v _ = Unit v