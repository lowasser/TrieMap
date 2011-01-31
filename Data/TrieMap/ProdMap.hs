{-# LANGUAGE UnboxedTuples, TupleSections, PatternGuards, TypeFamilies, FlexibleInstances #-}

module Data.TrieMap.ProdMap () where

import Data.TrieMap.Sized
import Data.TrieMap.TrieKey

import Control.Monad
import Data.Functor
import Data.Foldable hiding (foldlM, foldrM)
import Data.Monoid

import Data.Sequence ((|>))
import qualified Data.Sequence as Seq

import Prelude hiding (foldl, foldl1, foldr, foldr1)

instance (TrieKey k1, TrieKey k2) => Foldable (TrieMap (k1, k2)) where
  foldMap f (PMap m) = foldMap (foldMap f) m
  foldr f z (PMap m) = foldr (flip $ foldr f) z m
  foldl f z (PMap m) = foldl (foldl f) z m

-- | @'TrieMap' (k1, k2) a@ is implemented as a @'TrieMap' k1 ('TrieMap' k2 a)@.
instance (TrieKey k1, TrieKey k2) => TrieKey (k1, k2) where
	(k11, k12) =? (k21, k22) = k11 =? k21 && k12 =? k22
	(k11, k12) `cmp` (k21, k22) = (k11 `cmp` k21) `mappend` (k12 `cmp` k22)

	newtype TrieMap (k1, k2) a = PMap (TrieMap k1 (TrieMap k2 a))
	data Hole (k1, k2) a = PHole (Hole k1 (TrieMap k2 a)) (Hole k2 a)

	emptyM = PMap emptyM
	singletonM (k1, k2) = PMap . singletonM k1 . singletonM k2
	getSimpleM (PMap m) = getSimpleM m >>= getSimpleM
	sizeM (PMap m) = sizeM m
	lookupM (k1, k2) (PMap m) = lookupM k1 m >>= lookupM k2
	traverseM f (PMap m) = PMap <$> traverseM (traverseM f) m
	fmapM f (PMap m) = PMap (fmapM (fmapM f) m)
	mapMaybeM f (PMap m) = PMap (mapMaybeM (mapMaybeM' f) m)
	mapEitherM f (PMap m) = both PMap PMap (mapEitherM (mapEitherM' f)) m
	isSubmapM (<=) (PMap m1) (PMap m2) = isSubmapM (isSubmapM (<=)) m1 m2
	unionM f (PMap m1) (PMap m2) = PMap (unionM (unionM' f) m1 m2)
	isectM f (PMap m1) (PMap m2) = PMap (isectM (isectM' f) m1 m2)
	diffM f (PMap m1) (PMap m2) = PMap (diffM (diffM' f) m1 m2)
	fromAscListM f xs = PMap (fromDistAscListM
		[(a, fromAscListM f ys) | (a, Elem ys) <- breakFst xs])
	fromDistAscListM xs = PMap (fromDistAscListM
		[(a, fromDistAscListM ys) | (a, Elem ys) <- breakFst xs])

	singleHoleM (k1, k2) = PHole (singleHoleM k1) (singleHoleM k2)
	beforeM (PHole hole1 hole2) = PMap (beforeMM (gNull beforeM hole2) hole1)
	beforeWithM a (PHole hole1 hole2) = PMap (beforeWithM (beforeWithM a hole2) hole1)
	afterM (PHole hole1 hole2) = PMap (afterMM (gNull afterM hole2) hole1)
	afterWithM a (PHole hole1 hole2) = PMap (afterWithM (afterWithM a hole2) hole1)
	searchM (k1, k2) (PMap m) = onSnd (PHole hole1) (searchM' k2) m'
	  where	!(# m', hole1 #) = searchM k1 m
	indexM i (PMap m) = onThird (PHole hole1) (indexM i') m'
	  where	!(# i', m', hole1 #) = indexM i m
	extractHoleM (PMap m) = do
		(m', hole1) <- extractHoleM m
		(v, hole2) <- extractHoleM m'
		return (v, PHole hole1 hole2)
	
	clearM (PHole hole1 hole2) = PMap (fillHoleM (clearM' hole2) hole1)
	assignM a (PHole hole1 hole2) = PMap (assignM (assignM a hole2) hole1)
	
	unifyM (k11, k12) a1 (k21, k22) a2 = PMap <$> (match1 `mplus` match2) where
	  match1 = unifyM k11 (singletonM k12 a1) k21 (singletonM k22 a2)
	  match2 = singletonM k11 <$> unifyM k12 a1 k22 a2
	
	insertWithM f (k1, k2) a (PMap m) = PMap (insertWithM g k1 single m) where
	  single = singletonM k2 a
	  g _ = insertWithM f k2 a

gNull :: TrieKey k => (x -> TrieMap k a) -> x -> Maybe (TrieMap k a)
gNull = (guardNullM .)

breakFst :: TrieKey k1 => [((k1, k2), a)] -> [(k1, Elem [(k2, a)])]
breakFst [] = []
breakFst (((a, b),v):xs) = breakFst' a (Seq.singleton (b, v)) xs where
	breakFst' a vs (((a', b'), v'):xs)
		| a =? a'	= breakFst' a' (vs |> (b', v')) xs
		| otherwise	= (a, Elem $ toList vs):breakFst' a' (Seq.singleton (b', v')) xs
	breakFst' a vs [] = [(a, Elem $ toList vs)]