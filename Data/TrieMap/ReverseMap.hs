{-# LANGUAGE TypeFamilies, MagicHash, UnboxedTuples #-}
module Data.TrieMap.ReverseMap () where

import Control.Applicative

import Data.TrieMap.Applicative
import Data.TrieMap.TrieKey
import Data.TrieMap.Modifiers
import Data.TrieMap.Sized

import GHC.Exts

-- | @'TrieMap' ('Rev' k) a@ is a wrapper around a @'TrieMap' k a@ that reverses the order of the operations.
instance TrieKey k => TrieKey (Rev k) where
	newtype TrieMap (Rev k) a = RevMap (TrieMap k a)
	newtype Hole (Rev k) a = RHole (Hole k a)

	Rev k1 =? Rev k2 = k1 =? k2
	Rev k1 `cmp` Rev k2 = k2 `cmp` k1
	
	emptyM = RevMap emptyM
	singletonM (Rev k) a = RevMap (singletonM k a)
	lookupM (Rev k) (RevMap m) = lookupM k m
	sizeM (RevMap m) = sizeM m
	getSimpleM (RevMap m) = getSimpleM m
	
	fmapM f (RevMap m) = RevMap (fmapM f m)
	traverseM f (RevMap m) = RevMap <$> runDual (traverseM (Dual . f) m)
	
	foldlM f (RevMap m) = foldrM (flip f) m
	foldrM f (RevMap m) = foldlM (flip f) m
	
	mapMaybeM f (RevMap m) = RevMap (mapMaybeM f m)
	mapEitherM f (RevMap m) = both RevMap RevMap (mapEitherM f) m
	unionM f (RevMap m1) (RevMap m2) = RevMap (unionM f m1 m2)
	isectM f (RevMap m1) (RevMap m2) = RevMap (isectM f m1 m2)
	diffM f (RevMap m1) (RevMap m2) = RevMap (diffM f m1 m2)
	isSubmapM (<=) (RevMap m1) (RevMap m2) = isSubmapM (<=) m1 m2
	
	singleHoleM (Rev k) = RHole (singleHoleM k)
	beforeM a (RHole hole) = RevMap (afterM a hole)
	afterM a (RHole hole) = RevMap (beforeM a hole)
	searchM (Rev k) (RevMap m) = onSnd RHole (searchM k) m
	indexM i# (RevMap m) = case indexM (revIndex i# m) m of
		(# i'#, a, hole #) -> (# revIndex i'# a, a, RHole hole #)
	extractHoleM (RevMap m) = runDualPlus $ do
		(a, hole) <- extractHoleM m
		return (a, RHole hole)
	assignM v (RHole m) = RevMap (assignM v m)
	
	fromListM f xs = RevMap (fromListM f [(k, a) | (Rev k, a) <- xs])
	fromAscListM f xs = RevMap (fromAscListM (flip f) [(k, a) | (Rev k, a) <- reverse xs])
	fromDistAscListM xs = RevMap (fromDistAscListM [(k, a) | (Rev k, a) <- reverse xs])
	
	unifyM (Rev k1) a1 (Rev k2) a2 = either (Left . RHole) (Right . RevMap) (unifyM k1 a1 k2 a2)

revIndex :: Sized a => Int# -> a -> Int#
revIndex i# a = getSize# a -# 1# -# i#