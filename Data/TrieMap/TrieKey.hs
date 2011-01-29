{-# LANGUAGE TypeFamilies, UnboxedTuples, MagicHash #-}

module Data.TrieMap.TrieKey where

import Data.TrieMap.Sized
import Data.TrieMap.Utils

import Control.Applicative
import Control.Monad
import Control.Monad.Ends

import Data.Foldable hiding (foldrM, foldlM)

import Prelude hiding (foldr, foldl)

import GHC.Exts

type LEq a b = a -> b -> Bool
type Unified k a = Maybe (TrieMap k a)

data Simple a = Null | Singleton a | NonSimple

instance Monad Simple where
	return = Singleton
	Null >>= _ = Null
	Singleton a >>= k = k a
	NonSimple >>= _ = NonSimple

instance MonadPlus Simple where
	mzero = Null
	Null `mplus` simple	= simple
	simple `mplus` Null	= simple
	_ `mplus` _		= NonSimple

onSnd :: (c -> d) -> (a -> (# b, c #)) -> a -> (# b, d #)
onSnd g f a = case f a of
	(# b, c #) -> (# b, g c #)

onThird :: (d -> e) -> (a -> (# Int#, c, d #)) -> a -> (# Int#, c, e #)
onThird g f a = case f a of
	(# b, c, d #) -> (# b, c, g d #)

instance TrieKey k => Foldable (TrieMap k) where
	foldr f = flip $ foldrM f
	foldl f = flip $ foldlM f

-- | A @TrieKey k@ instance implies that @k@ is a standardized representation for which a
-- generalized trie structure can be derived.
class TrieKey k where
	(=?) :: k -> k -> Bool
	cmp :: k -> k -> Ordering

	data TrieMap k :: * -> *
	emptyM :: TrieMap k a
	singletonM :: Sized a => k -> a -> TrieMap k a
	getSimpleM :: TrieMap k a -> Simple a
	sizeM :: Sized a => TrieMap k a -> Int#
	lookupM :: k -> TrieMap k a -> Maybe a
	insertWithM :: Sized a => (a -> a -> a) -> k -> a -> TrieMap k a -> TrieMap k a
	fmapM :: Sized b => (a -> b) -> TrieMap k a -> TrieMap k b
	traverseM :: (Applicative f, Sized b) =>
		(a -> f b) -> TrieMap k a -> f (TrieMap k b)
	foldrM :: (a -> b -> b) -> TrieMap k a -> b -> b
	foldlM :: (b -> a -> b) -> TrieMap k a -> b -> b
	mapMaybeM :: Sized b => (a -> Maybe b) -> TrieMap k a -> TrieMap k b
	mapEitherM :: (Sized b, Sized c) => (a -> (# Maybe b, Maybe c #)) -> TrieMap k a -> (# TrieMap k b, TrieMap k c #)
	unionM :: Sized a => (a -> a -> Maybe a) -> TrieMap k a -> TrieMap k a -> TrieMap k a
	isectM :: (Sized a, Sized b, Sized c) =>
		(a -> b -> Maybe c) -> TrieMap k a -> TrieMap k b -> TrieMap k c
	diffM :: Sized a => (a -> b -> Maybe a) -> TrieMap k a -> TrieMap k b -> TrieMap k a
	isSubmapM :: (Sized a, Sized b) => LEq a b -> LEq (TrieMap k a) (TrieMap k b)
	
	fromListM, fromAscListM :: Sized a => (a -> a -> a) -> [(k, a)] -> TrieMap k a
	fromDistAscListM :: Sized a => [(k, a)] -> TrieMap k a
	
	insertWithM f k a m = case searchM k m of
		(# a', hole #)	-> assignM (maybe a (f a) a') hole
	fromListM f = foldr (\ (k, a) -> insertWithM f k a) emptyM
	fromAscListM = fromListM
	fromDistAscListM = fromAscListM const
	
	data Hole k :: * -> *
	singleHoleM :: k -> Hole k a
	beforeM :: Sized a => Hole k a -> TrieMap k a
	beforeWithM :: Sized a => a -> Hole k a -> TrieMap k a
	afterM :: Sized a => Hole k a -> TrieMap k a
	afterWithM :: Sized a => a -> Hole k a -> TrieMap k a
	searchM :: k -> TrieMap k a -> (# Maybe a, Hole k a #)
	indexM :: Sized a => Int# -> TrieMap k a -> (# Int#, a, Hole k a #)

	-- This, combined with the rewrite rules, allows each instance to define only
	-- extractHoleM, but to obtain automatically specialized firstHoleM and lastHoleM
	-- implementations.
	extractHoleM :: (Functor m, MonadPlus m) => Sized a => TrieMap k a -> m (a, Hole k a)
	{-# NOINLINE firstHoleM #-}
	{-# NOINLINE lastHoleM #-}
	firstHoleM :: Sized a => TrieMap k a -> First (a, Hole k a)
	firstHoleM m = inline extractHoleM m
	lastHoleM :: Sized a => TrieMap k a -> Last (a, Hole k a)
	lastHoleM m = inline extractHoleM m
	
	fillHoleM :: Sized a => Maybe a -> Hole k a -> TrieMap k a
	assignM :: Sized a => a -> Hole k a -> TrieMap k a
	clearM :: Sized a => Hole k a -> TrieMap k a
	fillHoleM = maybe clearM assignM
	assignM = fillHoleM . Just
	clearM = fillHoleM Nothing
	
	unifyM :: Sized a => k -> a -> k -> a -> Unified k a

instance (TrieKey k, Sized a) => Sized (TrieMap k a) where
	getSize# = sizeM

insertWithM' :: (TrieKey k, Sized a) => (a -> a -> a) -> k -> a -> Maybe (TrieMap k a) -> TrieMap k a
insertWithM' f k a = maybe (singletonM k a) (insertWithM f k a)

mapMaybeM' :: (TrieKey k, Sized b) => (a -> Maybe b) -> TrieMap k a -> Maybe (TrieMap k b)
mapMaybeM' = guardNullM .: mapMaybeM

mapEitherM' :: (TrieKey k, Sized b, Sized c) => (a -> (# Maybe b, Maybe c #)) -> TrieMap k a ->
	(# Maybe (TrieMap k b), Maybe (TrieMap k c) #)
mapEitherM' = both guardNullM guardNullM . mapEitherM

mapEitherM'' :: (TrieKey k, Sized b, Sized c) => (a -> (# Maybe b, Maybe c #)) -> Maybe (TrieMap k a) ->
	(# Maybe (TrieMap k b), Maybe (TrieMap k c) #)
mapEitherM'' = mapEitherMaybe . mapEitherM'

unionM' :: (TrieKey k, Sized a) => (a -> a -> Maybe a) -> TrieMap k a -> TrieMap k a -> Maybe (TrieMap k a)
unionM' f m1 m2 = guardNullM (unionM f m1 m2)

isectM' :: (TrieKey k, Sized a, Sized b, Sized c) => (a -> b -> Maybe c) -> TrieMap k a -> TrieMap k b -> Maybe (TrieMap k c)
isectM' f m1 m2 = guardNullM (isectM f m1 m2)

diffM' :: (TrieKey k, Sized a) => (a -> b -> Maybe a) -> TrieMap k a -> TrieMap k b -> Maybe (TrieMap k a)
diffM' f m1 m2 = guardNullM (diffM f m1 m2)

{-# INLINE beforeMM #-}
beforeMM :: (TrieKey k, Sized a) => Maybe a -> Hole k a -> TrieMap k a
beforeMM = maybe beforeM beforeWithM

{-# INLINE afterMM #-}
afterMM :: (TrieKey k, Sized a) => Maybe a -> Hole k a -> TrieMap k a
afterMM = maybe afterM afterWithM

searchM' :: TrieKey k => k -> Maybe (TrieMap k a) -> (# Maybe a, Hole k a #)
searchM' k Nothing = (# Nothing, singleHoleM k #)
searchM' k (Just m) = searchM k m

clearM' :: (TrieKey k, Sized a) => Hole k a -> Maybe (TrieMap k a)
clearM' hole = guardNullM (clearM hole)

{-# INLINE alterM #-}
alterM :: (TrieKey k, Sized a) => (Maybe a -> Maybe a) -> k -> TrieMap k a -> TrieMap k a
alterM f k m = case searchM k m of
	(# Nothing, hole #)	-> case f Nothing of
		Nothing		-> m
		Just a		-> assignM a hole
	(# a, hole #)		-> fillHoleM (f a) hole

nullM :: TrieKey k => TrieMap k a -> Bool
nullM m = case getSimpleM m of
	Null	-> True
	_	-> False

guardNullM :: TrieKey k => TrieMap k a -> Maybe (TrieMap k a)
guardNullM m
	| nullM m	= Nothing
	| otherwise	= Just m

sides :: (b -> d) -> (a -> (# b, c, b #)) -> a -> (# d, c, d #)
sides g f a = case f a of
	(# x, y, z #) -> (# g x, y, g z #)

both :: (b -> b') -> (c -> c') -> (a -> (# b, c #)) -> a -> (# b', c' #)
both g1 g2 f a = case f a of
	(# x, y #) -> (# g1 x, g2 y #)

elemsM :: TrieKey k => TrieMap k a -> [a]
elemsM m = build (\ f z -> foldrM f m z)

mapEitherMaybe :: (a -> (# Maybe b, Maybe c #)) -> Maybe a -> (# Maybe b, Maybe c #)
mapEitherMaybe f (Just a) = f a
mapEitherMaybe _ _ = (# Nothing, Nothing #)

{-# INLINE unionMaybe #-}
unionMaybe :: (a -> a -> Maybe a) -> Maybe a -> Maybe a -> Maybe a
unionMaybe f (Just x) (Just y) = f x y
unionMaybe _ Nothing y = y
unionMaybe _ x Nothing = x

isectMaybe :: (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
isectMaybe f (Just x) (Just y) = f x y
isectMaybe _ _ _ = Nothing

diffMaybe :: (a -> b -> Maybe a) -> Maybe a -> Maybe b -> Maybe a
diffMaybe _ Nothing _ = Nothing
diffMaybe _ (Just x) Nothing = Just x
diffMaybe f (Just x) (Just y) = f x y

subMaybe :: (a -> b -> Bool) -> Maybe a -> Maybe b -> Bool
subMaybe _ Nothing _ = True
subMaybe (<=) (Just a) (Just b) = a <= b
subMaybe _ _ _ = False

indexFail :: a -> (# Int#, b, c #)
indexFail _ = (# error err, error err, error err #) where
	err = "Error: not a valid index"

{-# RULES
  "extractHoleM/First" [0] extractHoleM = firstHoleM;
  "extractHoleM/Last" [0] extractHoleM = lastHoleM;
  "getSimpleM/emptyM" getSimpleM emptyM = Null;
  "getSimpleM/singletonM" forall k a . getSimpleM (singletonM k a) = Singleton a;
  #-}