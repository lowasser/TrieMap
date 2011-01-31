{-# LANGUAGE UnboxedTuples, TypeFamilies, PatternGuards, ViewPatterns, MagicHash, CPP, BangPatterns #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.TrieMap.UnionMap () where

import Data.TrieMap.TrieKey
import Data.TrieMap.Sized
import Data.TrieMap.UnitMap ()

import Control.Applicative
import Control.Monad

import Data.Foldable (foldr)
import Prelude hiding (foldr, (^))

(&) :: (TrieKey k1, TrieKey k2, Sized a) => TrieMap k1 a -> TrieMap k2 a -> TrieMap (Either k1 k2) a
m1 & m2 = guardNullM m1 ^ guardNullM m2

{-# INLINE (^) #-}
(^) :: (TrieKey k1, TrieKey k2, Sized a) => Maybe (TrieMap k1 a) -> Maybe (TrieMap k2 a) -> TrieMap (Either k1 k2) a
Nothing ^ Nothing	= Empty
Just m1 ^ Nothing	= K1 m1
Nothing ^ Just m2	= K2 m2
Just m1 ^ Just m2	= Union (sizeM m1 + sizeM m2) m1 m2

union :: (TrieKey k1, TrieKey k2, Sized a) => TrieMap k1 a -> TrieMap k2 a -> TrieMap (Either k1 k2) a
union m1 m2 = Union (sizeM m1 + getSize m2) m1 m2

singletonL :: (TrieKey k1, TrieKey k2, Sized a) => k1 -> a -> TrieMap (Either k1 k2) a
singletonL k a = K1 (singletonM k a)

singletonR :: (TrieKey k1, TrieKey k2, Sized a) => k2 -> a -> TrieMap (Either k1 k2) a
singletonR k a = K2 (singletonM k a)

data UView k1 k2 a = UView (Maybe (TrieMap k1 a)) (Maybe (TrieMap k2 a))
data HView k1 k2 a = Hole1 (Hole k1 a) (Maybe (TrieMap k2 a))
		    | Hole2 (Maybe (TrieMap k1 a)) (Hole k2 a)

uView :: TrieMap (Either k1 k2) a -> UView k1 k2 a
uView Empty = UView Nothing Nothing
uView (K1 m1) = UView (Just m1) Nothing
uView (K2 m2) = UView Nothing (Just m2)
uView (Union _ m1 m2) = UView (Just m1) (Just m2)

hView :: Hole (Either k1 k2) a -> HView k1 k2 a
hView (HoleX0 hole1) = Hole1 hole1 Nothing
hView (HoleX2 hole1 m2) = Hole1 hole1 (Just m2)
hView (Hole0X hole2) = Hole2 Nothing hole2
hView (Hole1X m1 hole2) = Hole2 (Just m1) hole2

hole1 :: Hole k1 a -> Maybe (TrieMap k2 a) -> Hole (Either k1 k2) a
hole1 hole1 Nothing = HoleX0 hole1
hole1 hole1 (Just m2) = HoleX2 hole1 m2

hole2 :: Maybe (TrieMap k1 a) -> Hole k2 a -> Hole (Either k1 k2) a
hole2 Nothing hole2 = Hole0X hole2
hole2 (Just m1) hole2 = Hole1X m1 hole2

#define UVIEW uView -> UView

-- | @'TrieMap' ('Either' k1 k2) a@ is essentially a @(TrieMap k1 a, TrieMap k2 a)@, but
-- specialized for the cases where one or both maps are empty.
instance (TrieKey k1, TrieKey k2) => TrieKey (Either k1 k2) where
	{-# SPECIALIZE instance TrieKey (Either () ()) #-}
  	Left k1 =? Left k2	= k1 =? k2
  	Right k1 =? Right k2	= k1 =? k2
  	_ =? _			= False
  	
  	Left k1 `cmp` Left k2	= k1 `cmp` k2
  	Left{} `cmp` Right{}	= LT
  	Right k1 `cmp` Right k2	= k1 `cmp` k2
  	Right{} `cmp` Left{}	= GT
  
	data TrieMap (Either k1 k2) a = 
		Empty
		| K1 (TrieMap k1 a)
		| K2 (TrieMap k2 a)
		| Union !Int (TrieMap k1 a) (TrieMap k2 a)
	data Hole (Either k1 k2) a =
		HoleX0 (Hole k1 a)
		| HoleX2 (Hole k1 a) (TrieMap k2 a)
		| Hole0X (Hole k2 a)
		| Hole1X (TrieMap k1 a) (Hole k2 a)
	emptyM = Empty
	
	singletonM = either singletonL singletonR
	
	getSimpleM (UVIEW m1 m2) = mSimple m1 `mplus` mSimple m2 where
		mSimple :: TrieKey k => Maybe (TrieMap k a) -> Simple a
		mSimple = maybe mzero getSimpleM
	
	sizeM Empty = 0
	sizeM (K1 m1) = sizeM m1
	sizeM (K2 m2) = sizeM m2
	sizeM (Union s _ _) = s
	
	lookupM (Left k) (UVIEW m1 _) = m1 >>= lookupM k
	lookupM (Right k) (UVIEW _ m2) = m2 >>= lookupM k
	insertWithM f (Left k) a (UVIEW m1 m2) = Just (insertWithM' f k a m1) ^ m2
	insertWithM f (Right k) a (UVIEW m1 m2) = m1 ^ Just (insertWithM' f k a m2)

	traverseM f (Union _ m1 m2) = union <$> traverseM f m1 <*> traverseM f m2
	traverseM f (K1 m1) = K1 <$> traverseM f m1
	traverseM f (K2 m2) = K2 <$> traverseM f m2
	traverseM _ _ = pure Empty

	foldrM f (UVIEW m1 m2) z = foldr (foldrM f) (foldr (foldrM f) z m2) m1

	foldlM f (UVIEW m1 m2) z = foldr (foldlM f) (foldr (foldlM f) z m1) m2

	fmapM f (Union _ m1 m2) = fmapM f m1 `union` fmapM f m2
	fmapM f (K1 m1)		= K1 (fmapM f m1)
	fmapM f (K2 m2)		= K2 (fmapM f m2)
	fmapM _ _		= Empty

	mapMaybeM f (UVIEW m1 m2) = (m1 >>= mapMaybeM' f) ^ (m2 >>= mapMaybeM' f)

	mapEitherM f (UVIEW m1 m2) = (# m1L ^ m2L, m1R ^ m2R #) where
	  !(# m1L, m1R #) = mapEitherM'' f m1
	  !(# m2L, m2R #) = mapEitherM'' f m2

	unionM _ Empty m2	= m2
	unionM f m1@(UVIEW m11 m12) m2@(UVIEW m21 m22)
		| Empty <- m2	= m1
		| otherwise	= unionMaybe (unionM' f) m11 m21 ^ unionMaybe (unionM' f) m12 m22

	isectM f (UVIEW m11 m12) (UVIEW m21 m22) =
		isectMaybe (isectM' f) m11 m21 ^ isectMaybe (isectM' f) m12 m22

	diffM f m1@(UVIEW m11 m12) m2@(UVIEW m21 m22)
		| Empty <- m2	= m1
		| otherwise	= diffMaybe (diffM' f) m11 m21 ^ diffMaybe (diffM' f) m12 m22

	isSubmapM (<=) (UVIEW m11 m12) (UVIEW m21 m22) =
		subMaybe (isSubmapM (<=)) m11 m21 && subMaybe (isSubmapM (<=)) m12 m22

	fromListM f = onPair (&) (fromListM f) (fromListM f) . partEithers

	fromAscListM f = onPair (&) (fromAscListM f) (fromAscListM f) . partEithers

	fromDistAscListM = onPair (&) fromDistAscListM fromDistAscListM . partEithers

	singleHoleM = either (HoleX0 . singleHoleM) (Hole0X . singleHoleM)

	beforeM hole = case hView hole of
		Hole1 h1 __	-> guardNullM (beforeM h1) ^ Nothing
		Hole2 m1 h2	-> m1 ^ guardNullM (beforeM h2)
	beforeWithM a hole = case hView hole of
		Hole1 h1 __	-> K1 (beforeWithM a h1)
		Hole2 m1 h2	-> m1 ^ Just (beforeWithM a h2)
	
	afterM hole = case hView hole of
		Hole1 h1 m2	-> guardNullM (afterM h1) ^ m2
		Hole2 __ h2	-> Nothing ^ guardNullM (afterM h2)
	afterWithM a hole = case hView hole of
		Hole1 h1 m2	-> Just (afterWithM a h1) ^ m2
		Hole2 __ h2	-> K2 (afterWithM a h2)
	
	searchM (Left k) (UVIEW m1 m2) = onSnd (`hole1` m2) (searchM' k) m1
	searchM (Right k) (UVIEW m1 m2) = onSnd (hole2 m1) (searchM' k) m2
	
	indexM i (K1 m1) = onThird HoleX0 (indexM i) m1
	indexM i (K2 m2) = onThird Hole0X (indexM i) m2
	indexM i (Union _ m1 m2)
		| i < s1	= onThird (`HoleX2` m2) (indexM i) m1
		| otherwise	= onThird (Hole1X m1) (indexM (i - s1)) m2
		where !s1 = sizeM m1
	indexM _ _ = indexFail ()

	extractHoleM (UVIEW !m1 !m2) = holes1 `mplus` holes2 where
	  holes1 = holes extractHoleM (`hole1` m2) m1
	  holes2 = holes extractHoleM (hole2 m1) m2
	
	clearM hole = case hView hole of
		Hole1 h1 m2	-> clearM' h1 ^ m2
		Hole2 m1 h2	-> m1 ^ clearM' h2
	assignM v hole = case hView hole of
		Hole1 h1 m2	-> Just (assignM v h1) ^ m2
		Hole2 m1 h2	-> m1 ^ Just (assignM v h2)
	
	unifyM (Left k1) a1 (Left k2) a2 = K1 <$> unifyM k1 a1 k2 a2
	unifyM (Left k1) a1 (Right k2) a2 = Just $ singletonM k1 a1 `union` singletonM k2 a2
	unifyM (Right k2) a2 (Left k1) a1 = Just $ singletonM k1 a1 `union` singletonM k2 a2
	unifyM (Right k1) a1 (Right k2) a2 = K2 <$> unifyM k1 a1 k2 a2

{-# INLINE holes #-}
holes :: (Functor m, Functor f, MonadPlus m) => (a -> m (f b)) -> (b -> c) -> Maybe a -> m (f c)
holes k f (Just a) = fmap f <$> k a
holes _ _ Nothing = mzero

onPair :: (c -> d -> e) -> (a -> c) -> (b -> d) -> (a, b) -> e
onPair f g h (a, b) = f (g a) (h b)

partEithers :: [(Either a b, x)] -> ([(a, x)], [(b, x)])
partEithers = foldr part ([], []) where
	  part (Left x, z) (xs, ys) = ((x,z):xs, ys)
	  part (Right y, z) (xs, ys) = (xs, (y, z):ys)