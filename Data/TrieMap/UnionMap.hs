{-# LANGUAGE UnboxedTuples, TypeFamilies, PatternGuards, ViewPatterns, MagicHash, CPP, BangPatterns, FlexibleInstances, RecordWildCards #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.TrieMap.UnionMap () where

import Data.TrieMap.TrieKey
import Data.TrieMap.Sized
import Data.TrieMap.UnitMap ()

import Prelude hiding (foldr, foldr1, foldl, foldl1, (^))
import GHC.Exts

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

instance (TrieKey k1, TrieKey k2) => Functor (TrieMap (Either k1 k2)) where
  fmap _ Empty = Empty
  fmap f (K1 m1) = K1 (f <$> m1)
  fmap f (K2 m2) = K2 (f <$> m2)
  fmap f (Union s m1 m2) = Union s (f <$> m1) (f <$> m2)

instance (TrieKey k1, TrieKey k2) => Foldable (TrieMap (Either k1 k2)) where
  foldMap f (UVIEW m1 m2) = fmap (foldMap f) m1 `mappendM` fmap (foldMap f) m2
  foldr f z (UVIEW m1 m2) = foldl (foldr f) (foldl (foldr f) z m2) m1
  foldl f z (UVIEW m1 m2) = foldl (foldl f) (foldl (foldl f) z m1) m2

instance (TrieKey k1, TrieKey k2) => Traversable (TrieMap (Either k1 k2)) where
  traverse _ Empty = pure Empty
  traverse f (K1 m1) = K1 <$> traverse f m1
  traverse f (K2 m2) = K2 <$> traverse f m2
  traverse f (Union s m1 m2) = Union s <$> traverse f m1 <*> traverse f m2

instance (TrieKey k1, TrieKey k2) => Subset (TrieMap (Either k1 k2)) where
  (UVIEW m11 m12) <=? (UVIEW m21 m22)
    = m11 <<=? m21 && m12 <<=? m22

-- | @'TrieMap' ('Either' k1 k2) a@ is essentially a @(TrieMap k1 a, TrieMap k2 a)@, but
-- specialized for the cases where one or both maps are empty.
instance (TrieKey k1, TrieKey k2) => TrieKey (Either k1 k2) where
	{-# SPECIALIZE instance TrieKey (Either () ()) #-}  
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
	
	lookupMC (Left k) (UVIEW (Just m1) _) = lookupMC k m1
	lookupMC (Right k) (UVIEW _ (Just m2)) = lookupMC k m2
	lookupMC _ _ = \ no _ -> no

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

	insertWithM f (Left k) a (UVIEW m1 m2)
		= Just (insertWithM' f k a m1) ^ m2
	insertWithM f (Right k) a (UVIEW m1 m2)
		= m1 ^ Just (insertWithM' f k a m2)
	
	{-# INLINE fromAscListFold #-}
	fromAscListFold f = combineFold (fromAscListFold f) (fromAscListFold f)
	{-# INLINE fromDistAscListFold #-}
	fromDistAscListFold = combineFold fromDistAscListFold fromDistAscListFold

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
	
	searchMC (Left k) (UVIEW m1 m2) = mapSearch (`hole1` m2) (searchMC' k m1)
	searchMC (Right k) (UVIEW m1 m2) = mapSearch (hole2 m1) (searchMC' k m2)
	
	indexMC i (K1 m1) result = mapIndex HoleX0 (indexMC i m1) result
	indexMC i (K2 m2) result = mapIndex Hole0X (indexMC i m2) result
	indexMC i# (Union _ m1 m2) result
		| i# <# s1#	= mapIndex (`HoleX2` m2) (indexMC i# m1) result
		| otherwise	= mapIndex (Hole1X m1) (indexMC (i# -# s1#) m2) result
		where !s1# = sizeM# m1
	indexMC _ _ _ = indexFail

	extractHoleM (UVIEW !m1 !m2) = holes1 `mplus` holes2 where
	  holes1 = holes extractHoleM (`hole1` m2) m1
	  holes2 = holes extractHoleM (hole2 m1) m2
	
	clearM hole = case hView hole of
		Hole1 h1 m2	-> clearM' h1 ^ m2
		Hole2 m1 h2	-> m1 ^ clearM' h2
	assignM v hole = case hView hole of
		Hole1 h1 m2	-> Just (assignM v h1) ^ m2
		Hole2 m1 h2	-> m1 ^ Just (assignM v h2)
	
	unifierM (Left k') (Left k) a = HoleX0 <$> unifierM k' k a
	unifierM (Left k') (Right k) a = Just $ HoleX2 (singleHoleM k') (singletonM k a)
	unifierM (Right k') (Left k) a = Just $ Hole1X (singletonM k a) (singleHoleM k')
	unifierM (Right k') (Right k) a = Hole0X <$> unifierM k' k a

{-# INLINE holes #-}
holes :: (Functor m, Functor f, MonadPlus m) => (a -> m (f b)) -> (b -> c) -> Maybe a -> m (f c)
holes k f (Just a) = fmap f <$> k a
holes _ _ Nothing = mzero

{-# INLINE combineFold #-}
combineFold :: (TrieKey k1, TrieKey k2, Sized a) =>
  FromList k1 a -> FromList k2 a -> FromList (Either k1 k2) a
combineFold Foldl{snoc = snocL, begin = beginL, done = doneL}
	    Foldl{snoc = snocR, begin = beginR, done = doneR}
  = Foldl{zero = Empty, ..}
  where	snoc (JustL s1)	(Left k) a = JustL (snocL s1 k a)
	snoc (JustL s1)	(Right k) a = Both s1 (beginR k a)
	snoc (JustR s2) (Left k) a = Both (beginL k a) s2
	snoc (JustR s2) (Right k) a = JustR (snocR s2 k a)
	snoc (Both s1 s2) (Left k) a = Both (snocL s1 k a) s2
	snoc (Both s1 s2) (Right k) a = Both s1 (snocR s2 k a)
	
	begin (Left k) a = JustL (beginL k a)
	begin (Right k) a = JustR (beginR k a)
	
	done (JustL sL) = K1 (doneL sL)
	done (JustR sR) = K2 (doneR sR)
	done (Both sL sR) = doneL sL `union` doneR sR

data Stack s1 s2 =
  JustL s1
  | JustR s2
  | Both s1 s2