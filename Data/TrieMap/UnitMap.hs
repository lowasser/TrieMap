{-# LANGUAGE TypeFamilies, UnboxedTuples, MagicHash, FlexibleInstances, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module Data.TrieMap.UnitMap () where

import Control.Monad.Unpack

import Data.TrieMap.TrieKey
import Data.TrieMap.Sized

import Data.Maybe

import Prelude hiding (foldr, foldl, foldr1, foldl1)

instance Functor (TrieMap ()) where
  fmap f (Unit m) = Unit (f <$> m)

instance Foldable (TrieMap ()) where
  foldMap f (Unit m) = foldMap f m
  foldr f z (Unit m) = foldr f z m
  foldl f z (Unit m) = foldl f z m

instance Traversable (TrieMap ()) where
  traverse f (Unit (Just a)) = Unit . Just <$> f a
  traverse _ _ = pure (Unit Nothing)

instance Subset (TrieMap ()) where
  Unit m1 <=? Unit m2 = m1 <=? m2

-- | @'TrieMap' () a@ is implemented as @'Maybe' a@.
instance TrieKey () where
	newtype TrieMap () a = Unit (Maybe a)
	data Hole () a = Hole
	
	emptyM = Unit Nothing
	singletonM _ = single
	getSimpleM (Unit m) = maybe Null Singleton m
	sizeM (Unit m) = getSize m
	lookupMC _ (Unit (Just a)) = return a
	lookupMC _ _ = mzero
	mapMaybeM f (Unit m) = Unit (m >>= f)
	mapEitherM f (Unit a) = both Unit Unit (mapEitherMaybe f) a
	unionM f (Unit m1) (Unit m2) = Unit (unionMaybe f m1 m2)
	isectM f (Unit m1) (Unit m2) = Unit (isectMaybe f m1 m2)
	diffM f (Unit m1) (Unit m2) = Unit (diffMaybe f m1 m2)
	
	insertWithM f _ a (Unit m) = Unit (Just (maybe a f m))
	
	singleHoleM _ = Hole
	beforeM _ = emptyM
	afterM _ = emptyM
	beforeWithM a _ = single a
	afterWithM a _ = single a
	
	searchMC _ (Unit (Just v)) _ g = g v Hole
	searchMC _ _ f _ = f Hole

	indexMC (Unit (Just v)) = unpack $ \ i result -> result $~ Indexed i v Hole
	indexMC _ = indexFail
	
	unifierM _ _ _ = mzero
	unifyM _ _ _ _ = mzero
	
	extractHoleM (Unit (Just v)) = return (v, Hole)
	extractHoleM _ = mzero
	
	clearM _ = emptyM
	assignM v _ = single v
	
	type FLStack () = Elem
	type FLAStack () = Elem
	type FDLAStack () = TrieMap ()
	fromListFold f = Foldl{
	    zero = emptyM,
	    begin = \ _ v -> Elem v,
	    snoc = \ (Elem z) _ v -> Elem (f v z),
	    done = \ (Elem a) -> single a}
	fromAscListFold = fromListFold
	fromDistAscListFold = Foldl{
	    zero = emptyM,
	    begin = \ _ v -> single v,
	    snoc = error "Error: duplicate keys",
	    done = id}

single :: a -> TrieMap () a
single = Unit . Just