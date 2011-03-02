{-# LANGUAGE TypeFamilies, FlexibleInstances, CPP, MultiParamTypeClasses, UnboxedTuples, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module Data.TrieMap.UnitMap () where

import Data.Maybe (fromMaybe)
import Data.TrieMap.TrieKey
import Data.Functor.Immoral

import Prelude hiding (foldr, foldl, foldr1, foldl1)

deriving instance ImmoralMap (Maybe a) (TrieMap () a)

instance Functor (TrieMap ()) where
  fmap f (Unit m) = Unit (f <$> m)

instance Foldable (TrieMap ()) where
  foldMap f (Unit m) = foldMap f m
  foldr f z (Unit m) = foldr f z m
  foldl f z (Unit m) = foldl f z m

instance Traversable (TrieMap ()) where
  traverse f (Unit m) = castMap $ traverse f m

instance Subset (TrieMap ()) where
  Unit m1 <=? Unit m2 = m1 <=? m2

instance Buildable (TrieMap ()) () where
  type UStack (TrieMap ()) = Elem
  uFold f = Foldl{
    zero = emptyM,
    begin = const Elem,
    snoc = \ (Elem a) _ a' -> Elem (f a' a),
    done = \ (Elem a) -> single a}
  type AStack (TrieMap ()) = Elem
  aFold = uFold
  type DAStack (TrieMap ()) = TrieMap ()
  daFold =  Foldl{
    zero = emptyM,
    begin = const single,
    snoc = error "Error: duplicate keys",
    done = id}

#define SETOP(op) op f (Unit m1) (Unit m2) = Unit (op f m1 m2)
instance SetOp (TrieMap ()) where
  SETOP(union)
  SETOP(isect)
  SETOP(diff)

instance Project (TrieMap ()) where
  mapMaybe f (Unit m) = Unit (mapMaybe f m)
  mapEither f (Unit m) = both Unit (mapEither f) m

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
	
	insertWithM f _ a (Unit m) = Unit (Just (maybe a f m))
	
	singleHoleM _ = Hole
	beforeM _ = emptyM
	afterM _ = emptyM
	beforeWithM a _ = single a
	afterWithM a _ = single a
	
	searchMC _ (Unit (Just v)) _ g = g v Hole
	searchMC _ _ f _ = f Hole

	indexM (Unit v) i = 
	  (# i, fromMaybe indexFail v, Hole #)
	
	unifierM _ _ _ = mzero
	unifyM _ _ _ _ = mzero
	
	extractHoleM (Unit (Just v)) = return (v, Hole)
	extractHoleM _ = mzero
	
	clearM _ = emptyM
	assignM v _ = single v

single :: a -> TrieMap () a
single = Unit . Just