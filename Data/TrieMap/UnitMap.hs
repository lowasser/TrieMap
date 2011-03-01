{-# LANGUAGE TypeFamilies, FlexibleInstances, CPP, MultiParamTypeClasses, UnboxedTuples #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module Data.TrieMap.UnitMap () where

import Data.TrieMap.TrieKey

import Data.Functor.Immoral

import Prelude hiding (foldr, foldl, foldr1, foldl1)

deriving instance ImmoralMap a (TrieMap () a)

instance Functor (TrieMap ()) where
  fmap f (Unit m) = Unit (f m)

instance Foldable (TrieMap ()) where
  foldMap f (Unit m) = f m
  foldr f z (Unit m) = m `f` z
  foldl f z (Unit m) = z `f` m
  foldr1 _ (Unit m) = m
  foldl1 _ (Unit m) = m

instance Traversable (TrieMap ()) where
  traverse f (Unit a) = castMap $ f a

instance Subset (TrieMap ()) where
  Unit m1 <=? Unit m2 = m1 <?= m2

instance Buildable (TrieMap ()) () where
  type UStack (TrieMap ()) = Elem
  uFold f = Foldl{
    begin = const Elem,
    snoc = \ (Elem a) _ a' -> Elem (f a' a),
    done = \ (Elem a) -> Unit a}
  type AStack (TrieMap ()) = Elem
  aFold = uFold
  type DAStack (TrieMap ()) = TrieMap ()
  daFold =  Foldl{
    begin = const Unit,
    snoc = error "Error: duplicate keys",
    done = id}

#define SETOP(op) op f (Unit m1) (Unit m2) = castMap (f m1 m2)
instance SetOp (TrieMap ()) where
  SETOP(union)
  SETOP(isect)
  SETOP(diff)

instance Project (TrieMap ()) where
  mapMaybe f (Unit m) = castMap (f m)
  mapEither f (Unit m) = case f m of
    (# mL, mR #) -> (# castMap mL, castMap mR #)

-- | @'TrieMap' () a@ is implemented as @'Maybe' a@.
instance TrieKey () where
	newtype TrieMap () a = Unit a
	data Hole () a = Hole
	
	singletonM _ a = Unit a
	getSimpleM (Unit m) = Singleton m
	sizeM (Unit m) = getSize m
	lookupMC _ (Unit a) = return a
	
	insertWithM f _ _ (Unit m) = Unit (f m)
	
	singleHoleM _ = Hole
	beforeM _ = Nothing
	afterM _ = Nothing
	beforeWithM a _ = Unit a
	afterWithM a _ = Unit a
	
	searchMC _ (Unit v) _ g = g v Hole

	indexM (Unit v) i = 
	  (# i, v, Hole #)
	
	unifierM _ _ _ = mzero
	unifyM _ _ _ _ = mzero
	
	extractHoleM (Unit v) = return (v, Hole)
	
	clearM _ = Nothing
	assignM v _ = Unit v