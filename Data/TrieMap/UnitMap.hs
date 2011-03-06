{-# LANGUAGE TypeFamilies, FlexibleInstances, CPP, MultiParamTypeClasses, UnboxedTuples, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module Data.TrieMap.UnitMap () where

import Data.TrieMap.TrieKey
import Data.Functor.Immoral

import Prelude hiding (foldr, foldl, foldr1, foldl1)

data instance Zipper (TrieMap ()) a = One
newtype instance TrieMap () a = Unit (Maybe a)

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
    zero = empty,
    begin = const Elem,
    snoc = \ (Elem a) _ a' -> Elem (f a' a),
    done = \ (Elem a) -> single a}
  type AStack (TrieMap ()) = Elem
  aFold = uFold
  type DAStack (TrieMap ()) = TrieMap ()
  daFold =  Foldl{
    zero = empty,
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
  
instance Zippable (TrieMap ()) where
  empty = Unit Nothing
  
  assign a _ = single a
  clear _ = empty

instance Searchable (TrieMap ()) () where
  search _ (Unit m) nomatch match = maybe nomatch match m One
  singleZip _ = One
  insertWith f _ a (Unit m) = single (maybe a f m)
  alter f _ (Unit m) = Unit (f m)

instance Splittable (TrieMap ()) where
  before = clear
  after = clear
  beforeWith = assign
  afterWith = assign

instance Indexable (TrieMap ()) where
  index i (Unit (Just a)) = (# i, a, One #)
  index _ _ = indexFail ()

-- | @'TrieMap' () a@ is implemented as @'Maybe' a@.
instance TrieKey () where
  getSimpleM (Unit m) = maybe Null Singleton m
  sizeM (Unit m) = getSize m
  unifierM _ _ _ = mzero
  unifyM _ _ _ _ = mzero

single :: a -> TrieMap () a
single = Unit . Just