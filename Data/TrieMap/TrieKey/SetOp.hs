{-# LANGUAGE LiberalTypeSynonyms, ImplicitParams, TypeOperators, CPP #-}
module Data.TrieMap.TrieKey.SetOp (
  IsectM, UnionM, DiffM,
  isectM, unionM, diffM,
  Isect, Union, Diff,
  SetOp(..)) where

import Data.TrieMap.TrieKey.Subset

type IsectM f a b c = f a -> f b -> Maybe (f c)
type UnionM f a = f a -> f a -> Maybe (f a)
type DiffM f a b = f a -> f b -> Maybe (f a)

type Isect f a b c = f a -> f b -> f c
type Union f a = f a -> f a -> f a
type Diff f a b = f a -> f b -> f a

type Id a = a

class SetOp f where
  isect :: IsectM Id a b c -> Isect f a b c
  union :: UnionM Id a -> Union f a
  diff :: DiffM Id a b -> Diff f a b

instance SetOp Maybe where
  {-# INLINE isect #-}
  {-# INLINE union #-}
  {-# INLINE diff #-}
  isect f (Just a) (Just b) = f a b
  isect _ _ _ = Nothing
  union f (Just a) (Just b) = f a b
  union _ (Just a) Nothing = Just a
  union _ Nothing (Just b) = Just b
  union _ Nothing  Nothing = Nothing
  diff f (Just a) (Just b) = f a b
  diff _ (Just a) Nothing = Just a
  diff _ Nothing _ = Nothing

{-# INLINE isectM #-}
isectM :: (Nullable f, SetOp f) => IsectM Id a b c -> IsectM f a b c
isectM f a b = guardNull (isect f a b)

{-# INLINE diffM #-}
diffM :: (Nullable f, SetOp f) => DiffM Id a b -> DiffM f a b
diffM f a b = guardNull (diff f a b)

{-# INLINE unionM #-}
unionM :: (Nullable f, SetOp f) => UnionM Id a -> UnionM f a
unionM f a b = guardNull (union f a b)