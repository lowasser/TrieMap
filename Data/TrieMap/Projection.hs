{-# LANGUAGE LiberalTypeSynonyms, UnboxedTuples, ScopedTypeVariables, Rank2Types #-}
module Data.TrieMap.Projection (MapMaybe, MapEither, Project(..), mapMaybeM, mapEitherM, both, both') where

import Data.TrieMap.Sized
import Data.TrieMap.Subset

type MapMaybe f a b = f a -> Maybe (f b)
type MapEither f a b c = f a -> (# Maybe (f b), Maybe (f c) #)
type Id a = a

class Project f where
  mapMaybe :: Sized b => MapMaybe Id a b -> f a -> f b
  mapEither :: (Sized b, Sized c) => MapEither Id a b c -> f a -> (# f b, f c #)
  
  mapEither f a = (# mapMaybe f1 a, mapMaybe f2 a #) where
    f1 a = case f a of
      (# b, _ #) -> b
    f2 a = case f a of
      (# _, c #) -> c
  mapMaybe (f :: MapMaybe Id a b) a = case mapEither g a of
    (# fb, _ #) -> fb
    where g :: MapEither Id a b (Elem a)
	  g a = (# f a, Nothing #)

instance Project Maybe where
  mapMaybe f m = m >>= f
  mapEither _ Nothing = (# Nothing, Nothing #)
  mapEither f (Just a) = f a

mapMaybeM :: (Sized b, Project f, Nullable f) => MapMaybe Id a b -> MapMaybe f a b
mapMaybeM f a = guardNull (mapMaybe f a)

mapEitherM :: (Sized b, Sized c, Project f, Nullable f) => MapEither Id a b c -> MapEither f a b c
mapEitherM f a = case mapEither f a of
  (# b, c #) -> (# guardNull b, guardNull c #)

both :: (Sized b, Sized c) => (forall x . Sized x => f x -> f' x) -> (a -> (# f b, f c #)) -> a -> (# f' b, f' c #)
both g f a = case f a of
	(# x, y #) -> (# g x, g y #)

both' :: (b -> b') -> (c -> c') -> (a -> (# b, c #)) -> a -> (# b', c' #)
both' g1 g2 f a = case f a of
	(# x, y #) -> (# g1 x, g2 y #)