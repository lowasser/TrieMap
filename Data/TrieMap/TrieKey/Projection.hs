{-# LANGUAGE LiberalTypeSynonyms, UnboxedTuples, ScopedTypeVariables, Rank2Types #-}
module Data.TrieMap.TrieKey.Projection (MapMaybe, MapEither, Project(..), mapMaybeM, mapEitherM) where

import Data.TrieMap.Sized

type MapMaybe f a b = f a -> Maybe (f b)
type MapEither f a b c = f a -> (# Maybe (f b), Maybe (f c) #)
type Id a = a

class Project f where
  mapMaybe :: Sized b => MapMaybe Id a b -> MapMaybe f a b
  mapEither :: (Sized b, Sized c) => MapEither Id a b c -> MapEither f a b c
  
  mapEither f a = (# mapMaybe f1 a, mapMaybe f2 a #) where
    f1 a = case f a of
      (# b, _ #) -> b
    f2 a = case f a of
      (# _, c #) -> c
  mapMaybe (f :: MapMaybe Id a b) a = case mapEither g a of
    (# fb, _ #) -> fb
    where g :: MapEither Id a b (Elem a)
	  g a = (# f a, Nothing #)

mapMaybeM :: MapMaybe Id a b -> Maybe a -> Maybe b
mapMaybeM = (=<<)

mapEitherM :: MapEither Id a b c -> Maybe a -> (# Maybe b, Maybe c #)
mapEitherM f (Just a) = f a
mapEitherM _ Nothing = (# Nothing, Nothing #)