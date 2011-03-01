{-# LANGUAGE LiberalTypeSynonyms, ImplicitParams, TypeOperators, CPP #-}
module Data.TrieMap.TrieKey.SetOp (
  isectM, unionM, diffM,
  Isect, Union, Diff,
  SetOp(..)) where

import Data.TrieMap.Sized
import Data.TrieMap.TrieKey.Subset

type Isect f a b c = f a -> f b -> Maybe (f c)
type Union f a = f a -> f a -> Maybe (f a)
type Diff f a b = f a -> f b -> Maybe (f a)

type Id a = a

class SetOp f where
  isect :: Sized c => Isect Id a b c -> Isect f a b c
  union :: Sized a => Union Id a -> Union f a
  diff :: Sized a => Diff Id a b -> Diff f a b

isectM f (Just a) (Just b) = f a b
isectM _ _ _ = Nothing
unionM f (Just a) (Just b) = f a b
unionM _ (Just a) Nothing = Just a
unionM _ Nothing (Just b) = Just b
unionM _ Nothing  Nothing = Nothing
diffM f (Just a) (Just b) = f a b
diffM _ (Just a) Nothing = Just a
diffM _ Nothing _ = Nothing