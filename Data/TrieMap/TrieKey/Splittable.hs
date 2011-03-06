module Data.TrieMap.TrieKey.Splittable where

import Data.TrieMap.TrieKey.Zippable
import Data.TrieMap.TrieKey.Subset
import Data.TrieMap.Sized

class Zippable f => Splittable f where
  before, after :: Sized a => Zipper f a -> f a
  beforeWith, afterWith :: Sized a => a -> Zipper f a -> f a

beforeM, afterM :: (Splittable f, Sized a) => Maybe a -> Zipper f a -> f a
beforeM = maybe before beforeWith
afterM = maybe after afterWith

beforeG, afterG :: (Splittable f, Nullable f, Sized a) => Zipper f a -> Maybe (f a)
beforeG = guardNull . before
afterG = guardNull . after
