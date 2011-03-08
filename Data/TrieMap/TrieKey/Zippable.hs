{-# LANGUAGE TypeFamilies #-}
module Data.TrieMap.TrieKey.Zippable where

import Data.TrieMap.TrieKey.Subset
import Data.TrieMap.Sized

data family Zipper (f :: * -> *) :: * -> *

class Zippable f where
  empty :: f a
  assign :: Sized a => a -> Zipper f a -> f a
  clear :: Sized a => Zipper f a -> f a

fill :: (Zippable f, Sized a) => Maybe a -> Zipper f a -> f a
fill = maybe clear assign

fillM :: (Zippable f, Nullable f, Sized a) => Maybe a -> Zipper f a -> Maybe (f a)
fillM Nothing z = clearM z
fillM (Just a) z = Just (assign a z)

clearM :: (Zippable f, Nullable f, Sized a) => Zipper f a -> Maybe (f a)
clearM = guardNull . clear