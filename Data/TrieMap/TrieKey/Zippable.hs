{-# LANGUAGE TypeFamilies #-}
module Data.TrieMap.TrieKey.Zippable where

import Control.Monad
import Control.Monad.Ends

import Data.TrieMap.TrieKey.Subset
import Data.TrieMap.Sized

import GHC.Exts

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

class Zippable f => Alternatable f where
  alternate :: MonadPlus m => f a -> m (a, Zipper f a)

  firstHole :: f a -> First (a, Zipper f a)
  lastHole :: f a -> Last (a, Zipper f a)

  {-# NOINLINE firstHole #-}
  {-# NOINLINE lastHole #-}
  firstHole m = inline alternate m
  lastHole m = inline alternate m

{-# RULES
  "alternate/First" alternate = firstHole;
  "alternate/Last" alternate = lastHole;
  #-}