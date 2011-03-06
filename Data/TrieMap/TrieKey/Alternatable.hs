module Data.TrieMap.TrieKey.Alternatable where

import Control.Monad
import Control.Monad.Ends

import Data.TrieMap.TrieKey.Zippable

import GHC.Exts

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