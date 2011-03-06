{-# LANGUAGE FlexibleInstances, FlexibleContexts, CPP #-}
module Data.TrieMap.ProdMap.Traversable where

import Data.TrieMap.ProdMap.Base
import Data.Functor.Immoral

import Prelude hiding (foldl, foldr)

#define INST(cl) (cl (TrieMap k1), cl (TrieMap k2)) => cl (TrieMap (k1, k2))
instance INST(Functor) where
  fmap f (PMap m) = PMap (fmap (fmap f) m)

instance INST(Foldable) where
  foldr f z (PMap m) = foldr (flip $ foldr f) z m
  foldl f z (PMap m) = foldl (foldl f) z m
  foldMap f (PMap m) = foldMap (foldMap f) m

instance INST(Traversable) where
  traverse f (PMap m) = castMap $ traverse (traverse f) m