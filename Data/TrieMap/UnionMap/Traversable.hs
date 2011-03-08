{-# LANGUAGE FlexibleContexts, FlexibleInstances, CPP, ViewPatterns #-}
module Data.TrieMap.UnionMap.Traversable () where

import Data.TrieMap.UnionMap.Base
import Prelude hiding (foldr, foldl)

#define INST(cl) (cl (TrieMap k1), cl (TrieMap k2)) => cl (TrieMap (Either k1 k2))
#define UVIEW uView -> UView

instance INST(Functor) where
  fmap _ Empty = Empty
  fmap f (MapL mL) = MapL (f <$> mL)
  fmap f (MapR mR) = MapR (f <$> mR)
  fmap f (Union s mL mR) = Union s (fmap f mL) (fmap f mR)

instance INST(Foldable) where
  foldMap f (UVIEW mL mR) = (foldMap f <$> mL) `mappendM` (foldMap f <$> mR)
  foldr f z (UVIEW mL mR) =
    foldl (foldr f) (foldl (foldr f) z mR) mL
  foldl f z (UVIEW mL mR) =
    foldl (foldl f) (foldl (foldl f) z mL) mR

instance INST(Traversable) where
  traverse _ Empty = pure Empty
  traverse f (MapL mL) = MapL <$> traverse f mL
  traverse f (MapR mR) = MapR <$> traverse f mR
  traverse f (Union s mL mR) = Union s <$> traverse f mL <*> traverse f mR