{-# LANGUAGE FlexibleInstances, CPP, ViewPatterns, MultiParamTypeClasses #-}
module Data.TrieMap.UnionMap.Searchable () where

import Data.TrieMap.UnionMap.Base
import Data.TrieMap.UnionMap.Zippable ()

import Prelude hiding ((^), lookup)

#define UVIEW uView -> UView

instance (TrieKey k1, TrieKey k2) => Searchable (TrieMap (Either k1 k2)) (Either k1 k2) where
  search (Left k) (UVIEW m1 m2) = mapHole (`hole1` m2) (searchM k m1)
  search (Right k) (UVIEW m1 m2) = mapHole (m1 `hole2`) (searchM k m2)
  
  singleZip = either (HoleX0 . singleZip) (Hole0X . singleZip)
  
  singleton (Left k) a = MapL (singleton k a)
  singleton (Right k) a = MapR (singleton k a)
  
  insertWith f (Left k) a (UVIEW m1 m2)
    = Just (insertWithM f k a m1) ^ m2
  insertWith f (Right k) a (UVIEW m1 m2)
    = m1 ^ Just (insertWithM f k a m2)
  
  lookup (Left k) (UVIEW (Just m1) _) = lookup k m1
  lookup (Right k) (UVIEW _ (Just m2)) = lookup k m2
  lookup _ _ = mzero