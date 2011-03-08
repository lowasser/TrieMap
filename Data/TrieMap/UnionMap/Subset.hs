{-# LANGUAGE ViewPatterns, CPP, FlexibleInstances, FlexibleContexts #-}
module Data.TrieMap.UnionMap.Subset () where

import Data.TrieMap.UnionMap.Base

#define UVIEW uView -> UView

instance (Subset (TrieMap k1), Subset (TrieMap k2)) => Subset (TrieMap (Either k1 k2)) where
  (UVIEW m1L m1R) <=? (UVIEW m2L m2R) =
    (m1L <<=? m2L) && (m1R <<=? m2R)