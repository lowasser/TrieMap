{-# LANGUAGE CPP, ViewPatterns, FlexibleInstances, FlexibleContexts, BangPatterns, UnboxedTuples #-}
module Data.TrieMap.UnionMap.Projection () where

import Data.TrieMap.UnionMap.Base

import Prelude hiding ((^))

#define UVIEW uView -> UView

instance (Project (TrieMap k1), Project (TrieMap k2), TrieKey k1, TrieKey k2) => Project (TrieMap (Either k1 k2)) where
  mapMaybe f (UVIEW mL mR) = mapMaybe (mapMaybeM f) mL ^ mapMaybe (mapMaybeM f) mR
  mapEither f (UVIEW mL mR) = (# mLL ^ mRL, mLR ^ mRR #)
    where !(# mLL, mLR #) = mapEither (mapEitherM f) mL
	  !(# mRL, mRR #) = mapEither (mapEitherM f) mR