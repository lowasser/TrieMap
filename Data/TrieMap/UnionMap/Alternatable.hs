{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Data.TrieMap.UnionMap.Alternatable () where

import Data.TrieMap.UnionMap.Base

instance (Alternatable (TrieMap k1), Alternatable (TrieMap k2)) => Alternatable (TrieMap (Either k1 k2)) where
  alternate Empty = mzero
  alternate (MapL mL) = do
    (a, hole) <- alternate mL
    return (a, HoleX0 hole)
  alternate (MapR mR) = do
    (a, hole) <- alternate mR
    return (a, Hole0X hole)
  alternate (Union _ mL mR) = (do
    (a, hL) <- alternate mL
    return (a, HoleXR hL mR)) `mplus` (do
    (a, hR) <- alternate mR
    return (a, HoleLX mL hR))