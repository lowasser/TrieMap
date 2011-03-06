{-# LANGUAGE UnboxedTuples, MagicHash, FlexibleInstances, FlexibleContexts #-}
module Data.TrieMap.UnionMap.Indexable () where

import Data.TrieMap.UnionMap.Base

import GHC.Exts

instance (Indexable (TrieMap k1), Indexable (TrieMap k2), TrieKey k1) => Indexable (TrieMap (Either k1 k2)) where
  index i m = case m of
    MapL mL	-> case index i mL of
      (# i', a, hL #) -> (# i', a, HoleX0 hL #)
    MapR mR	-> case index i mR of
      (# i', a, hR #) -> (# i', a, Hole0X hR #)
    Union _  mL mR
      | i <# sL, (# i', a, hL #) <- index i mL
	  -> (# i', a, HoleXR hL mR #)
      | (# i', a, hR #) <- index (i -# sL) mR
	  -> (# i', a, HoleLX mL hR #)
      where !sL = sizeM# mL
    Empty	-> indexFail ()
