{-# LANGUAGE CPP, BangPatterns, ViewPatterns, FlexibleInstances #-}
module Data.TrieMap.RadixTrie.Subset () where

import Control.Monad
import Control.Monad.Option

import Data.TrieMap.RadixTrie.Base

import Prelude hiding (lookup)

#define V(f) f (VVector) (k)
#define U(f) f (PVector) (Word)
#define EDGE(args) (!(eView -> Edge args))

instance TrieKey k => Subset (TrieMap (VVector k)) where
  Radix m1 <=? Radix m2 = m1 <<=? m2

instance Subset (TrieMap (PVector Word)) where
  WRadix m1 <=? WRadix m2 = m1 <<=? m2

instance (Eq k, Label v k) => Subset (Edge v k) where
  {-# SPECIALIZE instance (Eq k, TrieKey k) => Subset (V(Edge)) #-}
  {-# SPECIALIZE instance Subset (U(Edge)) #-}
  eK@EDGE(_ ks0 vK tsK) <=? EDGE(_ ls0 vL tsL) = matchSlice matcher matches ks0 ls0 where
    matcher k l z = k == l && z
    matches kLen lLen = case compare kLen lLen of
      LT	-> False
      EQ	-> vK <=? vL && tsK <<=? tsL
      GT	-> let k = ks0 !$ lLen in isSome (mfilter (dropEdge (lLen + 1) eK <=?) (lookup k tsL))
