{-# LANGUAGE CPP, MagicHash, UnboxedTuples, FlexibleInstances, BangPatterns #-}
module Data.TrieMap.RadixTrie.Index () where

import Data.TrieMap.RadixTrie.Base

import GHC.Exts

#define V(f) f (VVector) (k)
#define U(f) f (PVector) (Word)

instance TrieKey k => Indexable (TrieMap (VVector k)) where
  index i (Radix (Just e)) = case indexEdge e i of
    (# i', a, loc #) -> (# i', a, Hole loc #)
  index _ _ = indexFail ()

instance Indexable (TrieMap (PVector Word)) where
  index i (WRadix (Just e)) = case indexEdge e i of
    (# i', a, loc #) -> (# i', a, WHole loc #)
  index _ _ = indexFail ()

{-# SPECIALIZE indexEdge :: 
      (TrieKey k, Sized a) => V(Edge) a -> Int# -> (# Int#, a, V(EdgeLoc) a #),
      Sized a => U(Edge) a -> Int# -> (# Int#, a, U(EdgeLoc) a #) #-}
indexEdge :: (Label v k, Sized a) => Edge v k a -> Int# -> (# Int#, a, EdgeLoc v k a #)
indexEdge e i = let
  indexE i !e path = case eView e of
    Edge sE ks v@(Just a) ts
      | i <# sv		-> (# i, a, loc ks ts path #)
      | otherwise	-> case index (i -# sv) ts of
	  (# i', e', tHole #) -> indexE i' e' (deep path ks v tHole)
	  where	!sv = unbox $ sE - sizeM ts
    Edge _ ks Nothing ts -> case index i ts of
	  (# i', e', tHole #) -> indexE i' e' (deep path ks Nothing tHole)
  in indexE i e root