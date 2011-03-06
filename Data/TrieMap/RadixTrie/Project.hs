{-# LANGUAGE CPP, BangPatterns, ViewPatterns, UnboxedTuples, FlexibleInstances #-}
module Data.TrieMap.RadixTrie.Project () where

import Data.TrieMap.RadixTrie.Base

#define V(f) f (VVector) (k)
#define U(f) f (PVector) (Word)
#define EDGE(args) (!(eView -> Edge args))

instance TrieKey k => Project (TrieMap (VVector k)) where
  mapMaybe f (Radix m) = Radix (mapMaybe (mapMaybeEdge f) m)
  mapEither f (Radix m) = both' Radix Radix (mapEither (mapEitherEdge f)) m

instance Project (TrieMap (PVector Word)) where
  mapMaybe f (WRadix m) = WRadix (mapMaybe (mapMaybeEdge f) m)
  mapEither f (WRadix m) = both' WRadix WRadix (mapEither (mapEitherEdge f)) m

{-# SPECIALIZE mapMaybeEdge ::
      (TrieKey k, Sized b) => (a -> Maybe b) -> V(Edge) a -> V(MEdge) b,
      Sized b => (a -> Maybe b) -> U(Edge) a -> U(MEdge) b #-}
mapMaybeEdge :: (Label v k, Sized b) => (a -> Maybe b) -> Edge v k a -> MEdge v k b
mapMaybeEdge f = mapMaybeE where
  mapMaybeE !EDGE(_ ks !v ts) = let !v' = v >>= f in cEdge ks v' (mapMaybe mapMaybeE ts)

{-# SPECIALIZE mapEitherEdge ::
      (TrieKey k, Sized b, Sized c) => (a -> (# Maybe b, Maybe c #)) -> V(Edge) a -> (# V(MEdge) b, V(MEdge) c #),
      (Sized b, Sized c) => (a -> (# Maybe b, Maybe c #)) -> U(Edge) a -> (# U(MEdge) b, U(MEdge) c #) #-}
mapEitherEdge :: (Label v k, Sized b, Sized c) => 
	(a -> (# Maybe b, Maybe c #)) -> Edge v k a -> (# MEdge v k b, MEdge v k c #)
mapEitherEdge f = mapEitherE where
	mapEitherE EDGE(_ ks v ts) = (# cEdge ks vL tsL, cEdge ks vR tsR #)
	  where	!(# vL, vR #) = mapEither f v
		!(# tsL, tsR #) = mapEither mapEitherE ts
