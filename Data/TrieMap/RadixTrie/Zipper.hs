{-# LANGUAGE CPP, FlexibleInstances, BangPatterns, ViewPatterns #-}
#if __GLASGOW_HASKELL__ >= 700
{-# OPTIONS -fllvm #-}
#endif
module Data.TrieMap.RadixTrie.Zipper () where

import Data.TrieMap.RadixTrie.Base

#define V(f) f (VVector) (k)
#define U(f) f (PVector) (Word)
#define LOC(args) !(locView -> Loc args)
#define DEEP(args) !(pView -> Deep args)

instance TrieKey k => Zippable (TrieMap (VVector k)) where
  empty = Radix Nothing
  clear (Hole h) = Radix (clearEdge h)
  assign a (Hole h) = Radix (Just (assignEdge a h))

instance Zippable (TrieMap (PVector Word)) where
  empty = WRadix Nothing
  clear (WHole h) = WRadix (clearEdge h)
  assign a (WHole h) = WRadix (Just (assignEdge a h))

{-# INLINE assignEdge #-}
assignEdge :: (Label v k, Sized a) => a -> EdgeLoc v k a -> Edge v k a
assignEdge v LOC(ks ts path) = assignP (edge ks (Just v) ts) path

{-# SPECIALIZE assignP ::
      (TrieKey k, Sized a) => V(Edge) a -> V(Path) a -> V(Edge) a,
      Sized a => U(Edge) a -> U(Path) a -> U(Edge) a #-}
assignP :: (Label v k, Sized a) => Edge v k a -> Path v k a -> Edge v k a
assignP e DEEP(path ks v tHole)	= assignP (edge ks v (assign e tHole)) path
assignP e _			= e

{-# SPECIALIZE clearEdge :: 
      (TrieKey k, Sized a) => V(EdgeLoc) a -> V(MEdge) a,
      Sized a => U(EdgeLoc) a -> U(MEdge) a #-}
clearEdge :: (Label v k, Sized a) => EdgeLoc v k a -> MEdge v k a
clearEdge LOC(ks ts path) = rebuild (cEdge ks Nothing ts) path where
  rebuild Nothing DEEP(path ks v tHole)	= rebuild (cEdge ks v (clear tHole)) path
  rebuild Nothing _	= Nothing
  rebuild (Just e) path = Just $ assignP e path