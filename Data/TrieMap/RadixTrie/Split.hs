{-# LANGUAGE CPP, FlexibleInstances, BangPatterns, ViewPatterns #-}
module Data.TrieMap.RadixTrie.Split () where

import Data.TrieMap.RadixTrie.Base

#define V(f) f (VVector) (k)
#define U(f) f (PVector) (Word)
#define LOC(args) !(locView -> Loc args)
#define DEEP(args) !(pView -> Deep args)

instance TrieKey k => Splittable (TrieMap (VVector k)) where
  before (Hole LOC(_ _ path)) = Radix (beforeE path)
  after (Hole LOC(ks ts path)) = case cEdge ks Nothing ts of
    Nothing	-> Radix (afterE path)
    Just e	-> Radix (Just (afterWithE e path))
  beforeWith a (Hole LOC(ks _ path)) = Radix (Just (beforeWithE (singletonEdge ks a) path))
  afterWith a (Hole LOC(ks ts path)) = Radix (Just (afterWithE (edge ks (Just a) ts) path))

instance Splittable (TrieMap (PVector Word)) where
  before (WHole LOC(_ _ path)) = WRadix (beforeE path)
  after (WHole LOC(ks ts path)) = case cEdge ks Nothing ts of
    Nothing	-> WRadix (afterE path)
    Just e	-> WRadix (Just (afterWithE e path))
  beforeWith a (WHole LOC(ks _ path)) = WRadix (Just (beforeWithE (singletonEdge ks a) path))
  afterWith a (WHole LOC(ks ts path)) = WRadix (Just (afterWithE (edge ks (Just a) ts) path))

{-# SPECIALIZE beforeE ::
      (TrieKey k, Sized a) => V(Path) a -> V(MEdge) a, 
      Sized a => U(Path) a -> U(MEdge) a #-}
{-# SPECIALIZE afterE ::
      (TrieKey k, Sized a) => V(Path) a -> V(MEdge) a, 
      Sized a => U(Path) a -> U(MEdge) a #-}
{-# SPECIALIZE beforeWithE ::
      (TrieKey k, Sized a) => V(Edge) a -> V(Path) a -> V(Edge) a, 
      Sized a => U(Edge) a -> U(Path) a -> U(Edge) a #-}
{-# SPECIALIZE afterWithE ::
      (TrieKey k, Sized a) => V(Edge) a -> V(Path) a -> V(Edge) a, 
      Sized a => U(Edge) a -> U(Path) a -> U(Edge) a #-}
beforeE, afterE :: (Sized a, Label v k) => Path v k a -> MEdge v k a
beforeWithE, afterWithE :: (Sized a, Label v k) => Edge v k a -> Path v k a -> Edge v k a
beforeE DEEP(path ks v tHole) = case cEdge ks v (before tHole) of
    Nothing	-> beforeE path
    Just e	-> Just $ beforeWithE e path
beforeE _	= Nothing
beforeWithE e DEEP(path ks v tHole)
		= beforeWithE (edge ks v (beforeWith e tHole)) path
beforeWithE e _	= e

afterE DEEP(path ks _ tHole) = case cEdge ks Nothing (after tHole) of
	    Nothing	-> afterE path
	    Just e	-> Just $ afterWithE e path
afterE _ 	= Nothing
afterWithE e DEEP(path ks _ tHole)
		= afterWithE (edge ks Nothing (afterWith e tHole)) path
afterWithE e _	= e