{-# LANGUAGE CPP, BangPatterns, ViewPatterns, FlexibleInstances, TypeOperators, FlexibleContexts, TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#if __GLASGOW_HASKELL__ >= 700
{-# OPTIONS -fllvm #-}
#endif
module Data.TrieMap.RadixTrie.Search (insertEdge) where

import Control.Monad.Unpack
import Control.Monad.Option

import Data.TrieMap.RadixTrie.Base
import Data.TrieMap.RadixTrie.Zipper ()

import Data.Vector.Generic (length)

import Prelude hiding (lookup, length)

#define V(f) f (VVector) (k)
#define U(f) f (PVector) (Word)
#define EDGE(args) (!(eView -> Edge args))

instance TrieKey k => Searchable (TrieMap (VVector k)) (VVector k) where
  {-# INLINE search #-}
  search ks (Radix m) nomatch0 match0 = case m of
      Nothing	-> nomatch $~ singleLoc ks
      Just e	-> searchEdgeC ks e nomatch match
    where nomatch = unpack (nomatch0 . Hole)
	  match a = unpack (match0 a . Hole)
  
  singleZip ks = Hole (singleLoc ks)
  singleton ks a = Radix (Just (singletonEdge ks a))
  
  lookup ks (Radix m) = maybeToOption m >>= lookupEdge ks
  
  insertWith f ks a (Radix (Just e)) = Radix (Just (insertEdge f ks a e))
  insertWith _ ks a (Radix Nothing) = singleton ks a

instance Searchable (TrieMap (PVector Word)) (PVector Word) where
  {-# INLINE search #-}
  search ks (WRadix m) nomatch0 match0 = case m of
      Nothing	-> nomatch $~ singleLoc ks
      Just e	-> searchEdgeC ks e nomatch match
    where nomatch = unpack (nomatch0 . WHole)
	  match a = unpack (match0 a . WHole)
  
  singleZip ks = WHole (singleLoc ks)
  singleton ks a = WRadix (Just (singletonEdge ks a))
  
  lookup ks (WRadix m) = maybeToOption m >>= lookupEdge ks
  
  insertWith f ks a (WRadix (Just e)) = WRadix (Just (insertEdge f ks a e))
  insertWith _ ks a (WRadix Nothing) = singleton ks a

{-# SPECIALIZE lookupEdge ::
      TrieKey k => V() -> V(Edge) a -> Option a,
      U() -> U(Edge) a -> Option a #-}
lookupEdge :: (Eq k, Label v k) => v k -> Edge v k a -> Option a
lookupEdge ks e = option $ \ no yes -> let
  lookupE !ks !EDGE(_ ls !v ts) = if kLen < lLen then no else matchSlice matcher matches ks ls where
    !kLen = length ks
    !lLen = length ls
    matcher k l z
	    | k == l	  = z
	    | otherwise	  = no
    matches _ _
	    | kLen == lLen  = maybe no yes v
	    | (_, k, ks') <- splitSlice lLen ks
			  = runOption (lookup k ts) no (lookupE ks')
  in lookupE ks e

{-# SPECIALIZE INLINE searchEdgeC ::
      TrieKey k => V() -> V(Edge) a -> (V(EdgeLoc) a :~> r) -> (a -> V(EdgeLoc) a :~> r) -> r,
      U() -> U(Edge) a -> (U(EdgeLoc) a :~> r) -> (a -> U(EdgeLoc) a :~> r) -> r #-}
searchEdgeC :: (Eq k, Label v k, Unpackable (EdgeLoc v k a)) => 
  v k -> Edge v k a -> (EdgeLoc v k a :~> r) -> (a -> EdgeLoc v k a :~> r) -> r
searchEdgeC ks0 e nomatch match = searchE ks0 e root where
  searchE !ks e@EDGE(_ !ls !v ts) path = iMatchSlice matcher matches ks ls where
    matcher i k l z = 
      runOption (unifierM k l (dropEdge (i+1) e)) z 
	(\ tHole -> nomatch $~ loc (dropSlice (i+1) ks) empty (deep path (takeSlice i ls) Nothing tHole))
    matches kLen lLen = case compare kLen lLen of
      LT -> let lPre = takeSlice kLen ls; l = ls !$ kLen; e' = dropEdge (kLen + 1) e in
	      nomatch $~ loc lPre (singleton l e') path
      EQ -> maybe nomatch match v $~ loc ls ts path
      GT -> let
	  {-# INLINE kk #-}
	  kk = ks !$ lLen
	  ks' = dropSlice (lLen + 1) ks
	  nomatch' tHole = nomatch $~ loc ks' empty (deep path ls v tHole)
	  match' e' tHole = searchE ks' e' (deep path ls v tHole)
	  in search kk ts nomatch' match'

{-# SPECIALIZE insertEdge ::
      (TrieKey k, Sized a) => (a -> a) -> V() -> a -> V(Edge) a -> V(Edge) a,
      Sized a => (a -> a) -> U() -> a -> U(Edge) a -> U(Edge) a #-}
insertEdge :: (Label v k, Sized a) => (a -> a) -> v k -> a -> Edge v k a -> Edge v k a
insertEdge f ks0 a e = insertE ks0 e where
  !sza = getSize a
  insertE !ks eL@EDGE(szL ls !v ts) = iMatchSlice matcher matches ks ls where
    !szV = szL - sizeM ts
    matcher !i k l z = runOption (unifyM k eK' l eL') z (edge (takeSlice i ls) Nothing)
      where	eK' = edge' sza (dropSlice (i+1) ks) (Just a) empty
		eL' = dropEdge (i+1) eL
    matches kLen lLen = case compare kLen lLen of
      LT -> (edge' (sza + szL) ks (Just a) (singleton l eL'))
	  where	l = ls !$ kLen; eL' = dropEdge (kLen+1) eL
      EQ -> (edge ls (Just (maybe a f v)) ts)
      GT -> edge' sz' ls v ts' where
	ks' = dropSlice (lLen + 1) ks
	k = ks !$ lLen
	ts' = insertWith (insertE ks') k (edge' sza ks' (Just a) empty) ts
	sz' = sizeM ts' + szV
