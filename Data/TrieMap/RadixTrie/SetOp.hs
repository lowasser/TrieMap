{-# LANGUAGE CPP, BangPatterns, ViewPatterns, FlexibleInstances #-}
module Data.TrieMap.RadixTrie.SetOp () where

import Control.Monad.Option

import Data.TrieMap.RadixTrie.Base
import Data.TrieMap.RadixTrie.Search ()

import Prelude hiding (lookup)

#define V(f) f (VVector) (k)
#define U(f) f (PVector) (Word)
#define EDGE(args) (!(eView -> Edge args))
#define SETOP(rad,op,opE) op f (rad m1) (rad m2) = rad (op (opE f) m1 m2)

instance TrieKey k => SetOp (TrieMap (VVector k)) where
  SETOP(Radix,union,unionEdge)
  SETOP(Radix,isect,isectEdge)
  SETOP(Radix,diff,diffEdge)

instance SetOp (TrieMap (PVector Word)) where
  SETOP(WRadix,union,unionEdge)
  SETOP(WRadix,isect,isectEdge)
  SETOP(WRadix,diff,diffEdge)

{-# SPECIALIZE unionEdge :: 
      (TrieKey k, Sized a) => (a -> a -> Maybe a) -> V(Edge) a -> V(Edge) a -> V(MEdge) a,
      Sized a => (a -> a -> Maybe a) -> U(Edge) a -> U(Edge) a -> U(MEdge) a #-}
unionEdge :: (Label v k, Sized a) => 
	(a -> a -> Maybe a) -> Edge v k a -> Edge v k a -> MEdge v k a
unionEdge f = unionE where
  unionE !eK@EDGE(_ ks0 !vK tsK) !eL@EDGE(_ ls0 !vL tsL) = iMatchSlice matcher matches ks0 ls0 where
    matcher !i k l z = runOption (unifyM k eK' l eL') z $ Just . edge (takeSlice i ks0) Nothing
      where eK' = dropEdge (i+1) eK
	    eL' = dropEdge (i+1) eL
    
    matches kLen lLen = case compare kLen lLen of
      EQ -> cEdge ks0 (union f vK vL) $ union unionE tsK tsL
      LT -> cEdge ks0 vK $ alter g l tsK where
	eL' = dropEdge (kLen + 1) eL; l = ls0 !$ kLen
	g eK = union unionE eK (Just eL')
      GT -> cEdge ls0 vL $ alter g k tsL where
	eK' = dropEdge (lLen + 1) eK; k = ks0 !$ lLen
	g = union unionE (Just eK')

{-# SPECIALIZE isectEdge ::
      (TrieKey k, Sized c) => (a -> b -> Maybe c) -> V(Edge) a -> V(Edge) b -> V(MEdge) c,
      Sized c => (a -> b -> Maybe c) -> U(Edge) a -> U(Edge) b -> U(MEdge) c #-}
isectEdge :: (Eq k, Label v k, Sized c) =>
	(a -> b -> Maybe c) -> Edge v k a -> Edge v k b -> MEdge v k c
isectEdge f = isectE where
  isectE !eK@EDGE(_ ks0 vK tsK) !eL@EDGE(_ ls0 vL tsL) = matchSlice matcher matches ks0 ls0 where
    matcher k l z = guard (k == l) >> z
    matches kLen lLen = case compare kLen lLen of
      EQ -> cEdge ks0 (isect f vK vL) $ isect isectE tsK tsL
      LT -> let l = ls0 !$ kLen in runOption (lookup l tsK) Nothing $ \ eK' ->
	      let eL' = dropEdge (kLen + 1) eL in unDropEdge (kLen + 1) <$> eK' `isectE` eL'
      GT -> let k = ks0 !$ lLen in runOption (lookup k tsL) Nothing $ \ eL' ->
	      let eK' = dropEdge (lLen + 1) eK in unDropEdge (lLen + 1) <$> eK' `isectE` eL'

{-# SPECIALIZE diffEdge ::
      (TrieKey k, Sized a) => (a -> b -> Maybe a) -> V(Edge) a -> V(Edge) b -> V(MEdge) a,
      Sized a => (a -> b -> Maybe a) -> U(Edge) a -> U(Edge) b -> U(MEdge) a #-}
diffEdge :: (Eq k, Label v k, Sized a) =>
	(a -> b -> Maybe a) -> Edge v k a -> Edge v k b -> MEdge v k a
diffEdge f = diffE where
  diffE !eK@EDGE(_ ks0 !vK tsK) !eL@EDGE(_ ls0 !vL tsL) = matchSlice matcher matches ks0 ls0 where
    matcher k l z
      | k == l		= z
      | otherwise	= Just eK
    matches kLen lLen = case compare kLen lLen of
      EQ -> cEdge ks0 (diff f vK vL) $ diff diffE tsK tsL
      LT -> search l tsK nomatch match where
	l = ls0 !$ kLen; eL' = dropEdge (kLen + 1) eL 
	nomatch _ = Just eK
	match eK' holeKT = cEdge ks0 vK $ fill (eK' `diffE` eL') holeKT
      GT -> let k = ks0 !$ lLen; eK' = dropEdge (lLen + 1) eK in 
	runOption (lookup k tsL) (Just eK) (\ eL' -> fmap (unDropEdge (lLen + 1)) (eK' `diffE` eL'))
