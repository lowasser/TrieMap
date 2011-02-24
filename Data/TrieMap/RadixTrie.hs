{-# LANGUAGE BangPatterns, FlexibleContexts, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, CPP #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.TrieMap.RadixTrie () where

import Data.TrieMap.TrieKey

import Data.Vector (Vector)
import qualified Data.Vector.Primitive as P
import Data.Word

import Data.TrieMap.RadixTrie.Edge
import Data.TrieMap.RadixTrie.Label

import Prelude hiding (length, and, zip, zipWith, foldr, foldl)

#define VINSTANCE(cl) (TrieKey k, cl (TrieMap k)) => cl (TrieMap (Vector k))

instance VINSTANCE(Functor) where
  fmap f (Radix m) = Radix (fmap f <$> m)

instance VINSTANCE(Foldable) where
  foldMap f (Radix m) = foldMap (foldMap f) m
  foldr f z (Radix m) = foldl (foldr f) z m
  foldl f z (Radix m) = foldl (foldl f) z m

instance VINSTANCE(Traversable) where
  traverse _ (Radix Nothing) = pure emptyM
  traverse f (Radix (Just m)) = Radix . Just <$> traverse f m

instance VINSTANCE(Subset) where
  Radix m1 <=? Radix m2 = m1 <<=? m2

instance TrieKey k => Buildable (TrieMap (Vector k)) (Vector k) where
  type UStack (TrieMap (Vector k)) = Edge Vector k
  {-# INLINE uFold #-}
  uFold f = Foldl{
    zero = emptyM,
    begin = singletonEdge,
    snoc = \ e ks a -> insertEdge (f a) ks a e,
    done = Radix . Just}
  type AStack (TrieMap (Vector k)) = Stack Vector k
  {-# INLINE aFold #-}
  aFold f = Radix <$> fromAscListEdge f
  type DAStack (TrieMap (Vector k)) = Stack Vector k
  {-# INLINE daFold #-}
  daFold = aFold const

#define SETOP(rad,op,opE) op f (rad m1) (rad m2) = rad (op (opE f) m1 m2)

instance VINSTANCE(SetOp) where
  SETOP(Radix,union,unionEdge)
  SETOP(Radix,isect,isectEdge)
  SETOP(Radix,diff,diffEdge)

instance VINSTANCE(Project) where
  mapMaybe f (Radix m) = Radix (mapMaybe (mapMaybeEdge f) m)
  mapEither f (Radix m) = both' Radix Radix (mapEither (mapEitherEdge f)) m

-- | @'TrieMap' ('Vector' k) a@ is a traditional radix trie.
instance TrieKey k => TrieKey (Vector k) where
	newtype TrieMap (Vector k) a = Radix (MEdge Vector k a)
	newtype Hole (Vector k) a = Hole (EdgeLoc Vector k a)
	
	emptyM = Radix Nothing
	singletonM ks a = Radix (Just (singletonEdge ks a))
	getSimpleM (Radix Nothing)	= Null
	getSimpleM (Radix (Just e))	= getSimpleEdge e
	sizeM (Radix m) = getSize m
	lookupMC ks (Radix (Just e)) = lookupEdge ks e
	lookupMC _ _ = mzero

	singleHoleM ks = Hole (singleLoc ks)
	{-# INLINE searchMC #-}
	searchMC ks (Radix (Just e)) = mapSearch Hole (searchEdgeC ks e)
	searchMC ks _ = \ f _ -> f (singleHoleM ks)
	indexM (Radix (Just e)) i = case indexEdge e i of
	  (# i', a, loc #) -> (# i', a, Hole loc #)
	indexM _ _ = indexFail ()

	clearM (Hole loc) = Radix (clearEdge loc)
	{-# INLINE assignM #-}
	assignM a (Hole loc) = Radix (Just (assignEdge a loc))
	
	extractHoleM (Radix (Just e)) = fmap Hole <$> extractEdgeLoc e root
	extractHoleM _ = mzero
	
	beforeM (Hole loc) = Radix (beforeEdge Nothing loc)
	beforeWithM a (Hole loc) = Radix (beforeEdge (Just a) loc)
	afterM (Hole loc) = Radix (afterEdge Nothing loc)
	afterWithM a (Hole loc) = Radix (afterEdge (Just a) loc)
	
	insertWithM f ks v (Radix e) = Radix (Just (maybe (singletonEdge ks v) (insertEdge f ks v) e))

type WordVec = P.Vector Word

#define PINSTANCE(cl) cl (TrieMap (P.Vector Word))

instance PINSTANCE(Functor) where
  fmap f (WRadix m) = WRadix (fmap f <$> m)

instance PINSTANCE(Foldable) where
  foldMap f (WRadix m) = foldMap (foldMap f) m
  foldr f z (WRadix m) = foldl (foldr f) z m
  foldl f z (WRadix m) = foldl (foldl f) z m

instance PINSTANCE(Traversable) where
  traverse _ (WRadix Nothing) = pure emptyM
  traverse f (WRadix (Just m)) = WRadix . Just <$> traverse f m

instance PINSTANCE(Subset) where
  WRadix m1 <=? WRadix m2 = m1 <<=? m2

instance PINSTANCE(SetOp) where
  SETOP(WRadix,union,unionEdge)
  SETOP(WRadix,isect,isectEdge)
  SETOP(WRadix,diff,diffEdge)

instance Buildable (TrieMap WordVec) WordVec where
  type UStack (TrieMap WordVec) = Edge P.Vector Word
  {-# INLINE uFold #-}
  uFold f = Foldl{
    zero = emptyM,
    begin = singletonEdge,
    snoc = \ e ks a -> insertEdge (f a) ks a e,
    done = WRadix . Just}
  type AStack (TrieMap WordVec) = Stack P.Vector Word
  {-# INLINE aFold #-}
  aFold f = WRadix <$> fromAscListEdge f
  type DAStack (TrieMap WordVec) = Stack P.Vector Word
  {-# INLINE daFold #-}
  daFold = aFold const

instance PINSTANCE(Project) where
  mapMaybe f (WRadix m) = WRadix (mapMaybe (mapMaybeEdge f) m)
  mapEither f (WRadix m) = both' WRadix WRadix (mapEither (mapEitherEdge f)) m

-- | @'TrieMap' ('P.Vector' Word) a@ is a traditional radix trie specialized for word arrays.
instance TrieKey (P.Vector Word) where
	newtype TrieMap WordVec a = WRadix (MEdge P.Vector Word a)
	newtype Hole WordVec a = WHole (EdgeLoc P.Vector Word a)
	
	emptyM = WRadix Nothing
	singletonM ks a = WRadix (Just (singletonEdge ks a))
	getSimpleM (WRadix Nothing)	= Null
	getSimpleM (WRadix (Just e))	= getSimpleEdge e
	sizeM (WRadix m) = getSize m
	lookupMC ks (WRadix (Just e)) = lookupEdge ks e
	lookupMC _ _ = mzero

	singleHoleM ks = WHole (singleLoc ks)
	{-# INLINE searchMC #-}
	searchMC ks (WRadix (Just e)) f g = searchEdgeC ks e f' g' where
	  f' loc = f (WHole loc)
	  g' a loc = g a (WHole loc)
	searchMC ks _ f _ = f (singleHoleM ks)
	indexM (WRadix (Just e)) i = case indexEdge e i of
	  (# i', a, loc #) -> (# i', a, WHole loc #)
	indexM _ _ = indexFail ()
	
	clearM (WHole loc) = WRadix (clearEdge loc)
	{-# INLINE assignM #-}
	assignM a (WHole loc) = WRadix (Just (assignEdge a loc))

	extractHoleM (WRadix (Just e)) = do
		(a, loc) <- extractEdgeLoc e root
		return (a, WHole loc)
	extractHoleM _ = mzero

	beforeM (WHole loc) = WRadix (beforeEdge Nothing loc)
	beforeWithM a (WHole loc) = WRadix (beforeEdge (Just a) loc)
	afterM (WHole loc) = WRadix (afterEdge Nothing loc)
	afterWithM a (WHole loc) = WRadix (afterEdge (Just a) loc)
	
	insertWithM f ks v (WRadix e) = WRadix (Just (maybe (singletonEdge ks v) (insertEdge f ks v) e))