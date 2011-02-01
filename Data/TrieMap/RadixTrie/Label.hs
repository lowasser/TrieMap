{-# LANGUAGE MagicHash, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, BangPatterns, CPP, ViewPatterns #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.TrieMap.RadixTrie.Label where

import Data.TrieMap.TrieKey
import Data.TrieMap.Sized
import Data.TrieMap.RadixTrie.Slice
import Data.TrieMap.WordMap

import Data.Word
import Data.Vector.Generic
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S

import Prelude hiding (length)
import GHC.Exts

#define V(ty) (ty (V.Vector) (k))
#define U(ty) (ty (S.Vector) Word)

class (Vector v k, TrieKey k) => Label v k where
  data Edge v k :: * -> *
  data Path v k :: * -> *
  data EdgeLoc v k :: * -> *
  edge :: Sized a => v k -> Maybe a -> Branch v k a -> Edge v k a
  edge' :: Int -> v k -> Maybe a -> Branch v k a -> Edge v k a
  root :: Path v k a
  deep :: Path v k a -> v k -> Maybe a -> BHole v k a -> Path v k a
  loc :: v k -> Branch v k a -> Path v k a -> EdgeLoc v k a
  
  eView :: Edge v k a -> EView v k a
  pView :: Path v k a -> PView v k a
  locView :: EdgeLoc v k a -> LocView v k a

type BHole v k a = Hole k (Edge v k a)

type Branch v k a = TrieMap k (Edge v k a)
data EView v k a =
	Edge Int (v k) (Maybe a) (Branch v k a)
data LocView v k a = Loc !( v k) (Branch v k a) (Path v k a)
data PView v k a = Root
	| Deep (Path v k a) (v k) (Maybe a) (BHole v k a)
type MEdge v k a = Maybe (Edge v k a)

instance Sized (EView v k a) where
  getSize# (Edge (I# sz#) _ _ _) = sz#

instance Label v k => Sized (Edge v k a) where
  {-# SPECIALIZE instance TrieKey k => Sized (Edge V.Vector k a) #-}
  getSize# e = getSize# (eView e)

instance TrieKey k => Label V.Vector k where
  data Edge V.Vector k a =
    VEdge Int !(V()) (V(Branch) a)
    | VEdgeX Int !(V()) a (V(Branch) a)
  data Path V.Vector k a =
    VRoot
    | VDeep (V(Path) a) !(V()) (V(BHole) a)
    | VDeepX (V(Path) a) !(V()) a (V(BHole) a)
  data EdgeLoc V.Vector k a = VLoc !(V()) (V(Branch) a) (V(Path) a)
  
  edge !ks Nothing ts = VEdge (sizeM ts) ks ts
  edge !ks (Just a) ts = VEdgeX (sizeM ts + getSize a) ks a ts
  edge' s !ks Nothing ts = VEdge s ks ts
  edge' s !ks (Just a) ts = VEdgeX s ks a ts
  
  root = VRoot
  deep path !ks Nothing tHole = VDeep path ks tHole
  deep path !ks (Just a) tHole = VDeepX path ks a tHole
  
  loc = VLoc
  
  eView (VEdge sz# ks ts) = Edge sz# ks Nothing ts
  eView (VEdgeX sz# ks v ts) = Edge sz# ks (Just v) ts
  pView VRoot = Root
  pView (VDeep path ks tHole) = Deep path ks Nothing tHole
  pView (VDeepX path ks v tHole) = Deep path ks (Just v) tHole
  locView (VLoc ks ts path) = Loc ks ts path

instance Label S.Vector Word where
  data Edge S.Vector Word a =
    SEdge !Int !(U()) !(SNode (U(Edge) a))
    | SEdgeX !Int !(U()) a !(SNode (U(Edge) a))
  data Path S.Vector Word a =
    SRoot
    | SDeep (U(Path) a) !(U()) !(WHole (U(Edge) a))
    | SDeepX (U(Path) a) !(U()) a !(WHole (U(Edge) a))
  data EdgeLoc S.Vector Word a =
    SLoc !(U()) !(SNode (U(Edge) a)) (U(Path) a)
  
  edge !ks Nothing ts = SEdge (sizeM ts) ks (getWordMap ts)
  edge !ks (Just v) ts = SEdgeX (getSize v + sizeM ts) ks v (getWordMap ts)
  edge' sz# !ks Nothing ts = SEdge sz# ks (getWordMap ts)
  edge' sz# !ks (Just v) ts = SEdgeX sz# ks v (getWordMap ts)
  
  root = SRoot
  deep path !ks Nothing tHole = SDeep path ks (getHole tHole)
  deep path !ks (Just v) tHole = SDeepX path ks v (getHole tHole)

  loc ks ts path = SLoc ks (getWordMap ts) path

  eView (SEdge sz# ks ts) = Edge sz# ks Nothing (WordMap ts)
  eView (SEdgeX sz# ks v ts) = Edge sz# ks (Just v) (WordMap ts)
  pView SRoot = Root
  pView (SDeep path ks tHole) = Deep path ks Nothing (Hole tHole)
  pView (SDeepX path ks v tHole) = Deep path ks (Just v) (Hole tHole)
  locView (SLoc ks ts path) = Loc ks (WordMap ts) path

{-# SPECIALIZE singletonEdge ::
    (TrieKey k, Sized a) => V() -> a -> V(Edge) a,
    Sized a => U() -> a -> U(Edge) a #-}
singletonEdge :: (Label v k, Sized a) => v k -> a -> Edge v k a
singletonEdge ks a = edge ks (Just a) emptyM

{-# SPECIALIZE singleLoc :: 
    TrieKey k => V() -> V(EdgeLoc) a,
    U() -> U(EdgeLoc) a #-}
singleLoc :: Label v k => v k -> EdgeLoc v k a
singleLoc ks = loc ks emptyM root

{-# SPECIALIZE getSimpleEdge ::
    TrieKey k => V(Edge) a -> Simple a,
    U(Edge) a -> Simple a #-}
getSimpleEdge :: Label v k => Edge v k a -> Simple a
getSimpleEdge !(eView -> Edge _ _ v ts)
  | nullM ts	= maybe Null Singleton v
  | otherwise	= NonSimple

{-# SPECIALIZE INLINE dropEdge ::
    TrieKey k => Int -> V(Edge) a -> V(Edge) a,
    Int -> U(Edge) a -> U(Edge) a #-}
{-# SPECIALIZE INLINE unDropEdge ::
    TrieKey k => Int -> V(Edge) a -> V(Edge) a,
    Int -> U(Edge) a -> U(Edge) a #-}
dropEdge, unDropEdge :: Label v k => Int -> Edge v k a -> Edge v k a
dropEdge !n !(eView -> Edge sz# ks v ts) = edge' sz# (dropSlice n ks) v ts
unDropEdge !n !(eView -> Edge sz# ks v ts) = edge' sz# (unDropSlice n ks) v ts

{-# SPECIALIZE compact ::
    TrieKey k => V(Edge) a -> V(MEdge) a,
    U(Edge) a -> U(MEdge) a #-}
compact :: Label v k => Edge v k a -> MEdge v k a
compact !e@(eView -> Edge _ ks Nothing ts) = case getSimpleM ts of
  Null		-> Nothing
  Singleton e'	-> Just (unDropEdge (length ks + 1) e')
  NonSimple	-> Just e
compact e = Just e

{-# SPECIALIZE cEdge ::
    (TrieKey k, Sized a) => V() -> Maybe a -> V(Branch) a -> V(MEdge) a,
    Sized a => U() -> Maybe a -> U(Branch) a -> U(MEdge) a #-}
cEdge :: (Label v k, Sized a) => v k -> Maybe a -> Branch v k a -> MEdge v k a
cEdge ks v ts = compact (edge ks v ts)