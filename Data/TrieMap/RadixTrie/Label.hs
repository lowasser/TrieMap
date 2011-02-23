{-# LANGUAGE MagicHash, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, BangPatterns, CPP, ViewPatterns #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.TrieMap.RadixTrie.Label where

import Control.Monad.Unpack

import Data.TrieMap.TrieKey
import Data.TrieMap.Sized
import Data.TrieMap.RadixTrie.Slice
import Data.TrieMap.WordMap

import Data.Word
import Data.Vector.Generic
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as P

import Prelude hiding (length)

#define V(ty) (ty (V.Vector) (k))
#define U(ty) (ty (P.Vector) Word)

class (Unpackable (v k), Vector v k, TrieKey k) => Label v k where
  data Edge v k :: * -> *
  data Path v k :: * -> *
  data EdgeLoc v k :: * -> *
  data Stack v k :: * -> *
  edge :: Sized a => v k -> Maybe a -> Branch v k a -> Edge v k a
  edge' :: Int -> v k -> Maybe a -> Branch v k a -> Edge v k a
  root :: Path v k a
  deep :: Path v k a -> v k -> Maybe a -> BHole v k a -> Path v k a
  loc :: v k -> Branch v k a -> Path v k a -> EdgeLoc v k a
  stack :: v k -> Maybe a -> Maybe (FDLAStack k (Edge v k a)) -> Maybe (k, Stack v k a) -> Stack v k a
  
  eView :: Edge v k a -> EView v k a
  pView :: Path v k a -> PView v k a
  locView :: EdgeLoc v k a -> LocView v k a
  sView :: Stack v k a -> StackView v k a

type BHole v k a = Hole k (Edge v k a)

type Branch v k a = TrieMap k (Edge v k a)
data EView v k a =
	Edge Int (v k) (Maybe a) (Branch v k a)
data LocView v k a = Loc !( v k) (Branch v k a) (Path v k a)
data PView v k a = Root
	| Deep (Path v k a) (v k) (Maybe a) (BHole v k a)
data StackView v k a = Stack (v k) (Maybe a) (Maybe (FDLAStack k (Edge v k a))) (Maybe (k, Stack v k a))
type MEdge v k a = Maybe (Edge v k a)

instance Sized (EView v k a) where
  getSize# (Edge sz _ _ _) = unbox sz

instance Label v k => Sized (Edge v k a) where
  {-# SPECIALIZE instance TrieKey k => Sized (V(Edge) a) #-}
  {-# SPECIALIZE instance Sized (U(Edge) a) #-}
  getSize# e = getSize# (eView e)

instance TrieKey k => Label V.Vector k where
  data Edge V.Vector k a =
    VEdge !Int !(V()) (V(Branch) a)
    | VEdgeX !Int !(V()) a (V(Branch) a)
  data Path V.Vector k a =
    VRoot
    | VDeep (V(Path) a) !(V()) (V(BHole) a)
    | VDeepX (V(Path) a) !(V()) a (V(BHole) a)
  data EdgeLoc V.Vector k a = VLoc !(V()) (V(Branch) a) (V(Path) a)
  data Stack V.Vector k a =
    VStackAZ !(V()) a (FDLAStack k (V(Edge) a)) k (V(Stack) a)
    | VStackA !(V()) a k (V(Stack) a)
    | VStackZ !(V()) (FDLAStack k (V(Edge) a)) k (V(Stack) a)
    | VTip !(V()) a
  
  {-# INLINE edge #-}
  edge !ks Nothing ts = VEdge (sizeM ts) ks ts
  edge !ks (Just a) ts = VEdgeX (sizeM ts + getSize a) ks a ts
  edge' s !ks Nothing ts = VEdge s ks ts
  edge' s !ks (Just a) ts = VEdgeX s ks a ts
  
  root = VRoot
  deep path !ks Nothing tHole = VDeep path ks tHole
  deep path !ks (Just a) tHole = VDeepX path ks a tHole
  
  loc = VLoc
  
  eView (VEdge s ks ts) = Edge s ks Nothing ts
  eView (VEdgeX s ks v ts) = Edge s ks (Just v) ts
  pView VRoot = Root
  pView (VDeep path ks tHole) = Deep path ks Nothing tHole
  pView (VDeepX path ks v tHole) = Deep path ks (Just v) tHole
  locView (VLoc ks ts path) = Loc ks ts path
  
  {-# INLINE stack #-}
  stack !ks (Just a) (Just z) (Just (k, stack)) =
    VStackAZ ks a z k stack
  stack !ks (Just a) Nothing (Just (k, stack)) =
    VStackA ks a k stack
  stack !ks Nothing (Just z) (Just (k, stack)) =
    VStackZ ks z k stack
  stack !ks (Just a) Nothing Nothing = VTip ks a
  stack _ _ _ _ = error "Error: bad stack"
  {-# INLINE sView #-}
  sView (VTip ks v) = Stack ks (Just v) Nothing Nothing
  sView (VStackAZ ks a z k stack) = Stack ks (Just a) (Just z) (Just (k, stack))
  sView (VStackA ks a k stack) = Stack ks (Just a) Nothing (Just (k, stack))
  sView (VStackZ ks z k stack) = Stack ks Nothing (Just z) (Just (k, stack))
--   sView (VStack ks v ts) = Stack ks v (WordMap ts)

instance Label P.Vector Word where
  data Edge P.Vector Word a =
    SEdge !(U()) !(SNode (U(Edge) a))
    | SEdgeX !Int !(U()) a !(SNode (U(Edge) a))
  data Path P.Vector Word a =
    SRoot
    | SDeep (U(Path) a) !(U()) !(WHole (U(Edge) a))
    | SDeepX (U(Path) a) !(U()) a !(WHole (U(Edge) a))
  data EdgeLoc P.Vector Word a =
    SLoc !(U()) !(SNode (U(Edge) a)) (U(Path) a)
  data Stack P.Vector Word a =
    PStackAZ !(U()) a !(WordStack (U(Edge) a)) !Word (U(Stack) a)
    | PStackA !(U()) a !Word (U(Stack) a)
    | PStackZ !(U()) !(WordStack (U(Edge) a)) !Word (U(Stack) a)
    | PTip !(U()) a
  
  {-# INLINE stack #-}
  stack !ks a z stack = case (a, z, stack) of
    (Just a, Just z, Just (k, stack))	-> PStackAZ ks a z k stack
    (Just a, Nothing, Just (k, stack))	-> PStackA ks a k stack
    (Nothing, Just z, Just (k, stack))	-> PStackZ ks z k stack
    (Just a, Nothing, Nothing)		-> PTip ks a
    _			-> error "Error: bad stack"
  {-# INLINE sView #-}
  sView (PStackAZ ks a z k stack) = Stack ks (Just a) (Just z) (Just (k, stack))
  sView (PStackA ks a k stack) = Stack ks (Just a) Nothing (Just (k, stack))
  sView (PStackZ ks z k stack) = Stack ks Nothing (Just z) (Just (k, stack))
  sView (PTip ks a) = Stack ks (Just a) Nothing Nothing
  
  edge !ks Nothing ts = SEdge ks (getWordMap ts)
  edge !ks (Just v) ts = SEdgeX (getSize v + sizeM ts) ks v (getWordMap ts)
  edge' _ !ks Nothing ts = SEdge ks (getWordMap ts)
  edge' sz !ks (Just v) ts = SEdgeX sz ks v (getWordMap ts)
  
  root = SRoot
  deep path !ks Nothing tHole = SDeep path ks (getHole tHole)
  deep path !ks (Just v) tHole = SDeepX path ks v (getHole tHole)

  loc ks ts path = SLoc ks (getWordMap ts) path

  eView (SEdge ks ts) = Edge (getSize ts) ks Nothing (WordMap ts)
  eView (SEdgeX s ks v ts) = Edge s ks (Just v) (WordMap ts)
  pView SRoot = Root
  pView (SDeep path ks tHole) = Deep path ks Nothing (Hole tHole)
  pView (SDeepX path ks v tHole) = Deep path ks (Just v) (Hole tHole)
  locView (SLoc ks ts path) = Loc ks (WordMap ts) path
--   sView (SStack ks v ts) = Stack ks v (WordMap ts)

{-# SPECIALIZE singletonEdge ::
    (TrieKey k, Sized a) => V() -> a -> V(Edge) a,
    Sized a => U() -> a -> U(Edge) a #-}
singletonEdge :: (Label v k, Sized a) => v k -> a -> Edge v k a
singletonEdge !ks a = edge' (getSize a) ks (Just a) emptyM

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

{-# SPECIALIZE dropEdge ::
    TrieKey k => Int -> V(Edge) a -> V(Edge) a,
    Int -> U(Edge) a -> U(Edge) a #-}
{-# SPECIALIZE unDropEdge ::
    TrieKey k => Int -> V(Edge) a -> V(Edge) a,
    Int -> U(Edge) a -> U(Edge) a #-}
dropEdge, unDropEdge :: Label v k => Int -> Edge v k a -> Edge v k a
dropEdge !n !(eView -> Edge sz# ks v ts) = edge' sz# (dropSlice n ks) v ts
unDropEdge !n !(eView -> Edge sz# ks v ts) = edge' sz# (unDropSlice n ks) v ts

{-# SPECIALIZE cEdge ::
    (TrieKey k, Sized a) => V() -> Maybe a -> V(Branch) a -> V(MEdge) a,
    Sized a => U() -> Maybe a -> U(Branch) a -> U(MEdge) a #-}
cEdge :: (Label v k, Sized a) => v k -> Maybe a -> Branch v k a -> MEdge v k a
cEdge !ks v ts = case v of
  Nothing -> case getSimpleM ts of
    Null	-> Nothing
    Singleton e' -> Just (unDropEdge (length ks + 1) e')
    NonSimple	-> Just (edge ks Nothing ts)
  _		-> Just (edge ks v ts)

-- data StackView v k a z = Stack (v k) a (TrieMap Word (Hang a z))

data HangView a z = 
  Branch !Int (Maybe a) (Maybe z)
data Hang a z = H !Int z | HT !Int a z | T !Int a

branch :: Int -> Maybe a -> Maybe z -> Hang a z
branch !i Nothing (Just z) = H i z
branch !i (Just a) Nothing = T i a
branch !i (Just a) (Just z) = HT i a z
branch _ _ _ = error "Error: bad branch"

bView :: Hang a z -> HangView a z
bView (H i z) = Branch i Nothing (Just z)
bView (HT i a z) = Branch i (Just a) (Just z)
bView (T i a) = Branch i (Just a) Nothing

instance Sized (Hang a z) where
  getSize# _ = 1#

{-# RULES
    "sView/stack" forall ks a z branch . sView (stack ks a z branch) = Stack ks a z branch
    #-}