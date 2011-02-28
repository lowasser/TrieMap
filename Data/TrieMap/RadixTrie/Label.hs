{-# LANGUAGE MagicHash, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, BangPatterns, CPP, ViewPatterns #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.TrieMap.RadixTrie.Label where

import Control.Monad.Unpack
import Control.Monad.Trans.Reader

import Data.TrieMap.TrieKey
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
  edge :: v k -> Maybe a -> Branch v k a -> Edge v k a
  root :: Path v k a
  deep :: Path v k a -> v k -> Maybe a -> BHole v k a -> Path v k a
  loc :: v k -> Branch v k a -> Path v k a -> EdgeLoc v k a
  stack :: v k -> Maybe a -> Maybe (DAMStack k (Edge v k a)) -> Maybe (k, Stack v k a) -> Stack v k a
  
  eView :: Edge v k a -> EView v k a
  pView :: Path v k a -> PView v k a
  locView :: EdgeLoc v k a -> LocView v k a
  sView :: Stack v k a -> StackView v k a

type BHole v k a = Hole k (Edge v k a)

type Branch v k a = TrieMap k (Edge v k a)
data EView v k a =
	Edge (v k) (Maybe a) (Branch v k a)
data LocView v k a = Loc !( v k) (Branch v k a) (Path v k a)
data PView v k a = Root
	| Deep (Path v k a) (v k) (Maybe a) (BHole v k a)
data StackView v k a = Stack (v k) (Maybe a) (Maybe (DAMStack k (Edge v k a))) (Maybe (k, Stack v k a))
type MEdge v k a = Maybe (Edge v k a)

instance (Label v k, Sized a) => Sized (Edge v k a) where
  {-# SPECIALIZE instance (TrieKey k, Sized a) => Sized (V(Edge) a) #-}
  {-# SPECIALIZE instance Sized a => Sized (U(Edge) a) #-}
  getSize# e = unbox $ case eView e of
    Edge _ v ts -> getSize v + getSize ts

instance TrieKey k => Label V.Vector k where
  data Edge V.Vector k a =
    VEdge !(V()) (V(Branch) a)
    | VEdgeX !(V()) a (V(Branch) a)
  data Path V.Vector k a =
    VRoot
    | VDeep (V(Path) a) !(V()) (V(BHole) a)
    | VDeepX (V(Path) a) !(V()) a (V(BHole) a)
  data EdgeLoc V.Vector k a = VLoc !(V()) (V(Branch) a) (V(Path) a)
  data Stack V.Vector k a =
    VStackAZ !(V()) a (DAMStack k (V(Edge) a)) k (V(Stack) a)
    | VStackA !(V()) a k (V(Stack) a)
    | VStackZ !(V()) (DAMStack k (V(Edge) a)) k (V(Stack) a)
    | VTip !(V()) a
  
  {-# INLINE edge #-}
  edge !ks Nothing ts = VEdge ks ts
  edge !ks (Just a) ts = VEdgeX ks a ts
  
  root = VRoot
  deep path !ks Nothing tHole = VDeep path ks tHole
  deep path !ks (Just a) tHole = VDeepX path ks a tHole
  
  loc = VLoc
  
  eView (VEdge ks ts) = Edge ks Nothing ts
  eView (VEdgeX ks v ts) = Edge ks (Just v) ts
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

instance TrieKey k => Unpackable (V(EdgeLoc) a) where
  newtype UnpackedReaderT (EdgeLoc V.Vector k a) m r =
    VLocRT {runVLocRT :: UnpackedReaderT (V.Vector k) (ReaderT (V(Branch) a) (ReaderT (V(Path) a) m)) r}
  runUnpackedReaderT func (VLoc ks ts path) =
    runVLocRT func `runUnpackedReaderT` ks `runReaderT` ts `runReaderT` path
  unpackedReaderT func = VLocRT $ unpackedReaderT $ \ ks -> ReaderT $ \ ts -> ReaderT $ \ path -> func (VLoc ks ts path)

instance Label P.Vector Word where
  data Edge P.Vector Word a =
    SEdge !(U()) !(Node (U(Edge) a))
    | SEdgeX !(U()) a (Node (U(Edge) a))
  data Path P.Vector Word a =
    SRoot
    | SDeep (U(Path) a) !(U()) !(WHole (U(Edge) a))
    | SDeepX (U(Path) a) !(U()) a !(WHole (U(Edge) a))
  data EdgeLoc P.Vector Word a =
    SLoc !(U()) !(Node (U(Edge) a)) (U(Path) a)
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
  edge !ks (Just v) ts = SEdgeX ks v (getWordMap ts)
  
  root = SRoot
  deep path !ks Nothing tHole = SDeep path ks (getHole tHole)
  deep path !ks (Just v) tHole = SDeepX path ks v (getHole tHole)

  loc ks ts path = SLoc ks (getWordMap ts) path

  eView (SEdge ks ts) = Edge ks Nothing (WordMap ts)
  eView (SEdgeX ks v ts) = Edge ks (Just v) (WordMap ts)
  pView SRoot = Root
  pView (SDeep path ks tHole) = Deep path ks Nothing (Hole tHole)
  pView (SDeepX path ks v tHole) = Deep path ks (Just v) (Hole tHole)
  locView (SLoc ks ts path) = Loc ks (WordMap ts) path

instance Unpackable (U(EdgeLoc) a) where
  newtype UnpackedReaderT (U(EdgeLoc) a) m r =
    ULocRT {runULocRT :: UnpackedReaderT (U()) (UnpackedReaderT (Node (U(Edge) a)) (ReaderT (U(Path) a) m)) r}
  runUnpackedReaderT func (SLoc ks ts path) =
    runULocRT func `runUnpackedReaderT` ks `runUnpackedReaderT` ts `runReaderT` path
  unpackedReaderT func = ULocRT $ unpackedReaderT $ \ ks -> unpackedReaderT $ \ ts -> ReaderT $ \ path ->
    func (SLoc ks ts path)

{-# SPECIALIZE singletonEdge ::
    TrieKey k => V() -> a -> V(Edge) a,
    U() -> a -> U(Edge) a #-}
singletonEdge :: (Label v k) => v k -> a -> Edge v k a
singletonEdge !ks a = edge ks (Just a) emptyM

{-# SPECIALIZE singleLoc :: 
    TrieKey k => V() -> V(EdgeLoc) a,
    U() -> U(EdgeLoc) a #-}
singleLoc :: Label v k => v k -> EdgeLoc v k a
singleLoc ks = loc ks emptyM root

{-# SPECIALIZE getSimpleEdge ::
    TrieKey k => V(Edge) a -> Simple a,
    U(Edge) a -> Simple a #-}
getSimpleEdge :: Label v k => Edge v k a -> Simple a
getSimpleEdge !(eView -> Edge _ v ts)
  | isNull ts	= maybe Null Singleton v
  | otherwise	= NonSimple

{-# SPECIALIZE dropEdge ::
    TrieKey k => Int -> V(Edge) a -> V(Edge) a,
    Int -> U(Edge) a -> U(Edge) a #-}
{-# SPECIALIZE unDropEdge ::
    TrieKey k => Int -> V(Edge) a -> V(Edge) a,
    Int -> U(Edge) a -> U(Edge) a #-}
dropEdge, unDropEdge :: Label v k => Int -> Edge v k a -> Edge v k a
dropEdge !n !(eView -> Edge ks v ts) = edge (dropSlice n ks) v ts
unDropEdge !n !(eView -> Edge ks v ts) = edge (unDropSlice n ks) v ts

{-# SPECIALIZE cEdge ::
    TrieKey k => V() -> Maybe a -> V(Branch) a -> V(MEdge) a,
    U() -> Maybe a -> U(Branch) a -> U(MEdge) a #-}
cEdge :: (Label v k) => v k -> Maybe a -> Branch v k a -> MEdge v k a
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