{-# LANGUAGE MagicHash, BangPatterns, UnboxedTuples, PatternGuards, CPP #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.TrieMap.RadixTrie.Edge where

import Data.TrieMap.Sized
import Data.TrieMap.TrieKey
import Data.TrieMap.IntMap ()
import Data.TrieMap.RadixTrie.Slice

import Control.Applicative
import Control.Monad
import Data.Word
import Data.Traversable
import Data.Foldable (foldr, foldl)

import Data.Vector.Generic (length, Vector)
import qualified Data.Vector (Vector)
import qualified Data.Vector.Storable (Vector)
import Prelude hiding (length, foldr, foldl, zip, take)

import GHC.Exts

#define V(f) f (Data.Vector.Vector) (k)
#define U(f) f (Data.Vector.Storable.Vector) (Word)

type Branch v k a = TrieMap k (Edge v k a)
data Edge v k a =
	Edge Int# !( v k) !(Maybe a) (Branch v k a)
data EdgeLoc v k a = Loc !( v k) (Branch v k a) (Path v k a)
data Path v k a = Root
	| Deep (Path v k a) !( v k) !(Maybe a) (Hole k (Edge v k a))
type MEdge v k a = Maybe (Edge v k a)

instance Sized (Edge v k a) where
	getSize# (Edge s# _ _ _) = s#

{-# SPECIALIZE singleLoc :: U() -> U(EdgeLoc) a #-}
singleLoc :: TrieKey k => v k -> EdgeLoc v k a
singleLoc ks = Loc ks emptyM Root

{-# SPECIALIZE singletonEdge :: Sized a => U() -> a -> U(Edge) a #-}
singletonEdge :: (TrieKey k, Sized a) => v k -> a -> Edge v k a
singletonEdge ks a = edge ks (Just a) emptyM

{-# SPECIALIZE getSimpleEdge :: U(Edge) a -> Simple a #-}
getSimpleEdge :: TrieKey k => Edge v k a -> Simple a
getSimpleEdge (Edge _ _ v ts)
  | nullM ts	= maybe Null Singleton v
  | otherwise	= NonSimple

{-# SPECIALIZE edge :: Sized a => U() -> Maybe a -> U(Branch) a -> U(Edge) a #-}
edge :: (TrieKey k, Sized a) => v k -> Maybe a -> Branch v k a -> Edge v k a
edge ks v ts = Edge (getSize# v +# sizeM ts) ks v ts

{-# INLINE compact #-}
-- TODO: figure out a way to GC dead keys
compact :: (Vector v k, TrieKey k) => Edge v k a -> MEdge v k a
compact e@(Edge _ ks Nothing ts) = case getSimpleM ts of
	Null		-> Nothing
	Singleton e'	-> Just (unDropEdge (length ks + 1) e')
	_		-> Just e
compact e = Just e

dropEdge :: Vector v k => Int -> Edge v k a -> Edge v k a
dropEdge n (Edge s# ks v ts) = Edge s# (dropSlice n ks) v ts

unDropEdge :: Vector v k => Int -> Edge v k a -> Edge v k a
unDropEdge n (Edge s# ks v ts) = Edge s# (unDropSlice n ks) v ts

{-# SPECIALIZE lookupEdge :: TrieKey k => V() -> V(Edge) a -> Maybe a #-}
{-# SPECIALIZE lookupEdge :: U() -> U(Edge) a -> Maybe a #-}
lookupEdge :: (TrieKey k, Vector v k) => v k -> Edge v k a -> Maybe a
lookupEdge = lookupE where
	lookupE !ks (Edge _ ls v ts) = if kLen < lLen then Nothing else matchSlice matcher matches ks ls where
	  !kLen = length ks
	  !lLen = length ls
	  matcher k l z
		  | k =? l	  = z
		  | otherwise	  = Nothing
	  matches _ _
		  | kLen == lLen  = v
		  | (_, k, ks') <- splitSlice lLen ks = lookupM k ts >>= lookupE ks'

{-# SPECIALIZE searchEdge :: TrieKey k => V() -> V(Edge) a -> V(Path) a -> (Maybe a, V(EdgeLoc) a) #-}
{-# SPECIALIZE searchEdge :: U() -> U(Edge) a -> U(Path) a -> (Maybe a, U(EdgeLoc) a) #-}
searchEdge :: (TrieKey k, Vector v k) => v k -> Edge v k a -> Path v k a -> (Maybe a, EdgeLoc v k a)
searchEdge = searchE where
	searchE !ks e@(Edge _ ls v ts) path = iMatchSlice matcher matches ks ls where
	  matcher i k l z
	    | k =? l	= z
	    | (# _, tHole #) <- searchM k (singletonM l (dropEdge (i+1) e))
			= (Nothing, Loc (dropSlice (i+1) ks) emptyM (Deep path (takeSlice i ls) Nothing tHole))
	  matches kLen lLen = case compare kLen lLen of
		  EQ	-> (v, Loc ls ts path)
		  LT	-> let (lPre, !l, _) = splitSlice kLen ls in 
		      (Nothing, Loc lPre (singletonM l (dropEdge (kLen + 1) e)) path)
		  GT	-> let (_, !k, ks') =  splitSlice lLen ks in case searchM k ts of
		      (# Nothing, tHole #) -> (Nothing, Loc ks' emptyM (Deep path ls v tHole))
		      (# Just e', tHole #) -> searchE ks' e' (Deep path ls v tHole)

{-# SPECIALIZE mapEdge :: Sized b => (a -> b) -> U(Edge) a -> U(Edge) b #-}
mapEdge :: (TrieKey k, Sized b) => (a -> b) -> Edge v k a -> Edge v k b
mapEdge f = mapE where
	mapE (Edge _ ks v ts) = edge ks (f <$> v) (fmapM mapE ts)

{-# SPECIALIZE mapMaybeEdge :: Sized b => (a -> Maybe b) -> U(Edge) a -> U(MEdge) b #-}
mapMaybeEdge :: (Vector v k, TrieKey k, Sized b) => (a -> Maybe b) -> Edge v k a -> MEdge v k b
mapMaybeEdge f = mapMaybeE where
	mapMaybeE (Edge _ ks v ts) = compact (edge ks (v >>= f) (mapMaybeM mapMaybeE ts))

{-# SPECIALIZE mapEitherEdge :: (Sized b, Sized c) =>
	(a -> (# Maybe b, Maybe c #)) -> U(Edge) a -> (# U(MEdge) b, U(MEdge) c #) #-}
mapEitherEdge :: (Vector v k, TrieKey k, Sized b, Sized c) => 
	(a -> (# Maybe b, Maybe c #)) -> Edge v k a -> (# MEdge v k b, MEdge v k c #)
mapEitherEdge f = mapEitherE where
	mapEitherE (Edge _ ks v ts) = (# compact (edge ks vL tsL), compact (edge ks vR tsR) #)
	  where	!(# vL, vR #) = mapEitherMaybe f v
		!(# tsL, tsR #) = mapEitherM mapEitherE ts

{-# SPECIALIZE traverseEdge :: (Applicative f, Sized b) =>
	(a -> f b) -> U(Edge) a -> f (U(Edge) b) #-}
traverseEdge :: (TrieKey k, Applicative f, Sized b) =>
	(a -> f b) -> Edge v k a -> f (Edge v k b)
traverseEdge f = traverseE where
	traverseE (Edge _ ks v ts) = edge ks <$> traverse f v <*> traverseM traverseE ts

{-# SPECIALIZE foldrEdge :: (a -> b -> b) -> U(Edge) a -> b -> b #-}
foldrEdge :: TrieKey k => (a -> b -> b) -> Edge v k a -> b -> b
foldrEdge f = foldrE where
  foldrE (Edge _ _ v ts) z = foldr f (foldrM foldrE ts z) v

foldlEdge :: TrieKey k => (b -> a -> b) -> b -> Edge v k a -> b
foldlEdge f = foldlE where
  foldlE z (Edge _ _ v ts) = foldlM foldlE ts (foldl f z v)

{-# SPECIALIZE rebuild :: Sized a => U(MEdge) a -> U(Path) a -> U(MEdge) a #-}
rebuild :: (Vector v k, TrieKey k, Sized a) => MEdge v k a -> Path v k a -> MEdge v k a
rebuild !e Root = e
rebuild Nothing (Deep path ks v tHole) = rebuild (compact $ edge ks v $ clearM tHole) path
rebuild (Just e) (Deep path ks v tHole) = rebuild (compact $ edge ks v $ assignM e tHole) path

{-# SPECIALIZE assignEdge :: Sized a => a -> U(EdgeLoc) a -> U(MEdge) a #-}
assignEdge :: (Vector v k, TrieKey k, Sized a) => a -> EdgeLoc v k a -> MEdge v k a
assignEdge v (Loc ks ts path) = rebuild (Just (edge ks (Just v) ts)) path

{-# SPECIALIZE clearEdge :: Sized a => U(EdgeLoc) a -> U(MEdge) a #-}
clearEdge :: (Vector v k, TrieKey k, Sized a) => EdgeLoc v k a -> MEdge v k a
clearEdge (Loc ks ts path) = rebuild (compact (edge ks Nothing ts)) path

{-# SPECIALIZE unionEdge :: (TrieKey k, Sized a) => 
	(a -> a -> Maybe a) -> V(Edge) a -> V(Edge) a -> V(MEdge) a #-}
{-# SPECIALIZE unionEdge :: Sized a =>
	(a -> a -> Maybe a) -> U(Edge) a -> U(Edge) a -> U(MEdge) a #-}
unionEdge :: (TrieKey k, Vector v k, Sized a) => 
	(a -> a -> Maybe a) -> Edge v k a -> Edge v k a -> MEdge v k a
unionEdge f = unionE where
  eK@(Edge _ ks0 vK tsK) `unionE` eL@(Edge _ ls0 vL tsL) = iMatchSlice matcher matches ks0 ls0 where
    matcher i k l z = case unifyM k eK' l eL' of
      Nothing	-> z
      Just ts	-> Just (edge (takeSlice i ks0) Nothing ts)
      where eK' = dropEdge (i+1) eK
	    eL' = dropEdge (i+1) eL
    matches kLen lLen = case compare kLen lLen of
      EQ -> compact $ edge ks0 (unionMaybe f vK vL) $ unionM unionE tsK tsL
      LT -> let eL' = dropEdge (kLen + 1) eL; l = ls0 !$ kLen in case searchM l tsK of
	(# Nothing, holeKT #)	-> compact $ edge ks0 vK $ assignM eL' holeKT
	(# Just eK', holeKT #)	-> compact $ edge ks0 vK $ fillHoleM (eK' `unionE` eL') holeKT
      GT -> let eK' = dropEdge (lLen + 1) eK; k = ks0 !$ lLen in case searchM k tsL of
      	(# Nothing, holeLT #)	-> compact $ edge ls0 vL $ assignM eK' holeLT
      	(# Just eL', holeLT #)	-> compact $ edge ls0 vL $ fillHoleM (eK' `unionE` eL') holeLT

{-# SPECIALIZE isectEdge :: (TrieKey k, Sized c) =>
	(a -> b -> Maybe c) -> V(Edge) a -> V(Edge) b -> V(MEdge) c #-}
{-# SPECIALIZE isectEdge :: Sized c =>
	(a -> b -> Maybe c) -> U(Edge) a -> U(Edge) b -> U(MEdge) c #-}
isectEdge :: (TrieKey k, Vector v k, Sized c) =>
	(a -> b -> Maybe c) -> Edge v k a -> Edge v k b -> MEdge v k c
isectEdge f = isectE where
  eK@(Edge _ ks0 vK tsK) `isectE` eL@(Edge _ ls0 vL tsL) = matchSlice matcher matches ks0 ls0 where
    matcher k l z = guard (k =? l) >> z
    matches kLen lLen = case compare kLen lLen of
      EQ -> compact $ edge ks0 (isectMaybe f vK vL) $ isectM isectE tsK tsL
      LT -> let l = ls0 !$ kLen in do
	      eK' <- lookupM l tsK
	      let eL' = dropEdge (kLen + 1) eL
	      unDropEdge (kLen + 1) <$> eK' `isectE` eL'
      GT -> let k = ks0 !$ lLen in do
	      eL' <- lookupM k tsL
	      let eK' = dropEdge (lLen + 1) eK
	      unDropEdge (lLen + 1) <$> eK' `isectE` eL'

{-# SPECIALIZE diffEdge :: (TrieKey k, Sized a) =>
	(a -> b -> Maybe a) -> V(Edge) a -> V(Edge) b -> V(MEdge) a #-}
{-# SPECIALIZE diffEdge :: Sized a =>
	(a -> b -> Maybe a) -> U(Edge) a -> U(Edge) b -> U(MEdge) a #-}
diffEdge :: (TrieKey k, Vector v k, Sized a) =>
	(a -> b -> Maybe a) -> Edge v k a -> Edge v k b -> MEdge v k a
diffEdge f = diffE where
  eK@(Edge _ ks0 vK tsK) `diffE` eL@(Edge _ ls0 vL tsL) = matchSlice matcher matches ks0 ls0 where
    matcher k l z
      | k =? l		= z
      | otherwise	= Just eK
    matches kLen lLen = case compare kLen lLen of
      EQ -> compact $ edge ks0 (diffMaybe f vK vL) $ diffM diffE tsK tsL
      LT -> let l = ls0 !$ kLen; eL' = dropEdge (kLen + 1) eL in case searchM l tsK of
	(# Nothing, _ #)	-> Just eK
	(# Just eK', holeKT #)	-> compact $ edge ks0 vK $ fillHoleM (eK' `diffE` eL') holeKT
      GT -> let k = ks0 !$ lLen; eK' = dropEdge (lLen + 1) eK in case lookupM k tsL of
	Nothing	  -> Just eK
	Just eL'  -> fmap (unDropEdge (lLen + 1)) (eK' `diffE` eL')

{-# SPECIALIZE isSubEdge :: TrieKey k => LEq a b -> LEq (V(Edge) a) (V(Edge) b) #-}
{-# SPECIALIZE isSubEdge :: LEq a b -> LEq (U(Edge) a) (U(Edge) b) #-}
isSubEdge :: (TrieKey k, Vector v k) => LEq a b -> LEq (Edge v k a) (Edge v k b)
isSubEdge (<=) = isSubE where
  eK@(Edge _ ks0 vK tsK) `isSubE` (Edge _ ls0 vL tsL) = matchSlice matcher matches ks0 ls0 where
    matcher k l z = k =? l && z
    matches kLen lLen = case compare kLen lLen of
      LT	-> False
      EQ	-> subMaybe (<=) vK vL && isSubmapM isSubE tsK tsL
      GT	-> let k = ks0 !$ lLen in case lookupM k tsL of
	  Nothing	-> False
	  Just eL'	-> isSubE (dropEdge (lLen + 1) eK) eL'

{-# SPECIALIZE beforeEdge :: Sized a => Maybe a -> U(EdgeLoc) a -> U(MEdge) a #-}
beforeEdge :: (Vector v k, TrieKey k, Sized a) => Maybe a -> EdgeLoc v k a -> MEdge v k a
beforeEdge v (Loc ks ts path) = buildBefore (compact (edge ks v ts)) path where
	buildBefore !e Root
	  = e
	buildBefore e (Deep path ks v tHole)
	  = buildBefore (compact $ edge ks v $ beforeMM e tHole) path

{-# SPECIALIZE afterEdge :: Sized a => Maybe a -> U(EdgeLoc) a -> U(MEdge) a #-}
afterEdge :: (Vector v k, TrieKey k, Sized a) => Maybe a -> EdgeLoc v k a -> MEdge v k a
afterEdge v (Loc ks ts path) = buildAfter (compact (edge ks v ts)) path where
	buildAfter !e Root
	  = e
	buildAfter e (Deep path ks v tHole)
	  = buildAfter (compact $ edge ks v $ afterMM e tHole) path

{-# SPECIALIZE extractEdgeLoc :: (Functor m, MonadPlus m) => U(Edge) a -> U(Path) a -> m (a, U(EdgeLoc) a) #-}
extractEdgeLoc :: (TrieKey k, Functor m, MonadPlus m) => Edge v k a -> Path v k a -> m (a, EdgeLoc v k a)
extractEdgeLoc (Edge _ ks v ts) path = case v of
	Nothing	-> extractTS
	Just a	-> return (a, Loc ks ts path) `mplus` extractTS
	where	extractTS = do	(e', tHole) <- extractHoleM ts
				extractEdgeLoc e' (Deep path ks v tHole)

{-# SPECIALIZE indexEdge :: Sized a => Int# -> U(Edge) a -> U(Path) a -> (# Int#, a, U(EdgeLoc) a #) #-}
indexEdge :: (TrieKey k, Sized a) => Int# -> Edge v k a -> Path v k a -> (# Int#, a, EdgeLoc v k a #)
indexEdge = indexE where
  indexE i# (Edge _ ks v@(Just a) ts) path
	  | i# <# sv#	= (# i#, a, Loc ks ts path #)
	  | (# i'#, e', tHole #) <- indexM (i# -# sv#) ts
			  = indexE i'# e' (Deep path ks v tHole)
	  where	!sv# = getSize# a
  indexE i# (Edge _ ks Nothing ts) path
			  = indexE i'# e' (Deep path ks Nothing tHole)
	  where !(# i'#, e', tHole #) = indexM i# ts

{-# SPECIALIZE unifyEdge :: (TrieKey k, Sized a) => V() -> a -> V() -> a -> V(MEdge) a #-}
{-# SPECIALIZE unifyEdge :: Sized a => U() -> a -> U() -> a -> U(MEdge) a #-}
unifyEdge :: (Vector v k, TrieKey k, Sized a) => v k -> a -> v k -> a -> MEdge v k a
unifyEdge ks1 a1 ks2 a2 = iMatchSlice matcher matches ks1 ks2 where
	matcher !i k1 k2 z =
	  case unifyM k1 (singletonEdge (dropSlice (i+1) ks1) a1) k2 (singletonEdge (dropSlice (i+1) ks2) a2) of
	    Nothing	-> z
	    Just ts	-> Just (edge (takeSlice i ks1) Nothing ts)
	matches len1 len2 = case compare len1 len2 of
		LT	-> let (_,k2,ks2') = splitSlice len1 ks2 in
			      Just (edge ks1 (Just a1) (singletonM k2 (singletonEdge ks2' a2)))
		GT	-> let (_,k1,ks1') = splitSlice len2 ks1 in 
			      Just (edge ks2 (Just a2) (singletonM k1 (singletonEdge ks1' a1)))
		_	-> Nothing