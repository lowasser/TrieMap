{-# LANGUAGE MagicHash, BangPatterns, UnboxedTuples, PatternGuards, CPP, ViewPatterns #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.TrieMap.RadixTrie.Edge where

import Data.TrieMap.Sized
import Data.TrieMap.TrieKey
import Data.TrieMap.WordMap ()
import Data.TrieMap.RadixTrie.Label
import Data.TrieMap.RadixTrie.Slice

import Control.Applicative
import Control.Monad

import Data.Foldable
import Data.Monoid
import Data.Word

import Data.Vector.Generic (length)
import qualified Data.Vector (Vector)
import qualified Data.Vector.Storable (Vector)
import Prelude hiding (length, foldr, foldl, zip, take)

#define V(f) f (Data.Vector.Vector) (k)
#define U(f) f (Data.Vector.Storable.Vector) (Word)
#define EDGE(args) (eView -> Edge args)
#define LOC(args) !(locView -> Loc args)

{-# SPECIALIZE lookupEdge ::
      TrieKey k => V() -> V(Edge) a -> Maybe a,
      U() -> U(Edge) a -> Maybe a #-}
lookupEdge :: (Eq k, Label v k) => v k -> Edge v k a -> Maybe a
lookupEdge = lookupE where
	lookupE !ks !EDGE(_ ls v ts) = if kLen < lLen then Nothing else matchSlice matcher matches ks ls where
	  !kLen = length ks
	  !lLen = length ls
	  matcher k l z
		  | k == l	  = z
		  | otherwise	  = Nothing
	  matches _ _
		  | kLen == lLen  = v
		  | (_, k, ks') <- splitSlice lLen ks = lookupM k ts >>= lookupE ks'

{-# SPECIALIZE searchEdge :: 
      TrieKey k => V() -> V(Edge) a -> V(Path) a -> (Maybe a, V(EdgeLoc) a),
      U() -> U(Edge) a -> U(Path) a -> (Maybe a, U(EdgeLoc) a) #-}
searchEdge :: (Eq k, Label v k) => v k -> Edge v k a -> Path v k a -> (Maybe a, EdgeLoc v k a)
searchEdge = searchE where
  searchE !ks !e@EDGE(_ ls !v ts) path = iMatchSlice matcher matches ks ls where
    matcher i k l z = case unifierM k l (dropEdge (i+1) e) of
      Nothing	-> z
      Just tHole -> (Nothing, loc (dropSlice (i+1) ks) emptyM (deep path (takeSlice i ls) Nothing tHole))
    matches kLen lLen = case compare kLen lLen of
	    EQ	-> (v, loc ls ts path)
	    LT	-> let lPre = takeSlice kLen ls; l = ls !$ kLen; e' = dropEdge (kLen + 1) e in
		(Nothing, loc lPre (singletonM l e') path)
	    GT	-> let k = ks !$ lLen; ks' = dropSlice (lLen + 1) ks in case searchM k ts of
		(# Nothing, tHole #) -> (Nothing, loc ks' emptyM (deep path ls v tHole))
		(# Just e', tHole #) -> searchE ks' e' (deep path ls v tHole)

{-# SPECIALIZE mapEdge ::
      (TrieKey k, Sized b) => (a -> b) -> V(Edge) a -> V(Edge) b,
      Sized b => (a -> b) -> U(Edge) a -> U(Edge) b #-}
mapEdge :: (Label v k, Sized b) => (a -> b) -> Edge v k a -> Edge v k b
mapEdge f = mapE where
	mapE !EDGE(_ ks v ts) = edge ks (f <$> v) (fmapM mapE ts)

{-# SPECIALIZE mapMaybeEdge ::
      (TrieKey k, Sized b) => (a -> Maybe b) -> V(Edge) a -> V(MEdge) b,
      Sized b => (a -> Maybe b) -> U(Edge) a -> U(MEdge) b #-}
mapMaybeEdge :: (Label v k, Sized b) => (a -> Maybe b) -> Edge v k a -> MEdge v k b
mapMaybeEdge f = mapMaybeE where
	mapMaybeE EDGE(_ ks v ts) = cEdge ks (v >>= f) (mapMaybeM mapMaybeE ts)

{-# SPECIALIZE mapEitherEdge ::
      (TrieKey k, Sized b, Sized c) => (a -> (# Maybe b, Maybe c #)) -> V(Edge) a -> (# V(MEdge) b, V(MEdge) c #),
      (Sized b, Sized c) => (a -> (# Maybe b, Maybe c #)) -> U(Edge) a -> (# U(MEdge) b, U(MEdge) c #) #-}
mapEitherEdge :: (Label v k, Sized b, Sized c) => 
	(a -> (# Maybe b, Maybe c #)) -> Edge v k a -> (# MEdge v k b, MEdge v k c #)
mapEitherEdge f = mapEitherE where
	mapEitherE !EDGE(_ ks v ts) = (# cEdge ks vL tsL, cEdge ks vR tsR #)
	  where	!(# vL, vR #) = mapEitherMaybe f v
		!(# tsL, tsR #) = mapEitherM mapEitherE ts

{-# SPECIALIZE traverseEdge ::
      (TrieKey k, Applicative f, Sized b) => (a -> f b) -> V(Edge) a -> f (V(Edge) b),
      (Applicative f, Sized b) => (a -> f b) -> U(Edge) a -> f (U(Edge) b) #-}
traverseEdge :: (Label v k, Applicative f, Sized b) =>
	(a -> f b) -> Edge v k a -> f (Edge v k b)
traverseEdge f = traverseE where
	traverseE e = case eView e of
	  Edge _ ks Nothing ts	-> edge ks Nothing <$> traverseM traverseE ts
	  Edge _ ks (Just v) ts	-> edge ks . Just <$> f v <*> traverseM traverseE ts

instance Label v k => Foldable (EView v k) where
  {-# INLINE foldr #-}
  {-# INLINE foldl #-}
  {-# INLINE foldMap #-}
  foldMap f (Edge _ _ Nothing ts) = foldMap (foldMap f) ts
  foldMap f (Edge _ _ (Just v) ts) = f v `mappend` foldMap (foldMap f) ts
  foldr f z (Edge _ _ v ts) = foldr f (foldr (flip $ foldr f) z ts) v
  foldl f z (Edge _ _ v ts) = foldl (foldl f) (foldl f z v) ts

instance Label v k => Foldable (Edge v k) where
  {-# SPECIALIZE instance TrieKey k => Foldable (V(Edge)) #-}
  {-# SPECIALIZE instance Foldable (U(Edge)) #-}
  foldMap f e = foldMap f (eView e)
  foldr f z e = foldr f z (eView e)
  foldl f z e = foldl f z (eView e)

{-# SPECIALIZE assignEdge ::
      (TrieKey k, Sized a) => a -> V(EdgeLoc) a -> V(Edge) a,
      Sized a => a -> U(EdgeLoc) a -> U(Edge) a #-}
assignEdge :: (Label v k, Sized a) => a -> EdgeLoc v k a -> Edge v k a
assignEdge v LOC(ks ts path) = assign (edge ks (Just v) ts) path where
  assign e path = case pView path of
    Root	-> e
    Deep path ks v tHole
		-> assign (edge ks v (assignM e tHole)) path

{-# SPECIALIZE clearEdge :: 
      (TrieKey k, Sized a) => V(EdgeLoc) a -> V(MEdge) a,
      Sized a => U(EdgeLoc) a -> U(MEdge) a #-}
clearEdge :: (Label v k, Sized a) => EdgeLoc v k a -> MEdge v k a
clearEdge LOC(ks ts path) = rebuild (cEdge ks Nothing ts) path where
  rebuild !e path = case pView path of
    Root	-> e
    Deep path ks v tHole
    		-> rebuild (cEdge ks v (fillHoleM e tHole)) path

{-# SPECIALIZE unionEdge :: 
      (TrieKey k, Sized a) => (a -> a -> Maybe a) -> V(Edge) a -> V(Edge) a -> V(MEdge) a,
      Sized a => (a -> a -> Maybe a) -> U(Edge) a -> U(Edge) a -> U(MEdge) a #-}
unionEdge :: (Label v k, Sized a) => 
	(a -> a -> Maybe a) -> Edge v k a -> Edge v k a -> MEdge v k a
unionEdge f = unionE where
  unionE !eK@EDGE(_ ks0 vK tsK) !eL@EDGE(_ ls0 vL tsL) = iMatchSlice matcher matches ks0 ls0 where
    matcher i k l z = case unifyM k eK' l eL' of
      Nothing	-> z
      Just ts	-> Just (edge (takeSlice i ks0) Nothing ts)
      where eK' = dropEdge (i+1) eK
	    eL' = dropEdge (i+1) eL
    matches kLen lLen = case compare kLen lLen of
      EQ -> cEdge ks0 (unionMaybe f vK vL) $ unionM unionE tsK tsL
      LT -> let eL' = dropEdge (kLen + 1) eL; l = ls0 !$ kLen in case searchM l tsK of
	(# Nothing, holeKT #)	-> cEdge ks0 vK $ assignM eL' holeKT
	(# Just eK', holeKT #)	-> cEdge ks0 vK $ fillHoleM (eK' `unionE` eL') holeKT
      GT -> let eK' = dropEdge (lLen + 1) eK; k = ks0 !$ lLen in case searchM k tsL of
      	(# Nothing, holeLT #)	-> cEdge ls0 vL $ assignM eK' holeLT
      	(# Just eL', holeLT #)	-> cEdge ls0 vL $ fillHoleM (eK' `unionE` eL') holeLT

{-# SPECIALIZE isectEdge ::
      (TrieKey k, Sized c) => (a -> b -> Maybe c) -> V(Edge) a -> V(Edge) b -> V(MEdge) c,
      Sized c => (a -> b -> Maybe c) -> U(Edge) a -> U(Edge) b -> U(MEdge) c #-}
isectEdge :: (Eq k, Label v k, Sized c) =>
	(a -> b -> Maybe c) -> Edge v k a -> Edge v k b -> MEdge v k c
isectEdge f = isectE where
  isectE !eK@EDGE(_ ks0 vK tsK) !eL@EDGE(_ ls0 vL tsL) = matchSlice matcher matches ks0 ls0 where
    matcher k l z = guard (k == l) >> z
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

{-# SPECIALIZE diffEdge ::
      (TrieKey k, Sized a) => (a -> b -> Maybe a) -> V(Edge) a -> V(Edge) b -> V(MEdge) a,
      Sized a => (a -> b -> Maybe a) -> U(Edge) a -> U(Edge) b -> U(MEdge) a #-}
diffEdge :: (Eq k, Label v k, Sized a) =>
	(a -> b -> Maybe a) -> Edge v k a -> Edge v k b -> MEdge v k a
diffEdge f = diffE where
  diffE !eK@EDGE(_ ks0 vK tsK) !eL@EDGE(_ ls0 vL tsL) = matchSlice matcher matches ks0 ls0 where
    matcher k l z
      | k == l		= z
      | otherwise	= Just eK
    matches kLen lLen = case compare kLen lLen of
      EQ -> cEdge ks0 (diffMaybe f vK vL) $ diffM diffE tsK tsL
      LT -> let l = ls0 !$ kLen; eL' = dropEdge (kLen + 1) eL in case searchM l tsK of
	(# Nothing, _ #)	-> Just eK
	(# Just eK', holeKT #)	-> cEdge ks0 vK $ fillHoleM (eK' `diffE` eL') holeKT
      GT -> let k = ks0 !$ lLen; eK' = dropEdge (lLen + 1) eK in case lookupM k tsL of
	Nothing	  -> Just eK
	Just eL'  -> fmap (unDropEdge (lLen + 1)) (eK' `diffE` eL')

{-# SPECIALIZE isSubEdge ::
      TrieKey k => LEq a b -> LEq (V(Edge) a) (V(Edge) b),
      LEq a b -> LEq (U(Edge) a) (U(Edge) b) #-}
isSubEdge :: (Eq k, Label v k) => LEq a b -> LEq (Edge v k a) (Edge v k b)
isSubEdge (<=) = isSubE where
  isSubE !eK@EDGE(_ ks0 vK tsK) !EDGE(_ ls0 vL tsL) = matchSlice matcher matches ks0 ls0 where
    matcher k l z = k == l && z
    matches kLen lLen = case compare kLen lLen of
      LT	-> False
      EQ	-> subMaybe (<=) vK vL && isSubmapM isSubE tsK tsL
      GT	-> let k = ks0 !$ lLen in case lookupM k tsL of
	  Nothing	-> False
	  Just eL'	-> isSubE (dropEdge (lLen + 1) eK) eL'

{-# SPECIALIZE beforeEdge :: 
      (TrieKey k, Sized a) => Maybe a -> V(EdgeLoc) a -> V(MEdge) a,
      Sized a => Maybe a -> U(EdgeLoc) a -> U(MEdge) a #-}
beforeEdge :: (Label v k, Sized a) => Maybe a -> EdgeLoc v k a -> MEdge v k a
beforeEdge v LOC(ks ts path) = buildBefore (cEdge ks v ts) path where
	buildBefore !e path = case pView path of
	  Root	-> e
	  Deep path ks v tHole	-> buildBefore (cEdge ks v $ beforeMM e tHole) path

{-# SPECIALIZE afterEdge :: 
      (TrieKey k, Sized a) => Maybe a -> V(EdgeLoc) a -> V(MEdge) a,
      Sized a => Maybe a -> U(EdgeLoc) a -> U(MEdge) a #-}
afterEdge :: (Label v k, Sized a) => Maybe a -> EdgeLoc v k a -> MEdge v k a
afterEdge v LOC(ks ts path) = buildAfter (cEdge ks v ts) path where
	buildAfter !e path = case pView path of
	  Root	-> e
	  Deep path ks v tHole
	  	-> buildAfter (cEdge ks v $ afterMM e tHole) path

{-# SPECIALIZE extractEdgeLoc :: 
      (TrieKey k, Functor m, MonadPlus m) => V(Edge) a -> V(Path) a -> m (a, V(EdgeLoc) a),
      (Functor m, MonadPlus m) => U(Edge) a -> U(Path) a -> m (a, U(EdgeLoc) a) #-}
extractEdgeLoc :: (Label v k, Functor m, MonadPlus m) => Edge v k a -> Path v k a -> m (a, EdgeLoc v k a)
extractEdgeLoc !EDGE(_ ks v ts) path = case v of
	Nothing	-> extractTS
	Just a	-> return (a, loc ks ts path) `mplus` extractTS
	where	extractTS = do	(e', tHole) <- extractHoleM ts
				extractEdgeLoc e' (deep path ks v tHole)

{-# SPECIALIZE INLINE indexEdge :: 
      (TrieKey k, Sized a) => Int -> V(Edge) a -> V(Path) a -> (# Int, a, V(EdgeLoc) a #),
      Sized a => Int -> U(Edge) a -> U(Path) a -> (# Int, a, U(EdgeLoc) a #) #-}
indexEdge :: (Label v k, Sized a) => Int -> Edge v k a -> Path v k a -> (# Int, a, EdgeLoc v k a #)
indexEdge = indexE where
  indexE !i e path = case eView e of
    Edge _ ks v@(Just a) ts
	  | i < sv	-> (# i, a, loc ks ts path #)
	  | (# i', e', tHole #) <- indexM (i - sv) ts
			-> indexE i' e' (deep path ks v tHole)
	  where	!sv = getSize a
    Edge _ ks Nothing ts
			  -> indexE i' e' (deep path ks Nothing tHole)
	  where !(# i', e', tHole #) = indexM i ts

{-# SPECIALIZE unifyEdge :: 
      (TrieKey k, Sized a) => V() -> a -> V() -> a -> V(MEdge) a,
      Sized a => U() -> a -> U() -> a -> U(MEdge) a #-}
unifyEdge :: (Label v k, Sized a) => v k -> a -> v k -> a -> MEdge v k a
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

{-# SPECIALIZE insertEdge :: 
      (TrieKey k, Sized a) => (a -> a -> a) -> V() -> a -> V(Edge) a -> V(Edge) a,
      Sized a => (a -> a -> a) -> U() -> a -> U(Edge) a -> U(Edge) a #-}
insertEdge :: (Label v k, Sized a) => (a -> a -> a) -> v k -> a -> Edge v k a -> Edge v k a
insertEdge f ks a = insertE ks where
  !sa = getSize a
  insertE ks !e@EDGE(_ ls !v ts) = iMatchSlice matcher matches ks ls where
    single n = edge' sa (dropSlice n ks) (Just a) emptyM
    matcher !i k l z = case unifyM k (single (i+1)) l (dropEdge (i+1) e) of
      Nothing	-> z
      Just ts	-> edge (takeSlice i ks) Nothing ts
    matches lenK lenL = case compare lenK lenL of
      LT	-> edge ks (Just a) $ singletonM (ls !$ lenK) $ dropEdge (lenK+1) e
      EQ	-> edge ls (Just (maybe a (f a) v)) ts
      GT	->
	let	ks' = dropSlice (lenL + 1) ks
		g _ e' = insertE ks' e'
		in edge ls v $ insertWithM g (ks !$ lenL) (single (lenL+1)) ts
