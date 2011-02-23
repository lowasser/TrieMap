{-# LANGUAGE MagicHash, BangPatterns, UnboxedTuples, PatternGuards, CPP, ViewPatterns, NamedFieldPuns, ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards, TypeOperators #-}
{-# OPTIONS -funbox-strict-fields -O -fspec-constr -fliberate-case -fstatic-argument-transformation #-}
module Data.TrieMap.RadixTrie.Edge     ( searchEdgeC,
      afterEdge,
      assignEdge,
      beforeEdge,
      clearEdge,
      diffEdge,
      extractEdgeLoc,
      indexEdge,
      insertEdge,
      isectEdge,
      lookupEdge,
      mapEitherEdge,
      mapMaybeEdge,
      unionEdge,
      fromAscListEdge) where

import Control.Monad.Lookup
import Control.Monad.Ends
import Control.Monad.Unpack

import Data.TrieMap.Sized
import Data.TrieMap.TrieKey
import Data.TrieMap.WordMap ()
import Data.TrieMap.RadixTrie.Label
import Data.TrieMap.RadixTrie.Slice

import Data.Word

import Data.Vector.Generic (length)
import qualified Data.Vector (Vector)
import qualified Data.Vector.Primitive (Vector)
import Prelude hiding (length, foldr, foldl, zip, take, map)

import GHC.Exts

#define V(f) f (Data.Vector.Vector) (k)
#define U(f) f (Data.Vector.Primitive.Vector) (Word)
#define EDGE(args) (!(eView -> Edge args))
#define LOC(args) !(locView -> Loc args)
#define DEEP(args) !(pView -> Deep args)

instance Label v k => Functor (Edge v k) where
  {-# SPECIALIZE instance TrieKey k => Functor (V(Edge)) #-}
  {-# SPECIALIZE instance Functor (U(Edge)) #-}
  fmap f = map where
    map EDGE(sz ks v ts) = edge' sz ks (f <$> v) (map <$> ts)

instance Label v k => Foldable (Edge v k) where
  {-# SPECIALIZE instance TrieKey k => Foldable (V(Edge)) #-}
  {-# SPECIALIZE instance Foldable (U(Edge)) #-}
  foldMap f = fold where
    foldBranch = foldMap fold
    fold e = case eView e of
      Edge _ _ Nothing ts	-> foldBranch ts
      Edge _ _ (Just a) ts	-> f a `mappend` foldBranch ts
  
  foldr f = flip fold where
    foldBranch = foldr fold
    fold e z = case eView e of
      Edge _ _ Nothing ts -> foldBranch z ts
      Edge _ _ (Just a) ts -> a `f` foldBranch z ts

  foldl f = fold where
    foldBranch = foldl fold
    fold z e = case eView e of
      Edge _ _ Nothing ts -> foldBranch z ts
      Edge _ _ (Just a) ts -> foldBranch (z `f` a) ts

instance Label v k => Traversable (Edge v k) where
  {-# SPECIALIZE instance TrieKey k => Traversable (V(Edge)) #-}
  {-# SPECIALIZE instance Traversable (U(Edge)) #-}
  traverse f = trav where
    travBranch = traverse trav
    trav e = case eView e of
      Edge sz ks Nothing ts	-> edge' sz ks Nothing <$> travBranch ts
      Edge sz ks (Just a) ts	-> edge' sz ks . Just <$> f a <*> travBranch ts

{-# SPECIALIZE lookupEdge ::
      TrieKey k => V() -> V(Edge) a -> Lookup r a,
      U() -> U(Edge) a -> Lookup r a #-}
lookupEdge :: (Eq k, Label v k) => v k -> Edge v k a -> Lookup r a
lookupEdge ks e = Lookup $ \ no yes -> let
  lookupE !ks !EDGE(_ ls !v ts) = if kLen < lLen then no else matchSlice matcher matches ks ls where
    !kLen = length ks
    !lLen = length ls
    matcher k l z
	    | k == l	  = z
	    | otherwise	  = no
    matches _ _
	    | kLen == lLen  = maybe no yes v
	    | (_, k, ks') <- splitSlice lLen ks
			  = runLookup (lookupMC k ts) no (lookupE ks')
  in lookupE ks e

{-# INLINE searchEdgeC #-}
searchEdgeC :: (Eq k, Label v k) => v k -> Edge v k a -> SearchCont (EdgeLoc v k a) a r
searchEdgeC ks0 e nomatch0 match0 = searchE ks0 e root where
  nomatch !ls !ts path = nomatch0 (loc ls ts path)
  match a !ls !ts path = match0 a (loc ls ts path)
  searchE !ks e@EDGE(_ !ls !v ts) path = iMatchSlice matcher matches ks ls where
    matcher i k l z = 
      runLookup (unifierM k l (dropEdge (i+1) e)) z (nomatch (dropSlice (i+1) ks) emptyM . deep path (takeSlice i ls) Nothing)
    matches kLen lLen = case compare kLen lLen of
      LT -> let lPre = takeSlice kLen ls; l = ls !$ kLen; e' = dropEdge (kLen + 1) e in
	      nomatch lPre (singletonM l e') path
      EQ -> maybe nomatch match v ls ts path
      GT -> let
	  {-# INLINE kk #-}
	  kk = ks !$ lLen
	  ks' = dropSlice (lLen + 1) ks
	  nomatch' tHole = nomatch ks' emptyM (deep path ls v tHole)
	  match' e' tHole = searchE ks' e' (deep path ls v tHole)
	  in searchMC kk ts nomatch' match'

{-# SPECIALIZE mapMaybeEdge ::
      (TrieKey k, Sized b) => (a -> Maybe b) -> V(Edge) a -> V(MEdge) b,
      Sized b => (a -> Maybe b) -> U(Edge) a -> U(MEdge) b #-}
mapMaybeEdge :: (Label v k, Sized b) => (a -> Maybe b) -> Edge v k a -> MEdge v k b
mapMaybeEdge f = mapMaybeE where
  mapMaybeE !EDGE(_ ks !v ts) = let !v' = v >>= f in cEdge ks v' (mapMaybeM mapMaybeE ts)

{-# SPECIALIZE mapEitherEdge ::
      (TrieKey k, Sized b, Sized c) => (a -> (# Maybe b, Maybe c #)) -> V(Edge) a -> (# V(MEdge) b, V(MEdge) c #),
      (Sized b, Sized c) => (a -> (# Maybe b, Maybe c #)) -> U(Edge) a -> (# U(MEdge) b, U(MEdge) c #) #-}
mapEitherEdge :: (Label v k, Sized b, Sized c) => 
	(a -> (# Maybe b, Maybe c #)) -> Edge v k a -> (# MEdge v k b, MEdge v k c #)
mapEitherEdge f = mapEitherE where
	mapEitherE EDGE(_ ks v ts) = (# cEdge ks vL tsL, cEdge ks vR tsR #)
	  where	!(# vL, vR #) = mapEitherMaybe f v
		!(# tsL, tsR #) = mapEitherM mapEitherE ts

{-# INLINE assignEdge #-}
assignEdge :: (Label v k, Sized a) => a -> EdgeLoc v k a -> Edge v k a
assignEdge v LOC(ks ts path) = assign (edge ks (Just v) ts) path

{-# SPECIALIZE assign ::
      (TrieKey k, Sized a) => V(Edge) a -> V(Path) a -> V(Edge) a,
      Sized a => U(Edge) a -> U(Path) a -> U(Edge) a #-}
assign :: (Label v k, Sized a) => Edge v k a -> Path v k a -> Edge v k a
assign e DEEP(path ks v tHole)	= assign (edge ks v (assignM e tHole)) path
assign e _			= e

{-# SPECIALIZE clearEdge :: 
      (TrieKey k, Sized a) => V(EdgeLoc) a -> V(MEdge) a,
      Sized a => U(EdgeLoc) a -> U(MEdge) a #-}
clearEdge :: (Label v k, Sized a) => EdgeLoc v k a -> MEdge v k a
clearEdge LOC(ks ts path) = rebuild (cEdge ks Nothing ts) path where
  rebuild Nothing DEEP(path ks v tHole)	= rebuild (cEdge ks v (clearM tHole)) path
  rebuild Nothing _	= Nothing
  rebuild (Just e) path = Just $ assign e path

{-# SPECIALIZE unionEdge :: 
      (TrieKey k, Sized a) => (a -> a -> Maybe a) -> V(Edge) a -> V(Edge) a -> V(MEdge) a,
      Sized a => (a -> a -> Maybe a) -> U(Edge) a -> U(Edge) a -> U(MEdge) a #-}
unionEdge :: (Label v k, Sized a) => 
	(a -> a -> Maybe a) -> Edge v k a -> Edge v k a -> MEdge v k a
unionEdge f = unionE where
  unionE !eK@EDGE(_ ks0 !vK tsK) !eL@EDGE(_ ls0 !vL tsL) = iMatchSlice matcher matches ks0 ls0 where
    matcher !i k l z = runLookup (unifyM k eK' l eL') z $ Just . edge (takeSlice i ks0) Nothing
      where eK' = dropEdge (i+1) eK
	    eL' = dropEdge (i+1) eL
    
    matches kLen lLen = case compare kLen lLen of
      EQ -> cEdge ks0 (unionMaybe f vK vL) $ unionM unionE tsK tsL
      LT -> searchMC l tsK nomatch match where
	eL' = dropEdge (kLen + 1) eL; l = ls0 !$ kLen
	nomatch holeKT = cEdge ks0 vK $ assignM eL' holeKT
	match eK' holeKT = cEdge ks0 vK $ fillHoleM (eK' `unionE` eL') holeKT
      GT -> searchMC k tsL nomatch match where
	eK' = dropEdge (lLen + 1) eK; k = ks0 !$ lLen
	nomatch holeLT = cEdge ls0 vL $ assignM eK' holeLT
	match eL' holeLT = cEdge ls0 vL $ fillHoleM (eK' `unionE` eL') holeLT

{-# SPECIALIZE isectEdge ::
      (TrieKey k, Sized c) => (a -> b -> Maybe c) -> V(Edge) a -> V(Edge) b -> V(MEdge) c,
      Sized c => (a -> b -> Maybe c) -> U(Edge) a -> U(Edge) b -> U(MEdge) c #-}
isectEdge :: (Eq k, Label v k, Sized c) =>
	(a -> b -> Maybe c) -> Edge v k a -> Edge v k b -> MEdge v k c
isectEdge f = isectE where
  isectE !eK@EDGE(_ ks0 vK tsK) !eL@EDGE(_ ls0 vL tsL) = matchSlice matcher matches ks0 ls0 where
    matcher k l z = guard (k == l) >> z
    matches kLen lLen = case compare kLen lLen of
      EQ -> cEdge ks0 (isectMaybe f vK vL) $ isectM isectE tsK tsL
      LT -> let l = ls0 !$ kLen in runLookup (lookupMC l tsK) Nothing $ \ eK' ->
	      let eL' = dropEdge (kLen + 1) eL in unDropEdge (kLen + 1) <$> eK' `isectE` eL'
      GT -> let k = ks0 !$ lLen in runLookup (lookupMC k tsL) Nothing $ \ eL' -> 
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
      EQ -> cEdge ks0 (diffMaybe f vK vL) $ diffM diffE tsK tsL
      LT -> searchMC l tsK nomatch match where
	l = ls0 !$ kLen; eL' = dropEdge (kLen + 1) eL 
	nomatch _ = Just eK
	match eK' holeKT = cEdge ks0 vK $ fillHoleM (eK' `diffE` eL') holeKT
      GT -> let k = ks0 !$ lLen; eK' = dropEdge (lLen + 1) eK in 
	runLookup (lookupMC k tsL) (Just eK) (\ eL' -> fmap (unDropEdge (lLen + 1)) (eK' `diffE` eL'))

instance (Eq k, Label v k) => Subset (Edge v k) where
  {-# SPECIALIZE instance (Eq k, TrieKey k) => Subset (V(Edge)) #-}
  {-# SPECIALIZE instance Subset (U(Edge)) #-}
  eK@EDGE(_ ks0 vK tsK) <=? EDGE(_ ls0 vL tsL) = matchSlice matcher matches ks0 ls0 where
    matcher k l z = k == l && z
    matches kLen lLen = case compare kLen lLen of
      LT	-> False
      EQ	-> vK <=? vL && tsK <<=? tsL
      GT	-> let k = ks0 !$ lLen in runLookup (lookupMC k tsL) False (dropEdge (lLen + 1) eK <=?)

{-# SPECIALIZE beforeEdge :: 
      (TrieKey k, Sized a) => Maybe a -> V(EdgeLoc) a -> V(MEdge) a,
      Sized a => Maybe a -> U(EdgeLoc) a -> U(MEdge) a #-}
{-# SPECIALIZE afterEdge :: 
      (TrieKey k, Sized a) => Maybe a -> V(EdgeLoc) a -> V(MEdge) a,
      Sized a => Maybe a -> U(EdgeLoc) a -> U(MEdge) a #-}
beforeEdge, afterEdge :: (Label v k, Sized a) => Maybe a -> EdgeLoc v k a -> MEdge v k a
beforeEdge v LOC(ks ts path) = case cEdge ks v ts of
  Nothing	-> before path
  Just e	-> Just $ beforeWith e path
  where	before DEEP(path ks v tHole) = case cEdge ks v (beforeM tHole) of
	    Nothing	-> before path
	    Just e	-> Just $ beforeWith e path
	before _	= Nothing
	beforeWith e DEEP(path ks v tHole)
			= beforeWith (edge ks v (beforeWithM e tHole)) path
	beforeWith e _	= e

afterEdge v LOC(ks ts path) = case cEdge ks v ts of
  Nothing	-> after path
  Just e	-> Just $ afterWith e path
  where	after DEEP(path ks _ tHole) = case cEdge ks Nothing (afterM tHole) of
	    Nothing	-> after path
	    Just e	-> Just $ afterWith e path
	after _ 	= Nothing
	afterWith e DEEP(path ks _ tHole)
			= afterWith (edge ks Nothing (afterWithM e tHole)) path
	afterWith e _	= e

{-# SPECIALIZE extractEdgeLoc :: 
      TrieKey k => V(Edge) a -> V(Path) a -> First (a, V(EdgeLoc) a),
      TrieKey k => V(Edge) a -> V(Path) a -> Last (a, V(EdgeLoc) a),
      U(Edge) a -> U(Path) a -> First (a, U(EdgeLoc) a),
      U(Edge) a -> U(Path) a -> Last (a, U(EdgeLoc) a)#-}
extractEdgeLoc :: (Label v k, Functor m, MonadPlus m) => Edge v k a -> Path v k a -> m (a, EdgeLoc v k a)
extractEdgeLoc EDGE(_ ks v ts) path = case v of
	Nothing	-> extractTS
	Just a	-> return (a, loc ks ts path) `mplus` extractTS
  where	extractTS = do	(e', tHole) <- extractHoleM ts
			extractEdgeLoc e' (deep path ks v tHole)

{-# SPECIALIZE indexEdge :: 
      (TrieKey k, Sized a) => V(Edge) a -> Int :~> IndexCont (V(EdgeLoc) a) a r,
      Sized a => U(Edge) a -> Int :~> IndexCont (U(EdgeLoc) a) a r #-}
indexEdge :: (Label v k, Sized a) => Edge v k a -> Int :~> IndexCont (EdgeLoc v k a) a r
indexEdge e = unpack $ \ i result -> let
  indexE i !e path = case eView e of
    Edge _ ks v@(Just a) ts
      | i < sv		-> result $~ Indexed i a (loc ks ts path)
      | otherwise	-> indexMC' ts (i - sv) $ \ (Indexed i' e' tHole) -> indexE i' e' (deep path ks v tHole)
	  where	!sv = getSize a
    Edge _ ks Nothing ts
		-> indexMC' ts i $ \ (Indexed i' e' tHole) -> indexE i' e' (deep path ks Nothing tHole)
  in indexE i e root

{-# SPECIALIZE insertEdge ::
      (TrieKey k, Sized a) => (a -> a) -> V() -> a -> V(Edge) a -> V(Edge) a,
      Sized a => (a -> a) -> U() -> a -> U(Edge) a -> U(Edge) a #-}
insertEdge :: (Label v k, Sized a) => (a -> a) -> v k -> a -> Edge v k a -> Edge v k a
insertEdge f ks0 a e = insertE ks0 e where
  insertE !ks eL@EDGE(szL ls !v ts) = iMatchSlice matcher matches ks ls where
    !sza = getSize a
    !szV = szL - sizeM ts
    matcher !i k l z = runLookup (unifyM k eK' l eL') z (edge (takeSlice i ls) Nothing)
      where	eK' = edge' sza (dropSlice (i+1) ks) (Just a) emptyM
		eL' = dropEdge (i+1) eL
    matches kLen lLen = case compare kLen lLen of
      LT -> (edge' (sza + szL) ks (Just a) (singletonM l eL'))
	  where	l = ls !$ kLen; eL' = dropEdge (kLen+1) eL
      EQ -> (edge ls (Just (maybe a f v)) ts)
      GT -> edge' sz' ls v ts' where
	ks' = dropSlice (lLen + 1) ks
	k = ks !$ lLen
	ts' = insertWithM (insertE ks') k (edge' sza ks' (Just a) emptyM) ts
	sz' = sizeM ts' + szV

{-# SPECIALIZE fromAscListEdge ::
      (TrieKey k, Sized a) => (a -> a -> a) -> Foldl (V(Stack) a) (V()) a (V(MEdge) a),
      Sized a => (a -> a -> a) -> Foldl (U(Stack) a) (U()) a (U(MEdge) a) #-}
fromAscListEdge :: forall v k a .(Label v k, Sized a) => (a -> a -> a) -> 
  Foldl (Stack v k a) (v k) a (MEdge v k a)
fromAscListEdge f = case inline fromDistAscListFold of
  Foldl{snoc = snocB, begin = beginB, done = doneB} 
    -> Foldl{..} where
    begin ks a = stack ks (Just a) Nothing Nothing
    zero = Nothing
    
    snoc stk ks vK = snoc' ks stk where
      snoc' !ks !stk = case sView stk of
	Stack ls !vL !brL !lStack -> iMatchSlice matcher matches ks ls where
	  matcher i k l z
	    | k == l	= z
	    | otherwise	= let
		ksPre = takeSlice i ks
		ksSuf = dropSlice (i+1) ks
		ls' = dropSlice (i+1) ls
		eL = roll (stack ls' vL brL lStack)
		in stack ksPre Nothing (Just (beginB l eL)) (Just (k, begin ksSuf vK))
	  matches kLen lLen
	    | kLen > lLen	= let
		ksPre = takeSlice lLen ks
		k = ks !$ lLen
		ksSuf = dropSlice (lLen + 1) ks
		in case lStack of
		  Just (lChar, lStack)
		    | k == lChar	-> stack ksPre vL brL (Just (lChar, snoc' ksSuf lStack))
		    | otherwise	-> stack ksPre vL (Just $ snocBranch brL lChar lStack)
					(Just (k, begin ksSuf vK))
		  Nothing	-> stack ksPre vL brL (Just (k, begin ksSuf vK))
	    | otherwise	= stack ks (Just (maybe vK (f vK) vL)) brL lStack
		
    
    snocBranch Nothing k stack = beginB k (roll stack)
    snocBranch (Just s) k stack = snocB s k (roll stack)
    
    roll stack = case sView stack of
      Stack ks (Just vK) _ Nothing	-> singletonEdge ks vK
      Stack ks vK brK (Just (kChar, stack')) ->
	edge ks vK $ inline doneB $ snocBranch brK kChar stack'
      _ -> error "Error: bad stack"
    
    done = Just . roll