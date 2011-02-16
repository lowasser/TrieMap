{-# LANGUAGE BangPatterns, UnboxedTuples, TypeFamilies, PatternGuards, MagicHash, CPP, TupleSections, NamedFieldPuns, FlexibleInstances #-}
{-# LANGUAGE RecordWildCards, ImplicitParams #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.TrieMap.OrdMap () where

import Control.Monad.Lookup

import Data.TrieMap.TrieKey
import Data.TrieMap.Sized
import Data.TrieMap.Modifiers

import Prelude hiding (lookup, foldr, foldl, foldr1, foldl1, map)
import GHC.Exts

#define DELTA 5
#define RATIO 2

data Path k a =
	Root
	| LeftBin k a !(Path k a) !(SNode k a)
	| RightBin k a !(SNode k a) !(Path k a)

data Node k a =
  Tip
  | Bin k a !(SNode k a) !(SNode k a)
data SNode k a = SNode{sz :: !Int, count :: !Int, node :: Node k a}

#define TIP SNode{node=Tip}
#define BIN(args) SNode{node=Bin args}

instance Sized a => Sized (Node k a) where
  getSize# m = unbox $ case m of
    Tip	-> 0
    Bin _ a l r	-> getSize a + getSize l + getSize r

instance Sized (SNode k a) where
  getSize# SNode{sz} = unbox sz

nCount :: Node k a -> Int
nCount Tip = 0
nCount (Bin _ _ l r) = 1 + count l + count r

sNode :: Sized a => Node k a -> SNode k a
sNode !n = SNode (getSize n) (nCount n) n

tip :: SNode k a
tip = SNode 0 0 Tip

instance Ord k => Subset (TrieMap (Ordered k)) where
  OrdMap m1 <=? OrdMap m2 = m1 <=? m2

instance Functor (TrieMap (Ordered k)) where
  fmap f (OrdMap m) = OrdMap (f <$> m)

instance Foldable (TrieMap (Ordered k)) where
  foldMap f (OrdMap m) = foldMap f m
  foldr f z (OrdMap m) = foldr f z m
  foldl f z (OrdMap m) = foldl f z m

instance Traversable (TrieMap (Ordered k)) where
  traverse f (OrdMap m) = OrdMap <$> traverse f m

-- | @'TrieMap' ('Ordered' k) a@ is based on "Data.Map".
instance Ord k => TrieKey (Ordered k) where
	newtype TrieMap (Ordered k) a = OrdMap (SNode k a)
        data Hole (Ordered k) a = 
        	Empty k !(Path k a)
        	| Full k !(Path k a) !(SNode k a) !(SNode k a)
	emptyM = OrdMap tip
	singletonM (Ord k) a = OrdMap (singleton k a)
	lookupMC (Ord k) (OrdMap m) = lookupC k m
	getSimpleM (OrdMap m) = case m of
		TIP	-> Null
		BIN(_ a TIP TIP)
			-> Singleton a
		_	-> NonSimple
	sizeM (OrdMap m) = sz m
	mapMaybeM f (OrdMap m) = OrdMap (mapMaybe f m)
	mapEitherM f (OrdMap m) = both OrdMap OrdMap (mapEither f) m
	fromAscListFold f = combineKeys f fromDistAscListFold
	fromDistAscListFold = OrdMap <$> mapFoldlKey unOrd fromDistAscList
	unionM f (OrdMap m1) (OrdMap m2) = OrdMap $ hedgeUnion f (const LT) (const GT) m1 m2
	isectM f (OrdMap m1) (OrdMap m2) = OrdMap $ isect f m1 m2
	diffM f (OrdMap m1) (OrdMap m2) = OrdMap $ hedgeDiff f (const LT) (const GT) m1 m2
	
	singleHoleM (Ord k) = Empty k Root
	beforeM (Empty _ path) = OrdMap $ before tip path
	beforeM (Full _ path l _) = OrdMap $ before l path
	beforeWithM a (Empty k path) = OrdMap $ before (singleton k a) path
	beforeWithM a (Full k path l _) = OrdMap $ before (insertMax k a l) path
	afterM (Empty _ path) = OrdMap $ after tip path
	afterM (Full _ path _ r) = OrdMap $ after r path
	afterWithM a (Empty k path) = OrdMap $ after (singleton k a) path
	afterWithM a (Full k path _ r) = OrdMap $ after (insertMin k a r) path
	searchMC (Ord k) (OrdMap m) = search k m
	indexMC i (OrdMap m) result = indexT Root i m where
		indexT path i# BIN(kx x l r) 
		  | i# <# sl#	= indexT (LeftBin kx x path r) i# l
		  | otherwise	= let !sx# = sl# +# getSize# x in
		      if i# <# sx# then result (i# -# sl#) x (Full kx path l r)
			else indexT (RightBin kx x l path) (i# -# sx#) r
			where	!sl# = getSize# l
		indexT _ _ _ = indexFail
	extractHoleM (OrdMap m) = extractHole Root m where
		extractHole path BIN(kx x l r) =
			extractHole (LeftBin kx x path r) l `mplus`
			return (x, Full kx path l r) `mplus`
			extractHole (RightBin kx x l path) r
		extractHole _ _ = mzero
	
	clearM (Empty _ path) = OrdMap $ rebuild tip path
	clearM (Full _ path l r) = OrdMap $ rebuild (merge l r) path
	assignM x (Empty k path) = OrdMap $ rebuild (singleton k x) path
	assignM x (Full k path l r) = OrdMap $ rebuild (join k x l r) path
	
	unifierM (Ord k') (Ord k) a = case compare k' k of
		EQ	-> Nothing
		LT	-> Just $ Empty k' (LeftBin k a Root tip)
		GT	-> Just $ Empty k' (RightBin k a tip Root)

rebuild :: Sized a => SNode k a -> Path k a -> SNode k a
rebuild t Root = t
rebuild t (LeftBin kx x path r) = rebuild (balance kx x t r) path
rebuild t (RightBin kx x l path) = rebuild (balance kx x l t) path

lookupC :: Ord k => k -> SNode k a -> Lookup r a
lookupC k = look where
  look BIN(kx x l r) = case compare k kx of
	LT	-> look l
	EQ	-> return x
	GT	-> look r
  look _ = mzero

singleton :: Sized a => k -> a -> SNode k a
singleton k a = bin k a tip tip

instance Traversable (SNode k) where
  traverse f = trav where
    trav TIP = pure tip
    trav SNode{node = Bin k a l r, ..} =
      let done a' l' r' = SNode sz count (Bin k a' l' r') in
	done <$> f a <*> trav l <*> trav r

instance Foldable (SNode k) where
  foldMap _ TIP = mempty
  foldMap f BIN(_ a l r) = foldMap f l `mappend` f a `mappend` foldMap f r

  foldr _ z TIP	= z
  foldr f z BIN(_ a l r) = foldr f (a `f` foldr f z r) l
  foldl _ z TIP = z
  foldl f z BIN(_ a l r) = foldl f (foldl f z l `f` a) r

instance Functor (SNode k) where
  fmap f = map where
    map SNode{node = Bin k a l r, ..} = SNode {node = Bin k (f a) (map l) (map r), ..}
    map _ = tip

mapMaybe :: (Ord k, Sized b) => (a -> Maybe b) -> SNode k a -> SNode k b
mapMaybe f BIN(k a l r) = joinMaybe  k (f a) (mapMaybe f l) (mapMaybe f r)
mapMaybe _ _ = tip

mapEither :: (Ord k, Sized b, Sized c) => (a -> (# Maybe b, Maybe c #)) ->
	SNode k a -> (# SNode k b, SNode k c #)
mapEither f BIN(k a l r) = (# joinMaybe k aL lL rL, joinMaybe k aR lR rR #)
  where !(# aL, aR #) = f a; !(# lL, lR #) = mapEither f l; !(# rL, rR #) = mapEither f r
mapEither _ _ = (# tip, tip #)

splitLookup :: (Ord k, Sized a) => k -> SNode k a -> (SNode k a -> Maybe a -> SNode k a -> r) -> r
splitLookup k t cont = search k t (split Nothing) (split . Just) where
  split v (Empty _ path) = cont (before tip path) v (after tip path)
  split v (Full _ path l r) = cont (before l path) v (after r path)

instance Ord k => Subset (SNode k) where
  (<=?) = subMap where
    TIP `subMap` _	= True
    _ `subMap` TIP	= False
    BIN(kx x l r) `subMap` t = splitLookup kx t result
      where result _ Nothing _	= False
	    result tl (Just y) tr	= ?le x y && l `subMap` tl && r `subMap` tr

fromDistAscList :: (Eq k, Sized a) => Foldl k a (SNode k a)
fromDistAscList = Foldl{zero = tip, ..} where
  incr !t (Yes t' stk) = No (incr (t' `glue` t) stk)
  incr !t (No stk) = Yes t stk
  incr !t End = Yes t End
  
  begin k a = Yes (singleton k a) End
  
  snoc stk k a = incr (singleton k a) stk
  
  roll !t End = t
  roll !t (No stk) = roll t stk
  roll !t (Yes t' stk) = roll (t' `glue` t) stk
  
  done = roll tip

data Stack k a = No (Stack k a) | Yes !(SNode k a) (Stack k a) | End

hedgeUnion :: (Ord k, Sized a)
                  => (a -> a -> Maybe a)
                  -> (k -> Ordering) -> (k -> Ordering)
                  -> SNode k a -> SNode k a -> SNode k a
hedgeUnion _ _     _     t1 TIP
  = t1
hedgeUnion _ cmplo cmphi TIP BIN(kx x l r)
  = join kx x (filterGt  cmplo l) (filterLt  cmphi r)
hedgeUnion f cmplo cmphi BIN(kx x l r) t2
  = joinMaybe  kx newx (hedgeUnion  f cmplo cmpkx l lt) 
                (hedgeUnion  f cmpkx cmphi r gt)
  where
    cmpkx k     = compare kx k
    lt          = trim cmplo cmpkx t2
    (found,gt)  = trimLookupLo kx cmphi t2
    newx        = case found of
                    Nothing -> Just x
                    Just (_,y) -> f x y

filterGt :: (Ord k, Sized a) => (k -> Ordering) -> SNode k a -> SNode k a
filterGt _   TIP = tip
filterGt cmp BIN(kx x l r)
  = case cmp kx of
      LT -> join kx x (filterGt  cmp l) r
      GT -> filterGt  cmp r
      EQ -> r

filterLt :: (Ord k, Sized a) => (k -> Ordering) -> SNode k a -> SNode k a
filterLt _   TIP = tip
filterLt cmp BIN(kx x l r)
  = case cmp kx of
      LT -> filterLt cmp l
      GT -> join kx x l (filterLt  cmp r)
      EQ -> l

trim :: (k -> Ordering) -> (k -> Ordering) -> SNode k a -> SNode k a
trim cmplo cmphi = trimmer where
  trimmer TIP	= tip
  trimmer t@BIN(kx _ l r) = case (cmplo kx, cmphi kx) of
    (LT, GT)	-> t
    (LT, _)	-> trimmer l
    _		-> trimmer r
              
trimLookupLo :: Ord k => k -> (k -> Ordering) -> SNode k a -> (Maybe (k,a), SNode k a)
trimLookupLo _  _     TIP = (Nothing,tip)
trimLookupLo lo cmphi t@BIN(kx x l r)
  = case compare lo kx of
      LT -> case cmphi kx of
              GT -> (runLookup (lookupC lo t) Nothing (\ a -> Just (lo, a)), t)
              _  -> trimLookupLo lo cmphi l
      GT -> trimLookupLo lo cmphi r
      EQ -> (Just (kx,x),trim (compare lo) cmphi r)

isect :: (Ord k, Sized a, Sized b, Sized c) => (a -> b -> Maybe c) -> SNode k a -> SNode k b -> SNode k c
isect f t1@BIN(_ _ _ _) BIN(k2 x2 l2 r2) = splitLookup k2 t1 result where
  result tl found tr = joinMaybe k2 (found >>= \ x1' -> f x1' x2) (isect f tl l2) (isect f tr r2)
isect _ _ _ = tip

hedgeDiff :: (Ord k, Sized a)
                 => (a -> b -> Maybe a)
                 -> (k -> Ordering) -> (k -> Ordering)
                 -> SNode k a -> SNode k b -> SNode k a
hedgeDiff _ _     _     TIP _
  = tip
hedgeDiff _ cmplo cmphi BIN(kx x l r) TIP
  = join kx x (filterGt  cmplo l) (filterLt  cmphi r)
hedgeDiff  f cmplo cmphi t BIN(kx x l r) 
  = case found of
      Nothing -> merge  tl tr
      Just (ky,y) -> 
          case f y x of
            Nothing -> merge tl tr
            Just z  -> join ky z tl tr
  where
    cmpkx k     = compare kx k   
    lt          = trim cmplo cmpkx t
    (found,gt)  = trimLookupLo kx cmphi t
    tl          = hedgeDiff f cmplo cmpkx lt l
    tr          = hedgeDiff f cmpkx cmphi gt r

joinMaybe :: (Ord k, Sized a) => k -> Maybe a -> SNode k a -> SNode k a -> SNode k a
joinMaybe kx = maybe merge (join kx)

join :: Sized a => k -> a -> SNode k a -> SNode k a -> SNode k a
join kx x TIP r  = insertMin  kx x r
join kx x l TIP  = insertMax  kx x l
join kx x l@(SNode _ sL (Bin ky y ly ry)) r@(SNode _ sR (Bin kz z lz rz))
  | DELTA * sL <= sR = balance kz z (join kx x l lz) rz
  | DELTA * sR <= sL = balance ky y ly (join kx x ry r)
  | otherwise        = bin kx x l r

-- insertMin and insertMax don't perform potentially expensive comparisons.
insertMax,insertMin :: Sized a => k -> a -> SNode k a -> SNode k a
insertMax kx x = insMax where
  insMax TIP	= singleton kx x
  insMax BIN(ky y l r)
		= balance ky y l (insMax r)
             
insertMin kx x = insMin where
  insMin TIP	= singleton kx x
  insMin BIN(ky y l r)
  		= balance ky y (insMin l) r
             
{--------------------------------------------------------------------
  [merge l r]: merges two trees.
--------------------------------------------------------------------}
merge :: Sized a => SNode k a -> SNode k a -> SNode k a
merge TIP r   = r
merge l TIP   = l
merge l@(SNode _ sL (Bin kx x lx rx)) r@(SNode _ sR (Bin ky y ly ry))
  | DELTA * sL <= sR	= balance ky y (merge l ly) ry
  | DELTA * sR <= sL	= balance kx x lx (merge rx r)
  | otherwise		= glue l r

{--------------------------------------------------------------------
  [glue l r]: glues two trees together.
  Assumes that [l] and [r] are already balanced with respect to each other.
--------------------------------------------------------------------}
glue :: Sized a => SNode k a -> SNode k a -> SNode k a
glue TIP r = r
glue l TIP = l
glue l r
  | count l > count r	= let !(# f, l' #) = deleteFindMax balance l in f l' r
  | otherwise		= let !(# f, r' #) = deleteFindMin balance r in f l r'

deleteFindMin :: Sized a => (k -> a -> x) -> SNode k a -> (# x, SNode k a #)
deleteFindMin f t 
  = case t of
      BIN(k x TIP r)	-> (# f k x, r #)
      BIN(k x l r)	-> onSnd (\ l' -> balance k x l' r) (deleteFindMin f) l
      _			-> (# error "Map.deleteFindMin: can not return the minimal element of an empty fmap", tip #)

deleteFindMax :: Sized a => (k -> a -> x) -> SNode k a -> (# x, SNode k a #)
deleteFindMax f t
  = case t of
      BIN(k x l TIP)	-> (# f k x, l #)
      BIN(k x l r)	-> onSnd (balance k x l) (deleteFindMax f) r
      TIP		-> (# error "Map.deleteFindMax: can not return the maximal element of an empty fmap", tip #)

balance :: Sized a => k -> a -> SNode k a -> SNode k a -> SNode k a
balance k x l r
  | sR >= (DELTA * sL)	= rotateL  k x l r
  | sL >= (DELTA * sR)	= rotateR  k x l r
  | otherwise		= bin k x l r
  where
    !sL = count l
    !sR = count r

-- rotate
rotateL :: Sized a => k -> a -> SNode k a -> SNode k a -> SNode k a
rotateL k x l r@BIN(_ _ ly ry)
  | sL < (RATIO * sR)	= singleL k x l r
  | otherwise		= doubleL k x l r
  where	!sL = count ly
  	!sR = count ry
rotateL k x l TIP	= insertMax k x l

rotateR :: Sized a => k -> a -> SNode k a -> SNode k a -> SNode k a
rotateR k x l@BIN(_ _ ly ry) r
  | sR < (RATIO * sL)	= singleR k x l r
  | otherwise		= doubleR k x l r
  where	!sL = count ly
  	!sR = count ry
rotateR k x TIP r	= insertMin k x r

-- basic rotations
singleL, singleR :: Sized a => k -> a -> SNode k a -> SNode k a -> SNode k a
singleL k1 x1 t1 BIN(k2 x2 t2 t3)  = bin k2 x2 (bin k1 x1 t1 t2) t3
singleL k1 x1 t1 TIP = bin k1 x1 t1 tip
singleR  k1 x1 BIN(k2 x2 t1 t2) t3  = bin k2 x2 t1 (bin k1 x1 t2 t3)
singleR  k1 x1 TIP t2 = bin k1 x1 tip t2

doubleL, doubleR :: Sized a => k -> a -> SNode k a -> SNode k a -> SNode k a
doubleL  k1 x1 t1 BIN(k2 x2 BIN(k3 x3 t2 t3) t4) = bin k3 x3 (bin k1 x1 t1 t2) (bin k2 x2 t3 t4)
doubleL  k1 x1 t1 t2 = singleL k1 x1 t1 t2
doubleR  k1 x1 BIN(k2 x2 t1 BIN(k3 x3 t2 t3)) t4 = bin k3 x3 (bin k2 x2 t1 t2) (bin k1 x1 t3 t4)
doubleR  k1 x1 t1 t2 = singleR  k1 x1 t1 t2

bin :: Sized a => k -> a -> SNode k a -> SNode k a -> SNode k a
bin k x l r
  = sNode (Bin k x l r)

before :: Sized a => SNode k a -> Path k a -> SNode k a
before t (LeftBin _ _ path _) = before t path
before t (RightBin k a l path) = before (join k a l t) path
before t _ = t

after :: Sized a => SNode k a -> Path k a -> SNode k a
after t (LeftBin k a path r) = after (join k a t r) path
after t (RightBin _ _ _ path) = after t path
after t _ = t

search :: Ord k => k -> SNode k a -> SearchCont (Hole (Ordered k) a) a r
search k t f g = searcher Root t where
  searcher path TIP = f (Empty k path)
  searcher path BIN(kx x l r) = case compare k kx of
	LT	-> searcher (LeftBin kx x path r) l
	EQ	-> g x (Full k path l r)
	GT	-> searcher (RightBin kx x l path) r