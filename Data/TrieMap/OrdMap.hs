{-# LANGUAGE BangPatterns, UnboxedTuples, TypeFamilies, PatternGuards, MagicHash, CPP, TupleSections #-}

module Data.TrieMap.OrdMap () where

import Data.TrieMap.TrieKey
import Data.TrieMap.Sized
import Data.TrieMap.Modifiers

import Control.Applicative
import Control.Monad hiding (join, fmap)

import Prelude hiding (lookup, foldr, foldl, fmap)

import GHC.Exts

#define DELTA 5#
#define RATIO 2#

type OrdMap k = TrieMap (Ordered k)

data Path k a =
	Root
	| LeftBin k a !(Path k a) !(OrdMap k a)
	| RightBin k a !(OrdMap k a) !(Path k a)

-- | @'TrieMap' ('Ordered' k) a@ is based on "Data.Map".
instance Ord k => TrieKey (Ordered k) where
	Ord k1 =? Ord k2	= k1 == k2
	Ord k1 `cmp` Ord k2	= k1 `compare` k2
  
	data TrieMap (Ordered k) a = Tip 
              | Bin Int# k a !(OrdMap k a) !(OrdMap k a)
        data Hole (Ordered k) a = 
        	Empty k !(Path k a)
        	| Full k !(Path k a) !(OrdMap k a) !(OrdMap k a)
	emptyM = Tip
	singletonM (Ord k) = singleton k
	lookupM (Ord k) = lookup k
	insertWithM f (Ord k) = insertWith f k
	getSimpleM Tip			= Null
	getSimpleM (Bin _ _ a Tip Tip)	= Singleton a
	getSimpleM _			= NonSimple
	sizeM = size#
	traverseM = traverse
	foldrM = foldr
	foldlM = foldl
	fmapM = fmap
	mapMaybeM = mapMaybe
	mapEitherM = mapEither
	isSubmapM = isSubmap
	fromAscListM  f xs = fromAscList f [(k, a) | (Ord k, a) <- xs]
	fromDistAscListM  xs = fromDistinctAscList  [(k, a) | (Ord k, a) <- xs]
	unionM _ Tip m2 = m2
	unionM _ m1 Tip = m1
	unionM f m1 m2 = hedgeUnion f (const LT) (const GT) m1 m2
	isectM = isect
	diffM _ Tip _ = Tip
	diffM _ m1 Tip = m1
	diffM f m1 m2 = hedgeDiff f (const LT) (const GT) m1 m2
	
	singleHoleM (Ord k) = Empty k Root
	beforeM (Empty _ path) = before Tip path
	beforeM (Full _ path l _) = before l path
	beforeWithM a (Empty k path) = before (singleton k a) path
	beforeWithM a (Full k path l _) = before (insertMax k a l) path
	afterM (Empty _ path) = after Tip path
	afterM (Full _ path _ r) = after r path
	afterWithM a (Empty k path) = after (singleton k a) path
	afterWithM a (Full k path _ r) = after (insertMin k a r) path
	searchM (Ord k) = search k Root
	indexM i# = indexT Root i# where
		indexT path i# (Bin _ kx x l r) 
		  | i# <# sl#	= indexT (LeftBin kx x path r) i# l
		  | i# <# sx#	= (# i# -# sl#, x, Full kx path l r #)
		  | otherwise	= indexT (RightBin kx x l path) (i# -# sx#) r
			where	!sl# = size# l
				!sx# = getSize# x +# sl#
		indexT _ _ _ = indexFail ()
	extractHoleM = extractHole Root where
		extractHole path (Bin _ kx x l r) =
			extractHole (LeftBin kx x path r) l `mplus`
			return (x, Full kx path l r) `mplus`
			extractHole (RightBin kx x l path) r
		extractHole _ _ = mzero
	
	clearM (Empty _ path) = rebuild Tip path
	clearM (Full _ path l r) = rebuild (merge l r) path
	assignM x (Empty k path) = rebuild (singleton k x) path
	assignM x (Full k path l r) = rebuild (join k x l r) path
	
	unifyM (Ord k1) a1 (Ord k2) a2 = case compare k1 k2 of
		EQ	-> Nothing
		LT	-> Just $ bin k1 a1 Tip (singleton k2 a2)
		GT	-> Just $ bin k1 a1 (singleton k2 a2) Tip
	
rebuild :: Sized a => OrdMap k a -> Path k a -> OrdMap k a
rebuild t Root = t
rebuild t (LeftBin kx x path r) = rebuild (balance kx x t r) path
rebuild t (RightBin kx x l path) = rebuild (balance kx x l t) path

lookup :: Ord k => k -> OrdMap k a -> Maybe a
lookup k (Bin _ k' v l r) = case compare k k' of
	LT	-> lookup k l
	EQ	-> Just v
	GT	-> lookup k r
lookup _ _ = Nothing

insertWith :: (Ord k, Sized a) => (a -> a -> a) -> k -> a -> OrdMap k a -> OrdMap k a
insertWith f k a = ins where
  ins Tip = singleton k a
  ins (Bin _ k' a' l r) = case compare k k' of
    LT	-> join k' a' (ins l) r
    EQ	-> bin k' (f a a') l r
    GT	-> join k' a' l (ins r)

singleton :: Sized a => k -> a -> OrdMap k a
singleton k a = Bin (getSize# a) k a Tip Tip

traverse :: (Applicative f, Sized b) => (a -> f b) -> OrdMap k a -> f (OrdMap k b)
traverse _ Tip = pure Tip
traverse f (Bin _ k a l r) = balance k <$> f a <*> traverse f l <*> traverse f r

foldr :: (a -> b -> b) -> OrdMap k a -> b -> b
foldr _ Tip = id
foldr f (Bin _ _ a l r) = foldr f l . f a . foldr f r

foldl :: (b -> a -> b) -> OrdMap k a -> b -> b
foldl _ Tip = id
foldl f (Bin _ _ a l r) = foldl f r . flip f a . foldl f l

fmap :: (Ord k, Sized b) => (a -> b) -> OrdMap k a -> OrdMap k b
fmap f (Bin _ k a l r) = join k (f a) (fmap f l) (fmap f r)
fmap _ _ = Tip

mapMaybe :: (Ord k, Sized b) => (a -> Maybe b) -> OrdMap k a -> OrdMap k b
mapMaybe f (Bin _ k a l r) = joinMaybe  k (f a) (mapMaybe f l) (mapMaybe f r)
mapMaybe _ _ = Tip

mapEither :: (Ord k, Sized b, Sized c) => (a -> (# Maybe b, Maybe c #)) ->
	OrdMap k a -> (# OrdMap k b, OrdMap k c #)
mapEither f (Bin _ k a l r) = (# joinMaybe k aL lL rL, joinMaybe k aR lR rR #)
  where !(# aL, aR #) = f a; !(# lL, lR #) = mapEither f l; !(# rL, rR #) = mapEither f r
mapEither _ _ = (# Tip, Tip #)

isSubmap :: (Ord k, Sized a, Sized b) => LEq a b -> LEq (OrdMap k a) (OrdMap k b)
isSubmap _ Tip _ = True
isSubmap _ _ Tip = False
isSubmap (<=) (Bin _ kx x l r) t = case search kx Root t of
	  (# Nothing, _ #)	-> False
	  (# Just y, hole #)	-> x <= y && isSubmap (<=) l (beforeM hole) && isSubmap (<=) r (afterM hole)

fromAscList :: (Eq k, Sized a) => (a -> a -> a) -> [(k, a)] -> OrdMap k a
fromAscList f xs = fromDistinctAscList (combineEq xs) where
	combineEq (x:xs) = combineEq' x xs
	combineEq [] = []
	
	combineEq' z [] = [z]
	combineEq' (kz, zz) (x@(kx, xx):xs)
		| kz == kx	= combineEq' (kx, f xx zz) xs
		| otherwise	= (kz,zz):combineEq' x xs

fromDistinctAscList :: Sized a => [(k, a)] -> OrdMap k a
fromDistinctAscList xs = build const (length xs) xs
  where
    -- 1) use continutations so that we use heap space instead of stack space.
    -- 2) special case for n==5 to build bushier trees. 
    build c 0 xs'  = c Tip xs'
    build c 5 xs'  = case xs' of
                      ((k1,x1):(k2,x2):(k3,x3):(k4,x4):(k5,x5):xx) 
                            -> c (bin k4 x4 (bin k2 x2 (singleton k1 x1) (singleton k3 x3)) (singleton k5 x5)) xx
                      _ -> error "fromDistinctAscList build"
    build c n xs'  = seq nr $ build (buildR nr c) nl xs'
                   where
                     nl = n `div` 2
                     nr = n - nl - 1

    buildR n c l ((k,x):ys) = build (buildB l k x c) n ys
    buildR _ _ _ []         = error "fromDistinctAscList buildR []"
    buildB l k x c r zs     = c (bin k x l r) zs

hedgeUnion :: (Ord k, Sized a)
                  => (a -> a -> Maybe a)
                  -> (k -> Ordering) -> (k -> Ordering)
                  -> OrdMap k a -> OrdMap k a -> OrdMap k a
hedgeUnion _ _     _     t1 Tip
  = t1
hedgeUnion _ cmplo cmphi Tip (Bin _ kx x l r)
  = join kx x (filterGt  cmplo l) (filterLt  cmphi r)
hedgeUnion f cmplo cmphi (Bin _ kx x l r) t2
  = joinMaybe  kx newx (hedgeUnion  f cmplo cmpkx l lt) 
                (hedgeUnion  f cmpkx cmphi r gt)
  where
    cmpkx k     = compare kx k
    lt          = trim cmplo cmpkx t2
    (found,gt)  = trimLookupLo kx cmphi t2
    newx        = case found of
                    Nothing -> Just x
                    Just (_,y) -> f x y

filterGt :: (Ord k, Sized a) => (k -> Ordering) -> OrdMap k a -> OrdMap k a
filterGt _   Tip = Tip
filterGt cmp (Bin _ kx x l r)
  = case cmp kx of
      LT -> join kx x (filterGt  cmp l) r
      GT -> filterGt  cmp r
      EQ -> r
      
filterLt :: (Ord k, Sized a) => (k -> Ordering) -> OrdMap k a -> OrdMap k a
filterLt _   Tip = Tip
filterLt cmp (Bin _ kx x l r)
  = case cmp kx of
      LT -> filterLt cmp l
      GT -> join kx x l (filterLt  cmp r)
      EQ -> l

trim :: (k -> Ordering) -> (k -> Ordering) -> OrdMap k a -> OrdMap k a
trim _     _     Tip = Tip
trim cmplo cmphi t@(Bin _ kx _ l r)
  = case cmplo kx of
      LT -> case cmphi kx of
              GT -> t
              _  -> trim cmplo cmphi l
      _  -> trim cmplo cmphi r
              
trimLookupLo :: Ord k => k -> (k -> Ordering) -> OrdMap k a -> (Maybe (k,a), OrdMap k a)
trimLookupLo _  _     Tip = (Nothing,Tip)
trimLookupLo lo cmphi t@(Bin _ kx x l r)
  = case compare lo kx of
      LT -> case cmphi kx of
              GT -> ((lo,) <$> lookup lo t, t)
              _  -> trimLookupLo lo cmphi l
      GT -> trimLookupLo lo cmphi r
      EQ -> (Just (kx,x),trim (compare lo) cmphi r)

isect :: (Ord k, Sized a, Sized b, Sized c) => (a -> b -> Maybe c) -> OrdMap k a -> OrdMap k b -> OrdMap k c
isect f t1@Bin{} (Bin _ k2 x2 l2 r2) 
  = joinMaybe k2 (found >>= \ x1' -> f x1' x2) tl tr
  where	!(# found, hole #) = search k2 Root t1
	tl = isect f (beforeM hole) l2
	tr = isect f (afterM hole) r2
isect _ _ _ = Tip

hedgeDiff :: (Ord k, Sized a)
                 => (a -> b -> Maybe a)
                 -> (k -> Ordering) -> (k -> Ordering)
                 -> OrdMap k a -> OrdMap k b -> OrdMap k a
hedgeDiff _ _     _     Tip _
  = Tip
hedgeDiff _ cmplo cmphi (Bin _ kx x l r) Tip
  = join kx x (filterGt  cmplo l) (filterLt  cmphi r)
hedgeDiff  f cmplo cmphi t (Bin _ kx x l r) 
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

joinMaybe :: (Ord k, Sized a) => k -> Maybe a -> OrdMap k a -> OrdMap k a -> OrdMap k a
joinMaybe kx = maybe merge (join kx)

join :: Sized a => k -> a -> OrdMap k a -> OrdMap k a -> OrdMap k a
join kx x Tip r  = insertMin  kx x r
join kx x l Tip  = insertMax  kx x l
join kx x l@(Bin sL# ky y ly ry) r@(Bin sR# kz z lz rz)
  | DELTA *# sL# <=# sR# = balance kz z (join kx x l lz) rz
  | DELTA *# sR# <=# sL# = balance ky y ly (join kx x ry r)
  | otherwise            = bin kx x l r

-- insertMin and insertMax don't perform potentially expensive comparisons.
insertMax,insertMin :: Sized a => k -> a -> OrdMap k a -> OrdMap k a
insertMax kx x t
  = case t of
      Tip -> singleton kx x
      Bin _ ky y l r
          -> balance ky y l (insertMax kx x r)
             
insertMin kx x t
  = case t of
      Tip -> singleton kx x
      Bin _ ky y l r
          -> balance ky y (insertMin kx x l) r
             
{--------------------------------------------------------------------
  [merge l r]: merges two trees.
--------------------------------------------------------------------}
merge :: Sized a => OrdMap k a -> OrdMap k a -> OrdMap k a
merge Tip r   = r
merge l Tip   = l
merge l@(Bin sL# kx x lx rx) r@(Bin sR# ky y ly ry)
  | DELTA *# sL# <=# sR# = balance ky y (merge l ly) ry
  | DELTA *# sR# <=# sL# = balance kx x lx (merge rx r)
  | otherwise		  = glue l r

{--------------------------------------------------------------------
  [glue l r]: glues two trees together.
  Assumes that [l] and [r] are already balanced with respect to each other.
--------------------------------------------------------------------}
glue :: Sized a => OrdMap k a -> OrdMap k a -> OrdMap k a
glue Tip r = r
glue l Tip = l
glue l r
  | size# l ># size# r	= let !(# f, l' #) = deleteFindMax (\ k a -> (# balance k a, Nothing #)) l in f l' r
  | otherwise		= let !(# f, r' #) = deleteFindMin (\ k a -> (# balance k a, Nothing #)) r in f l r'

deleteFindMin :: Sized a => (k -> a -> (# x, Maybe a #)) -> OrdMap k a -> (# x, OrdMap k a #)
deleteFindMin f t 
  = case t of
      Bin _ k x Tip r	-> onSnd (maybe r (\ y' -> bin k y' Tip r)) (f k) x
      Bin _ k x l r	-> onSnd (\ l' -> balance k x l' r) (deleteFindMin f) l
      _			-> (# error "Map.deleteFindMin: can not return the minimal element of an empty fmap", Tip #)

deleteFindMax :: Sized a => (k -> a -> (# x, Maybe a #)) -> OrdMap k a -> (# x, OrdMap k a #)
deleteFindMax f t
  = case t of
      Bin _ k x l Tip -> onSnd (maybe l (\ y -> bin k y l Tip)) (f k) x
      Bin _ k x l r   -> onSnd (balance k x l) (deleteFindMax f) r
      Tip             -> (# error "Map.deleteFindMax: can not return the maximal element of an empty fmap", Tip #)

size# :: OrdMap k a -> Int#
size# Tip = 0#
size# (Bin sz _ _ _ _) = sz

balance :: Sized a => k -> a -> OrdMap k a -> OrdMap k a -> OrdMap k a
balance k x l r
  | sR# >=# (DELTA *# sL#)	= rotateL  k x l r
  | sL# >=# (DELTA *# sR#)	= rotateR  k x l r
  | otherwise			= Bin sX# k x l r
  where
    !sL# = size# l
    !sR# = size# r
    !sX# = sL# +# sR# +# getSize# x

-- rotate
rotateL :: Sized a => k -> a -> OrdMap k a -> OrdMap k a -> OrdMap k a
rotateL k x l r@(Bin _ _ _ ly ry)
  | sL# <# (RATIO *# sR#)	= singleL k x l r
  | otherwise			= doubleL k x l r
  where	!sL# = size# ly
  	!sR# = size# ry
rotateL _ _ _ Tip = error "rotateL Tip"

rotateR :: Sized a => k -> a -> OrdMap k a -> OrdMap k a -> OrdMap k a
rotateR k x l@(Bin _ _ _ ly ry) r
  | sR# <# (RATIO *# sL#)	= singleR k x l r
  | otherwise			= doubleR k x l r
  where	!sL# = size# ly
  	!sR# = size# ry
rotateR _ _ _ _ = error "rotateR Tip"

-- basic rotations
singleL, singleR :: Sized a => k -> a -> OrdMap k a -> OrdMap k a -> OrdMap k a
singleL k1 x1 t1 (Bin _ k2 x2 t2 t3)  = bin k2 x2 (bin k1 x1 t1 t2) t3
singleL k1 x1 t1 Tip = bin k1 x1 t1 Tip
singleR  k1 x1 (Bin _ k2 x2 t1 t2) t3  = bin k2 x2 t1 (bin k1 x1 t2 t3)
singleR  k1 x1 Tip t2 = bin k1 x1 Tip t2

doubleL, doubleR :: Sized a => k -> a -> OrdMap k a -> OrdMap k a -> OrdMap k a
doubleL  k1 x1 t1 (Bin _ k2 x2 (Bin _ k3 x3 t2 t3) t4) = bin k3 x3 (bin k1 x1 t1 t2) (bin k2 x2 t3 t4)
doubleL  k1 x1 t1 t2 = singleL k1 x1 t1 t2
doubleR  k1 x1 (Bin _ k2 x2 t1 (Bin _ k3 x3 t2 t3)) t4 = bin k3 x3 (bin k2 x2 t1 t2) (bin k1 x1 t3 t4)
doubleR  k1 x1 t1 t2 = singleR  k1 x1 t1 t2

bin :: Sized a => k -> a -> OrdMap k a -> OrdMap k a -> OrdMap k a
bin k x l r
  = Bin (size# l +# size# r +# getSize# x) k x l r

before :: Sized a => OrdMap k a -> Path k a -> OrdMap k a
before t (LeftBin _ _ path _) = before t path
before t (RightBin k a l path) = before (join k a l t) path
before t _ = t

after :: Sized a => OrdMap k a -> Path k a -> OrdMap k a
after t (LeftBin k a path r) = after (join k a t r) path
after t (RightBin _ _ _ path) = after t path
after t _ = t

search :: Ord k => k -> Path k a -> OrdMap k a -> (# Maybe a, Hole (Ordered k) a #)
search k path Tip = (# Nothing, Empty k path #)
search k path (Bin _ kx x l r) = case compare k kx of
	LT	-> search k (LeftBin kx x path r) l
	EQ	-> (# Just x, Full k path l r #)
	GT	-> search k (RightBin kx x l path) r