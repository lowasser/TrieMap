{-# LANGUAGE UnboxedTuples, BangPatterns, TypeFamilies, PatternGuards, MagicHash, CPP, NamedFieldPuns, FlexibleInstances #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.TrieMap.WordMap (SNode, WHole, TrieMap(WordMap), Hole(Hole), getWordMap, getHole) where

import Data.TrieMap.TrieKey
import Data.TrieMap.Sized

import Control.Exception (assert)
import Control.Applicative
import Control.Monad hiding (join)

import Data.Bits
import Data.Foldable
import Data.Maybe hiding (mapMaybe)
import Data.Monoid
import Data.TrieMap.Utils

import GHC.Exts

import Prelude hiding (lookup, null, map, foldl, foldr, foldl1, foldr1)

#include "MachDeps.h"
#define NIL SNode{node = Nil}
#define TIP(args) SNode{node = (Tip args)}
#define BIN(args) SNode{node = (Bin args)}

type Nat = Word

type Prefix = Word
type Mask   = Word
type Key    = Word
type Size   = Int

data Path a = Root 
	| LeftBin !Prefix !Mask (Path a) !(SNode a)
	| RightBin !Prefix !Mask !(SNode a) (Path a)

data SNode a = SNode {sz :: !Size, node :: (Node a)}
{-# ANN type SNode ForceSpecConstr #-}
data Node a = Nil | Tip !Key a | Bin !Prefix !Mask !(SNode a) !(SNode a)
{-# ANN type Node ForceSpecConstr #-}

instance Sized (SNode a) where
  getSize# SNode{sz} = unbox sz

instance Sized a => Sized (Node a) where
  getSize# t = unbox $ case t of
    Nil		-> 0
    Tip _ a	-> getSize a
    Bin _ _ l r	-> getSize l + getSize r

{-# INLINE sNode #-}
sNode :: Sized a => Node a -> SNode a
sNode !n = SNode (getSize n) n

data WHole a = WHole !Key (Path a)

{-# INLINE hole #-}
hole :: Key -> Path a -> Hole Word a
hole k path = Hole (WHole k path)

#define HOLE(args) (Hole (WHole args))

-- | @'TrieMap' 'Word' a@ is based on "Data.IntMap".
instance TrieKey Word where
	newtype TrieMap Word a = WordMap {getWordMap :: SNode a}
        newtype Hole Word a = Hole {getHole :: WHole a}
	emptyM = WordMap nil
	singletonM k a = WordMap (singleton k a)
	getSimpleM (WordMap (SNode _ n)) = case n of
	  Nil		-> Null
	  Tip _ a	-> Singleton a
	  _		-> NonSimple
	sizeM (WordMap t) = getSize t
	lookupM k (WordMap m) = lookup k m
	traverseM f (WordMap m) = WordMap <$> traverse f m
	fmapM f (WordMap m) = WordMap (map f m)
	mapMaybeM f (WordMap m) = WordMap (mapMaybe f m)
	mapEitherM f (WordMap m) = both WordMap WordMap (mapEither f) m
	unionM f (WordMap m1) (WordMap m2) = WordMap (unionWith f m1 m2)
	isectM f (WordMap m1) (WordMap m2) = WordMap (intersectionWith f m1 m2)
	diffM f (WordMap m1) (WordMap m2) = WordMap (differenceWith f m1 m2)
	isSubmapM (<=) (WordMap m1) (WordMap m2) = isSubmapOfBy (<=) m1 m2
	
	singleHoleM k = hole k Root
	beforeM HOLE(_ path) = WordMap (before nil path)
	beforeWithM a HOLE(k path) = WordMap (before (singleton k a) path)
	afterM HOLE(_ path) = WordMap (after nil path)
	afterWithM a HOLE(k path) = WordMap (after (singleton k a) path)

	{-# INLINE searchMC #-}
	searchMC !k (WordMap t) = mapSearch (hole k) (searchC k t)
	indexM i (WordMap m) = indexT i m Root where
		indexT !i TIP(kx x) path = (# i, x, hole kx path #)
		indexT !i BIN(p m l r) path
			| i < sl	= indexT i l (LeftBin p m path r)
			| otherwise	= indexT (i - sl) r (RightBin p m l path)
			where !sl = getSize l
		indexT _ NIL _		= indexFail ()
	extractHoleM (WordMap m) = extractHole Root m where
		extractHole _ (SNode _ Nil) = mzero
		extractHole path TIP(kx x) = return (x, hole kx path)
		extractHole path BIN(p m l r) =
			extractHole (LeftBin p m path r) l `mplus`
				extractHole (RightBin p m l path) r
	clearM HOLE(_ path) = WordMap (assign nil path)
	{-# INLINE assignM #-}
	assignM v HOLE(kx path) = WordMap (assign (singleton kx v) path)

	{-# INLINE unifierM #-}
	unifierM k' k a = Hole <$> unifier k' k a

{-# INLINE searchC #-}
searchC :: Key -> SNode a -> SearchCont (Path a) a r
searchC !k t notfound found = seek Root t where
  seek path t@BIN(p m l r)
    | nomatch k p m	= notfound (branchHole k p path t)
    | zero k m
	    = seek (LeftBin p m path r) l
    | otherwise
	    = seek (RightBin p m l path) r
  seek path t@TIP(ky y)
    | k == ky	= found y path
    | otherwise	= notfound (branchHole k ky path t)
  seek path NIL = notfound path

before, after :: SNode a -> Path a -> SNode a
before !t Root = t
before !t (LeftBin _ _ path _) = before t path
before !t (RightBin p m l path) = before (bin p m l t) path
after !t Root = t
after !t (RightBin _ _ _ path) = after t path
after !t (LeftBin p m path r) = after (bin p m t r) path

assign :: Sized a => SNode a -> Path a -> SNode a
assign NIL Root = nil
assign NIL (LeftBin _ _ path r) = assign' r path
assign NIL (RightBin _ _ l path) = assign' l path
assign t Root = t
assign t (LeftBin p m path r) = assign' (bin' p m t r) path
assign t (RightBin p m l path) = assign' (bin' p m l t) path

assign' :: Sized a => SNode a -> Path a -> SNode a
assign' !t Root = t
assign' !t (LeftBin p m path r) = assign' (bin' p m t r) path
assign' !t (RightBin p m l path) = assign' (bin' p m l t) path

branchHole :: Key -> Prefix -> Path a -> SNode a -> Path a
branchHole !k !p path t
  | zero k m	= LeftBin p' m path t
  | otherwise	= RightBin p' m t path
  where	m = branchMask k p
  	p' = mask k m

lookup :: Key -> SNode a -> Maybe a
lookup !k = look where
  look BIN(_ m l r) = look (if zeroN k m then l else r)
  look TIP(kx x)
    | k == kx	= Just x
  look _ = Nothing

singleton :: Sized a => Key -> a -> SNode a
singleton k a = sNode (Tip k a)

singletonMaybe :: Sized a => Key -> Maybe a -> SNode a
singletonMaybe k = maybe nil (singleton k)

traverse :: (Applicative f, Sized b) => (a -> f b) -> SNode a -> f (SNode b)
traverse f (SNode _ t) = case t of
	Nil		-> pure nil
	Tip kx x	-> singleton kx <$> f x
	Bin p m l r	-> bin' p m <$> traverse f l <*> traverse f r

instance Foldable SNode where
  foldMap _ NIL = mempty
  foldMap f TIP(_ x) = f x
  foldMap f BIN(_ _ l r) = foldMap f l `mappend` foldMap f r

  foldr f z BIN(_ _ l r) = foldr f (foldr f z r) l
  foldr f z TIP(_ x) = f x z
  foldr _ z NIL = z
  
  foldl f z BIN(_ _ l r) = foldl f (foldl f z l) r
  foldl f z TIP(_ x) = f z x
  foldl _ z NIL = z
  
  foldr1 _ NIL = foldr1Empty
  foldr1 _ TIP(_ x) = x
  foldr1 f BIN(_ _ l r) = foldr f (foldr1 f r) l
  
  foldl1 _ NIL = foldl1Empty
  foldl1 _ TIP(_ x) = x
  foldl1 f BIN(_ _ l r) = foldl f (foldl1 f l) r

instance Foldable (TrieMap Word) where
  foldMap f (WordMap m) = foldMap f m
  foldr f z (WordMap m) = foldr f z m
  foldl f z (WordMap m) = foldl f z m
  foldr1 f (WordMap m) = foldr1 f m
  foldl1 f (WordMap m) = foldl1 f m

map :: Sized b => (a -> b) -> SNode a -> SNode b
map f BIN(p m l r)	= bin' p m (map f l) (map f r)
map f TIP(kx x)		= singleton kx (f x)
map _ _			= nil

mapMaybe :: Sized b => (a -> Maybe b) -> SNode a -> SNode b
mapMaybe f BIN(p m l r)	= bin p m (mapMaybe f l) (mapMaybe f r)
mapMaybe f TIP(kx x)	= singletonMaybe  kx (f x)
mapMaybe _ _		= nil

mapEither :: (Sized b, Sized c) => (a -> (# Maybe b, Maybe c #)) -> 
	SNode a -> (# SNode b, SNode c #)
mapEither f BIN(p m l r) = both (bin p m lL) (bin p m lR) (mapEither f) r
	where !(# lL, lR #) = mapEither f l
mapEither f TIP(kx x)	= both (singletonMaybe kx) (singletonMaybe kx) f x
mapEither _ _				= (# nil, nil #)

unionWith :: Sized a => (a -> a -> Maybe a) -> SNode a -> SNode a -> SNode a
unionWith f n1@(SNode _ t1) n2@(SNode _ t2) = case (t1, t2) of
  (Nil, _)	-> n2
  (_, Nil)	-> n1
  (Tip k x, _)	-> alter (maybe (Just x) (f x)) k n2
  (_, Tip k x)	-> alter (maybe (Just x) (`f` x)) k n1
  (Bin p1 m1 l1 r1, Bin p2 m2 l2 r2)
    | shorter m1 m2  -> union1
    | shorter m2 m1  -> union2
    | p1 == p2       -> bin p1 m1 (unionWith f l1 l2) (unionWith f r1 r2)
    | otherwise      -> join p1 n1 p2 n2
    where
      union1  | nomatch p2 p1 m1  = join p1 n1 p2 n2
	      | zero p2 m1        = bin p1 m1 (unionWith f l1 n2) r1
	      | otherwise         = bin p1 m1 l1 (unionWith f r1 n2)

      union2  | nomatch p1 p2 m2  = join p1 n1 p2 n2
	      | zero p1 m2        = bin p2 m2 (unionWith f n1 l2) r2
	      | otherwise         = bin p2 m2 l2 (unionWith f n1 r2)

{-# INLINE alter #-}
alter :: Sized a => (Maybe a -> Maybe a) -> Key -> SNode a -> SNode a
alter f k t = getWordMap $ alterM f k (WordMap t)

intersectionWith :: Sized c => (a -> b -> Maybe c) -> SNode a -> SNode b -> SNode c
intersectionWith f n1@(SNode _ t1) n2@(SNode _ t2) = case (t1, t2) of
  (Nil, _)	-> nil
  (_, Nil)	-> nil
  (Tip k x, _)	-> singletonMaybe k (lookup k n2 >>= f x)
  (_, Tip k y)	-> singletonMaybe k (lookup k n1 >>= flip f y)
  (Bin p1 m1 l1 r1, Bin p2 m2 l2 r2)
    | shorter m1 m2  -> intersection1
    | shorter m2 m1  -> intersection2
    | p1 == p2       -> bin p1 m1 (intersectionWith f l1 l2) (intersectionWith f r1 r2)
    | otherwise      -> nil
    where
      intersection1 | nomatch p2 p1 m1  = nil
		    | zero p2 m1        = intersectionWith f l1 n2
		    | otherwise         = intersectionWith f r1 n2

      intersection2 | nomatch p1 p2 m2  = nil
		    | zero p1 m2        = intersectionWith f n1 l2
		    | otherwise         = intersectionWith f n1 r2

differenceWith :: Sized a => (a -> b -> Maybe a) -> SNode a -> SNode b -> SNode a
differenceWith f n1@(SNode _ t1) n2@(SNode _ t2) = case (t1, t2) of
  (Nil, _)	-> nil
  (_, Nil)	-> n1
  (Tip k x, _)	-> maybe n1 (singletonMaybe k . f x) (lookup k n2)
  (_, Tip k y)	-> alter (>>= flip f y) k n1
  (Bin p1 m1 l1 r1, Bin p2 m2 l2 r2)
    | shorter m1 m2  -> difference1
    | shorter m2 m1  -> difference2
    | p1 == p2       -> bin p1 m1 (differenceWith f l1 l2) (differenceWith f r1 r2)
    | otherwise      -> n1
    where
      difference1 | nomatch p2 p1 m1  = n1
		  | zero p2 m1        = bin p1 m1 (differenceWith f l1 n2) r1
		  | otherwise         = bin p1 m1 l1 (differenceWith f r1 n2)

      difference2 | nomatch p1 p2 m2  = n1
		  | zero p1 m2        = differenceWith f n1 l2
		  | otherwise         = differenceWith f n1 r2

isSubmapOfBy :: LEq a b -> LEq (SNode a) (SNode b)
isSubmapOfBy (<=) t1@BIN(p1 m1 l1 r1) BIN(p2 m2 l2 r2)
    | shorter m1 m2  = False
    | shorter m2 m1  = match p1 p2 m2 && (if zero p1 m2 then isSubmapOfBy (<=) t1 l2
							else isSubmapOfBy (<=) t1 r2)
    | otherwise      = (p1==p2) && isSubmapOfBy (<=) l1 l2 && isSubmapOfBy (<=) r1 r2
isSubmapOfBy _ BIN(_ _ _ _) _	= False
isSubmapOfBy (<=) TIP(k x) t2	= maybe False (x <=) (lookup k t2)
isSubmapOfBy _ NIL _		= True

zero :: Key -> Mask -> Bool
zero i m
  = i .&. m == 0

nomatch,match :: Key -> Prefix -> Mask -> Bool
nomatch i p m
  = (mask i m) /= p

match i p m
  = (mask i m) == p

zeroN :: Nat -> Nat -> Bool
zeroN i m = (i .&. m) == 0

mask :: Nat -> Nat -> Prefix
mask i m
  = i .&. compl ((m-1) .|. m)

shorter :: Mask -> Mask -> Bool
shorter m1 m2
  = m1 > m2

branchMask :: Prefix -> Prefix -> Mask
branchMask p1 p2
  = highestBitMask (p1 `xor` p2)

highestBitMask :: Nat -> Nat
highestBitMask x0
  = case (x0 .|. shiftR x0 1) of
     x1 -> case (x1 .|. shiftR x1 2) of
      x2 -> case (x2 .|. shiftR x2 4) of
       x3 -> case (x3 .|. shiftR x3 8) of
        x4 -> case (x4 .|. shiftR x4 16) of
         x5 -> case (x5 .|. shiftR x5 32) of   -- for 64 bit platforms
          x6 -> (x6 `xor` (shiftR x6 1))

{-# INLINE join #-}
join :: Prefix -> SNode a -> Prefix -> SNode a -> SNode a
join p1 t1 p2 t2
  | zero p1 m = bin' p m t1 t2
  | otherwise = bin' p m t2 t1
  where
    m = branchMask p1 p2
    p = mask p1 m

nil :: SNode a
nil = SNode 0 Nil

bin :: Prefix -> Mask -> SNode a -> SNode a -> SNode a
bin p m l@(SNode sl tl) r@(SNode sr tr) = case (tl, tr) of
  (Nil, _)	-> r
  (_, Nil)	-> l
  _		-> SNode (sl + sr) (Bin p m l r)

bin' :: Prefix -> Mask -> SNode a -> SNode a -> SNode a
bin' p m l@SNode{sz=sl} r@SNode{sz=sr} = assert (nonempty l && nonempty r) $ SNode (sl + sr) (Bin p m l r)
  where	nonempty NIL = False
  	nonempty _ = True

{-# INLINE unifier #-}
unifier :: Sized a => Key -> Key -> a -> Maybe (WHole a)
unifier k' k a
    | k' == k	= Nothing
    | otherwise	= Just (WHole k' $ branchHole k' k Root (singleton k a))