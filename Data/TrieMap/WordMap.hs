{-# LANGUAGE UnboxedTuples, BangPatterns, TypeFamilies, PatternGuards, MagicHash, CPP #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.TrieMap.WordMap (SNode, TrieMap(WordMap), getWordMap) where

import Data.TrieMap.TrieKey
import Data.TrieMap.Sized

import Control.Applicative
import Control.Monad hiding (join)

import Data.Bits
import Data.Maybe hiding (mapMaybe)
import Data.Word

import GHC.Exts

import Prelude hiding (lookup, null, foldl, foldr)

#include "MachDeps.h"
type Nat = Word

type Prefix = Word
type Mask   = Word
type Key    = Word
type Size   = Int#

data Path a = Root 
	| LeftBin !Prefix !Mask (Path a) !(SNode a)
	| RightBin !Prefix !Mask !(SNode a) (Path a)

data SNode a = SNode {_sSize :: !Size, sContents :: (Node a)}
{-# ANN type SNode ForceSpecConstr #-}
data Node a = Nil | Tip !Key a | Bin !Prefix !Mask !(SNode a) !(SNode a)
{-# ANN type Node ForceSpecConstr #-}

instance Sized (SNode a) where
  getSize# (SNode sz# _) = sz#

instance Sized a => Sized (Node a) where
  getSize# Nil	= 0#
  getSize# (Tip _ a) = getSize# a
  getSize# (Bin _ _ l r) = getSize# l +# getSize# r

sNode :: Sized a => Node a -> SNode a
sNode !n = SNode (getSize# n) n

-- | @'TrieMap' 'Word' a@ is based on "Data.IntMap".
instance TrieKey Word where
	(=?) = (==)
	cmp = compare

	newtype TrieMap Word a = WordMap {getWordMap :: SNode a}
        data Hole Word a = Hole !Key !(Path a)
	emptyM = WordMap nil
	singletonM k a = WordMap (singleton k a)
	getSimpleM (WordMap (SNode _ n)) = case n of
	  Nil		-> Null
	  Tip _ a	-> Singleton a
	  _		-> NonSimple
	sizeM (WordMap t) = getSize# t
	lookupM k (WordMap m) = lookup k (sContents m)
	insertWithM f k a (WordMap m) = WordMap (insertWith f k a m)
	traverseM f (WordMap m) = WordMap <$> traverse f m
	foldrM f (WordMap m) = foldr f m
	foldlM f (WordMap m) = foldl f m
	fmapM f (WordMap m) = WordMap (mapWithKey f m)
	mapMaybeM f (WordMap m) = WordMap (mapMaybe f m)
	mapEitherM f (WordMap m) = both WordMap WordMap (mapEither f) m
	unionM f (WordMap m1) (WordMap m2) = WordMap (unionWith f m1 m2)
	isectM f (WordMap m1) (WordMap m2) = WordMap (intersectionWith f m1 m2)
	diffM f (WordMap m1) (WordMap m2) = WordMap (differenceWith f m1 m2)
	isSubmapM (<=) (WordMap m1) (WordMap m2) = isSubmapOfBy (<=) m1 m2
	
	singleHoleM k = Hole k Root
	beforeM (Hole _ path) = WordMap (before nil path)
	beforeWithM a (Hole k path) = WordMap (before (singleton k a) path)
	afterM (Hole _ path) = WordMap (after nil path)
	afterWithM a (Hole k path) = WordMap (after (singleton k a) path)

	searchM !k (WordMap t) = onSnd (Hole k) (search k Root) t
	indexM i# (WordMap m) = indexT i# m Root where
		indexT _ (SNode _ Nil) _ = indexFail ()
		indexT i# (SNode _ (Tip kx x)) path = (# i#, x, Hole kx path #)
		indexT i# (SNode _ (Bin p m l r)) path
			| i# <# sl#	= indexT i# l (LeftBin p m path r)
			| otherwise	= indexT (i# -# sl#) r (RightBin p m l path)
			where !sl# = getSize# l
	extractHoleM (WordMap m) = extractHole Root m where
		extractHole _ (SNode _ Nil) = mzero
		extractHole path (SNode _ (Tip kx x)) = return (x, Hole kx path)
		extractHole path (SNode _ (Bin p m l r)) =
			extractHole (LeftBin p m path r) l `mplus`
				extractHole (RightBin p m l path) r
	clearM (Hole _ path) = WordMap (assign nil path)
	assignM v (Hole kx path) = WordMap (assign (singleton kx v) path)

	{-# INLINE unifyM #-}
	unifyM k1 a1 k2 a2 = WordMap <$> unify k1 a1 k2 a2

search :: Key -> Path a -> SNode a -> (# Maybe a, Path a #)
search !k path n@(SNode _ (Bin p m l r))
	| nomatch k p m	= (# Nothing, branchHole k p path n #)
	| zero k m
		= search k (LeftBin p m path r) l
	| otherwise
		= search k (RightBin p m l path) r
search !k path n@(SNode _ (Tip ky y))
	| k == ky	= (# Just y, path #)
	| otherwise	= (# Nothing, branchHole k ky path n #)
search _ path _ = (# Nothing, path #)

before, after :: SNode a -> Path a -> SNode a
before !t Root = t
before !t (LeftBin _ _ path _) = before t path
before !t (RightBin p m l path) = before (bin p m l t) path
after !t Root = t
after !t (RightBin _ _ _ path) = after t path
after !t (LeftBin p m path r) = after (bin p m t r) path

assign :: Sized a => SNode a -> Path a -> SNode a
assign (SNode _ Nil) path = case path of
  Root			-> nil
  LeftBin _ _ path r	-> assign' r path
  RightBin _ _ l path	-> assign' l path
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

shiftRL :: Nat -> Key -> Nat
-- #if __GLASGOW_HASKELL__
{--------------------------------------------------------------------
  GHC: use unboxing to get @shiftRL@ inlined.
--------------------------------------------------------------------}
-- shiftRL (W# x) (I# i)
--   = W# (shiftRL# x i)
-- #else
shiftRL x i   = shiftR x (fromIntegral i)
-- #endif

lookup :: Key -> Node a -> Maybe a
lookup !k (Bin _ m l r) = lookup k (sContents (if zeroN k m then l else r))
lookup k (Tip kx x)
	| k == kx	= Just x
lookup _ _ = Nothing

insertWith :: Sized a => (a -> a -> a) -> Key -> a -> SNode a -> SNode a
insertWith f !k a = ins where
  !tip = singleton k a
  ins n@(SNode _ t) = case t of
    Nil	-> tip
    Tip kx x
      | k == kx		-> singleton k (f a x)
      | otherwise	-> join' kx n k tip
    Bin p m l r
      | nomatch k p m	-> join' p n k tip
      | zero k m	-> bin' p m (ins l) r
      | otherwise	-> bin' p m l (ins r)

singleton :: Sized a => Key -> a -> SNode a
singleton k a = sNode (Tip k a)

singletonMaybe :: Sized a => Key -> Maybe a -> SNode a
singletonMaybe k = maybe nil (singleton k)

traverse :: (Applicative f, Sized b) => (a -> f b) -> SNode a -> f (SNode b)
traverse f (SNode _ t) = case t of
	Nil		-> pure nil
	Tip kx x	-> singleton kx <$> f x
	Bin p m l r	-> bin' p m <$> traverse f l <*> traverse f r

foldr :: (a -> b -> b) -> SNode a -> b -> b
foldr f (SNode _ t)
  = case t of
      Bin _ _ l r -> foldr f l . foldr f r
      Tip _ x     -> f x
      Nil         -> id

foldl :: (b -> a -> b) -> SNode a -> b -> b
foldl f (SNode _ t)
  = case t of
      Bin _ _ l r -> foldl f r . foldl f l
      Tip _ x     -> flip f x
      Nil         -> id

mapWithKey :: Sized b => (a -> b) -> SNode a -> SNode b
mapWithKey f (SNode _ (Bin p m l r))	= bin' p m (mapWithKey f l) (mapWithKey f r)
mapWithKey f (SNode _ (Tip kx x))	= singleton kx (f x)
mapWithKey _ _				= nil

mapMaybe :: Sized b => (a -> Maybe b) -> SNode a -> SNode b
mapMaybe f (SNode _ (Bin p m l r))	= bin p m (mapMaybe f l) (mapMaybe f r)
mapMaybe f (SNode _ (Tip kx x))		= singletonMaybe  kx (f x)
mapMaybe _ _				= nil

mapEither :: (Sized b, Sized c) => (a -> (# Maybe b, Maybe c #)) -> 
	SNode a -> (# SNode b, SNode c #)
mapEither f (SNode _ (Bin p m l r)) = both (bin p m lL) (bin p m lR) (mapEither f) r
	where	!(# lL, lR #) = mapEither f l
mapEither f (SNode _ (Tip kx x))	= both (singletonMaybe kx) (singletonMaybe kx) f x
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
    | otherwise      -> join' p1 n1 p2 n2
    where
      union1  | nomatch p2 p1 m1  = join' p1 n1 p2 n2
	      | zero p2 m1        = bin p1 m1 (unionWith f l1 n2) r1
	      | otherwise         = bin p1 m1 l1 (unionWith f r1 n2)

      union2  | nomatch p1 p2 m2  = join' p1 n1 p2 n2
	      | zero p1 m2        = bin p2 m2 (unionWith f n1 l2) r2
	      | otherwise         = bin p2 m2 l2 (unionWith f n1 r2)

{-# INLINE alter #-}
alter :: Sized a => (Maybe a -> Maybe a) -> Key -> SNode a -> SNode a
alter f k n = case searchM k (WordMap n) of
  (# Nothing, hole #) -> case f Nothing of
	Nothing	-> n
	Just a	-> getWordMap (assignM a hole)
  (# a, hole #) -> getWordMap (fillHoleM (f a) hole)

intersectionWith :: Sized c => (a -> b -> Maybe c) -> SNode a -> SNode b -> SNode c
intersectionWith f n1@(SNode _ t1) n2@(SNode _ t2) = case (t1, t2) of
  (Nil, _)	-> nil
  (_, Nil)	-> nil
  (Tip k x, _)	-> singletonMaybe k (lookup k t2 >>= f x)
  (_, Tip k y)	-> singletonMaybe k (lookup k t1 >>= flip f y)
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
  (Tip k x, _)	-> maybe n1 (singletonMaybe k . f x) (lookup k t2)
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
isSubmapOfBy (<=) n1@(SNode _ t1) (SNode _ t2) = case (t1, t2) of
  (Bin p1 m1 l1 r1, Bin p2 m2 l2 r2)
    | shorter m1 m2  -> False
    | shorter m2 m1  -> match p1 p2 m2 && (if zero p1 m2 then isSubmapOfBy (<=) n1 l2
							else isSubmapOfBy (<=) n1 r2)                     
    | otherwise      -> (p1==p2) && isSubmapOfBy (<=) l1 l2 && isSubmapOfBy (<=) r1 r2
  (Bin{}, _)	-> False
  (Tip k x, t)	-> maybe False (x <=) (lookup k t)
  (Nil, _)	-> True

mask :: Key -> Mask -> Prefix
mask i m
  = maskW i m

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

maskW :: Nat -> Nat -> Prefix
maskW i m
  = i .&. (complement (m-1) `xor` m)

shorter :: Mask -> Mask -> Bool
shorter m1 m2
  = m1 > m2

branchMask :: Prefix -> Prefix -> Mask
branchMask p1 p2
  = highestBitMask (p1 `xor` p2)

highestBitMask :: Nat -> Nat
highestBitMask x0
  = case (x0 .|. shiftRL x0 1) of
     x1 -> case (x1 .|. shiftRL x1 2) of
      x2 -> case (x2 .|. shiftRL x2 4) of
       x3 -> case (x3 .|. shiftRL x3 8) of
        x4 -> case (x4 .|. shiftRL x4 16) of
#if WORD_SIZE_IN_BITS > 32
         x5 -> case (x5 .|. shiftRL x5 32) of   -- for 64 bit platforms
          x6 -> (x6 `xor` (shiftRL x6 1))
#else
	 x5 -> x5 `xor` shiftRL x5 1
#endif

{-# INLINE join' #-}
join' :: Sized a => Prefix -> SNode a -> Prefix -> SNode a -> SNode a
join' p1 t1 p2 t2
  | zero p1 m = bin' p m t1 t2
  | otherwise = bin' p m t2 t1
  where
    m = branchMask p1 p2
    p = mask p1 m

{-# INLINE join #-}
join :: Prefix -> SNode a -> Prefix -> SNode a -> SNode a
join p1 t1 p2 t2
  | zero p1 m = bin p m t1 t2
  | otherwise = bin p m t2 t1
  where
    m = branchMask p1 p2
    p = mask p1 m

nil :: SNode a
nil = SNode 0# Nil

bin :: Prefix -> Mask -> SNode a -> SNode a -> SNode a
bin _ _ l r
  | l `seq` r `seq` False	= undefined
bin _ _ (SNode _ Nil) r = r
bin _ _ l (SNode _ Nil) = l
bin p m l@(SNode sl# _) r@(SNode sr# _)
			= SNode (sl# +# sr#) (Bin p m l r)

bin' :: Sized a => Prefix -> Mask -> SNode a -> SNode a -> SNode a
bin' p m l r = sNode (Bin p m l r)

{-# INLINE unify #-}
unify :: Sized a => Key -> a -> Key -> a -> Maybe (SNode a)
unify k1 _ k2 _
    | k1 == k2	= Nothing
unify k1 a1 k2 a2 = Just (join k1 (singleton k1 a1) k2 (singleton k2 a2))