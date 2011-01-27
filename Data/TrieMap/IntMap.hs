{-# LANGUAGE UnboxedTuples, BangPatterns, TypeFamilies, PatternGuards, MagicHash, CPP #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.TrieMap.IntMap () where

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
	| LeftBin !Prefix !Mask !(Path a) !(TrieMap Word a)
	| RightBin !Prefix !Mask !(TrieMap Word a) !(Path a)

-- | @'TrieMap' 'Word' a@ is based on "Data.IntMap".
instance TrieKey Word where
	(=?) = (==)
	cmp = compare

	data TrieMap Word a = Nil
              | Tip !Size !Key a
              | Bin !Size !Prefix !Mask !(TrieMap Word a) !(TrieMap Word a)
        data Hole Word a = Hole !Key !(Path a)
	emptyM = Nil
	singletonM = singleton
	getSimpleM Nil		= Null
	getSimpleM (Tip _ _ a)	= Singleton a
	getSimpleM _		= NonSimple
	sizeM = size
	lookupM = lookup
	traverseM = traverse
	foldrM = foldr
	foldlM = foldl
	fmapM = mapWithKey
	mapMaybeM = mapMaybe
	mapEitherM = mapEither
	unionM = unionWith
	isectM = intersectionWith
	diffM = differenceWith
	isSubmapM = isSubmapOfBy
	
	singleHoleM k = Hole k Root
	beforeM (Hole _ path) = before Nil path
	beforeWithM a (Hole k path) = before (singleton k a) path
	afterM (Hole _ path) = after Nil path
	afterWithM a (Hole k path) = after (singleton k a) path
	searchM !k = onSnd (Hole k) (search Root) where
		search path t@(Bin _ p m l r)
			| nomatch k p m	= (# Nothing, branchHole k p path t #)
			| zero k m
				= search (LeftBin p m path r) l
			| otherwise
				= search (RightBin p m l path) r
		search path t@(Tip _ ky y)
			| k == ky	= (# Just y, path #)
			| otherwise	= (# Nothing, branchHole k ky path t #)
		search path _ = (# Nothing, path #)
	indexM i# t = indexT i# t Root where
		indexT _ Nil _ = indexFail ()
		indexT i# (Tip _ kx x) path = (# i#, x, Hole kx path #)
		indexT i# (Bin _ p m l r) path
			| i# <# sl#	= indexT i# l (LeftBin p m path r)
			| otherwise	= indexT (i# -# sl#) r (RightBin p m l path)
			where !sl# = size l
	extractHoleM = extractHole Root where
		extractHole _ Nil = mzero
		extractHole path (Tip _ kx x) = return (x, Hole kx path)
		extractHole path (Bin _ p m l r) =
			extractHole (LeftBin p m path r) l `mplus`
				extractHole (RightBin p m l path) r
	clearM (Hole _ path) = assign Nil path
	assignM v (Hole kx path) = assign (singletonM kx v) path where
		
	
	{-# INLINE unifyM #-}
	unifyM = unify

before, after :: TrieMap Word a -> Path a -> TrieMap Word a
before t Root = t
before t (LeftBin _ _ path _) = before t path
before t (RightBin p m l path) = before (bin p m l t) path
after t Root = t
after t (RightBin _ _ _ path) = after t path
after t (LeftBin p m path r) = after (bin p m t r) path

assign :: TrieMap Word a -> Path a -> TrieMap Word a
assign t Root = t
assign t (LeftBin p m path r) = assign (bin p m t r) path
assign t (RightBin p m l path) = assign (bin p m l t) path

branchHole :: Key -> Prefix -> Path a -> TrieMap Word a -> Path a
branchHole !k !p path t
  | zero k m	= LeftBin p' m path t
  | otherwise	= RightBin p' m t path
  where	m = branchMask k p
  	p' = mask k m

natFromInt :: Word -> Nat
natFromInt = id

intFromNat :: Nat -> Word
intFromNat = id

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

size :: TrieMap Word a -> Int#
size Nil = 0#
size (Tip sz _ _) = sz
size (Bin sz _ _ _ _) = sz

lookup :: Nat -> TrieMap Word a -> Maybe a
lookup !k (Bin _ _ m l r) = lookup k (if zeroN k m then l else r)
lookup k (Tip _ kx x)
	| k == kx	= Just x
lookup _ _ = Nothing

singleton :: Sized a => Key -> a -> TrieMap Word a
singleton k a = Tip (getSize# a) k a

singletonMaybe :: Sized a => Key -> Maybe a -> TrieMap Word a
singletonMaybe k = maybe Nil (singleton k)

traverse :: (Applicative f, Sized b) => (a -> f b) -> TrieMap Word a -> f (TrieMap Word b)
traverse f t = case t of
	Nil		-> pure Nil
	Tip _ kx x	-> singleton kx <$> f x
	Bin _ p m l r	-> bin p m <$> traverse f l <*> traverse f r

foldr :: (a -> b -> b) -> TrieMap Word a -> b -> b
foldr f t
  = case t of
      Bin _ _ _ l r -> foldr f l . foldr f r
      Tip _ _ x     -> f x
      Nil         -> id

foldl :: (b -> a -> b) -> TrieMap Word a -> b -> b
foldl f t
  = case t of
      Bin _ _ _ l r -> foldl f r . foldl f l
      Tip _ _ x     -> flip f x
      Nil         -> id

mapWithKey :: Sized b => (a -> b) -> TrieMap Word a -> TrieMap Word b
mapWithKey f (Bin _ p m l r)	= bin p m (mapWithKey f l) (mapWithKey f r)
mapWithKey f (Tip _ kx x)	= singleton kx (f x)
mapWithKey _ _			= Nil

mapMaybe :: Sized b => (a -> Maybe b) -> TrieMap Word a -> TrieMap Word b
mapMaybe f (Bin _ p m l r)	= bin p m (mapMaybe f l) (mapMaybe f r)
mapMaybe f (Tip _ kx x)		= singletonMaybe  kx (f x)
mapMaybe _ _			= Nil

mapEither :: (Sized b, Sized c) => (a -> (# Maybe b, Maybe c #)) -> 
	TrieMap Word a -> (# TrieMap Word b, TrieMap Word c #)
mapEither f (Bin _ p m l r) = both (bin p m lL) (bin p m lR) (mapEither f) r
	where	!(# lL, lR #) = mapEither f l
mapEither f (Tip _ kx x)	= both (singletonMaybe kx) (singletonMaybe kx) f x
mapEither _ _			= (# Nil, Nil #)

unionWith :: Sized a => (a -> a -> Maybe a) -> TrieMap Word a -> TrieMap Word a -> TrieMap Word a
unionWith _ Nil t  = t
unionWith _ t Nil  = t
unionWith f (Tip _ k x) t = alterM (maybe (Just x) (f x)) k t
unionWith f t (Tip _ k x) = alterM (maybe (Just x) (flip f x)) k t
unionWith f t1@(Bin _ p1 m1 l1 r1) t2@(Bin _ p2 m2 l2 r2)
  | shorter m1 m2  = union1
  | shorter m2 m1  = union2
  | p1 == p2       = bin p1 m1 (unionWith f l1 l2) (unionWith f r1 r2)
  | otherwise      = join p1 t1 p2 t2
  where
    union1  | nomatch p2 p1 m1  = join p1 t1 p2 t2
            | zero p2 m1        = bin p1 m1 (unionWith f l1 t2) r1
            | otherwise         = bin p1 m1 l1 (unionWith f r1 t2)

    union2  | nomatch p1 p2 m2  = join p1 t1 p2 t2
            | zero p1 m2        = bin p2 m2 (unionWith f t1 l2) r2
            | otherwise         = bin p2 m2 l2 (unionWith f t1 r2)

intersectionWith :: Sized c => (a -> b -> Maybe c) -> TrieMap Word a -> TrieMap Word b -> TrieMap Word c
intersectionWith _ Nil _ = Nil
intersectionWith _ _ Nil = Nil
intersectionWith f (Tip _ k x) t2
  = singletonMaybe k (lookup (natFromInt k) t2 >>= f x)
intersectionWith f t1 (Tip _ k y) 
  = singletonMaybe k (lookup (natFromInt k) t1 >>= flip f y)
intersectionWith f t1@(Bin _ p1 m1 l1 r1) t2@(Bin _ p2 m2 l2 r2)
  | shorter m1 m2  = intersection1
  | shorter m2 m1  = intersection2
  | p1 == p2       = bin p1 m1 (intersectionWith f l1 l2) (intersectionWith f r1 r2)
  | otherwise      = Nil
  where
    intersection1 | nomatch p2 p1 m1  = Nil
                  | zero p2 m1        = intersectionWith f l1 t2
                  | otherwise         = intersectionWith f r1 t2

    intersection2 | nomatch p1 p2 m2  = Nil
                  | zero p1 m2        = intersectionWith f t1 l2
                  | otherwise         = intersectionWith f t1 r2

differenceWith :: Sized a => (a -> b -> Maybe a) -> TrieMap Word a -> TrieMap Word b -> TrieMap Word a
differenceWith _ Nil _       = Nil
differenceWith _ t Nil       = t
differenceWith f t1@(Tip _ k x) t2 
  = maybe t1 (singletonMaybe k . f x) (lookup (natFromInt k) t2)
differenceWith f t (Tip _ k y) = alterM  (>>= flip f y) k t
differenceWith f t1@(Bin _ p1 m1 l1 r1) t2@(Bin _ p2 m2 l2 r2)
  | shorter m1 m2  = difference1
  | shorter m2 m1  = difference2
  | p1 == p2       = bin p1 m1 (differenceWith f l1 l2) (differenceWith f r1 r2)
  | otherwise      = t1
  where
    difference1 | nomatch p2 p1 m1  = t1
                | zero p2 m1        = bin p1 m1 (differenceWith f l1 t2) r1
                | otherwise         = bin p1 m1 l1 (differenceWith f r1 t2)

    difference2 | nomatch p1 p2 m2  = t1
                | zero p1 m2        = differenceWith f t1 l2
                | otherwise         = differenceWith f t1 r2

isSubmapOfBy :: LEq a b -> LEq (TrieMap Word a) (TrieMap Word b)
isSubmapOfBy (<=) t1@(Bin _ p1 m1 l1 r1) (Bin _ p2 m2 l2 r2)
  | shorter m1 m2  = False
  | shorter m2 m1  = match p1 p2 m2 && (if zero p1 m2 then isSubmapOfBy (<=) t1 l2
                                                      else isSubmapOfBy (<=) t1 r2)                     
  | otherwise      = (p1==p2) && isSubmapOfBy (<=) l1 l2 && isSubmapOfBy (<=) r1 r2
isSubmapOfBy _		(Bin _ _ _ _ _) _
	= False
isSubmapOfBy (<=)	(Tip _ k x) t
	= maybe False (x <=) (lookup (natFromInt k) t)
isSubmapOfBy _		Nil _
	= True

mask :: Key -> Mask -> Prefix
mask i m
  = maskW (natFromInt i) (natFromInt m)

zero :: Key -> Mask -> Bool
zero i m
  = (natFromInt i) .&. (natFromInt m) == 0

nomatch,match :: Key -> Prefix -> Mask -> Bool
nomatch i p m
  = (mask i m) /= p

match i p m
  = (mask i m) == p

zeroN :: Nat -> Nat -> Bool
zeroN i m = (i .&. m) == 0

maskW :: Nat -> Nat -> Prefix
maskW i m
  = intFromNat (i .&. (complement (m-1) `xor` m))

shorter :: Mask -> Mask -> Bool
shorter m1 m2
  = (natFromInt m1) > (natFromInt m2)

branchMask :: Prefix -> Prefix -> Mask
branchMask p1 p2
  = intFromNat (highestBitMask (natFromInt p1 `xor` natFromInt p2))

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

{-# INLINE join #-}
join :: Prefix -> TrieMap Word a -> Prefix -> TrieMap Word a -> TrieMap Word a
join p1 t1 p2 t2
  | zero p1 m = bin p m t1 t2
  | otherwise = bin p m t2 t1
  where
    m = branchMask p1 p2
    p = mask p1 m

bin :: Prefix -> Mask -> TrieMap Word a -> TrieMap Word a -> TrieMap Word a
bin _ _ l Nil = l
bin _ _ Nil r = r
bin p m l r   = Bin (size l +# size r) p m l r

{-# INLINE unify #-}
unify :: Sized a => Key -> a -> Key -> a -> Unified Word a
unify k1 _ k2 _
    | k1 == k2	= Nothing
unify k1 a1 k2 a2 = Just (if zero k1 m then outBin t1 t2 else outBin t2 t1)
      where !s1# = getSize# a1
	    !s2# = getSize# a2
	    t1 = Tip s1# k1 a1
	    t2 = Tip s2# k2 a2
	    m = branchMask k1 k2
	    outBin = Bin (s1# +# s2#) (mask k1 m) m