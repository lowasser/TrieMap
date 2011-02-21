{-# LANGUAGE UnboxedTuples, BangPatterns, TypeFamilies, PatternGuards, MagicHash, CPP, NamedFieldPuns, FlexibleInstances, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, ImplicitParams, TemplateHaskell, TypeOperators #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.TrieMap.WordMap (SNode, WHole, TrieMap(WordMap), Hole(Hole), getWordMap, getHole) where

import Data.TrieMap.TrieKey
import Data.TrieMap.Sized
import Data.TrieMap.Utils

import Control.Exception (assert)
import Control.Monad.Lookup
import Control.Monad.Unpack

import Data.Bits
import Data.Maybe hiding (mapMaybe)

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
{-# ANN type WHole ForceSpecConstr #-}

$(noUnpackInstance ''Path)
$(noUnpackInstance ''Node)
$(unpackInstance ''WHole)
$(unpackInstance ''SNode)

{-# INLINE hole #-}
hole :: Key -> Path a -> Hole Word a
hole k path = Hole (WHole k path)

#define HOLE(args) (Hole (WHole args))

instance Subset (TrieMap Word) where
  WordMap m1 <=? WordMap m2 = m1 <=? m2

instance Functor (TrieMap Word) where
  fmap f (WordMap m) = WordMap (f <$> m)

instance Foldable (TrieMap Word) where
  foldMap f (WordMap m) = foldMap f m
  foldr f z (WordMap m) = foldr f z m
  foldl f z (WordMap m) = foldl f z m
  foldr1 f (WordMap m) = foldr1 f m
  foldl1 f (WordMap m) = foldl1 f m

instance Traversable (TrieMap Word) where
  traverse f (WordMap m) = WordMap <$> traverse f m

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
	lookupMC k (WordMap m) = lookupC k m
	mapMaybeM f (WordMap m) = WordMap (mapMaybe f m)
	mapEitherM f (WordMap m) = both WordMap WordMap (mapEither f) m
	unionM f (WordMap m1) (WordMap m2) = WordMap (unionWith f m1 m2)
	isectM f (WordMap m1) (WordMap m2) = WordMap (intersectionWith f m1 m2)
	diffM f (WordMap m1) (WordMap m2) = WordMap (differenceWith f m1 m2)
	
	singleHoleM k = hole k Root
	beforeM HOLE(_ path) = WordMap (before path)
	beforeWithM a HOLE(k path) = WordMap (beforeWith (singleton k a) path)
	afterM HOLE(_ path) = WordMap (after path)
	afterWithM a HOLE(k path) = WordMap (afterWith (singleton k a) path)

	{-# INLINE searchMC #-}
	searchMC !k (WordMap t) notfound found = searchC k t (unpack (notfound .  Hole)) (\ a -> unpack (found a . Hole))
	{-# INLINE indexMC #-}
	indexMC i (WordMap m) result = index i m (\ i# x -> unpack (result i# x . Hole))
	extractHoleM (WordMap m) = extractHole Root m where
		extractHole _ (SNode _ Nil) = mzero
		extractHole path TIP(kx x) = return (x, hole kx path)
		extractHole path BIN(p m l r) =
			extractHole (LeftBin p m path r) l `mplus`
				extractHole (RightBin p m l path) r
	clearM HOLE(_ path) = WordMap (clear path)
	{-# INLINE assignM #-}
	assignM v HOLE(kx path) = WordMap (assign (singleton kx v) path)

	{-# INLINE unifyM #-}
	unifyM k1 a1 k2 a2 = WordMap <$> unify k1 a1 k2 a2

	{-# INLINE unifierM #-}
	unifierM k' k a = Hole <$> unifier k' k a
	
	{-# INLINE fromAscListFold #-}
	fromAscListFold f = WordMap <$> fromAscList f
	
	{-# INLINE insertWithM #-}
	insertWithM f k a (WordMap m) = case insertWithC f k a m of
	  (# sz#, node #) -> WordMap (SNode{sz = I# sz#, node})

insertWithC :: Sized a => (a -> a) -> Key -> a -> SNode a -> (# Int#, Node a #)
insertWithC f !k a !t = ins t (#, #) where
  {-# INLINE tip #-}
  tip = singleton k a
  {-# INLINE ins' #-}
  ins' !t cont = ins t $ \ sz# node -> cont SNode{sz = I# sz#, node}
  result ret SNode{sz = I# sz#, node}  = ret sz# node
  ins !t ret = case t of
    BIN(p m l r)
      | nomatch k p m	-> result ret (join k tip p t)
      | mask0 k m	-> ins' l $ \ l' -> result ret (bin' p m l' r)
      | otherwise	-> ins' r $ \ r' -> result ret (bin' p m l r')
    TIP(kx x)
      | k == kx		-> result ret (singleton kx (f x))
      | otherwise	-> result ret (join k tip kx t)
    NIL			-> result ret tip

index :: Int# -> SNode a -> (Int# -> a -> WHole a :~> r) -> r
index i !t result = indexT i t Root where
  indexT i# TIP(kx x) path = result i# x $~ WHole kx path
  indexT i# BIN(p m l r) path
	  | i# <# sl#	= indexT i# l (LeftBin p m path r)
	  | otherwise	= indexT (i# -# sl#) r (RightBin p m l path)
	  where !sl# = getSize# l
  indexT _ NIL _		= indexFail

searchC :: Key -> SNode a -> (WHole a :~> r) -> (a -> WHole a :~> r) -> r
searchC !k t notfound found = seek Root t where
  seek path t@BIN(p m l r)
    | nomatch k p m	= notfound $~ WHole k (branchHole k p path t)
    | mask0 k m
	    = seek (LeftBin p m path r) l
    | otherwise
	    = seek (RightBin p m l path) r
  seek path t@TIP(ky y)
    | k == ky	= found y $~ WHole k path
    | otherwise	= notfound $~ WHole k (branchHole k ky path t)
  seek path NIL = notfound $~ WHole k path

before, after :: Path a -> SNode a
beforeWith, afterWith :: SNode a -> Path a -> SNode a

before Root			= nil
before (LeftBin _ _ path _)	= before path
before (RightBin _ _ l path)	= beforeWith l path

beforeWith !t Root			= t
beforeWith !t (LeftBin _ _ path _)	= beforeWith t path
beforeWith !t (RightBin p m l path)	= beforeWith (bin' p m l t) path

after Root			= nil
after (RightBin _ _ _ path)	= after path
after (LeftBin _ _ path r)	= afterWith r path

afterWith !t Root			= t
afterWith !t (RightBin _ _ _ path)	= afterWith t path
afterWith !t (LeftBin p m path r)	= afterWith (bin' p m t r) path

clear :: Path a -> SNode a
assign :: SNode a -> Path a -> SNode a
clear Root = nil
clear (LeftBin _ _ path r) = assign r path
clear (RightBin _ _ l path) = assign l path

assign !t Root = t
assign !t (LeftBin p m path r) = assign (bin' p m t r) path
assign !t (RightBin p m l path) = assign (bin' p m l t) path

branchHole :: Key -> Prefix -> Path a -> SNode a -> Path a
branchHole !k !p path t
  | mask0 k m	= LeftBin p' m path t
  | otherwise	= RightBin p' m t path
  where	m = branchMask k p
  	p' = mask k m

{-# INLINE lookupC #-}
lookupC :: Key -> SNode a -> Lookup r a
lookupC !k !t = Lookup $ \ no yes -> let
  look BIN(_ m l r) = if zeroN k m then look l else look r
  look TIP(kx x)
      | k == kx = yes x
  look _ = no
  in look t

singleton :: Sized a => Key -> a -> SNode a
singleton k a = sNode (Tip k a)

singletonMaybe :: Sized a => Key -> Maybe a -> SNode a
singletonMaybe k = maybe nil (singleton k)

instance Functor SNode where
  fmap f = map where
    map SNode{sz, node} = SNode sz $ case node of
      Nil		-> Nil
      Tip k x		-> Tip k (f x)
      Bin p m l r	-> Bin p m (map l) (map r)

instance Foldable SNode where
  foldMap f = fold where
    fold NIL = mempty
    fold TIP(_ x) = f x
    fold BIN(_ _ l r) = fold l `mappend` fold r
  
  foldr f = flip fold where
    fold BIN(_ _ l r) z = fold l (fold r z)
    fold TIP(_ x) z = f x z
    fold NIL z = z
  
  foldl f = fold where
    fold z BIN(_ _ l r) = fold (fold z l) r
    fold z TIP(_ x) = f z x
    fold z NIL = z

instance Traversable SNode where
  traverse f = trav where
    trav NIL	= pure nil
    trav SNode{sz, node = Tip kx x}
    		= SNode sz . Tip kx <$> f x
    trav SNode{sz, node = Bin p m l r}
		= SNode sz .: Bin p m <$> trav l <*> trav r

mapMaybe :: Sized b => (a -> Maybe b) -> SNode a -> SNode b
mapMaybe f !t = mMaybe t where
  mMaybe BIN(p m l r)	= bin p m (mMaybe l) (mMaybe r)
  mMaybe TIP(kx x)	= singletonMaybe kx (f x)
  mMaybe _		= nil

mapEither :: (Sized b, Sized c) => (a -> (# Maybe b, Maybe c #)) -> 
	SNode a -> (# SNode b, SNode c #)
mapEither f BIN(p m l r) = both (bin p m lL) (bin p m lR) (mapEither f) r
	where !(# lL, lR #) = mapEither f l
mapEither f TIP(kx x)	= both (singletonMaybe kx) (singletonMaybe kx) f x
mapEither _ _		= (# nil, nil #)

unionWith :: Sized a => (a -> a -> Maybe a) -> SNode a -> SNode a -> SNode a
unionWith f = union where
  n1@(SNode _ t1) `union` n2@(SNode _ t2) = case (t1, t2) of
    (Nil, _)	-> n2
    (_, Nil)	-> n1
    (Tip k x, _)	-> alter (maybe (Just x) (f x)) k n2
    (_, Tip k x)	-> alter (maybe (Just x) (`f` x)) k n1
    (Bin p1 m1 l1 r1, Bin p2 m2 l2 r2)
      | shorter m1 m2  -> union1
      | shorter m2 m1  -> union2
      | p1 == p2       -> bin p1 m1 (l1 `union` l2) (r1 `union` r2)
      | otherwise      -> join p1 n1 p2 n2
      where
	union1  | nomatch p2 p1 m1  = join p1 n1 p2 n2
		| mask0 p2 m1       = bin p1 m1 (l1 `union` n2) r1
		| otherwise         = bin p1 m1 l1 (r1 `union` n2)

	union2  | nomatch p1 p2 m2  = join p1 n1 p2 n2
		| mask0 p1 m2       = bin p2 m2 (n1 `union` l2) r2
		| otherwise         = bin p2 m2 l2 (n1 `union` r2)

{-# INLINE alter #-}
alter :: Sized a => (Maybe a -> Maybe a) -> Key -> SNode a -> SNode a
alter f k t = getWordMap $ alterM f k (WordMap t)

intersectionWith :: Sized c => (a -> b -> Maybe c) -> SNode a -> SNode b -> SNode c
intersectionWith f = isect where
  n1@(SNode _ t1) `isect` n2@(SNode _ t2) = case (t1, t2) of
    (Nil, _)	-> nil
    (Tip{}, Nil)	-> nil
    (Bin{}, Nil)	-> nil
    (Tip k x, _)	-> runLookup (lookupC k n2) nil (singletonMaybe k . f x)
    (_, Tip k y)	-> runLookup (lookupC k n1) nil (singletonMaybe k . flip f y)
    (Bin p1 m1 l1 r1, Bin p2 m2 l2 r2)
      | shorter m1 m2  -> intersection1
      | shorter m2 m1  -> intersection2
      | p1 == p2       -> bin p1 m1 (l1 `isect` l2) (r1 `isect` r2)
      | otherwise      -> nil
      where
	intersection1 | nomatch p2 p1 m1  = nil
		      | mask0 p2 m1       = l1 `isect` n2
		      | otherwise         = r1 `isect` n2

	intersection2 | nomatch p1 p2 m2  = nil
		      | mask0 p1 m2       = n1 `isect` l2
		      | otherwise         = n1 `isect` r2

differenceWith :: Sized a => (a -> b -> Maybe a) -> SNode a -> SNode b -> SNode a
differenceWith f = diff where
  n1@(SNode _ t1) `diff` n2@(SNode _ t2) = case (t1, t2) of
    (Nil, _)	-> nil
    (_, Nil)	-> n1
    (Tip k x, _)	-> runLookup (lookupC k n2) n1 (singletonMaybe k . f x)
    (_, Tip k y)	-> alter (>>= flip f y) k n1
    (Bin p1 m1 l1 r1, Bin p2 m2 l2 r2)
      | shorter m1 m2  -> difference1
      | shorter m2 m1  -> difference2
      | p1 == p2       -> bin p1 m1 (l1 `diff` l2) (r1 `diff` r2)
      | otherwise      -> n1
      where
	difference1 | nomatch p2 p1 m1  = n1
		    | mask0 p2 m1       = bin p1 m1 (l1 `diff` n2) r1
		    | otherwise         = bin p1 m1 l1 (r1 `diff` n2)

	difference2 | nomatch p1 p2 m2  = n1
		    | mask0 p1 m2       = n1 `diff` l2
		    | otherwise         = n1 `diff` r2

instance Subset SNode where
  (<=?) = subMap where
    t1@BIN(p1 m1 l1 r1) `subMap` BIN(p2 m2 l2 r2)
      | shorter m1 m2 	= False
      | shorter m2 m1	= match p1 p2 m2 && (if mask0 p1 m2 then t1 `subMap` l2
							  else t1 `subMap` r2)
      | otherwise	= (p1==p2) && l1 `subMap` l2 && r1 `subMap` r2
    BIN({}) `subMap` _		= False
    TIP(k x) `subMap` t2	= runLookup (lookupC k t2) False (?le x)
    NIL `subMap` _		= True

mask0 :: Key -> Mask -> Bool
mask0 i m
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
  = i .&. compl ((m-1) `xor` m)

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
  | mask0 p1 m = SNode{sz = sz', node = Bin p m t1 t2}
  | otherwise = SNode{sz = sz', node = Bin p m t2 t1}
  where
    m = branchMask p1 p2
    p = mask p1 m
    sz' = sz t1 + sz t2

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

{-# INLINE unify #-}
unify :: Sized a => Key -> a -> Key -> a -> Maybe (SNode a)
unify k1 a1 k2 a2
  | k1 == k2	= Nothing
  | otherwise	= Just (join k1 (singleton k1 a1) k2 (singleton k2 a2))

{-# INLINE unifier #-}
unifier :: Sized a => Key -> Key -> a -> Maybe (WHole a)
unifier k' k a
    | k' == k	= Nothing
    | otherwise	= Just (WHole k' $ branchHole k' k Root (singleton k a))

{-# INLINE fromAscList #-}
fromAscList :: forall a . Sized a => (a -> a -> a) -> Foldl Key a (SNode a)
fromAscList f = Foldl{zero = nil, ..} where
  begin kx vx = Stack' kx vx Nada

  snoc (Stack' kx vx stk) kz vz
    | kx == kz	= Stack' kx (f vz vx) stk
    | otherwise	= Stack' kz vz $ reduce (branchMask kx kz) kx (singleton kx vx) stk
  
  reduce :: Mask -> Prefix -> SNode a -> Stack a -> Stack a
  reduce !m !px !tx (Push py ty stk')
    | shorter m mxy	= reduce m pxy (bin' pxy mxy ty tx) stk'
    where mxy = branchMask px py; pxy = mask px mxy
  reduce _ px tx stk	= Push px tx stk

  done (Stack' kx vx stk) = finish kx (singleton kx vx) stk
  
  finish !px !tx (Push py ty stk) = finish p (join py ty px tx) stk
    where m = branchMask px py; p = mask px m
  finish _ t Nada = t

data Stack' a = Stack' !Key a (Stack a)
data Stack a = Push !Prefix !(SNode a) !(Stack a) | Nada