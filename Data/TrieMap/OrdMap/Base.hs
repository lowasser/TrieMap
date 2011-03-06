{-# LANGUAGE CPP, UnboxedTuples, MagicHash, NamedFieldPuns, BangPatterns, TypeFamilies, StandaloneDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.TrieMap.OrdMap.Base (
    module Data.TrieMap.TrieKey,
    module Data.TrieMap.OrdMap.Base,
    Ordered(..)) where

import Data.TrieMap.TrieKey
import Data.TrieMap.Modifiers

import Data.Functor.Immoral

#define DELTA 5
#define RATIO 2
#define TIP SNode{node=Tip}
#define BIN(args) SNode{node=Bin args}

data Path k a =
  Root
  | LeftBin k a !(Path k a) !(SNode k a)
  | RightBin k a !(SNode k a) !(Path k a)

data Node k a =
  Tip
  | Bin k a !(SNode k a) !(SNode k a)
data SNode k a = SNode{sz :: !Int, count :: !Int, node :: Node k a}

type OrdMap k = TrieMap (Ordered k)
type OHole k = Zipper (SNode k)

newtype instance TrieMap (Ordered k) a = OrdMap (SNode k a)
data instance Zipper (SNode k) a = Empty k !(Path k a)
  | Full k !(Path k a) !(SNode k a) !(SNode k a)
newtype instance Zipper (OrdMap k) a = Hole (OHole k a)

deriving instance ImmoralMap (SNode k a) (TrieMap (Ordered k) a)
deriving instance ImmoralMap (OHole k a) (Hole (Ordered k) a)

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

single :: Sized a => k -> a -> SNode k a
single k a = bin k a tip tip

joinMaybe :: Sized a => k -> Maybe a -> SNode k a -> SNode k a -> SNode k a
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
  insMax TIP	= single kx x
  insMax BIN(ky y l r)
		= balance ky y l (insMax r)
             
insertMin kx x = insMin where
  insMin TIP	= single kx x
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