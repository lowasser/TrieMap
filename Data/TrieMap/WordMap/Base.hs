{-# LANGUAGE TypeFamilies, TemplateHaskell, NamedFieldPuns, MagicHash, BangPatterns, FlexibleInstances, CPP #-}
{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.TrieMap.WordMap.Base where

import Control.Exception (assert)
import Control.Monad.Unpack
import Control.Monad.Trans.Reader

import Data.Word
import Data.Bits
import Data.Functor.Immoral

import Data.TrieMap.TrieKey

import GHC.Exts

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

nil :: SNode a
nil = SNode 0 Nil

single :: Sized a => Key -> a -> SNode a
single k a = sNode (Tip k a)

singletonMaybe :: Sized a => Key -> Maybe a -> SNode a
singletonMaybe k = maybe nil (single k)

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

$(noUnpackInstance ''Path)
$(noUnpackInstance ''Node)
$(unpackInstance ''SNode)

deriving instance ImmoralMap (SNode a) (TrieMap Word a)
deriving instance ImmoralMap (Zipper SNode a) (Hole Word a)

instance Unpackable (Zipper SNode a) where
  newtype UnpackedReaderT (Zipper SNode a) m r =
    ZRT {runZRT :: UnpackedReaderT Word (ReaderT (Path a) m) r}
  runUnpackedReaderT func (WHole k path) =
    runZRT func `runUnpackedReaderT` k `runReaderT` path
  unpackedReaderT func = ZRT $ unpackedReaderT $ \ k -> ReaderT $ \ path -> func (WHole k path)

newtype instance TrieMap Word a = WordMap {getWordMap :: SNode a}
type WHole = Zipper SNode
data instance Zipper SNode a = WHole !Key (Path a)
newtype instance Zipper (TrieMap Word) a = Hole {getHole :: Zipper SNode a}

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

bin :: Prefix -> Mask -> SNode a -> SNode a -> SNode a
bin p m l@(SNode sl tl) r@(SNode sr tr) = case (tl, tr) of
  (Nil, _)	-> r
  (_, Nil)	-> l
  _		-> SNode (sl + sr) (Bin p m l r)

#define NIL SNode{node = Nil}

bin' :: Prefix -> Mask -> SNode a -> SNode a -> SNode a
bin' p m l@SNode{sz=sl} r@SNode{sz=sr} = assert (nonempty l && nonempty r) $ SNode (sl + sr) (Bin p m l r)
  where	nonempty NIL = False
  	nonempty _ = True

branchHole :: Key -> Prefix -> Path a -> SNode a -> Path a
branchHole !k !p path t
  | mask0 k m	= LeftBin p' m path t
  | otherwise	= RightBin p' m t path
  where	m = branchMask k p
  	p' = mask k m
