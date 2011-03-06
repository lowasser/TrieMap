{-# LANGUAGE BangPatterns, TypeOperators, CPP, MultiParamTypeClasses, UnboxedTuples, MagicHash, NamedFieldPuns, FlexibleInstances #-}
module Data.TrieMap.WordMap.Searchable () where

import Control.Monad.Unpack
import Control.Monad.Option

import Data.TrieMap.TrieKey

import Data.Word

import Data.TrieMap.WordMap.Base
import Data.TrieMap.WordMap.Zippable ()

import Prelude hiding (lookup)
import GHC.Exts

#define BIN(args) SNode{node = (Bin args)}
#define TIP(args) SNode{node = (Tip args)}
#define NIL SNode{node = Nil}

instance Searchable (TrieMap Word) Word where
  search k (WordMap m) = mapHole Hole (search k m)
  singleZip = Hole . singleZip
  singleton = WordMap .: singleton
  lookup k (WordMap m) = lookup k m
  insertWith f k a (WordMap m) = WordMap (insertWith f k a m)

instance Searchable SNode Word where
  {-# INLINE search #-}
  search k t notfound found = searchC k t (unpack notfound) (unpack . found)
  
  singleZip k = WHole k Root
  singleton = single
  
  {-# INLINE lookup #-}
  lookup !k !t = option $ \ no yes -> let
    look BIN(_ m l r) = if zeroN k m then look l else look r
    look TIP(kx x)
	| k == kx = yes x
    look _ = no
    in look t
  
  {-# INLINE insertWith #-}
  insertWith f k a m = insertWithC f k (getSize a) a m

searchC :: Key -> SNode a -> (Zipper SNode a :~> r) -> (a -> Zipper SNode a :~> r) -> r
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

insertWithC :: Sized a => (a -> a) -> Key -> Int -> a -> SNode a -> SNode a
insertWithC f !k !szA a !t = ins' t where
  {-# INLINE tip #-}
  tip = SNode {sz = szA, node = Tip k a}
  
  {-# INLINE out #-}
  out SNode{sz = I# sz#, node} = (# sz#, node #)
  {-# INLINE ins' #-}
  ins' t = case ins t of
    (# sz#, node #) -> SNode{sz = I# sz#, node}
  ins !t = case t of
    BIN(p m l r)
      | nomatch k p m	-> out $ join k tip p t
      | mask0 k m	-> out $ bin' p m (ins' l) r
      | otherwise	-> out $ bin' p m l (ins' r)
    TIP(kx x)
      | k == kx		-> out $ singleton kx (f x)
      | otherwise	-> out $ join k tip kx t
    NIL			-> out tip