{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, CPP, BangPatterns, TypeSynonymInstances #-}
module Data.TrieMap.OrdMap.Searchable () where

import Control.Monad.Option

import Data.TrieMap.OrdMap.Base
import Data.TrieMap.OrdMap.Zippable ()

import Prelude hiding (lookup)

#define TIP SNode{node=Tip}
#define BIN(args) SNode{node=Bin args}

instance Ord k => Searchable (OrdMap k) (Ordered k) where
  search (Ord k) (OrdMap t) = mapHole Hole (search k t)
  singleZip (Ord k) = Hole (singleZip k)
  singleton (Ord k) a = OrdMap (single k a)
  insertWith f (Ord k) a (OrdMap m) = OrdMap (insertWith f k a m)
  lookup (Ord k) (OrdMap m) = lookup k m

instance Ord k => Searchable (SNode k) k where
  search k t nomatch match = searcher Root t where
    searcher path TIP = nomatch (Empty k path)
    searcher path BIN(kx x l r) = case compare k kx of
	LT	-> searcher (LeftBin kx x path r) l
	EQ	-> match x (Full k path l r)
	GT	-> searcher (RightBin kx x l path) r

  singleZip k = Empty k Root
  singleton = single
  
  insertWith f k a = k `seq` ins where
    ins BIN(kx x l r) = case compare k kx of
      EQ -> bin kx (f x) l r
      LT -> balance kx x (ins l) r
      GT -> balance kx x l (ins r)
    ins TIP = singleton k a
    
  lookup k !t = option $ \ no yes -> let
    look BIN(kx x l r) = case compare k kx of
	  LT	-> look l
	  EQ	-> yes x
	  GT	-> look r
    look _ = no
    in look t