{-# LANGUAGE CPP, NamedFieldPuns, FlexibleInstances #-}
module Data.TrieMap.WordMap.Traversable () where

import Control.Applicative

import Data.TrieMap.TrieKey

import Data.Functor.Immoral

import Data.Word

import Data.TrieMap.WordMap.Base

import Prelude hiding (foldr, foldl)

#define BIN(args) SNode{node = (Bin args)}
#define TIP(args) SNode{node = (Tip args)}
#define NIL SNode{node = Nil}

instance Functor (TrieMap Word) where
  fmap f (WordMap m) = WordMap (fmap f m)

instance Foldable (TrieMap Word) where
  foldMap f (WordMap m) = foldMap f m
  foldr f z (WordMap m) = foldr f z m
  foldl f z (WordMap m) = foldl f z m

instance Traversable (TrieMap Word) where
  traverse f (WordMap m) = castMap $ traverse f m

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
