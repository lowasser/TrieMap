{-# LANGUAGE RecordWildCards, CPP, TypeSynonymInstances #-}
module Data.TrieMap.OrdMap.Traversable () where

import Data.TrieMap.OrdMap.Base

import Data.Functor.Immoral

import Prelude hiding (foldr, foldl)

#define TIP SNode{node = Tip}
#define BIN(args) SNode{node = (Bin args)}

instance Functor (SNode k) where
  fmap f = mapF where
    mapF TIP = tip
    mapF SNode{node = Bin kx x l r, ..} =
      SNode{node = Bin kx (f x) (mapF l) (mapF r), ..}

instance Foldable (SNode k) where
  foldMap f = fold where
    fold TIP = mempty
    fold BIN(_ x l r) =
      fold l `mappend` f x `mappend` fold r
  foldr f = flip fold where
    fold TIP z = z
    fold BIN(_ x l r) z = fold l (x `f` fold r z)
  foldl f = fold where
    fold z TIP = z
    fold z BIN(_ x l r) = z `fold` l `f` x `fold` r

instance Traversable (SNode k) where
  traverse f = trav where
    trav TIP = pure tip
    trav SNode{node = Bin kx x l r, ..} =
      let finish l' x' r' = SNode{node = Bin kx x' l' r', ..} in
      finish <$> trav l <*> f x <*> trav r

instance Functor (OrdMap k) where
  fmap f (OrdMap m) = OrdMap (f <$> m)

instance Foldable (OrdMap k) where
  foldMap f (OrdMap m) = foldMap f m
  foldr f z (OrdMap m) = foldr f z m
  foldl f z (OrdMap m) = foldl f z m

instance Traversable (OrdMap k) where
  traverse f (OrdMap m) = castMap $ traverse f m