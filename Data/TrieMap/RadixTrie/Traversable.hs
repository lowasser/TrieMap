{-# LANGUAGE CPP, BangPatterns, ViewPatterns, FlexibleInstances #-}
module Data.TrieMap.RadixTrie.Traversable () where

import Data.TrieMap.RadixTrie.Base
import Data.Functor.Immoral

import Prelude hiding (foldl, foldr)

#define V(f) f (VVector) (k)
#define U(f) f (PVector) (Word)
#define EDGE(args) (!(eView -> Edge args))

instance TrieKey k => Functor (TrieMap (VVector k)) where
  fmap f (Radix m) = Radix (fmap (fmap f) m)

instance Functor (TrieMap (PVector Word)) where
  fmap f (WRadix m) = WRadix (fmap (fmap f) m)

instance TrieKey k => Foldable (TrieMap (VVector k)) where
  foldMap f (Radix m) = foldMap (foldMap f) m
  foldr f z (Radix m) = foldl (foldr f) z m
  foldl f z (Radix m) = foldl (foldl f) z m

instance Foldable (TrieMap (PVector Word)) where
  foldMap f (WRadix m) = foldMap (foldMap f) m
  foldr f z (WRadix m) = foldl (foldr f) z m
  foldl f z (WRadix m) = foldl (foldl f) z m

instance TrieKey k => Traversable (TrieMap (VVector k)) where
  traverse f (Radix m) = castMap (traverse (traverse f) m)

instance Traversable (TrieMap (PVector Word)) where
  traverse f (WRadix m) = castMap (traverse (traverse f) m)

instance Label v k => Functor (Edge v k) where
  {-# SPECIALIZE instance TrieKey k => Functor (V(Edge)) #-}
  {-# SPECIALIZE instance Functor (U(Edge)) #-}
  fmap f = map where
    map EDGE(sz ks v ts) = edge' sz ks (f <$> v) (map <$> ts)

instance Label v k => Foldable (Edge v k) where
  {-# SPECIALIZE instance TrieKey k => Foldable (V(Edge)) #-}
  {-# SPECIALIZE instance Foldable (U(Edge)) #-}
  foldMap f = fold where
    foldBranch = foldMap fold
    fold e = case eView e of
      Edge _ _ Nothing ts	-> foldBranch ts
      Edge _ _ (Just a) ts	-> f a `mappend` foldBranch ts
  
  foldr f = flip fold where
    foldBranch = foldr fold
    fold e z = case eView e of
      Edge _ _ Nothing ts -> foldBranch z ts
      Edge _ _ (Just a) ts -> a `f` foldBranch z ts

  foldl f = fold where
    foldBranch = foldl fold
    fold z e = case eView e of
      Edge _ _ Nothing ts -> foldBranch z ts
      Edge _ _ (Just a) ts -> foldBranch (z `f` a) ts

instance Label v k => Traversable (Edge v k) where
  {-# SPECIALIZE instance TrieKey k => Traversable (V(Edge)) #-}
  {-# SPECIALIZE instance Traversable (U(Edge)) #-}
  traverse f = trav where
    travBranch = traverse trav
    trav e = case eView e of
      Edge sz ks Nothing ts	-> edge' sz ks Nothing <$> travBranch ts
      Edge sz ks (Just a) ts	-> edge' sz ks . Just <$> f a <*> travBranch ts