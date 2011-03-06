{-# LANGUAGE BangPatterns, TypeSynonymInstances, CPP #-}
module Data.TrieMap.OrdMap.Splittable () where

import Data.TrieMap.OrdMap.Base
import Data.TrieMap.OrdMap.Zippable ()

instance Splittable (SNode k) where
  before (Empty _ path) = beforePath tip path
  before (Full _ path l _) = beforePath l path
  beforeWith a (Empty k path) = beforePath (single k a) path
  beforeWith a (Full k path l _) = beforePath (insertMax k a l) path
  after (Empty _ path) = afterPath tip path
  after (Full _ path _ r) = afterPath r path
  afterWith a (Empty k path) = afterPath (single k a) path
  afterWith a (Full k path _ r) = afterPath (insertMin k a r) path

#define SPLIT(op) op (Hole hole) = OrdMap (op hole)
instance Splittable (OrdMap k) where
  SPLIT(before)
  SPLIT(beforeWith a)
  SPLIT(after)
  SPLIT(afterWith a)

beforePath :: Sized a => SNode k a -> Path k a -> SNode k a
beforePath !t (LeftBin _ _ path _) = beforePath t path
beforePath !t (RightBin k a l path) = beforePath (join k a l t) path
beforePath !t _ = t

afterPath :: Sized a => SNode k a -> Path k a -> SNode k a
afterPath !t (LeftBin k a path r) = afterPath (join k a t r) path
afterPath !t (RightBin _ _ _ path) = afterPath t path
afterPath !t _ = t

