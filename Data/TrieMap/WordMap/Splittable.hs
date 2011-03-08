{-# LANGUAGE BangPatterns, FlexibleInstances #-}
{-# OPTIONS -O -fspec-constr -fliberate-case -fstatic-argument-transformation #-}
module Data.TrieMap.WordMap.Splittable () where

import Data.TrieMap.TrieKey

import Data.Word

import Data.TrieMap.WordMap.Base
import Data.TrieMap.WordMap.Zippable ()

instance Splittable SNode where
  before (WHole _ path) = beforePath path
  after (WHole _ path) = afterPath path
  beforeWith a (WHole k path) = beforeWithPath (single k a) path
  afterWith a (WHole k path) = afterWithPath (single k a) path

instance Splittable (TrieMap Word) where
  before (Hole hole) = WordMap (before hole)
  after (Hole hole) = WordMap (after hole)
  beforeWith a (Hole hole) = WordMap (beforeWith a hole)
  afterWith a (Hole hole) = WordMap (afterWith a hole)

beforePath, afterPath :: Path a -> SNode a
beforeWithPath, afterWithPath :: SNode a -> Path a -> SNode a

beforePath Root			= nil
beforePath (LeftBin _ _ path _)	= beforePath path
beforePath (RightBin _ _ l path)	= beforeWithPath l path

beforeWithPath !t Root			= t
beforeWithPath !t (LeftBin _ _ path _)	= beforeWithPath t path
beforeWithPath !t (RightBin p m l path)	= beforeWithPath (bin' p m l t) path

afterPath Root			= nil
afterPath (RightBin _ _ _ path)	= afterPath path
afterPath (LeftBin _ _ path r)	= afterWithPath r path

afterWithPath !t Root			= t
afterWithPath !t (RightBin _ _ _ path)	= afterWithPath t path
afterWithPath !t (LeftBin p m path r)	= afterWithPath (bin' p m t r) path