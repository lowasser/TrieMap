{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns, NamedFieldPuns, FlexibleInstances #-}
module Data.TrieMap.WordMap.Zippable () where

import Data.TrieMap.TrieKey
import Data.TrieMap.WordMap.Base

import GHC.Exts

instance Zippable SNode where
  empty = nil
  clear (WHole _ path) = fromUTuple clearPath path
  assign a (WHole k path) = fromUTuple (assignPath $ single k a) path

instance Zippable (TrieMap Word) where
  empty = WordMap empty
  clear (Hole hole) = WordMap (clear hole)
  assign a (Hole hole) = WordMap (assign a hole)

{-# INLINE fromUTuple #-}
fromUTuple :: (a -> (# Int#, Node b #)) -> a -> SNode b
fromUTuple f a = case f a of
  (# sz#, node #) -> SNode {sz = I# sz#, node}

clearPath :: Path a -> (# Int#, Node a #)
assignPath :: SNode a -> Path a -> (# Int#, Node a #)
clearPath Root = (# 0#, Nil #)
clearPath (LeftBin _ _ path r) = assignPath r path
clearPath (RightBin _ _ l path) = assignPath l path

assignPath SNode{sz = I# sz#, node} Root = (# sz#, node #)
assignPath !t (LeftBin p m path r) = assignPath (bin' p m t r) path
assignPath !t (RightBin p m l path) = assignPath (bin' p m l t) path