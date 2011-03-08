{-# LANGUAGE CPP, MagicHash, UnboxedTuples, BangPatterns, NamedFieldPuns, FlexibleInstances #-}
module Data.TrieMap.WordMap.Indexable () where

import Data.TrieMap.TrieKey

import Data.Word

import Data.TrieMap.WordMap.Base
import Data.TrieMap.WordMap.Zippable ()

import GHC.Exts

instance Indexable (TrieMap Word) where
  index i (WordMap m) = case index i m of
    (# i', a, hole #) -> (# i', a, Hole hole #)

instance Indexable SNode where
  index i !t = indexT i t Root where
    indexT i SNode{node} path = case node of
      Tip kx x		-> (# i, x, WHole kx path #)
      Bin p m l r
	    | i <# sl	-> indexT i l (LeftBin p m path r)
	    | otherwise	-> indexT (i -# sl) r (RightBin p m l path)
	    where !sl = getSize# l
      Nil		-> indexFail ()