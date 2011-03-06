{-# LANGUAGE NamedFieldPuns, FlexibleInstances #-}
module Data.TrieMap.WordMap.Alternatable () where

import Data.TrieMap.TrieKey

import Data.Word

import Data.TrieMap.WordMap.Base
import Data.TrieMap.WordMap.Zippable ()

instance Alternatable SNode where
  alternate = alt Root where
    alt path SNode{node} = case node of
      Nil	-> mzero
      Tip k a	-> return (a, WHole k path)
      Bin p m l r -> alt (LeftBin p m path r) l `mplus` alt (RightBin p m l path) r

instance Alternatable (TrieMap Word) where
  alternate (WordMap m) = do
    (a, hole) <- alternate m
    return (a, Hole hole)