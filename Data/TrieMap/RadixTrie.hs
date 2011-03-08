{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Data.TrieMap.RadixTrie () where

import Data.TrieMap.TrieKey
import Data.TrieMap.RadixTrie.Base
import Data.TrieMap.RadixTrie.Traversable ()
import Data.TrieMap.RadixTrie.Subset ()
import Data.TrieMap.RadixTrie.SetOp ()
import Data.TrieMap.RadixTrie.Project ()
import Data.TrieMap.RadixTrie.Zipper ()
import Data.TrieMap.RadixTrie.Split ()
import Data.TrieMap.RadixTrie.Build ()
import Data.TrieMap.RadixTrie.Index ()
import Data.TrieMap.RadixTrie.Alternate ()

instance TrieKey k => TrieKey (VVector k) where
  sizeM (Radix m) = getSize m

  getSimpleM (Radix Nothing) = Null
  getSimpleM (Radix (Just e)) = case eView e of
    Edge _ _ (Just v) ts
      | isNull ts	-> Singleton v
      | otherwise	-> NonSimple
    _	-> NonSimple

instance TrieKey (PVector Word) where
  sizeM (WRadix m) = getSize m

  getSimpleM (WRadix Nothing) = Null
  getSimpleM (WRadix (Just e)) = case eView e of
    Edge _ _ (Just v) ts
      | isNull ts	-> Singleton v
      | otherwise	-> NonSimple
    _	-> NonSimple