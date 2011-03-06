{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Data.TrieMap.RadixTrie.Base (
  module Data.TrieMap.RadixTrie.Label,
  module Data.TrieMap.RadixTrie.Slice,
  module Data.TrieMap.RadixTrie.Base,
  module Data.TrieMap.TrieKey,
  Word)
  where

import Data.TrieMap.TrieKey

import Data.TrieMap.RadixTrie.Slice
import Data.TrieMap.RadixTrie.Label

import Data.Functor.Immoral
import Data.Word

import qualified Data.Vector as V
import qualified Data.Vector.Primitive as P

type VVector = V.Vector
type PVector = P.Vector

newtype instance TrieMap (VVector k) a = Radix (MEdge VVector k a)
  deriving (ImmoralMap (MEdge VVector k a))
newtype instance TrieMap (PVector Word) a = WRadix (MEdge PVector Word a)
  deriving (ImmoralMap (MEdge PVector Word a))
newtype instance Zipper (TrieMap (VVector k)) a = Hole (EdgeLoc VVector k a)
newtype instance Zipper (TrieMap (PVector Word)) a = WHole (EdgeLoc PVector Word a)