{-# LANGUAGE UnboxedTuples, MagicHash #-}
module Data.TrieMap.TrieKey.Indexable where

import Data.TrieMap.TrieKey.Zippable
import Data.TrieMap.Sized

import GHC.Exts

class Zippable f => Indexable f where
  index :: Sized a => Int# -> f a -> (# Int#, a, Zipper f a #)

index' :: (Indexable f, Sized a) => Int -> f a -> (Int, a, Zipper f a)
index' (I# i#) m = case index i# m of
  (# i'#, a, z #) -> (I# i'#, a, z)

indexFail :: a
indexFail = error "Error: index out of bounds"