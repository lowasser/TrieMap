{-# LANGUAGE BangPatterns #-}
module Data.TrieMap.WordMap () where

import Control.Monad.Option

import Data.TrieMap.TrieKey

import Data.Word

import Data.Functor.Immoral

import Data.TrieMap.WordMap.Base
import Data.TrieMap.WordMap.Zippable ()
import Data.TrieMap.WordMap.Searchable ()
import Data.TrieMap.WordMap.Indexable ()
import Data.TrieMap.WordMap.Splittable ()
import Data.TrieMap.WordMap.Projection ()
import Data.TrieMap.WordMap.Subset ()
import Data.TrieMap.WordMap.SetOp ()
import Data.TrieMap.WordMap.Traversable ()
import Data.TrieMap.WordMap.Buildable ()

-- | @'TrieMap' 'Word' a@ is based on "Data.IntMap".
instance TrieKey Word where
  getSimpleM (WordMap (SNode _ n)) = case n of
    Nil		-> Null
    Tip _ a	-> Singleton a
    _		-> NonSimple
  sizeM (WordMap t) = getSize t

  {-# INLINE unifyM #-}
  unifyM k1 a1 k2 a2 = castMap $ unify k1 a1 k2 a2

  {-# INLINE unifierM #-}
  unifierM k' k a = castMap $ unifier k' k a

{-# INLINE unify #-}
unify :: Sized a => Key -> a -> Key -> a -> Option (SNode a)
unify !k1 a1 !k2 a2 = do
  guard (k1 /= k2)
  return (join k1 (singleton k1 a1) k2 (singleton k2 a2))

{-# INLINE unifier #-}
unifier :: Sized a => Key -> Key -> a -> Option (Zipper SNode a)
unifier !k' !k a = do
  guard (k /= k')
  return (WHole k' $ branchHole k' k Root (singleton k a))