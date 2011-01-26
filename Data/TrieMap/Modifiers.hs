{-# LANGUAGE FlexibleContexts, UndecidableInstances, TypeFamilies #-}
module Data.TrieMap.Modifiers where

import Data.TrieMap.Representation.Class

newtype Ordered a = Ord {unOrd :: a} deriving (Eq, Ord)
newtype Rev k = Rev {getRev :: k} deriving (Eq)
instance Ord k => Ord (Rev k) where
	compare (Rev a) (Rev b) = compare b a

instance Functor Ordered where
	fmap f (Ord a) = Ord (f a)

instance Functor Rev where
	fmap f (Rev a) = Rev (f a)

newtype Key k = Key {getKey :: k}

instance Repr k => Repr (Key k) where
	type Rep (Key k) = Rep k
	toRep (Key k) = toRep k