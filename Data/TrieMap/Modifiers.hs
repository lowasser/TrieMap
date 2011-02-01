{-# LANGUAGE FlexibleContexts, UndecidableInstances, TypeFamilies #-}
module Data.TrieMap.Modifiers where

import Data.TrieMap.Representation.Class

newtype Ordered a = Ord {unOrd :: a} deriving (Eq, Ord)
newtype Rev k = Rev {getRev :: k} deriving (Eq)
instance Ord k => Ord (Rev k) where
	compare (Rev a) (Rev b) = compare b a
	Rev a <  Rev b	= b < a
	Rev a <= Rev b	= b <= a
	(>)		= flip (<)
	(>=)		= flip (<=)
	

instance Functor Ordered where
	fmap f (Ord a) = Ord (f a)

instance Functor Rev where
	fmap f (Rev a) = Rev (f a)

newtype Key k = Key {getKey :: k}

instance (Repr k, Eq (Rep k)) => Eq (Key k) where
	Key a == Key b	= toRep a == toRep b

instance (Repr k, Ord (Rep k)) => Ord (Key k) where
	Key a `compare` Key b = toRep a `compare` toRep b
	Key a < Key b	= toRep a < toRep b
	Key a <= Key b	= toRep a <= toRep b
	(>)		= flip (<)
	(>=)		= flip (<=)

instance Repr k => Repr (Key k) where
	type Rep (Key k) = Rep k
	type RepList (Key k) = RepList k
	toRep (Key k) = toRep k
	toRepList ks = toRepList [k | Key k <- ks]