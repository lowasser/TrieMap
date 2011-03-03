{-# LANGUAGE FlexibleContexts, UndecidableInstances, TypeFamilies #-}

-- | 
-- Operators for use in 'Repr' instances for types.
module Data.TrieMap.Modifiers where

import Data.TrieMap.Representation.Class
import Data.Vector.Generic

-- | Denotes that maps on this type should be implemented with traditional binary search trees.
newtype Ordered a = Ord {unOrd :: a} deriving (Eq, Ord)

instance Repr (Ordered k) where
  type Rep (Ordered k) = Ordered k
  toRep = id
  type RepStream (Ordered k) = DRepStream (Ordered k)
  toRepStream = unstream

-- | Denotes that maps on this type should be treated as reversed.  For instance, @'Rep' 'Int'@ might be
-- implemented as @'Either' ('Rev' Word) Word@, to handle negative numbers properly.
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

-- | Indicates that the map for this type should be bootstrapped from its @TKey@ instance.
-- This modifier is necessary to define a @TKey@ instance for a recursively defined type.
-- For example:
-- 
-- > data Tree = Node Char [Tree]
-- > 
-- > instance 'Repr' Tree where
-- > 	type 'Rep' Tree = ('Rep' 'Char', ['Key' Tree])
-- > 	'toRep' (Node label children) = ('toRep' label, 'map' 'Key' children)
-- > 	...
-- 
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
	type RepStream (Key k) = RepStream k
	toRep (Key k) = toRep k
	toRepStream ks = toRepStream (fmap getKey ks)