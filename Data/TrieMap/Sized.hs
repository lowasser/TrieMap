{-# LANGUAGE MagicHash, DeriveFunctor, DeriveFoldable, DeriveTraversable, ImplicitParams #-}

module Data.TrieMap.Sized where

import Data.TrieMap.TrieKey.Subset
import Data.Foldable
import Data.Traversable
import GHC.Exts

class Sized a where
  getSize# :: a -> Int#

data Assoc k a = Assoc {getK :: k, getValue :: a} deriving (Eq, Functor, Foldable, Traversable)

newtype Elem a = Elem {getElem :: a} deriving (Functor, Foldable, Traversable)

instance Subset Elem where
  Elem a <=? Elem b = ?le a b

instance Subset (Assoc k) where
  Assoc _ a <=? Assoc _ b = ?le a b

instance Sized (Elem a) where
  getSize# _ = 1#

instance Sized (Assoc k a) where
  getSize# _ = 1#

instance Sized a => Sized (Maybe a) where
	getSize# (Just a) = getSize# a
	getSize# _ = 0#

{-# INLINE getSize #-}
getSize :: Sized a => a -> Int
getSize a = I# (getSize# a)

{-# INLINE unbox #-}
unbox :: Int -> Int#
unbox (I# i#) = i#