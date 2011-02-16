{-# LANGUAGE MagicHash, DeriveFunctor, DeriveFoldable, DeriveTraversable, ImplicitParams #-}

module Data.TrieMap.Sized where

import Data.Foldable
import Data.Traversable
import GHC.Exts

class Sized a where
	getSize# :: a -> Int#

class Subset f where
  (<=?) :: (Sized b, ?le :: a -> b -> Bool) => f a -> f b -> Bool

(<<=?) :: (Sized b, Sized (g b), Subset f, Subset g, ?le :: a -> b -> Bool) => f (g a) -> f (g b) -> Bool
f <<=? g = let ?le = (<=?) in f <=? g

instance Subset Maybe where
  Nothing <=? _ = True
  Just a <=? Just b = ?le a b
  Just{} <=? Nothing = False

data Assoc k a = Assoc {getK :: k, getValue :: a} deriving (Functor, Foldable, Traversable)

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