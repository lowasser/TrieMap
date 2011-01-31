{-# LANGUAGE MagicHash, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Data.TrieMap.Sized where

import Data.Foldable
import Data.Traversable
import GHC.Exts

class Sized a where
	getSize# :: a -> Int#

data Assoc k a = Assoc {getK :: k, getValue :: a} deriving (Functor, Foldable, Traversable)

newtype Elem a = Elem {getElem :: a} deriving (Functor, Foldable, Traversable)

instance Sized (Elem a) where
	getSize# _ = 1#

instance Sized (Assoc k a) where
	getSize# _ = 1#

instance Sized a => Sized (Maybe a) where
	getSize# (Just a) = getSize# a
	getSize# _ = 0#

getSize :: Sized a => a -> Int
getSize a = I# (getSize# a)

unbox :: Int -> Int#
unbox (I# i#) = i#