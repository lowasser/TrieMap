{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module Data.TrieMap.Class (TMap(..), TSet(..), TKey, Rep, TrieMap, TrieKey) where

import Data.TrieMap.TrieKey
import Data.TrieMap.Representation.Class
import Data.TrieMap.Sized

import Control.Applicative
import Data.Foldable hiding (foldrM, foldlM)
import Data.Traversable

import Prelude hiding (foldr)

newtype TMap k a = TMap {getTMap :: TrieMap (Rep k) (Assoc k a)}

newtype TSet a = TSet (TMap a a)

-- | @'TKey' k@ is a handy alias for @('Repr' k, 'TrieKey' ('Rep' k))@.  To make a type an instance of 'TKey',
-- use the methods available in "Data.TrieMap.Representation.TH" to generate a 'Repr' instance that will
-- satisfy @'TrieKey' ('Rep' k)@.
class (Repr k, TrieKey (Rep k)) => TKey k

instance (Repr k, TrieKey (Rep k)) => TKey k

instance TKey k => Functor (TMap k) where
	fmap = fmapDefault

instance TKey k => Foldable (TMap k) where
	foldr f z (TMap m) = foldrM (\ (Assoc _ a) -> f a) m z

instance TKey k => Traversable (TMap k) where
	traverse f (TMap m) = TMap <$> traverseM (\ (Assoc k a) -> Assoc k <$> f a) m