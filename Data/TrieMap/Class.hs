{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module Data.TrieMap.Class (TMap(..), TSet(..), TKey, Rep, TrieMap, TrieKey) where

import Data.TrieMap.TrieKey
import Data.TrieMap.Representation.Class
import Data.TrieMap.Sized

import Data.Functor
import Data.Foldable hiding (foldrM, foldlM)
import Data.Traversable

import Prelude hiding (foldr)

-- | A map from keys @k@ to values @a@, backed by a trie.
newtype TMap k a = TMap {getTMap :: TrieMap (Rep k) (Assoc k a)}

-- | A set of values @a@, backed by a trie.
newtype TSet a = TSet {getTSet :: TrieMap (Rep a) (Elem a)}

-- | @'TKey' k@ is a handy alias for @('Repr' k, 'TrieKey' ('Rep' k))@.  To make a type an instance of 'TKey',
-- create a 'Repr' instance that will satisfy @'TrieKey' ('Rep' k)@, possibly using the Template Haskell methods
-- provided by "Data.TrieMap.Representation".
class (Repr k, TrieKey (Rep k)) => TKey k

instance (Repr k, TrieKey (Rep k)) => TKey k

instance TKey k => Functor (TMap k) where
	fmap f (TMap m) = TMap (fmapM (\ (Assoc k a) -> Assoc k (f a)) m)

instance TKey k => Foldable (TMap k) where
	foldr f z (TMap m) = foldrM (\ (Assoc _ a) -> f a) m z
	foldl f z (TMap m) = foldlM (\ z (Assoc _ a) -> f z a) m z

instance TKey k => Traversable (TMap k) where
	traverse f (TMap m) = TMap <$> traverseM (\ (Assoc k a) -> Assoc k <$> f a) m