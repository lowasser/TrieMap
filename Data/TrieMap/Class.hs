{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module Data.TrieMap.Class (TMap(..), TSet(..), TKey, Rep, TrieMap, TrieKey, tMap, tSet) where

import Data.TrieMap.TrieKey
import Data.TrieMap.Representation.Class

import Prelude hiding (foldr, foldl, foldl1, foldr1)

-- | A map from keys @k@ to values @a@, backed by a trie.
data TMap k a = TMap {mapSize :: {-# UNPACK #-} !Int, getTMap :: TrieMap (Rep k) (Assoc k a)}

-- | A set of values @a@, backed by a trie.
data TSet a = TSet {setSize :: {-# UNPACK #-} !Int, getTSet :: TrieMap (Rep a) (Elem a)}

tMap :: TKey k => TrieMap (Rep k) (Assoc k a) -> TMap k a
tMap m = TMap (sizeM m) m

tSet :: TKey a => TrieMap (Rep a) (Elem a) -> TSet a
tSet s = TSet (sizeM s) s

-- | @'TKey' k@ is a handy alias for @('Repr' k, 'TrieKey' ('Rep' k))@.  To make a type an instance of 'TKey',
-- create a 'Repr' instance that will satisfy @'TrieKey' ('Rep' k)@, possibly using the Template Haskell methods
-- provided by "Data.TrieMap.Representation".
class (Repr k, TrieKey (Rep k)) => TKey k

instance (Repr k, TrieKey (Rep k)) => TKey k

instance TKey k => Functor (TMap k) where
	fmap f (TMap sz m) = TMap sz (fmap (fmap f) m)

instance TKey k => Foldable (TMap k) where
	foldMap f (TMap _ m) = foldMap (foldMap f) m
	foldr f z (TMap _ m) = foldr (flip $ foldr f) z m
	foldl f z (TMap _ m) = foldl (foldl f) z m
	foldr1 f (TMap _ m) = foldr1 f (getValue <$> m)
	foldl1 f (TMap _ m) = foldl1 f (getValue <$> m)

instance TKey k => Traversable (TMap k) where
	traverse f (TMap sz m) = TMap sz <$> traverse (traverse f) m