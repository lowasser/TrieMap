{-# LANGUAGE TypeFamilies, UnboxedTuples, MagicHash, FlexibleContexts, TupleSections, Rank2Types, ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards, ImplicitParams, TypeOperators #-}

module Data.TrieMap.TrieKey (
  module Data.TrieMap.TrieKey,
  module Data.Foldable,
  module Data.Traversable,
  module Control.Applicative,
  module Data.TrieMap.Sized,
  module Data.TrieMap.Utils,
  module Data.TrieMap.TrieKey.Subset,
  module Data.TrieMap.TrieKey.Buildable,
  module Data.TrieMap.TrieKey.SetOp,
  module Data.TrieMap.TrieKey.Projection,
  module Data.TrieMap.TrieKey.Searchable,
  module Data.TrieMap.TrieKey.Zippable,
  module Data.TrieMap.TrieKey.Splittable,
  module Data.TrieMap.TrieKey.Indexable,
  module Data.TrieMap.TrieKey.Alternatable,
  MonadPlus(..),
  Monoid(..),
  guard) where

import Data.TrieMap.Sized
import Data.TrieMap.Utils
import Data.TrieMap.TrieKey.Subset
import Data.TrieMap.TrieKey.Buildable
import Data.TrieMap.TrieKey.SetOp
import Data.TrieMap.TrieKey.Projection
import Data.TrieMap.TrieKey.Searchable
import Data.TrieMap.TrieKey.Zippable
import Data.TrieMap.TrieKey.Splittable
import Data.TrieMap.TrieKey.Indexable
import Data.TrieMap.TrieKey.Alternatable

import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.Option

import Data.Monoid (Monoid(..))
import Data.Foldable
import Data.Traversable

import Prelude hiding (foldr, foldl)

import GHC.Exts

type FromList stack k a = Foldl stack k a (TrieMap k a)
type UMStack k = UStack (TrieMap k)
type AMStack k = AStack (TrieMap k)
type DAMStack k = DAStack (TrieMap k)

data Simple a = Null | Singleton a | NonSimple deriving (Eq)

instance Monad Simple where
	return = Singleton
	Null >>= _ = Null
	Singleton a >>= k = k a
	NonSimple >>= _ = NonSimple

instance MonadPlus Simple where
	mzero = Null
	Null `mplus` simple	= simple
	simple `mplus` Null	= simple
	_ `mplus` _		= NonSimple

{-# INLINE onSnd #-}
onSnd :: (c -> d) -> (a -> (# b, c #)) -> a -> (# b, d #)
onSnd g f a = case f a of
	(# b, c #) -> (# b, g c #)

{-# INLINE onThird #-}
onThird :: (d -> e) -> (a -> (# Int, c, d #)) -> a -> (# Int, c, e #)
onThird g f a = case f a of
	(# b, c, d #) -> (# b, c, g d #)

data family TrieMap k :: * -> *
type Hole k = Zipper (TrieMap k)

-- | A @TrieKey k@ instance implies that @k@ is a standardized representation for which a
-- generalized trie structure can be derived.
class (Ord k,
	Buildable (TrieMap k) k,
	Subset (TrieMap k),
	Traversable (TrieMap k),
	SetOp (TrieMap k),
	Project (TrieMap k),
	Zippable (TrieMap k),
	Searchable (TrieMap k) k,
	Splittable (TrieMap k),
	Indexable (TrieMap k)) => TrieKey k where
  getSimpleM :: TrieMap k a -> Simple a
  sizeM# :: Sized a => TrieMap k a -> Int#
  sizeM :: Sized a => TrieMap k a -> Int
  
  -- By combining rewrite rules and these NOINLINE pragmas, we automatically derive
  -- specializations of functions for every instance of TrieKey.
  {-# NOINLINE sizeM# #-}
  sizeM# m = unbox (inline sizeM m)
  
  unifierM :: Sized a => k -> k -> a -> Option (Hole k a)
  unifyM :: Sized a => k -> a -> k -> a -> Option (TrieMap k a)
  
  unifierM k' k a = option $ \ no yes -> search k' (singleton k a) yes (\ _ _ -> no)
  unifyM k1 a1 k2 a2 = assign a1 <$> unifierM k1 k2 a2

instance (TrieKey k, Sized a) => Sized (TrieMap k a) where
	getSize# = sizeM#

instance TrieKey k => Nullable (TrieMap k) where
  isNull m = case getSimpleM m of
    Null -> True
    _ -> False

foldl1Empty :: a
foldl1Empty = error "Error: cannot call foldl1 on an empty map"

foldr1Empty :: a
foldr1Empty = error "Error: cannot call foldr1 on an empty map"

{-# INLINE mappendM #-}
mappendM :: Monoid m => Maybe m -> Maybe m -> m
Nothing `mappendM` Nothing = mempty
Nothing `mappendM` Just m = m
Just m `mappendM` Nothing = m
Just m1 `mappendM` Just m2 = m1 `mappend` m2

{-# RULES
  "sizeM" [0] forall m . sizeM m = I# (sizeM# m);
  #-}