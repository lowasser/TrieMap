{-# LANGUAGE TypeFamilies, UnboxedTuples, MagicHash, FlexibleContexts, TupleSections, Rank2Types, ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards, ImplicitParams, TypeOperators #-}

module Data.TrieMap.TrieKey (
  module Data.TrieMap.TrieKey,
  module Data.Foldable,
  module Data.Traversable,
  module Control.Applicative,
  module Data.TrieMap.Sized,
  module Data.TrieMap.Subset,
  module Data.TrieMap.Utils,
  module Data.TrieMap.Buildable,
  module Data.TrieMap.SetOp,
  module Data.TrieMap.Projection,
  module Data.TrieMap.IndexedHole,
  module Data.TrieMap.Search,
  MonadPlus(..),
  Monoid(..),
  guard) where

import Data.TrieMap.Sized
import Data.TrieMap.Subset
import Data.TrieMap.Utils
import Data.TrieMap.Buildable
import Data.TrieMap.SetOp
import Data.TrieMap.Projection
import Data.TrieMap.IndexedHole
import Data.TrieMap.Search

import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.Lookup
import Control.Monad.Ends
import Control.Monad.Unpack

import Data.Monoid (Monoid(..))
import Data.Foldable
import Data.Traversable

import Prelude hiding (foldr, foldl)

import GHC.Exts

type FromList stack k a = Foldl stack k a (TrieMap k a)
type UMStack k = UStack (TrieMap k)
type AMStack k = AStack (TrieMap k)
type DAMStack k = DAStack (TrieMap k)

data Simple a = Null | Singleton a | NonSimple

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

-- | A @TrieKey k@ instance implies that @k@ is a standardized representation for which a
-- generalized trie structure can be derived.
class (Ord k,
	Buildable (TrieMap k) k,
	Subset (TrieMap k),
	Traversable (TrieMap k),
	SetOp (TrieMap k),
	Project (TrieMap k)) => TrieKey k where
  data TrieMap k :: * -> *
  emptyM :: TrieMap k a
  singletonM :: Sized a => k -> a -> TrieMap k a
  getSimpleM :: TrieMap k a -> Simple a
  sizeM# :: Sized a => TrieMap k a -> Int#
  sizeM :: Sized a => TrieMap k a -> Int
  lookupMC :: k -> TrieMap k a -> Lookup r a
  
  insertWithM :: (TrieKey k, Sized a) => (a -> a) -> k -> a -> TrieMap k a -> TrieMap k a
  
  data Hole k :: * -> *
  singleHoleM :: k -> Hole k a
  beforeM, afterM :: Sized a => Hole k a -> TrieMap k a
  beforeWithM, afterWithM :: Sized a => a -> Hole k a -> TrieMap k a
  searchMC :: k -> TrieMap k a -> SearchCont (Hole k a) a r
  indexMC :: Sized a => TrieMap k a -> Int :~> IndexCont (Hole k a) a r

  -- By combining rewrite rules and these NOINLINE pragmas, we automatically derive
  -- specializations of functions for every instance of TrieKey.
  extractHoleM :: (Functor m, MonadPlus m) => Sized a => TrieMap k a -> m (a, Hole k a)
  {-# NOINLINE firstHoleM #-}
  {-# NOINLINE lastHoleM #-}
  {-# NOINLINE sizeM# #-}
  sizeM# m = unbox (inline sizeM m)
  firstHoleM :: Sized a => TrieMap k a -> First (a, Hole k a)
  firstHoleM m = inline extractHoleM m
  lastHoleM :: Sized a => TrieMap k a -> Last (a, Hole k a)
  lastHoleM m = inline extractHoleM m
  
  insertWithM f k a m = inline searchMC k m (assignM a) (assignM . f)
  
  assignM :: Sized a => a -> Hole k a -> TrieMap k a
  clearM :: Sized a => Hole k a -> TrieMap k a
  unifierM :: Sized a => k -> k -> a -> Lookup r (Hole k a)
  unifyM :: Sized a => k -> a -> k -> a -> Lookup r (TrieMap k a)
  
  unifierM k' k a = Lookup $ \ no yes -> searchMC k' (singletonM k a) yes (\ _ _ -> no)
  unifyM k1 a1 k2 a2 = assignM a1 <$> unifierM k1 k2 a2

instance (TrieKey k, Sized a) => Sized (TrieMap k a) where
	getSize# = sizeM#

instance TrieKey k => Nullable (TrieMap k) where
  isNull m = case getSimpleM m of
    Null -> True
    _ -> False

{-# INLINE indexMC' #-}
indexMC' :: (TrieKey k, Sized a) => TrieMap k a -> Int -> (IndexedHole a (Hole k a) -> r) -> r
indexMC' m i result = (indexMC m $~ i) (unpack result)

foldl1Empty :: a
foldl1Empty = error "Error: cannot call foldl1 on an empty map"

foldr1Empty :: a
foldr1Empty = error "Error: cannot call foldr1 on an empty map"

{-# INLINE fillHoleM #-}
fillHoleM :: (TrieKey k, Sized a) => Maybe a -> Hole k a -> TrieMap k a
fillHoleM = maybe clearM assignM

{-# INLINE lookupM #-}
lookupM :: TrieKey k => k -> TrieMap k a -> Maybe a
lookupM k m = runLookup (lookupMC k m) Nothing Just

{-# INLINE mappendM #-}
mappendM :: Monoid m => Maybe m -> Maybe m -> m
Nothing `mappendM` Nothing = mempty
Nothing `mappendM` Just m = m
Just m `mappendM` Nothing = m
Just m1 `mappendM` Just m2 = m1 `mappend` m2

insertWithM' :: (TrieKey k, Sized a) => (a -> a) -> k -> a -> Maybe (TrieMap k a) -> TrieMap k a
insertWithM' f k a = maybe (singletonM k a) (insertWithM f k a)

{-# INLINE beforeMM #-}
beforeMM :: (TrieKey k, Sized a) => Maybe a -> Hole k a -> TrieMap k a
beforeMM = maybe beforeM beforeWithM

{-# INLINE afterMM #-}
afterMM :: (TrieKey k, Sized a) => Maybe a -> Hole k a -> TrieMap k a
afterMM = maybe afterM afterWithM

clearM' :: (TrieKey k, Sized a) => Hole k a -> Maybe (TrieMap k a)
clearM' hole = guardNull (clearM hole)

{-# INLINE alterM #-}
alterM :: (TrieKey k, Sized a) => (Maybe a -> Maybe a) -> k -> TrieMap k a -> TrieMap k a
alterM f k m = searchMC k m g h where
  g hole = case f Nothing of
    Nothing	-> m
    Just a	-> assignM a hole
  h = fillHoleM . f . Just

{-# INLINE searchMC' #-}
searchMC' :: TrieKey k => k -> Maybe (TrieMap k a) -> (Hole k a -> r) -> (a -> Hole k a -> r) -> r
searchMC' k Nothing f _ = f (singleHoleM k)
searchMC' k (Just m) f g = searchMC k m f g

elemsM :: TrieKey k => TrieMap k a -> [a]
elemsM m = build (\ f z -> foldr f z m)

{-# RULES
  "extractHoleM/First" [0] extractHoleM = firstHoleM;
  "extractHoleM/Last" [0] extractHoleM = lastHoleM;
  "sizeM" [0] forall m . sizeM m = I# (sizeM# m);
  "getSimpleM/emptyM" getSimpleM emptyM = Null;
  "getSimpleM/singletonM" forall k a . getSimpleM (singletonM k a) = Singleton a;
  #-}