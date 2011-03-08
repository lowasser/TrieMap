{-# LANGUAGE TypeFamilies, FlexibleContexts, GeneralizedNewtypeDeriving, FlexibleInstances, NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses, CPP, UnboxedTuples, MagicHash #-}
module Data.TrieMap.ReverseMap () where

import Control.Monad.Ends

import qualified Data.Monoid as M

import Data.TrieMap.TrieKey
import Data.TrieMap.Modifiers

import Data.TrieMap.ReverseMap.Dual

import Prelude hiding (foldr, foldl, foldr1, foldl1, lookup)
import GHC.Exts

#define INSTANCE(cl) (cl (TrieMap k)) => cl (TrieMap (Rev k))

instance INSTANCE(Functor) where
  fmap f (RevMap m) = RevMap (f <$> m)

instance INSTANCE(Foldable) where
  foldMap f (RevMap m) = M.getDual (foldMap (M.Dual . f) m)
  foldr f z (RevMap m) = foldl (flip f) z m
  foldl f z (RevMap m) = foldr (flip f) z m

instance INSTANCE(Traversable) where
  traverse f (RevMap m) = RevMap <$> runDual (traverse (Dual . f) m)

instance INSTANCE(Subset) where
  RevMap m1 <=? RevMap m2 = m1 <=? m2

instance TrieKey k => Buildable (TrieMap (Rev k)) (Rev k) where
  type UStack (TrieMap (Rev k)) = UMStack k
  uFold = fmap RevMap . mapFoldlKeys getRev . uFold
  type AStack (TrieMap (Rev k)) = RevFold (AMStack k) k
  aFold = fmap RevMap . mapFoldlKeys getRev . reverseFold . aFold
  type DAStack (TrieMap (Rev k)) = RevFold (DAMStack k) k
  daFold = RevMap <$> mapFoldlKeys getRev (reverseFold daFold)

#define SETOP(op) op f (RevMap m1) (RevMap m2) = RevMap (op f m1 m2)

instance INSTANCE(SetOp) where
  SETOP(union)
  SETOP(diff)
  SETOP(isect)

instance INSTANCE(Project) where
  mapMaybe f (RevMap m) = RevMap $ mapMaybe f m
  mapEither f (RevMap m) = both RevMap (mapEither f) m

newtype instance TrieMap (Rev k) a = RevMap (TrieMap k a)
newtype instance Zipper (TrieMap (Rev k)) a = RHole (Hole k a)

instance INSTANCE(Zippable) where
  empty = RevMap empty
  clear (RHole hole) = RevMap (clear hole)
  assign a (RHole hole) = RevMap (assign a hole)

instance INSTANCE(Alternatable) where
  alternate (RevMap m) = do
    (a, hole) <- runDualPlus (alternate m)
    return (a, RHole hole)
  firstHole (RevMap m) = First (fmap RHole <$> getLast (lastHole m))
  lastHole  (RevMap m) = Last (fmap RHole <$> getFirst (firstHole m))

instance TrieKey k => Searchable (TrieMap (Rev k)) (Rev k) where
  search (Rev k) (RevMap m) = mapHole RHole $ search k m
  singleZip (Rev k) = RHole (singleZip k)
  singleton (Rev k) a = RevMap (singleton k a)
  lookup (Rev k) (RevMap m) = lookup k m
  insertWith f (Rev k) a (RevMap m) = RevMap (insertWith f k a m)
  alter f (Rev k) (RevMap m) = RevMap (alter f k m)

instance Splittable (TrieMap k) => Splittable (TrieMap (Rev k)) where
  before (RHole hole) = RevMap (after hole)
  beforeWith a (RHole hole) = RevMap (afterWith a hole)
  after (RHole hole) = RevMap (before hole)
  afterWith a (RHole hole) = RevMap (beforeWith a hole)

instance (TrieKey k, Indexable (TrieMap k)) => Indexable (TrieMap (Rev k)) where
  index i (RevMap m) = case index (revIndex i m) m of
    (# i', a, hole #) -> (# revIndex i' a, a, RHole hole #)
    where revIndex :: Sized a => Int# -> a -> Int#
	  revIndex i# a = getSize# a -# 1# -# i#

-- | @'TrieMap' ('Rev' k) a@ is a wrapper around a @'TrieMap' k a@ that reverses the order of the operations.
instance TrieKey k => TrieKey (Rev k) where
  sizeM (RevMap m) = sizeM m
  getSimpleM (RevMap m) = getSimpleM m
  unifierM (Rev k') (Rev k) a = RHole <$> unifierM k' k a
  unifyM (Rev k1) a1 (Rev k2) a2 = RevMap <$> unifyM k1 a1 k2 a2

{-# INLINE reverseFold #-}
reverseFold :: FromList z k a -> FromList (RevFold z k) k a
reverseFold Foldl{snoc = snoc0, begin = begin0, zero, done = done0}
  = Foldl {..} where
  snoc g k a = RevFold $ \ m -> case m of
    Nothing -> runRevFold g (Just $ begin0 k a)
    Just m -> runRevFold g (Just $ snoc0 m k a)
  
  begin = snoc (RevFold $ maybe zero done0)
  
  done g = runRevFold g Nothing

newtype RevFold z k a = RevFold {runRevFold :: Maybe (z a) -> TrieMap k a}