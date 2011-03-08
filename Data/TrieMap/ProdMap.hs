{-# LANGUAGE TupleSections, TypeFamilies, FlexibleInstances, RecordWildCards, CPP, FlexibleContexts, UnboxedTuples #-}
{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module Data.TrieMap.ProdMap () where

import Data.TrieMap.TrieKey
import Data.Functor.Immoral

import Data.TrieMap.ProdMap.Base
import Data.TrieMap.ProdMap.Traversable ()
import Data.TrieMap.ProdMap.Searchable ()
import Data.TrieMap.ProdMap.Zippable ()
import Data.TrieMap.ProdMap.Splittable ()
import Data.TrieMap.ProdMap.SetOp ()
import Data.TrieMap.ProdMap.Buildable ()
import Data.TrieMap.ProdMap.Indexable ()

import Prelude hiding (foldl, foldl1, foldr, foldr1)

-- | @'TrieMap' (k1, k2) a@ is implemented as a @'TrieMap' k1 ('TrieMap' k2 a)@.
instance (TrieKey k1, TrieKey k2) => TrieKey (k1, k2) where
	getSimpleM (PMap m) = getSimpleM m >>= getSimpleM
	sizeM (PMap m) = sizeM m
	unifierM (k1', k2') (k1, k2) a = 
	  (fmap (`PHole` singleZip k2') $ unifierM k1' k1 (singleton k2 a))
	  `mplus` (PHole (singleZip k1) <$> unifierM k2' k2 a)
	unifyM (k11, k12) a1 (k21, k22) a2 =
	  let unify1 = unifyM k11 (singleton k12 a1) k21 (singleton k22 a2)
	      unify2 = singleton k11 <$> unifyM k12 a1 k22 a2
	      in castMap (unify1 `mplus` unify2)