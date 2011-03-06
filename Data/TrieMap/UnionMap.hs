{-# LANGUAGE ViewPatterns #-}
module Data.TrieMap.UnionMap () where

import Data.TrieMap.UnionMap.Base
import Data.TrieMap.UnionMap.Traversable ()
import Data.TrieMap.UnionMap.Subset ()
import Data.TrieMap.UnionMap.SetOp ()
import Data.TrieMap.UnionMap.Projection ()
import Data.TrieMap.UnionMap.Splittable ()
import Data.TrieMap.UnionMap.Searchable ()
import Data.TrieMap.UnionMap.Indexable ()
import Data.TrieMap.UnionMap.Buildable ()

-- | @'TrieMap' ('Either' k1 k2) a@ is essentially a @(TrieMap k1 a, TrieMap k2 a)@, but
-- specialized for the cases where one or both maps are empty.
instance (TrieKey k1, TrieKey k2) => TrieKey (Either k1 k2) where
	getSimpleM (uView -> UView m1 m2) = mSimple m1 `mplus` mSimple m2 where
		mSimple :: TrieKey k => Maybe (TrieMap k a) -> Simple a
		mSimple = maybe mzero getSimpleM
	
	sizeM Empty = 0
	sizeM (MapL m1) = sizeM m1
	sizeM (MapR m2) = sizeM m2
	sizeM (Union s _ _) = s
	
	unifierM (Left k') (Left k) a = HoleX0 <$> unifierM k' k a
	unifierM (Left k') (Right k) a = return $ HoleXR (singleZip k') (singleton k a)
	unifierM (Right k') (Left k) a = return $ HoleLX (singleton k a) (singleZip k')
	unifierM (Right k') (Right k) a = Hole0X <$> unifierM k' k a
	
	unifyM (Left k1) a1 (Left k2) a2 = MapL <$> unifyM k1 a1 k2 a2
	unifyM (Left k1) a1 (Right k2) a2 = return $ singleton k1 a1 `mapLR` singleton k2 a2
	unifyM (Right k2) a2 (Left k1) a1 = return $ singleton k1 a1 `mapLR` singleton k2 a2
	unifyM (Right k1) a1 (Right k2) a2 = MapR <$> unifyM k1 a1 k2 a2