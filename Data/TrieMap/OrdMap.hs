{-# LANGUAGE CPP #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.TrieMap.OrdMap () where

import Data.TrieMap.TrieKey
import Data.TrieMap.OrdMap.Base
import Data.TrieMap.OrdMap.Alternatable ()
import Data.TrieMap.OrdMap.Searchable ()
import Data.TrieMap.OrdMap.Indexable ()
import Data.TrieMap.OrdMap.Traversable ()
import Data.TrieMap.OrdMap.Subset ()
import Data.TrieMap.OrdMap.SetOp ()
import Data.TrieMap.OrdMap.Projection ()
import Data.TrieMap.OrdMap.Splittable ()
import Data.TrieMap.OrdMap.Buildable ()

#define TIP SNode{node = Tip}
#define BIN(args) SNode{node = (Bin args)}

-- | @'TrieMap' ('Ordered' k) a@ is based on "Data.Map".
instance Ord k => TrieKey (Ordered k) where
	getSimpleM (OrdMap m) = case m of
		TIP	-> Null
		BIN(_ a TIP TIP)
			-> Singleton a
		_	-> NonSimple
	sizeM (OrdMap m) = sz m

	unifierM (Ord k') (Ord k) a = case compare k' k of
	  EQ	-> mzero
	  LT	-> return $ Hole $ Empty k' (LeftBin k a Root tip)
	  GT	-> return $ Hole $ Empty k' (RightBin k a tip Root)
	unifyM (Ord k1) a1 (Ord k2) a2 = case compare k1 k2 of
	  EQ	-> mzero
	  LT	-> return $ OrdMap $ bin k1 a1 tip (singleton k2 a2)
	  GT	-> return $ OrdMap $ bin k1 a1 (singleton k2 a2) tip