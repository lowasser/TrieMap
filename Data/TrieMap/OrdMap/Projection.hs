{-# LANGUAGE CPP, BangPatterns, UnboxedTuples, TypeSynonymInstances #-}
module Data.TrieMap.OrdMap.Projection () where

import Data.TrieMap.OrdMap.Base

#define TIP SNode{node=Tip}
#define BIN(args) SNode{node=Bin args}

instance Project (SNode k) where
  mapMaybe f = mMaybe where
    mMaybe BIN(k a l r) = joinMaybe k (f a) (mMaybe l) (mMaybe r)
    mMaybe _ = tip
  mapEither f = mEither where
    mEither BIN(k a l r) = (# joinMaybe k aL lL rL, joinMaybe k aR lR rR #)
      where !(# aL, aR #) = f a
	    !(# lL, lR #) = mEither l
	    !(# rL, rR #) = mEither r
    mEither _ = (# tip, tip #)

instance Project (OrdMap k) where
  mapMaybe f (OrdMap m) = OrdMap (mapMaybe f m)
  mapEither f (OrdMap m) = both OrdMap (mapEither f) m