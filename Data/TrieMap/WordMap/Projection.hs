{-# LANGUAGE CPP, UnboxedTuples, FlexibleInstances #-}
module Data.TrieMap.WordMap.Projection () where

import Data.TrieMap.TrieKey

import Data.Word

import Data.TrieMap.WordMap.Base

#define BIN(args) SNode{node = (Bin args)}
#define TIP(args) SNode{node = (Tip args)}
#define NIL SNode{node = Nil}

instance Project SNode where
  mapMaybe f = mMaybe where
    mMaybe BIN(p m l r) = bin p m (mMaybe l) (mMaybe r)
    mMaybe TIP(kx x) = singletonMaybe kx (f x)
    mMaybe NIL = nil
  mapEither f = mEither where
    mEither BIN(p m l r) = (# bin p m l1 r1, bin p m l2 r2 #)
      where !(# l1, l2 #) = mEither l
	    !(# r1, r2 #) = mEither r
    mEither TIP(kx x) = both (singletonMaybe kx) f x
    mEither NIL = (# nil, nil #)

instance Project (TrieMap Word) where
  mapMaybe f (WordMap m) = WordMap (mapMaybe f m)
  mapEither f (WordMap m) = both WordMap (mapEither f) m