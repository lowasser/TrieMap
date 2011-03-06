{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Data.TrieMap.UnionMap.SetOp () where

import Data.TrieMap.UnionMap.Base
import Prelude hiding ((^))
import GHC.Exts

{-# INLINE runUView #-}
runUView :: TrieMap (Either k1 k2) a -> (Maybe (TrieMap k1 a) -> Maybe (TrieMap k2 a) -> r) -> r
runUView Empty f = inline f Nothing Nothing
runUView (MapL mL) f = inline f (Just mL) Nothing
runUView (MapR mR) f = inline f Nothing (Just mR)
runUView (Union _ mL mR) f = inline f (Just mL) (Just mR)

instance (TrieKey k1, TrieKey k2, SetOp (TrieMap k1), SetOp (TrieMap k2)) => SetOp (TrieMap (Either k1 k2)) where
  union f m1 m2 
    | Empty <- m1	= m2
    | otherwise		= runUView m1 (runUView m2 .: run)
    where {-# INLINE run #-}
	  run m1L m1R m2L m2R 
	    | Empty <- m2	= m1
	    | otherwise		= union (unionM f) m1L m2L ^ union (unionM f) m1R m2R
  isect f m1 m2 = runUView m1 (runUView m2 .: run) where
    run m1L m1R m2L m2R = isect (isectM f) m1L m2L ^ isect (isectM f) m1R m2R
  diff _ m1 Empty	= m1
  diff f m1 m2 = runUView m2 (runUView m1 .: run) where
    run m2L m2R m1L m1R = diff (diffM f) m1L m2L ^ diff (diffM f) m1R m2R