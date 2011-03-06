{-# LANGUAGE NamedFieldPuns, TypeSynonymInstances, FlexibleInstances #-}
module Data.TrieMap.OrdMap.Alternatable () where

import Data.TrieMap.OrdMap.Base

instance Alternatable (SNode k) where
  alternate = alt Root where
    alt path SNode{node} = case node of
      Tip	-> mzero
      Bin kx x l r ->
	alt (LeftBin kx x path l) r `mplus` return (x, Full kx path l r) `mplus`
	  alt (RightBin kx x l path) r

instance Alternatable (TrieMap (Ordered k)) where
  alternate (OrdMap m) = do
    (a, hole) <- alternate m
    return (a, Hole hole)