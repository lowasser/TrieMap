{-# LANGUAGE NamedFieldPuns, TypeSynonymInstances #-}
module Data.TrieMap.OrdMap.Alternatable () where

import Data.TrieMap.OrdMap.Base

instance Alternatable (SNode k) where
  alternate = alt Root where
    alt path SNode{node} = case node of
      Tip	-> mzero
      Bin kx x l r ->
	alt (LeftBin kx x path l) r `mplus` return (x, Full kx path l r) `mplus`
	  alt (RightBin kx x l path) r