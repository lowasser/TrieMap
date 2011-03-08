{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples, NamedFieldPuns, TypeSynonymInstances #-}
module Data.TrieMap.OrdMap.Indexable () where

import Data.TrieMap.OrdMap.Base
import Data.TrieMap.OrdMap.Zippable ()

import GHC.Exts

instance Indexable (OrdMap k) where
  index i (OrdMap m) = case index i m of
    (# i', a, hole #) -> (# i', a, Hole hole #)

instance Indexable (SNode k) where
  index = indexT Root where
    indexT path !i SNode{sz, node = Bin kx x l r}
      | i <# sl	= indexT (LeftBin kx x path r) i l
      | i <# sx	= (# i -# sl, x, Full kx path l r #)
      | otherwise	= indexT (RightBin kx x l path) (i -# sx) r
	where !sl = getSize# l
	      !sx = unbox $ sz - getSize r
    indexT _ _ _ = indexFail ()
