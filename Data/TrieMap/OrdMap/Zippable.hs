{-# LANGUAGE TypeSynonymInstances #-}
module Data.TrieMap.OrdMap.Zippable () where

import Data.TrieMap.OrdMap.Base

instance Zippable (SNode k) where
  empty = tip
  
  clear (Empty _ path) = rebuild tip path
  clear (Full _ path l r) = rebuild (l `glue` r) path
  assign a (Empty k path) = rebuild (single k a) path
  assign a (Full k path l r) = rebuild (join k a l r) path

instance Zippable (OrdMap k) where
  empty = OrdMap empty
  clear (Hole hole) = OrdMap (clear hole)
  assign a (Hole hole) = OrdMap (assign a hole)

rebuild :: Sized a => SNode k a -> Path k a -> SNode k a
rebuild t Root = t
rebuild t (LeftBin kx x path r) = rebuild (balance kx x t r) path
rebuild t (RightBin kx x l path) = rebuild (balance kx x l t) path