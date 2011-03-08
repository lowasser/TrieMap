{-# LANGUAGE FlexibleInstances, FlexibleContexts, UnboxedTuples #-}
module Data.TrieMap.ProdMap.Indexable () where

import Data.TrieMap.ProdMap.Base

instance (Indexable (TrieMap k1), Indexable (TrieMap k2), TrieKey k2) =>
    Indexable (TrieMap (k1, k2)) where
  index i (PMap m) = case index i m of
    (# i', m', h1 #) -> case index i' m' of
      (# i'', a, h2 #) -> (# i'', a, PHole h1 h2 #)