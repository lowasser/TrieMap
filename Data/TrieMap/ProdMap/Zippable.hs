{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Data.TrieMap.ProdMap.Zippable () where

import Data.TrieMap.ProdMap.Base

instance (Zippable (TrieMap k1), TrieKey k2, Zippable (TrieMap k2)) => Zippable (TrieMap (k1, k2)) where
  empty = PMap empty
  
  clear (PHole h1 h2) = PMap (fill (clearM h2) h1)
  assign a (PHole h1 h2) = PMap (assign (assign a h2) h1)

instance (Alternatable (TrieMap k1), Alternatable (TrieMap k2)) => Alternatable (TrieMap (k1, k2)) where
  alternate (PMap m) = do
    (m', h1) <- alternate m
    (a, h2) <- alternate m'
    return (a, PHole h1 h2)