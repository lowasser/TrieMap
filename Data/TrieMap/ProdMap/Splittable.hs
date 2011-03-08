{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module Data.TrieMap.ProdMap.Splittable () where

import Data.TrieMap.ProdMap.Base

instance (Splittable (TrieMap k1), Splittable (TrieMap k2), TrieKey k2) =>
    Splittable (TrieMap (k1, k2)) where
  
  before (PHole h1 h2) = PMap (beforeM (beforeG h2) h1)
  after (PHole h1 h2) = PMap (afterM (afterG h2) h1)
  beforeWith a (PHole h1 h2) = PMap (beforeWith (beforeWith a h2) h1)
  afterWith a (PHole h1 h2) = PMap (afterWith (afterWith a h2) h1)