{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
module Data.TrieMap.ProdMap.Searchable () where

import Data.TrieMap.ProdMap.Base
import Data.TrieMap.ProdMap.Zippable ()

import Prelude hiding (lookup)

instance (Searchable (TrieMap k1) k1, Searchable (TrieMap k2) k2, TrieKey k2) => 
    Searchable (TrieMap (k1, k2)) (k1, k2) where
  search (k1, k2) (PMap m) nomatch match = search k1 m nomatch1 match1 where
    nomatch1 h1 = nomatch (PHole h1 (singleZip k2))
    match1 m' h1 = search k2 m' nomatch2 match2 where
      nomatch2 h2 = nomatch (PHole h1 h2)
      match2 a h2 = match a (PHole h1 h2)
  
  singleZip (k1, k2) = PHole (singleZip k1) (singleZip k2)
  
  lookup (k1, k2) (PMap m) = lookup k1 m >>= lookup k2
  
  insertWith f (k1, k2) a (PMap m) = PMap $ insertWith (insertWith f k2 a) k1 (singleton k2 a) m
  
  alter f (k1, k2) (PMap m) = PMap $ alter g k1 m where
    g Nothing = singleton k2 <$> f Nothing
    g (Just m') = guardNull $ alter f k2 m'