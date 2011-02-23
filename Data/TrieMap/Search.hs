module Data.TrieMap.Search where

type SearchCont h a r = (h -> r) -> (a -> h -> r) -> r

{-# INLINE mapSearch #-}
mapSearch :: (hole -> hole') -> SearchCont hole a r -> SearchCont hole' a r
mapSearch f run nomatch match = run nomatch' match' where
  nomatch' hole = nomatch (f hole)
  match' a hole = match a (f hole)

