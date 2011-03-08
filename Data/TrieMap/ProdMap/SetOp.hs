{-# LANGUAGE FlexibleInstances, FlexibleContexts, CPP #-}
module Data.TrieMap.ProdMap.SetOp () where

import Data.TrieMap.ProdMap.Base

#define SETOP(op,opM) op f (PMap m1) (PMap m2) = PMap ((op) ((opM) f) m1 m2)

instance (SetOp (TrieMap k1), SetOp (TrieMap k2), TrieKey k2) =>
    SetOp (TrieMap (k1, k2)) where
  SETOP(union,unionM)
  SETOP(isect,isectM)
  SETOP(diff,diffM)

instance (Subset (TrieMap k1), Subset (TrieMap k2)) => Subset (TrieMap (k1, k2)) where
  PMap m1 <=? PMap m2 = m1 <<=? m2

instance (Project (TrieMap k1), Project (TrieMap k2), TrieKey k2) =>
    Project (TrieMap (k1, k2)) where
  mapMaybe f (PMap m) = PMap (mapMaybe (mapMaybeM f) m)
  mapEither f (PMap m) = both' PMap PMap (mapEither (mapEitherM f)) m