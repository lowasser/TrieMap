{-# LANGUAGE TypeFamilies, StandaloneDeriving, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Data.TrieMap.ProdMap.Base (
  module Data.TrieMap.ProdMap.Base,
  module Data.TrieMap.TrieKey) where

import Data.TrieMap.TrieKey
import Data.Functor.Immoral

newtype instance TrieMap (k1, k2) a = PMap (TrieMap k1 (TrieMap k2 a))
data instance Zipper (TrieMap (k1, k2)) a = PHole (Hole k1 (TrieMap k2 a)) (Hole k2 a)

deriving instance ImmoralMap (TrieMap k1 (TrieMap k2 a)) (TrieMap (k1, k2) a)