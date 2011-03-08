{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.TrieMap.UnionMap.Base (
  module Data.TrieMap.UnionMap.Base,
  module Data.TrieMap.TrieKey)
  where

import Data.TrieMap.TrieKey

data instance TrieMap (Either k1 k2) a = 
  Empty
  | MapL (TrieMap k1 a)
  | MapR (TrieMap k2 a)
  | Union !Int (TrieMap k1 a) (TrieMap k2 a)
data instance Zipper (TrieMap (Either k1 k2)) a =
  HoleX0 (Hole k1 a)
  | HoleXR (Hole k1 a) (TrieMap k2 a)
  | Hole0X (Hole k2 a)
  | HoleLX (TrieMap k1 a) (Hole k2 a)

{-# INLINE (^) #-}
(^) :: (TrieKey k1, TrieKey k2, Sized a) => Maybe (TrieMap k1 a) -> Maybe (TrieMap k2 a) -> TrieMap (Either k1 k2) a
Nothing ^ Nothing	= Empty
Just m1 ^ Nothing	= MapL m1
Nothing ^ Just m2	= MapR m2
Just m1 ^ Just m2	= Union (sizeM m1 + sizeM m2) m1 m2

mapLR :: (TrieKey k1, TrieKey k2, Sized a) => TrieMap k1 a -> TrieMap k2 a -> TrieMap (Either k1 k2) a
mapLR m1 m2 = Union (sizeM m1 + getSize m2) m1 m2

singletonL :: (TrieKey k1, TrieKey k2, Sized a) => k1 -> a -> TrieMap (Either k1 k2) a
singletonL k a = MapL (singleton k a)

singletonR :: (TrieKey k1, TrieKey k2, Sized a) => k2 -> a -> TrieMap (Either k1 k2) a
singletonR k a = MapR (singleton k a)

data UView k1 k2 a = UView (Maybe (TrieMap k1 a)) (Maybe (TrieMap k2 a))
data HView k1 k2 a = Hole1 (Hole k1 a) (Maybe (TrieMap k2 a))
		    | Hole2 (Maybe (TrieMap k1 a)) (Hole k2 a)		    

{-# INLINE uView #-}
uView :: TrieMap (Either k1 k2) a -> UView k1 k2 a
uView Empty = UView Nothing Nothing
uView (MapL m1) = UView (Just m1) Nothing
uView (MapR m2) = UView Nothing (Just m2)
uView (Union _ m1 m2) = UView (Just m1) (Just m2)

hView :: Hole (Either k1 k2) a -> HView k1 k2 a
hView (HoleX0 hole1) = Hole1 hole1 Nothing
hView (HoleXR hole1 m2) = Hole1 hole1 (Just m2)
hView (Hole0X hole2) = Hole2 Nothing hole2
hView (HoleLX m1 hole2) = Hole2 (Just m1) hole2

hole1 :: Hole k1 a -> Maybe (TrieMap k2 a) -> Hole (Either k1 k2) a
hole1 hole1 Nothing = HoleX0 hole1
hole1 hole1 (Just m2) = HoleXR hole1 m2

hole2 :: Maybe (TrieMap k1 a) -> Hole k2 a -> Hole (Either k1 k2) a
hole2 Nothing hole2 = Hole0X hole2
hole2 (Just m1) hole2 = HoleLX m1 hole2
