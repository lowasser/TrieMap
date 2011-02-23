{-# LANGUAGE ImplicitParams #-}

module Data.TrieMap.Subset where

type LEq a b = a -> b -> Bool
class Subset f where
  (<=?) :: (?le :: LEq a b) => LEq (f a) (f b)

(<<=?) :: (Subset f, Subset g, ?le :: LEq a b) => LEq (f (g a)) (f (g b))
f <<=? g = let ?le = (<=?) in f <=? g

instance Subset Maybe where
  Nothing <=? _ = True
  Just a <=? Just b = a <?= b
  Just{} <=? Nothing = False

class Nullable f where
  isNull :: f a -> Bool

{-# INLINE guardNull #-}
guardNull :: Nullable f => f a -> Maybe (f a)
guardNull a
  | isNull a	= Nothing
  | otherwise	= Just a

instance Nullable Maybe where
  isNull Nothing = True
  isNull Just{} = False

(<?=) :: (?le :: LEq a b) => LEq a b
(<?=) = ?le