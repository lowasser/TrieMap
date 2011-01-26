{-# LANGUAGE Rank2Types, BangPatterns, MagicHash #-}
module Data.TrieMap.Utils (toVectorN, toVectorF) where

import Data.Vector.Generic
import Data.Vector.Generic.Mutable
import qualified Data.Foldable
import GHC.Exts

{-# INLINE toVectorN #-}
toVectorN :: Vector v a => (forall b . (a -> b -> b) -> b -> f -> b) -> (f -> Int) -> f -> v a
toVectorN fold size xs = create $ do
	!mv <- unsafeNew (size xs)
	fold (\ x m i# -> unsafeWrite mv (I# i#) x >> m (i# +# 1#)) (\ _ -> return mv) xs 0#

{-# INLINE toVectorF #-}
toVectorF :: (Vector v b, Data.Foldable.Foldable f) => (a -> b) -> (f a -> Int) -> f a -> v b
toVectorF g = toVectorN (\ f -> Data.Foldable.foldr (f . g))
