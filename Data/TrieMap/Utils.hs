{-# LANGUAGE Rank2Types, BangPatterns, MagicHash #-}
module Data.TrieMap.Utils where

import Data.Bits
import qualified Data.Foldable

import Data.Vector.Generic
import Data.Vector.Generic.Mutable

import GHC.Exts

{-# INLINE toVectorN #-}
toVectorN :: Vector v a => (forall b . (a -> b -> b) -> b -> f -> b) -> (f -> Int) -> f -> v a
toVectorN fold size xs = create $ do
	!mv <- unsafeNew (size xs)
	fold (\ x m i# -> unsafeWrite mv (I# i#) x >> m (i# +# 1#)) (\ _ -> return mv) xs 0#

{-# INLINE toVectorF #-}
toVectorF :: (Vector v b, Data.Foldable.Foldable f) => (a -> b) -> (f a -> Int) -> f a -> v b
toVectorF g = toVectorN (\ f -> Data.Foldable.foldr (f . g))

{-# INLINE quoPow #-}
quoPow :: Int -> Int -> Int
n `quoPow` 1 = n
n `quoPow` 2 = n `shiftR` 1
n `quoPow` 4 = n `shiftR` 2
n `quoPow` 8 = n `shiftR` 3
n `quoPow` 16 = n `shiftR` 4
n `quoPow` 32 = n `shiftR` 5
n `quoPow` 64 = n `shiftR` 6
n `quoPow` k = n `quot` k

{-# INLINE remPow #-}
remPow :: Int -> Int -> Int
n `remPow` k = if k .&. (k-1) == 0 then n .&. (k-1) else n `rem` k

(.<<.) :: Word -> Int -> Word
W# w# .<<. I# i# = W# (uncheckedShiftL# w# i#)

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) a b = f (g a b)