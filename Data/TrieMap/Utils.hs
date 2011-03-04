{-# LANGUAGE Rank2Types, BangPatterns, MagicHash, TypeOperators #-}
module Data.TrieMap.Utils where

import Control.Monad.Unpack
import Control.Monad.Primitive

import Data.Bits
import qualified Data.Foldable

import Data.Vector.Generic
import Data.Vector.Generic.Mutable
import Data.Vector.Fusion.Stream (MStream)

import GHC.Exts

{-# INLINE mapInput #-}
mapInput :: (Unpackable a, Unpackable b) => (a -> b) -> (b :~> c) -> (a :~> c)
mapInput f func = unpack $ \ a -> func $~ f a

{-# INLINE toVectorN #-}
toVectorN :: Vector v a => (forall b . (a -> b -> b) -> b -> f -> b) -> (f -> Int) -> f -> v a
toVectorN fold size xs = create $ do
	!mv <- unsafeNew (size xs)
	fold (\ x m i# -> unsafeWrite mv (I# i#) x >> m (i# +# 1#)) (\ _ -> return mv) xs 0#

{-# INLINE unstreamM #-}
unstreamM :: (Vector v a, PrimMonad m) => MStream m a -> m (v a)
unstreamM strm = munstream strm >>= unsafeFreeze

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

compl :: Word -> Word
compl (W# w#) = W# (not# w#)

(.<<.) :: Word -> Int -> Word
W# w# .<<. I# i# = W# (uncheckedShiftL# w# i#)

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) a b = f (g a b)

{-# RULES
	"or 0" forall w# . or# w# 0## = w#;
	"0 or" forall w# . or# 0## w# = w#;
	"shiftL 0" forall w# . uncheckedShiftL# w# 0# = w#;
	"plusAddr 0" forall a# . plusAddr# a# 0# = a#;
	#-}