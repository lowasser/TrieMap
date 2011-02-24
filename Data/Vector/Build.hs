{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns #-}
module Data.Vector.Build where

import Control.Monad.Primitive
import Data.Vector.Generic (Vector, create)
import Data.Vector.Generic.Mutable

import Data.Foldable

import Prelude hiding (foldr)
import GHC.Exts

{-# INLINE toMVectorMapN #-}
toMVectorMapN :: (Foldable f, PrimMonad m, MVector v b) => 
  Int -> (a -> b) -> f a -> m (v (PrimState m) b)
toMVectorMapN !n f xs = do
  !mv <- new n
  let writer a k i# = unsafeWrite mv (I# i#) (f a) >> k (i# +# 1#)
  foldr writer (\ _ -> return ()) xs 0#
  return mv

{-# INLINE toVectorMapN #-}
toVectorMapN :: (Foldable f, Vector v b) => Int -> (a -> b) -> f a -> v b
toVectorMapN !n f xs = create (toMVectorMapN n f xs)