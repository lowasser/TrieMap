{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.TrieMap.RadixTrie.Slice where

import Control.Exception (assert)
import Data.Vector.Generic

import Prelude hiding (length, zip, foldr)

{-# INLINE splitSlice #-}
splitSlice :: Vector v a => Int -> v a -> (v a, a, v a)
splitSlice !i !slice = (takeSlice i slice, slice !$ i, dropSlice (i+1) slice)

takeSlice :: Vector v a => Int -> v a -> v a
takeSlice !n xs = assert (n <= length xs) $ unsafeTake n xs

dropSlice :: Vector v a => Int -> v a -> v a
dropSlice !n xs = assert (n <= length xs) $ unsafeDrop n xs

-- | Do you have any idea how unsafe this method is?  No, because you're STILL SANE ENOUGH TO READ THIS.
unDropSlice :: Vector v a => Int -> v a -> v a
unDropSlice !n = unsafeDrop (-n)

{-# INLINE matchSlice #-}
matchSlice :: (Vector v a, Vector v b) => (a -> b -> z -> z) -> (Int -> Int -> z) -> v a -> v b -> z
matchSlice f = iMatchSlice (const f)

{-# INLINE iMatchSlice #-}
iMatchSlice :: (Vector v a, Vector v b) => (Int -> a -> b -> z -> z) -> (Int -> Int -> z) -> v a -> v b -> z
iMatchSlice f z !xs !ys = matcher 0 where
  !xLen = length xs
  !yLen = length ys
  !len = min xLen yLen
  matcher i
    | i < len	= f i (xs !$ i) (ys !$ i) (matcher (i+1))
    | otherwise	= z xLen yLen

{-# INLINE (!$) #-}
(!$) :: Vector v a => v a -> Int -> a
xs !$ j = assert (j >= 0 && j < length xs) $ unsafeIndex xs j