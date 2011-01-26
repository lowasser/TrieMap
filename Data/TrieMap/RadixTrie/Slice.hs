{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.TrieMap.RadixTrie.Slice where

import Control.Exception (assert)
import Data.Vector.Generic
import qualified Data.Vector as V

import Prelude hiding (length, zip, foldr)

data Slice v a = Slice {sliceSrc :: v a, _sliceIx :: !Int, len :: !Int}

{-# INLINE splitSlice #-}
splitSlice :: Vector v a => Int -> Slice v a -> (Slice v a, a, Slice v a)
splitSlice !i !slice = (takeSlice i slice, slice !$ i, dropSlice (i+1) slice)

takeSlice :: Int -> Slice v a -> Slice v a
takeSlice !n (Slice xs i _) = Slice xs i n

dropSlice :: Int -> Slice v a -> Slice v a
dropSlice !m (Slice xs i n) = assert (n >= m) $ Slice xs (i+m) (n-m)

unDropSlice :: Int -> Slice v a -> Slice v a
unDropSlice !m (Slice xs i n) = assert (i >= m) $ Slice xs (i-m) (n+m)

{-# INLINE s2V #-}
s2V :: Vector v a => Slice v a -> v a
s2V (Slice xs i n) = assert (i >= 0) $ assert (i + n < length xs) $ unsafeSlice i n xs

{-# INLINE v2S #-}
v2S :: Vector v a => v a -> Slice v a
v2S xs = Slice xs 0 (length xs)

{-# INLINE matchSliceV #-}
matchSliceV :: (Vector v a, Vector v b) => (a -> b -> z -> z) -> (Int -> Int -> z) -> v a -> Slice v b -> z
matchSliceV f z !xs !ys = foldr (\ (a, b) -> f a b) (z (length xs) (len ys)) (V.zip (convert xs) (convert $ s2V ys))

{-# INLINE matchSlice #-}
matchSlice :: (Vector v a, Vector v b) => (a -> b -> z -> z) -> (Int -> Int -> z) -> Slice v a -> Slice v b -> z
matchSlice f z !xs !ys = foldr (\ (a, b) -> f a b) (z (len xs) (len ys)) (V.zip (convert $ s2V xs) (convert $ s2V ys))

{-# INLINE iMatchSlice #-}
iMatchSlice :: (Vector v a, Vector v b) => (Int -> a -> b -> z -> z) -> (Int -> Int -> z) -> Slice v a -> Slice v b -> z
iMatchSlice f z !xs !ys = ifoldr (\ i (a, b) -> f i a b) (z (len xs) (len ys)) (V.zip (convert $ s2V xs) (convert $ s2V ys))

{-# INLINE (!$) #-}
(!$) :: Vector v a => Slice v a -> Int -> a
Slice xs i n !$ j = assert (j >= 0 && j < n) $ unsafeIndex xs (i + j)