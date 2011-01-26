{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, FlexibleInstances #-}
module Data.TrieMap.Representation.Instances () where

import Data.Tree
import Data.Ratio
import Data.Word
import Data.Bits
import Data.TrieMap.Modifiers
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Sequence as Seq

import Data.TrieMap.Utils
import Data.TrieMap.Representation.Class
import Data.TrieMap.Representation.Instances.Prim ()
import Data.TrieMap.Representation.Instances.Basic ()
import Data.TrieMap.Representation.Instances.ByteString ()
import Data.TrieMap.Representation.Instances.Vectors ()
import Data.TrieMap.Representation.Instances.Foreign ()
import Data.TrieMap.Representation.TH

instance Repr a => Repr (S.Set a) where
	type Rep (S.Set a) = V.Vector (Rep a)
	toRep s = toVectorN (\ f -> S.fold (f . toRep)) S.size s

instance (Repr k, Repr a) => Repr (M.Map k a) where
	type Rep (M.Map k a) = V.Vector (Rep k, Rep a)
	toRep m = toVectorN (\ f -> M.foldrWithKey (\ k a -> f (toRep k, toRep a)))
			M.size m

instance Repr a => Repr (Seq.Seq a) where
	type Rep (Seq.Seq a) = V.Vector (Rep a)
	toRep = toVectorF toRep Seq.length

genRepr ''Tree
genRepr ''Ratio

instance Repr Integer where
	type Rep Integer = Either (Rev (Word, S.Vector Word)) (Word, S.Vector Word)
	toRep x
	  | x < 0	= let bs = unroll (-x); n = fromIntegral (S.length bs) in Left (Rev (n, bs))
	  | otherwise	= let bs = unroll x; n = fromIntegral (S.length bs) in Right (n, bs)

unroll :: Integer -> S.Vector Word
unroll x = S.reverse (S.unfoldr split x)
  where	wSize = bitSize (0 :: Word)
	split 0 = Nothing
	split x = Just (fromIntegral x :: Word, shiftR x wSize)