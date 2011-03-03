{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, FlexibleInstances, CPP, UndecidableInstances #-}
module Data.TrieMap.Representation.Instances () where

import Data.Tree
import Data.Ratio
import Data.Word
import Data.Bits
import Data.TrieMap.Modifiers
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as P
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

#define DefStream(ty) \
  type RepStream (ty) = DRepStream (ty); \
  toRepStream = dToRepStream

instance Repr a => Repr (S.Set a) where
	type Rep (S.Set a) = V.Vector (Rep a)
	toRep s = toVectorN (\ f -> S.fold (f . toRep)) S.size s
	DefStream(S.Set a)

instance (Repr k, Repr a) => Repr (M.Map k a) where
	type Rep (M.Map k a) = V.Vector (Rep k, Rep a)
	toRep m = toVectorN (\ f -> M.foldrWithKey (\ k a -> f (toRep k, toRep a)))
			M.size m
	DefStream(M.Map k a)

instance Repr a => Repr (Seq.Seq a) where
	type Rep (Seq.Seq a) = V.Vector (Rep a)
	toRep = toVectorF toRep Seq.length
	DefStream(Seq.Seq a)

genRepr ''Tree
genRepr ''Ratio

instance Repr Integer where
	type Rep Integer = Either (Rev (Word, P.Vector Word)) (Word, P.Vector Word)
	toRep x
	  | x < 0	= let bs = unroll (-x); n = fromIntegral (P.length bs) in Left (Rev (n, bs))
	  | otherwise	= let bs = unroll x; n = fromIntegral (P.length bs) in Right (n, bs)
	DefStream(Integer)

unroll :: Integer -> P.Vector Word
unroll x = P.reverse (P.unfoldr split x)
  where	wSize = bitSize (0 :: Word)
	split 0 = Nothing
	split x = Just (fromIntegral x :: Word, shiftR x wSize)