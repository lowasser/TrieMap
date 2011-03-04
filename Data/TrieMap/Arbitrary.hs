module Data.TrieMap.Arbitrary where

import Data.Vector
import Data.ByteString

import Test.QuickCheck

import qualified Data.Vector.Primitive as P

instance Arbitrary a => Arbitrary (Vector a) where
  arbitrary = fmap fromList arbitrary
  shrink xs = Prelude.map fromList (shrink (toList xs))

instance Arbitrary ByteString where
  arbitrary = fmap pack arbitrary
  shrink xs = Prelude.map pack (shrink (unpack xs))

instance (P.Prim a, Arbitrary a) => Arbitrary (P.Vector a) where
  arbitrary = fmap P.fromList arbitrary
  shrink xs = Prelude.map P.fromList (shrink (P.toList xs))