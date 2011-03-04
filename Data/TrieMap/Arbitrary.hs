module Data.TrieMap.Arbitrary where

import Data.Vector
import Data.ByteString

import Test.QuickCheck

instance Arbitrary a => Arbitrary (Vector a) where
  arbitrary = fmap fromList arbitrary
  shrink xs = Prelude.map fromList (shrink (toList xs))

instance Arbitrary ByteString where
  arbitrary = fmap pack arbitrary
  shrink xs = Prelude.map pack (shrink (unpack xs))