{-# LANGUAGE FlexibleContexts #-}
module Data.TrieMap.Representation.Tests where

import Data.TrieMap.Representation.Class
import Test.QuickCheck

import Data.Vector
import Data.ByteString
import Data.String

type ToRep a = a -> Rep a

testRepr :: (Arbitrary a, Repr a, Show a, Ord a, Ord (Rep a)) => ToRep a -> Property
testRepr toRep = property (\ a1 a2 -> compare a1 a2 == compare (toRep a1) (toRep a2))

conjoinN :: Testable a => [a] -> Property
conjoinN tests = property $ oneof (Prelude.map property tests)

instance Arbitrary a => Arbitrary (Vector a) where
  arbitrary = fmap fromList arbitrary
  shrink xs = Prelude.map fromList (shrink (toList xs))

instance Arbitrary ByteString where
  arbitrary = fmap pack arbitrary
  shrink xs = Prelude.map pack (shrink (unpack xs))