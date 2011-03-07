{-# LANGUAGE TemplateHaskell, TypeFamilies, UndecidableInstances #-}
module Data.TrieMap.Representation.TH.Tests where

import Control.Applicative

import Data.TrieMap.Representation.Class
import Data.TrieMap.Representation.TH
import Data.TrieMap.Representation.Instances ()
import Data.TrieMap.Representation.Tests
import Data.TrieMap.Arbitrary ()

import Data.Int
import Data.ByteString

import Test.QuickCheck

data Key1 = A1 (ByteString, Int) | B1 Int [Int64] | C1 [Bool] | D1 [Char] | E1 (Either String Double) deriving (Eq, Ord, Show)

data Key2 = A2 (ByteString, Int) | B2 Int [Int64] | C2 [Bool] | D2 [Char] | E2 (Either String Double) deriving (Eq, Ord, Show)

data Key3 = A3 (ByteString, Int) | B3 Int [Int64] | C3 [Bool] | D3 [Char] | E3 (Either String Double) deriving (Eq, Ord, Show)

instance Arbitrary Key1 where
	arbitrary = oneof [A1 <$> arbitrary,
				B1 <$> arbitrary <*> arbitrary,
				C1 <$> arbitrary,
				D1 <$> arbitrary,
				E1 <$> arbitrary]

instance Arbitrary Key2 where
	arbitrary = oneof [A2 <$> arbitrary,
				B2 <$> arbitrary <*> arbitrary,
				C2 <$> arbitrary,
				D2 <$> arbitrary,
				E2 <$> arbitrary]

instance Arbitrary Key3 where
	arbitrary = oneof [A3 <$> arbitrary,
				B3 <$> arbitrary <*> arbitrary,
				C3 <$> arbitrary,
				D3 <$> arbitrary,
				E3 <$> arbitrary]

genRepr ''Key1
genOrdRepr ''Key2
genOptRepr ''Key3

tests :: Property
tests = conjoin
  [printTestCase "genRepr" $ testRepr (toRep :: ToRep Key1),
    printTestCase "genOrdRepr" $ testRepr (toRep :: ToRep Key2),
    printTestCase "genOptRepr" $ \ a1 a2 -> ((a1 :: Key3) == a2) == (toRep a1 == toRep a2)]