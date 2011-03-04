{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module Data.TrieMap.ReverseMap.Tests where

import Data.TrieMap.ReverseMap ()
import Data.TrieMap.WordMap ()
import Data.TrieMap.Modifiers

import Data.Word

import qualified Data.TrieMap.TrieKey.Tests as TrieKeyTests
import Test.QuickCheck

deriving instance Arbitrary a => Arbitrary (Rev a)

tests :: Property
tests = TrieKeyTests.tests "Data.TrieMap.ReverseMap" (Rev (0 :: Word))