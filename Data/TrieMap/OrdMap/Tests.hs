{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module Data.TrieMap.OrdMap.Tests where

import Data.TrieMap.OrdMap
import Data.TrieMap.Modifiers

import qualified Data.TrieMap.TrieKey.Tests as TrieKeyTests
import Test.QuickCheck

deriving instance Arbitrary a => Arbitrary (Ordered a)

tests :: Property
tests = TrieKeyTests.tests "Data.TrieMap.OrdMap" (Ord (0.0 :: Double))