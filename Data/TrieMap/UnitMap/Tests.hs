module Data.TrieMap.UnitMap.Tests where

import Data.TrieMap.UnitMap ()

import qualified Data.TrieMap.TrieKey.Tests as TrieKeyTests
import Test.QuickCheck

tests :: Property
tests = TrieKeyTests.tests "Data.TrieMap.UnitMap" ()