module Data.TrieMap.WordMap.Tests where

import Data.TrieMap.WordMap

import Data.Word

import qualified Data.TrieMap.TrieKey.Tests as TrieKeyTests
import Test.QuickCheck

tests :: Property
tests = TrieKeyTests.tests "Data.TrieMap.WordMap" (0 :: Word)