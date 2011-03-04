module Data.TrieMap.ProdMap.Tests where

import Data.TrieMap.ProdMap
import Data.TrieMap.WordMap

import Data.Word

import qualified Data.TrieMap.TrieKey.Tests as TrieKeyTests
import Test.QuickCheck

tests :: Property
tests = TrieKeyTests.tests "Data.TrieMap.ProdMap" (0 :: Word, 0 :: Word)