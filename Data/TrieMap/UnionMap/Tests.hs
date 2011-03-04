module Data.TrieMap.UnionMap.Tests where

import Data.TrieMap.UnionMap ()
import Data.TrieMap.WordMap ()

import Data.Word

import qualified Data.TrieMap.TrieKey.Tests as TrieKeyTests
import Test.QuickCheck

tests :: Property
tests = TrieKeyTests.tests "Data.TrieMap.UnionMap" (Left 0 :: Either Word Word)