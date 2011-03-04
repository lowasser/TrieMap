module Data.TrieMap.Tests (tests, main) where

import qualified Data.TrieMap.Representation.Instances.Tests as ReprInst
import qualified Data.TrieMap.Representation.TH.Tests as ReprTH
import qualified Data.TrieMap.WordMap.Tests as WordMapTests
import qualified Data.TrieMap.OrdMap.Tests as OrdMapTests
import qualified Data.TrieMap.UnitMap.Tests as UnitMapTests
import qualified Data.TrieMap.UnionMap.Tests as UnionMapTests
import qualified Data.TrieMap.ProdMap.Tests as ProdMapTests
import qualified Data.TrieMap.ReverseMap.Tests as ReverseMapTests
import qualified Data.TrieMap.RadixTrie.Tests as RadixTrieTests

import Test.QuickCheck

tests :: Property
tests = conjoin [ReprInst.tests, ReprTH.tests, WordMapTests.tests, OrdMapTests.tests, UnitMapTests.tests,
  UnionMapTests.tests, ProdMapTests.tests, ReverseMapTests.tests, RadixTrieTests.tests]

main :: IO ()
main = quickCheckWith stdArgs{maxSize = 50} tests