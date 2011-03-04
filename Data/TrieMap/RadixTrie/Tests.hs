module Data.TrieMap.RadixTrie.Tests (tests) where

import Data.TrieMap.RadixTrie ()
import Data.TrieMap.ProdMap ()
import Data.TrieMap.WordMap ()
import Data.TrieMap.Arbitrary ()

import Data.Word
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as P

import qualified Data.TrieMap.TrieKey.Tests as TrieKeyTests
import Test.QuickCheck

wordVectorTests :: Property
wordVectorTests = TrieKeyTests.tests "Data.TrieMap.RadixTrie/Prim.Vector Word" (P.empty :: P.Vector Word)

normalVectorTests :: Property
normalVectorTests = TrieKeyTests.tests "Data.TrieMap.RadixTrie/Vector (Word,Word)" (V.empty :: V.Vector (Word, Word))

tests :: Property
tests = conjoin [wordVectorTests, normalVectorTests]