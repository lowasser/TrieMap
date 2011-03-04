module Data.TrieMap.Tests (tests, main) where

import qualified Data.TrieMap.Representation.Instances.Tests as ReprInstTests

import Test.QuickCheck

tests :: Property
tests = ReprInstTests.tests

main :: IO ()
main = quickCheck tests