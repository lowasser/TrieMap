module Data.TrieMap.Tests (tests, main) where

import qualified Data.TrieMap.Representation.Instances.Tests as ReprInst
import qualified Data.TrieMap.Representation.TH.Tests as ReprTH

import Test.QuickCheck

tests :: Property
tests = conjoin [ReprInst.tests, ReprTH.tests]

main :: IO ()
main = quickCheck tests