module Data.TrieMap.Representation.Instances.Tests where

import qualified Data.TrieMap.Representation.Instances.Prim.Tests as PrimTests
import qualified Data.TrieMap.Representation.Instances.Vectors.Tests as VecTests
import qualified Data.TrieMap.Representation.Instances.ByteString.Tests as BSTests
import qualified Data.TrieMap.Representation.Instances.Foreign.Tests as ForeignTests

import Test.QuickCheck

tests :: Property
tests = conjoin [PrimTests.tests, VecTests.tests, BSTests.tests, ForeignTests.tests]