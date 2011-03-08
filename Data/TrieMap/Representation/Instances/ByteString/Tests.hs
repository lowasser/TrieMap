module Data.TrieMap.Representation.Instances.ByteString.Tests where

import Data.TrieMap.Representation.Instances.ByteString ()
import Data.TrieMap.Representation.Class
import Data.TrieMap.Representation.Tests
import Data.TrieMap.Arbitrary ()
import Data.ByteString

import Test.QuickCheck

tests :: Property
tests = printTestCase "ByteString" $ testRepr (toRep :: ToRep ByteString)