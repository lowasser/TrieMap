{-# LANGUAGE CPP #-}
module Data.TrieMap.Representation.Instances.Vectors.Tests where

import Data.TrieMap.Representation.Class
import Data.TrieMap.Representation.Instances.Vectors ()
import Data.TrieMap.Representation.Instances.Prim ()
import Data.TrieMap.Representation.Tests
import Data.TrieMap.Arbitrary ()

import Data.Vector (Vector)
import Data.Int
import Data.Word

import Test.QuickCheck

#define TEST(ty) printTestCase "Vector ty" (testRepr (toRep :: ToRep (Vector ty)))

tests :: Property
tests = conjoin
  [TEST(Int),
    TEST(Int8),
    TEST(Int16),
    TEST(Int32),
    TEST(Int64),
    TEST(Word),
    TEST(Word8),
    TEST(Word16),
    TEST(Word32),
    TEST(Word64),
    TEST(Char),
    TEST(Bool)]