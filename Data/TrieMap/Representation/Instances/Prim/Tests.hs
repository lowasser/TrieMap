{-# LANGUAGE CPP #-}
module Data.TrieMap.Representation.Instances.Prim.Tests where

import Data.TrieMap.Representation.Tests
import Data.TrieMap.Representation.Class
import Data.TrieMap.Representation.Instances.Prim

import Data.Int
import Data.Word

import Test.QuickCheck

#define TEST(ty) printTestCase "ty" (testRepr (toRep :: ToRep (ty)))

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