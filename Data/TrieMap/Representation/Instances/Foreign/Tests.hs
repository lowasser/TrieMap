{-# LANGUAGE CPP #-}
module Data.TrieMap.Representation.Instances.Foreign.Tests where

import Data.TrieMap.Representation.Class
import Data.TrieMap.Representation.Instances.Foreign ()
import Data.TrieMap.Representation.Tests

import Foreign.C.Types

import Test.QuickCheck

#define ARB_INT_INST(ty) \
instance Arbitrary (ty) where {	\
  arbitrary = arbitraryBoundedIntegral; \
  shrink = shrinkIntegral }
  
#define ARB_RF_INST(ty) \
instance Arbitrary (ty) where { \
  arbitrary = arbitrarySizedFractional; \
  shrink = shrinkRealFrac }

ARB_INT_INST(CChar)
ARB_INT_INST(CSChar)
ARB_INT_INST(CUChar)
ARB_INT_INST(CShort)
ARB_INT_INST(CUShort)
ARB_INT_INST(CInt)
ARB_INT_INST(CUInt)
ARB_INT_INST(CLong)
ARB_INT_INST(CULong)
ARB_INT_INST(CLLong)
ARB_INT_INST(CULLong)
ARB_INT_INST(CWchar)
ARB_RF_INST(CFloat)
ARB_RF_INST(CDouble)

tests :: Property
tests = conjoin
  [testRepr (toRep :: ToRep CChar),
    testRepr (toRep :: ToRep CSChar),
    testRepr (toRep :: ToRep CUChar),
    testRepr (toRep :: ToRep CShort),
    testRepr (toRep :: ToRep CUShort),
    testRepr (toRep :: ToRep CInt),
    testRepr (toRep :: ToRep CUInt),
    testRepr (toRep :: ToRep CLong),
    testRepr (toRep :: ToRep CULong),
    testRepr (toRep :: ToRep CLLong),
    testRepr (toRep :: ToRep CULLong),
    testRepr (toRep :: ToRep CWchar),
    testRepr (toRep :: ToRep CFloat),
    testRepr (toRep :: ToRep CDouble)]