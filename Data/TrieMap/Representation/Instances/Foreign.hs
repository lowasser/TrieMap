{-# LANGUAGE TemplateHaskell, TypeFamilies, UndecidableInstances #-}

module Data.TrieMap.Representation.Instances.Foreign () where

import Foreign.C.Types
import Data.TrieMap.Representation.Instances.Prim ()
import Data.TrieMap.Representation.Instances.Basic ()
import Data.TrieMap.Representation.TH

genRepr ''CChar
genRepr ''CSChar
genRepr ''CUChar
genRepr ''CShort
genRepr ''CUShort
genRepr ''CInt
genRepr ''CUInt
genRepr ''CLong
genRepr ''CULong
genRepr ''CWchar
genRepr ''CLLong
genRepr ''CULLong
genRepr ''CFloat
genRepr ''CDouble