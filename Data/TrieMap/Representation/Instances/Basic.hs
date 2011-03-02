{-# LANGUAGE TemplateHaskell, TypeFamilies, UndecidableInstances #-}
module Data.TrieMap.Representation.Instances.Basic () where

import Data.TrieMap.Representation.TH
import Data.TrieMap.Representation.Class
import Data.TrieMap.Modifiers

import Data.Word
import Data.Vector.Primitive (Vector, fromList)

import Language.Haskell.TH

$(fmap concat $ mapM (genBaseRepr . tupleTypeName) [2..10])

genOrdRepr ''Float
genOrdRepr ''Double
genRepr ''Maybe
genBaseRepr ''Either
genRepr ''Ordering

instance Repr Word where
  type Rep Word = Word
  toRep = id
  type RepList Word = Vector Word
  toRepList = fromList

instance Repr () where
  type Rep () = ()
  toRep _ = ()
  type RepList () = Word
  toRepList = fromIntegral . length

instance Repr (Ordered k) where
  type Rep (Ordered k) = Ordered k
  toRep = id
  type RepList (Ordered k) = DRepList (Ordered k)
  toRepList = dToRepList