{-# LANGUAGE TemplateHaskell, TypeFamilies, UndecidableInstances #-}
module Data.TrieMap.Representation.Instances.Basic () where

import Data.TrieMap.Representation.TH
import Data.TrieMap.Representation.Class

import Data.Word
import Data.Vector.Primitive (Vector)
import Data.Vector.Generic (unstream)
import Data.Vector.Fusion.Stream (length)

import Language.Haskell.TH
import Prelude hiding (length)

$(fmap concat $ mapM (genBaseRepr . tupleTypeName) [2..10])

genOrdRepr ''Float
genOrdRepr ''Double
genRepr ''Maybe
genBaseRepr ''Either
genRepr ''Ordering

instance Repr Word where
  type Rep Word = Word
  toRep = id
  type RepStream Word = Vector Word
  toRepStream = unstream

instance Repr () where
  type Rep () = ()
  toRep _ = ()
  type RepStream () = Word
  toRepStream = fromIntegral . length