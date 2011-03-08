{-# LANGUAGE TemplateHaskell, TypeFamilies, UndecidableInstances #-}
module Data.TrieMap.Representation.Instances.Basic () where

import Data.TrieMap.Representation.TH
import Data.TrieMap.Representation.Class

import Data.Word
import Data.Vector.Fusion.Stream.Monadic (length)

import Language.Haskell.TH
import Prelude hiding (length)

$(fmap concat $ mapM (genBaseRepr . tupleTypeName) [2..10])

genOrdRepr ''Float
genOrdRepr ''Double
genRepr ''Maybe
genBaseRepr ''Either

instance Repr () where
  type Rep () = ()
  toRep _ = ()
  type RepStream () = Word
  toRepStreamM strm = fmap fromIntegral (length strm)