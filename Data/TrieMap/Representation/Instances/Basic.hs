{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Data.TrieMap.Representation.Instances.Basic () where

import Data.TrieMap.Representation.Class
import Data.TrieMap.Representation.TH

import Control.Monad

import qualified Data.Vector as V

import Language.Haskell.TH

-- | @'Rep' [a] = 'V.Vector' ('Rep' a)@
instance Repr a => Repr [a] where
	type Rep [a] = V.Vector (Rep a)
	toRep = V.map toRep . V.fromList

$(liftM concat $ mapM (genRepr . tupleTypeName) [2..10])

genOrdRepr ''Float
genOrdRepr ''Double
genRepr ''Maybe
genRepr ''Either
genRepr ''Bool
genRepr ''()
genRepr ''Ordering