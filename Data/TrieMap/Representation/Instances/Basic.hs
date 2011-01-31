{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Data.TrieMap.Representation.Instances.Basic () where

import Data.TrieMap.Representation.TH
import Data.TrieMap.Representation.Class

import Control.Monad
import Data.Word

import Language.Haskell.TH

$(liftM concat $ mapM (genRepr . tupleTypeName) [2..10])

genOrdRepr ''Float
genOrdRepr ''Double
genRepr ''Maybe
genRepr ''Either
genRepr ''Ordering

instance Repr () where
	type Rep () = ()
	toRep _ = ()
	type RepList () = Word
	toRepList = fromIntegral . length