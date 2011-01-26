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

$(let genTupleRepr n = do
	let ts = [mkName [a] | a <- take n ['a'..]]
	xs <- sequence [newName [a] | a <- take n ['a'..]]
	let toR = 'toRep
	let tupleT = foldl AppT (TupleT n) [VarT t | t <- ts]
	return [InstanceD [ClassP ''Repr [VarT t] | t <- ts]
	  (ConT ''Repr `AppT` tupleT)
	  [TySynInstD ''Rep [tupleT] (foldl AppT (TupleT n) [ConT ''Rep `AppT` VarT t | t <- ts]),
	      FunD toR
		[Clause [TupP [VarP x | x <- xs]]
		  (NormalB (TupE [VarE toR `AppE` VarE x |  x <- xs])) []] {-,
	      FunD fromR
		[Clause [TupP [VarP xRep | xRep <- xReps]]
		  (NormalB (TupE [VarE fromR `AppE` VarE xRep | xRep <- xReps])) []] -}]]
  in liftM concat $ mapM genTupleRepr [2..10])

genOrdRepr ''Float
genOrdRepr ''Double
genRepr ''Maybe
genRepr ''Either
genRepr ''Bool
genRepr ''()
genRepr ''Ordering