{-# LANGUAGE ParallelListComp, NamedFieldPuns, RecordWildCards #-}
module Data.TrieMap.Representation.TH.Factorized (unify) where

import Control.Exception

import Data.List
import Data.Maybe
import Data.Ord

import Language.Haskell.TH
import Data.TrieMap.Representation.TH.Representation
import Data.TrieMap.Representation.TH.Utils

data FactorCase = FCase {fInput :: [Pat], fFactor :: Exp, fOutput :: Exp}
data Factored = Factored {factorType :: Type, fRestType :: Type, fCases :: [FactorCase]}

factorRepr, otherRepr :: Factored -> Representation
factorRepr Factored{..} =
  Repr {reprType = factorType, cases = map factorCase fCases}
otherRepr  Factored{..} =
  Repr {reprType = fRestType, cases = map otherCase fCases}

factorCase, otherCase :: FactorCase -> Case
factorCase FCase{..} = Case{input = fInput, output = fFactor}
otherCase  FCase{..} = Case{input = fInput, output = fOutput}

caseFactor :: Case -> FactorCase
caseFactor Case{..} = FCase{fInput = input, fFactor = output, fOutput = TupE []}

combFCase ::  Case -> FactorCase -> FactorCase
combFCase Case{..} FCase{..} = 
  assert (input == fInput) $ FCase{fOutput = TupE [output, fOutput], ..}

combFactor :: Representation -> Factored -> Factored
combFactor Repr{..} Factored{fRestType = TupleT 0,..} =
  Factored{factorType, fRestType = reprType, fCases = [FCase{fOutput = output,..} | (FCase{..}, Case{output}) <- zip fCases cases]}
combFactor Repr{..} Factored{..} =
  Factored{factorType, fRestType = reprType `tyProd` fRestType, fCases = zipWith combFCase cases fCases}

factors :: Representation -> [Factored]
factors repr@Repr{..} = case reprType of
  TupleT 2 `AppT` _ `AppT` _
    -> let  fs1 = map (combFactor (sndRepr repr)) (factors (fstRepr repr))
	    fs2 = map (combFactor (fstRepr repr)) (factors (sndRepr repr))
	    in baseFactor:fs1 ++ fs2
  _ -> [baseFactor]
  where baseFactor = Factored {
	  factorType = reprType,
	  fRestType = TupleT 0,
	  fCases = map caseFactor cases}

distinctFactors :: [Representation] -> [Type]
distinctFactors reprs = nub [factorType | repr <- reprs, Factored{factorType} <- factors repr, factorType /= TupleT 0]

factorWith :: Type -> Representation -> Maybe Factored
factorWith fTy repr = listToMaybe [factor | factor@Factored{factorType} <- factors repr, factorType == fTy]

factorOut :: Type -> [Representation] -> ([Factored], [Representation])
factorOut _ [] = ([], [])
factorOut fTy (repr:reprs) = case (factorWith fTy repr, factorOut fTy reprs) of
  (Nothing, (factors, others))	-> (factors, repr:others)
  (Just f, (factors, others))	-> (f:factors, others)

unify :: [Representation] -> Representation
unify reprs = case (allFactors, bestOption) of
  ([], _)	-> checkEnumRepr (mergeWith sumRepr reprs)
  (_, ([_], _))	-> checkEnumRepr (mergeWith sumRepr reprs)
  (_, (factors, []))	 -> distributeMany factors
  (_, (factors, others)) -> distributeMany factors `sumRepr` unify others
  where allFactors = distinctFactors reprs
	options = map (`factorOut` reprs) (distinctFactors reprs)
	bestOption = maximumBy (comparing (length . fst)) options

distributeMany :: [Factored] -> Representation
distributeMany factors =
  foldr1 unifySumRepr (map factorRepr factors) `unifyProdRepr` unify (map otherRepr factors)