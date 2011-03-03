{-# LANGUAGE TemplateHaskell, RecordWildCards, NamedFieldPuns, PatternGuards, FlexibleContexts #-}
module Data.TrieMap.Representation.TH.Representation (
  Representation(..),
  Case(..),
  fstRepr,
  sndRepr,
  prodRepr,
  sumRepr,
  unifyProdRepr,
  unifySumRepr,
  checkEnumRepr,
  unitRepr,
  vectorizeRepr,
  mapReprInput,
  conify,
  ordRepr,
  outputRepr,
  recursiveRepr,
  keyRepr) where

import Control.Exception (assert)
import Control.Monad

import Data.Word
import Data.Maybe
import qualified Data.Vector as V

import Language.Haskell.TH.Syntax

import Data.TrieMap.Modifiers
import Data.TrieMap.Representation.Class
import Data.TrieMap.Representation.TH.Utils
import Data.TrieMap.Representation.TH.ReprMonad

data Representation = Repr {reprType :: Type, cases :: [Case]} deriving (Show)
data Case = Case {input :: [Pat], output :: Exp} deriving (Show)

unitRepr :: Representation
unitRepr = Repr {reprType = TupleT 0, cases = [Case [] (TupE [])]}

vectorizeRepr :: Quasi m => Exp -> Representation -> m Representation
vectorizeRepr toVecE Repr{..} = do
  xs <- qNewName "xs"
  eToR <- qNewName "eToR"
  let mapE f xs = VarE 'V.map `AppE` f `AppE` xs
  let eToRDec = FunD eToR (map caseToClause cases)
  return $ Repr {
	reprType = ConT ''V.Vector `AppT` reprType,
	cases = [Case {input = [VarP xs],
			output = mapE (LetE [eToRDec] (VarE eToR)) (toVecE `AppE` VarE xs)}]}

fstRepr, sndRepr :: Representation -> Representation
fstRepr = mapReprOutput fstTy fstExp
sndRepr = mapReprOutput sndTy sndExp

prodCase :: Case -> Case -> Case
prodCase Case{input = input1, output = output1} Case{input = input2, output = output2}
  = Case {input = input1 ++ input2, output = TupE [output1, output2]}

unifyProdCase :: Case -> Case -> Maybe Case
unifyProdCase Case{input = input1, output = output1} Case{input = input2, output = output2}
 = do	guard (input1 == input2)
 	return Case{input = input1, output = TupE [output1, output2]}

mapCaseInput :: ([Pat] -> Pat) -> Case -> Case
mapCaseInput f Case{..} = Case{input = [f input],..}

mapCaseOutput :: (Exp -> Exp) -> Case -> Case
mapCaseOutput f Case{..} = Case{output = f output,..}

prodRepr, sumRepr, unifySumRepr, unifyProdRepr :: Representation -> Representation -> Representation
prodRepr Repr{reprType = repr1, cases = cases1} Repr{reprType = repr2, cases = cases2}
	= Repr {reprType = repr1 `tyProd` repr2, cases = liftM2 prodCase cases1 cases2}

sumRepr Repr{reprType = repr1, cases = cases1} Repr{reprType = repr2, cases = cases2}
	= Repr {reprType = repr1 `tySum` repr2, 
		cases = map (mapCaseOutput leftExp) cases1 ++ map (mapCaseOutput rightExp) cases2}

unifySumRepr Repr{reprType = repr1, cases = cases1} Repr{reprType = repr2, cases = cases2}
  = assert (repr1 == repr2) $ Repr {reprType = repr1, cases = cases1 ++ cases2}

unifyProdRepr Repr{reprType = repr1, cases = cases1} Repr{reprType = repr2, cases = cases2}
  = Repr {reprType = repr1 `tyProd` repr2, cases = catMaybes (liftM2 unifyProdCase cases1 cases2)}

mapReprInput :: ([Pat] -> Pat) -> Representation -> Representation
mapReprInput f Repr{..} = Repr{cases = map (mapCaseInput f) cases, ..}

conify :: Name -> Representation -> Representation
conify con = mapReprInput (ConP con)

mapReprOutput :: (Type -> Type) -> (Exp -> Exp) -> Representation -> Representation
mapReprOutput tyOp outOp Repr{..} = Repr{reprType = tyOp reprType, cases = map (mapCaseOutput outOp) cases}

checkEnumRepr :: Representation -> Representation
checkEnumRepr Repr{..}
  | isEnumTy reprType, length cases > 2
  	= Repr {reprType = ConT ''Word, cases = [Case{input, output = LitE (IntegerL i)} | (i, Case{..}) <- zip [0..] cases]}
checkEnumRepr repr = repr

ordRepr :: Quasi m => Type -> m Representation
ordRepr ty = do
  x <- qNewName "ordK"
  return Repr{reprType = ConT ''Ordered `AppT` ty, 
		cases = [Case {input = [VarP x], output = ConE 'Ord `AppE` VarE x}]}

caseToClause :: Case -> Clause
caseToClause Case{..} = Clause input (NormalB output) []

{-# INLINE toRepListImpl #-}
toRepListImpl :: (Repr a, Repr (Rep a)) => [a] -> RepList (Rep a)
toRepListImpl = toRepList . map toRep

outputRepr :: Cxt -> Type -> Representation -> ReprMonad Type
outputRepr cxt ty Repr{..} = do
  forceDef <- forceDefaultListRep
  let listReps = if forceDef then
	[TySynInstD ''RepList [ty] (ConT ''DRepList `AppT` ty),
	ValD (VarP 'toRepList) (NormalB (VarE 'dToRepList)) []]
	else [TySynInstD ''RepList [ty] (ConT ''RepList `AppT` reprType),
	  ValD (VarP 'toRepList) (NormalB (VarE 'toRepListImpl)) []]
  outputInstance ty reprType
    [InstanceD cxt (ConT ''Repr `AppT` ty)
      ([TySynInstD ''Rep [ty] reprType,
	FunD 'toRep
	  (map caseToClause cases)] ++ listReps)]
  return reprType

recursiveRepr :: Quasi m => Type -> Exp -> m Representation
recursiveRepr reprType toRepE = do
  deep <- qNewName "deep"
  return Repr{reprType, cases = [Case{input = [VarP deep], output = toRepE `AppE` VarE deep}]}

keyRepr :: Quasi m => Type -> m Representation
keyRepr ty = do
  shallow <- qNewName "shallow"
  let keyCon = ConE 'Key
  return Repr{reprType = ConT ''Key `AppT` ty, cases = [Case{input = [VarP shallow], output = keyCon `AppE` VarE shallow}]}