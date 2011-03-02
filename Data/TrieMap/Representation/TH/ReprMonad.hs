{-# LANGUAGE ViewPatterns, TemplateHaskell #-}
module Data.TrieMap.Representation.TH.ReprMonad (
  ReprMonad,
  liftQuasi,
  recurse,
  getInstance,
  outputInstance,
  mustBreak,
  execReprMonad,
  forceDefaultListRep) where

import Data.TrieMap.Representation.Class
import Data.TrieMap.Representation.TH.Utils

import Control.Monad
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.ExpandSyns

type Instances = [(Name, ([Name], Type))]

newtype ReprMonad a = ReprMonad {runReprMonad ::
	Bool -- whether to force default list reps
	-> Instances -- tycons of known instances
	-> [Name] -- tycons of instances in progress (breakpoints of recursive loopies)
	-> Q ([Dec], Instances, a) -- output decs, new known instances
	}

instance Monad ReprMonad where
	return x = ReprMonad $ \ _ knowns _ -> return ([], knowns, x)
	m >>= k = ReprMonad $ \ def knowns breaks -> do
	  (outDecs, knowns', a) <- runReprMonad m def knowns breaks
	  (outDecs', knowns'', b) <- runReprMonad (k a) def knowns' breaks
	  return (outDecs ++ outDecs', knowns'', b)
	fail err = ReprMonad $ \ _ _ _ -> fail err

instance Functor ReprMonad where
	fmap = liftM

liftQuasi :: Q a -> ReprMonad a
liftQuasi q = ReprMonad $ \ _ knowns _ -> do
    a <- q
    return ([], knowns, a)

instance Quasi ReprMonad where
	qNewName = liftQuasi . qNewName
	qReport b str = liftQuasi (qReport b str)
	qRecover m k = ReprMonad $ \ def knowns breaks -> qRecover (runReprMonad m def knowns breaks) (runReprMonad k def knowns breaks)
	qReify = liftQuasi . qReify
	qClassInstances name typs =  liftQuasi (qClassInstances name typs)
	qLocation = liftQuasi qLocation
	qRunIO = liftQuasi . qRunIO

insNub :: Eq a => a -> [a] -> [a]
insNub x ys0@(y:ys)
  | x == y	= ys0
  | otherwise	= y:insNub x ys
insNub x [] = [x]

recurse :: Name -> ReprMonad a -> ReprMonad a
recurse breakTy m = ReprMonad $ \ def knowns breaks -> runReprMonad m def knowns (breakTy `insNub` breaks)

outputInstance :: Type -> Type -> [Dec] -> ReprMonad ()
outputInstance ty tyRep decs = ReprMonad $ \ _ knowns _ -> case decompose' ty of
	Just (tyCon, tyArgs)
		-> return (decs, (tyCon, (tyArgs, tyRep)):knowns, ())
	_	-> return (decs, knowns, ())

getInstance :: Type -> ReprMonad (Maybe Type)
getInstance typ = case decompose typ of
	(ConT tyCon, tyArgs) -> ReprMonad $ \ _ knowns _ -> case lookup tyCon knowns of
	  Nothing	-> return ([], knowns, Nothing)
	  Just (tyArgs', tyRep) -> return ([], knowns, Just $ foldr substInType tyRep (zip tyArgs' tyArgs))
	_ -> return Nothing

mustBreak :: Name -> ReprMonad Bool
mustBreak tyCon = ReprMonad $ \ _ knowns breaks -> return ([], knowns, tyCon `elem` breaks)

execReprMonad :: Bool -> ReprMonad a -> Q [Dec]
execReprMonad def m = do
	ClassI _ instances <- reify ''Repr
	let instanceHeads = [(tyConName, (tyArgs, ConT ''Rep `AppT` compose tyConName tyArgs))
		| ClassInstance{ci_tys = [decompose' -> Just (tyConName, tyArgs)]} <- instances]
	(decs, _, _) <- runReprMonad m def instanceHeads []
	return decs

forceDefaultListRep :: ReprMonad Bool
forceDefaultListRep = ReprMonad $ \ def known _ -> return ([], known, def)