{-# LANGUAGE TemplateHaskell #-}
module Data.TrieMap.Representation.TH.Utils where

import Language.Haskell.TH
import Language.Haskell.TH.ExpandSyns

decompose :: Type -> (Type, [Type])
decompose (tyfun `AppT` ty) = case decompose tyfun of
	(tyfun, tys)	-> (tyfun, tys ++ [ty])
decompose ty = (ty, [])

decompose' :: Type -> Maybe (Name, [Name])
decompose' (tyfun `AppT` VarT ty) = do
	(tyfun, tys) <- decompose' tyfun
	return (tyfun, tys ++ [ty])
decompose' (ConT ty) = return (ty, [])
decompose' _ = Nothing

compose :: Name -> [Name] ->  Type
compose tyCon tyArgs = foldl AppT (ConT tyCon) (map VarT tyArgs)

tyVarBndrVar :: TyVarBndr -> Name
tyVarBndrVar (PlainTV tyvar) = tyvar
tyVarBndrVar (KindedTV tyvar _) = tyvar

tyVarBndrType :: TyVarBndr -> Type
tyVarBndrType = VarT . tyVarBndrVar

tyProd, tySum :: Type -> Type -> Type
tyProd t1 t2 = TupleT 2 `AppT` t1 `AppT` t2
tySum t1 t2 = ConT ''Either `AppT` t1 `AppT` t2

fstExp, sndExp :: Exp -> Exp
fstExp (TupE [e, _]) = e
fstExp e = VarE 'fst `AppE` e
sndExp (TupE [_, e]) = e
sndExp e = VarE 'snd `AppE` e

leftN, rightN :: Name
leftN = 'Left
rightN = 'Right

leftExp, rightExp :: Exp -> Exp
leftExp = AppE (ConE leftN)
rightExp = AppE (ConE rightN)

fstTy, sndTy :: Type -> Type
fstTy (TupleT 2 `AppT` t1 `AppT` _) = t1
fstTy _ = error "Error: not a pair type"
sndTy (TupleT 2 `AppT` _ `AppT` t2) = t2
sndTy _ = error "Error: not a pair type"

isEnumTy :: Type -> Bool
isEnumTy (ConT eith `AppT` t1 `AppT` t2)
	= eith == ''Either && isEnumTy t1 && isEnumTy t2
isEnumTy (TupleT 0)
	= True
isEnumTy _ = False

type AlgCon = (Name, [Type])

algCon :: Con -> AlgCon
algCon (NormalC name args) = (name, map snd args)
algCon (RecC name args) = (name, [argTy | (_, _, argTy) <- args])
algCon (InfixC (_, ty1) name (_, ty2)) = (name, [ty1, ty2])
algCon _ = error "Error: universally quantified constructors are not algebraic"

substInAlgCon :: (Name, Type) -> AlgCon -> AlgCon
substInAlgCon sub (conName, args) = (conName, map (substInType sub) args)

substInPred :: (Name, Type) -> Pred -> Pred
substInPred sub (ClassP cName tys) = ClassP cName (map (substInType sub) tys)
substInPred sub (EqualP ty1 ty2) = EqualP (substInType sub ty1) (substInType sub ty2)

mergeWith :: (a -> a -> a) -> [a] -> a
mergeWith _ [a] = a
mergeWith _ [] = error "Error: mergeWith called with empty list"
mergeWith f xs = mergeWith f (combine xs) where
  combine (x1:x2:xs) = f x1 x2:combine xs
  combine xs = xs