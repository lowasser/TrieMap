{-# LANGUAGE BangPatterns, TypeFamilies, TemplateHaskell, PatternGuards, DoAndIfThenElse, ImplicitParams #-}

module Data.TrieMap.Representation.TH (genRepr, genOptRepr, genOrdRepr) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.ExpandSyns

import qualified Data.Vector as V

import Data.TrieMap.Representation.Class
import Data.TrieMap.Representation.TH.Utils
import Data.TrieMap.Representation.TH.Representation
import Data.TrieMap.Representation.TH.Factorized
import Data.TrieMap.Representation.TH.ReprMonad

-- | Given a type with an associated 'Ord' instance, generates a representation that will cause its 'TMap'
-- implementation to be essentially equivalent to "Data.Map".
genOrdRepr :: Name -> Q [Dec]
genOrdRepr tycon = execReprMonad $ do
	(cxt, ty, _) <- getDataForName tycon
	outputRepr cxt ty =<< ordRepr ty

getDataForName :: Quasi m => Name -> m (Cxt, Type, [AlgCon])
getDataForName tycon = do
	TyConI dec <- qReify tycon
	let theTyp = compose tycon . map tyVarBndrVar
	case dec of
		DataD cxt _ tyvars cons _ ->
			return (cxt, theTyp tyvars, map algCon cons)
		NewtypeD cxt _ tyvars con _ ->
			return (cxt, theTyp tyvars, [algCon con])
		_ -> error "Error: could not get kind of type constructor"

getDataForType :: Quasi m => Type -> m (Cxt, [AlgCon])
getDataForType ty
  | (ConT tyCon, args) <- decompose ty
    = do  TyConI dec <- qReify tyCon
	  let subAll tyvars cxt cons = let subs = zip (map tyVarBndrVar tyvars) args in
		([foldr substInPred p subs | p <- cxt], [foldr substInAlgCon (algCon con) subs | con <- cons])
	  case dec of
		DataD cxt _ tyvars cons _ ->
		  return (subAll tyvars cxt cons)
		NewtypeD cxt _ tyvars con _ ->
		  return (subAll tyvars cxt [con])
		_ -> failure
  | otherwise	= failure
  where failure = fail "Error: could not reify type constructor"

-- | Given the name of a type constructor, automatically generates an efficient 'Repr' instance.
-- If you have several mutually dependent (or even mutually recursive) types, 'genRepr' will
-- construct instances for all of them.  
-- 
-- 'genRepr' guarantees that any instances it generates are consistent with the ordering that
-- would be generated by @deriving ('Ord')@ in the data declaration.  That is, if 'genRepr'
-- generates an instance @Repr a@, then it is guaranteed that if @x, y :: a@, and @a@
-- has a derived 'Ord' instance, then @compare x y == compare (toRep x) (toRep y)@.
genRepr :: Name -> Q [Dec]
genRepr tyCon = execReprMonad $ do
  (_, ty, _) <- getDataForName tyCon
  let ?combine = mergeWith sumRepr
  genReprMain ty

-- | Given the name of a type constructor, automatically generates an efficient 'Repr' instance.
-- If you have several mutually dependent (or even mutually recursive) types, 'genOptRepr' will
-- construct instances for all of them.  The instance generated by 'genOptRepr' may, in some
-- cases, be more efficient than the instance generated by 'genRepr' -- in particular,
-- arguments common to several constructors may be factored out, reducing the complexity of the
-- associated 'TrieKey' instance, but leaving an ordering inconsistent with 'Ord'.
-- 
-- Therefore, 'genOptRepr' guarantees that any instances it generates are consistent with the
-- ordering that would be generated by @deriving ('Eq')@ in the data declaration.  That is, if
-- 'genOptRepr' generates an instance @Repr a@, then it is guaranteed that if @x, y :: a@, and
-- @a@ has a derived 'Eq' instance, then @(x == y) == (toRep x == toRep y)@.
genOptRepr :: Name -> Q [Dec]
genOptRepr tyCon = execReprMonad $ do
  (_, ty, _) <- getDataForName tyCon
  let ?combine = unify
  genReprMain ty

mustBreakTy :: Type -> ReprMonad Bool
mustBreakTy ty = case decompose ty of
  (ConT tyCon,  _) -> mustBreak tyCon
  _		   -> return False

recurseTy :: Type -> ReprMonad a -> ReprMonad a
recurseTy ty m = case decompose ty of
  (ConT tyCon, _) -> recurse tyCon m
  _		  -> m

genReprMain :: (?combine :: [Representation] -> Representation) => Type -> ReprMonad Type
genReprMain ty = do
  breakTy <- mustBreakTy ty
  if breakTy then fail "Cannot recurse here"
  else do
    knownInst <- getInstance ty
    case knownInst of
      Just known -> return known
      Nothing	 -> do
	(cxt, cons) <- getDataForType ty
	conReprs <- mapM (recurseTy ty . conRepr) cons
	outputRepr cxt ty (checkEnumRepr $ ?combine conReprs)

conRepr :: (?combine :: [Representation] -> Representation) => AlgCon -> ReprMonad Representation
conRepr (con, []) = return $ conify con unitRepr
conRepr (con, args) = do
	argReprs <- mapM typeRepr args
	return (conify con (foldr1 prodRepr argReprs))

typeRepr :: (?combine :: [Representation] -> Representation) => Type -> ReprMonad Representation
typeRepr t00 = liftQuasi (expandSyns t00) >>= \ t0 -> case decompose t0 of
	(ListT, [t])	-> do
		tRepr <- typeRepr t
		vectorizeRepr (VarE 'V.fromList) tRepr
	(TupleT 0, _)	-> return unitRepr
	(TupleT _, ts)	-> do
		reps <- mapM typeRepr ts
		return $ mapReprInput TupP $ mergeWith prodRepr reps
	(ConT con, ts)
		| con == ''()	-> return unitRepr
		| con == ''Either, [tL, tR] <- ts
			-> do	reprL <- typeRepr tL
				reprR <- typeRepr tR
				return (mapReprInput (ConP leftN) reprL `sumRepr` mapReprInput (ConP rightN) reprR)
		| con == ''Maybe, [t] <- ts
			-> do	tRepr <- typeRepr t
				return (conify 'Nothing unitRepr `sumRepr` conify 'Just tRepr)
	_	-> bootstrapRepr t0

bootstrapRepr :: (?combine :: [Representation] -> Representation) => Type -> ReprMonad Representation
bootstrapRepr t0 = qRecover fallback
    (do	_tRep <- genReprMain t0
	recursiveRepr (ConT ''Rep `AppT` t0) (VarE 'toRep))
  where	fallback = keyRepr t0