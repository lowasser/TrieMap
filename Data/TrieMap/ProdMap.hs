{-# LANGUAGE UnboxedTuples, TupleSections, PatternGuards, TypeFamilies, FlexibleInstances, RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.TrieMap.ProdMap () where

import Control.Monad.Unpack

import Data.TrieMap.TrieKey

import Prelude hiding (foldl, foldl1, foldr, foldr1)

instance (TrieKey k1, TrieKey k2) => Functor (TrieMap (k1, k2)) where
  fmap f (PMap m) = PMap (fmap (fmap f) m)

instance (TrieKey k1, TrieKey k2) => Foldable (TrieMap (k1, k2)) where
  foldMap f (PMap m) = foldMap (foldMap f) m
  foldr f z (PMap m) = foldr (flip $ foldr f) z m
  foldl f z (PMap m) = foldl (foldl f) z m

instance (TrieKey k1, TrieKey k2) => Traversable (TrieMap (k1, k2)) where
  traverse f (PMap m) = PMap <$> traverse (traverse f) m

instance (TrieKey k1, TrieKey k2) => Subset (TrieMap (k1, k2)) where
  PMap m1 <=? PMap m2 = m1 <<=? m2

instance (TrieKey k1, TrieKey k2) => Buildable (TrieMap (k1, k2)) (k1, k2) where
  type UStack (TrieMap (k1, k2)) = TrieMap (k1, k2)
  uFold = defaultUFold emptyM singletonM insertWithM
  type AStack (TrieMap (k1, k2)) = Stack k1 k2 (DAMStack k1) (AMStack k2)
  aFold f = prodFold daFold (aFold f)
  type DAStack (TrieMap (k1, k2)) = Stack k1 k2 (DAMStack k1) (DAMStack k2)
  daFold = prodFold daFold daFold

-- | @'TrieMap' (k1, k2) a@ is implemented as a @'TrieMap' k1 ('TrieMap' k2 a)@.
instance (TrieKey k1, TrieKey k2) => TrieKey (k1, k2) where
	newtype TrieMap (k1, k2) a = PMap (TrieMap k1 (TrieMap k2 a))
	data Hole (k1, k2) a = PHole (Hole k1 (TrieMap k2 a)) (Hole k2 a)

	emptyM = PMap emptyM
	singletonM (k1, k2) = PMap . singletonM k1 . singletonM k2
	getSimpleM (PMap m) = getSimpleM m >>= getSimpleM
	sizeM (PMap m) = sizeM m
	lookupMC (k1, k2) (PMap m) = lookupMC k1 m >>= lookupMC k2
	mapMaybeM f (PMap m) = PMap (mapMaybeM (mapMaybeM' f) m)
	mapEitherM f (PMap m) = both PMap PMap (mapEitherM (mapEitherM' f)) m
	unionM f (PMap m1) (PMap m2) = PMap (unionM (unionM' f) m1 m2)
	isectM f (PMap m1) (PMap m2) = PMap (isectM (isectM' f) m1 m2)
	diffM f (PMap m1) (PMap m2) = PMap (diffM (diffM' f) m1 m2)
	insertWithM f (k1, k2) a (PMap m) = PMap (insertWithM f' k1 (singletonM k2 a) m) where
	  f' = insertWithM f k2 a
	
	singleHoleM (k1, k2) = PHole (singleHoleM k1) (singleHoleM k2)
	beforeM (PHole hole1 hole2) = PMap (beforeMM (gNull beforeM hole2) hole1)
	beforeWithM a (PHole hole1 hole2) = PMap (beforeWithM (beforeWithM a hole2) hole1)
	afterM (PHole hole1 hole2) = PMap (afterMM (gNull afterM hole2) hole1)
	afterWithM a (PHole hole1 hole2) = PMap (afterWithM (afterWithM a hole2) hole1)
	searchMC (k1, k2) (PMap m) f g = searchMC k1 m f' g' where
	  f' hole1 = f (PHole hole1 (singleHoleM k2))
	  g' m' hole1 = mapSearch (PHole hole1) (searchMC k2 m') f g
	indexMC (PMap m) = unpack $ \ i result ->
	  indexMC' m i $ \ (Indexed i' m' hole1) -> mapIndex (PHole hole1) (indexMC m' $~ i') result
	extractHoleM (PMap m) = do
		(m', hole1) <- extractHoleM m
		(v, hole2) <- extractHoleM m'
		return (v, PHole hole1 hole2)
	
	clearM (PHole hole1 hole2) = PMap (fillHoleM (clearM' hole2) hole1)
	assignM a (PHole hole1 hole2) = PMap (assignM (assignM a hole2) hole1)
	
	unifierM (k1', k2') (k1, k2) a = 
	  (fmap (`PHole` singleHoleM k2') $ unifierM k1' k1 (singletonM k2 a))
	  `mplus` (PHole (singleHoleM k1) <$> unifierM k2' k2 a)
	unifyM (k11, k12) a1 (k21, k22) a2 =
	  let unify1 = unifyM k11 (singletonM k12 a1) k21 (singletonM k22 a2)
	      unify2 = singletonM k11 <$> unifyM k12 a1 k22 a2
	      in PMap <$> (unify1 `mplus` unify2)

gNull :: TrieKey k => (x -> TrieMap k a) -> x -> Maybe (TrieMap k a)
gNull = (guardNullM .)

prodFold :: Eq k1 => FromList z1 k1 (TrieMap k2 a) -> FromList z2 k2 a -> 
  FromList (Stack k1 k2 z1 z2) (k1, k2) a
prodFold Foldl{snoc = snoc1, begin = begin1, zero = zero1, done = done1}
	    Foldl{snoc = snoc2, begin = begin2, done = done2}
  = Foldl{zero = PMap zero1, ..}
  where	snoc (First k1 stk2) (k1', k2') a
	  | k1' == k1	= First k1 (snoc2 stk2 k2' a)
	snoc (Stack k1 stk1 stk2) (k1', k2') a
	  | k1' == k1	= Stack k1 stk1 (snoc2 stk2 k2' a)
	snoc stk (k1, k2) a = Stack k1 (collapse stk) (begin2 k2 a)
	
	collapse (First k1 stk2) = begin1 k1 (done2 stk2)
	collapse (Stack k1 stk1 stk2) = snoc1 stk1 k1 (done2 stk2)
	
	begin (k1, k2) a = First k1 (begin2 k2 a)
	
	done = PMap . done1 . collapse

data Stack k1 k2 z1 z2 a = First k1 (z2 a) | Stack k1 (z1 (TrieMap k2 a)) (z2 a)