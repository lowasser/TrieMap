{-# LANGUAGE TypeFamilies, UnboxedTuples, GeneralizedNewtypeDeriving, FlexibleInstances, NamedFieldPuns, RecordWildCards #-}
module Data.TrieMap.ReverseMap () where

import Control.Applicative
import Control.Monad
import Control.Monad.Ends

import Data.Foldable
import qualified Data.Monoid as M

import Data.TrieMap.TrieKey
import Data.TrieMap.Modifiers
import Data.TrieMap.Sized

import Prelude hiding (foldr, foldl, foldr1, foldl1)

newtype DualPlus m a = DualPlus {runDualPlus :: m a} deriving (Functor, Monad)
newtype Dual f a = Dual {runDual :: f a} deriving (Functor)

instance Applicative f => Applicative (Dual f) where
  pure a = Dual (pure a)
  Dual f <*> Dual x = Dual (x <**> f)

instance MonadPlus m => MonadPlus (DualPlus m) where
  mzero = DualPlus mzero
  DualPlus m `mplus` DualPlus k = DualPlus (k `mplus` m)

instance TrieKey k => Foldable (TrieMap (Rev k)) where
  foldMap f (RevMap m) = M.getDual (foldMap (M.Dual . f) m)
  foldr f z (RevMap m) = foldl (flip f) z m
  foldl f z (RevMap m) = foldr (flip f) z m
  foldr1 f (RevMap m) = foldl1 (flip f) m
  foldl1 f (RevMap m) = foldr1 (flip f) m

-- | @'TrieMap' ('Rev' k) a@ is a wrapper around a @'TrieMap' k a@ that reverses the order of the operations.
instance TrieKey k => TrieKey (Rev k) where
	newtype TrieMap (Rev k) a = RevMap (TrieMap k a)
	newtype Hole (Rev k) a = RHole (Hole k a)

	emptyM = RevMap emptyM
	singletonM (Rev k) a = RevMap (singletonM k a)
	lookupM (Rev k) (RevMap m) = lookupM k m
	sizeM (RevMap m) = sizeM m
	getSimpleM (RevMap m) = getSimpleM m
	
	fmapM f (RevMap m) = RevMap (fmapM f m)
	traverseM f (RevMap m) = RevMap <$> runDual (traverseM (Dual . f) m)
	
	mapMaybeM f (RevMap m) = RevMap (mapMaybeM f m)
	mapEitherM f (RevMap m) = both RevMap RevMap (mapEitherM f) m
	unionM f (RevMap m1) (RevMap m2) = RevMap (unionM f m1 m2)
	isectM f (RevMap m1) (RevMap m2) = RevMap (isectM f m1 m2)
	diffM f (RevMap m1) (RevMap m2) = RevMap (diffM f m1 m2)
	isSubmapM (<=) (RevMap m1) (RevMap m2) = isSubmapM (<=) m1 m2
	
	singleHoleM (Rev k) = RHole (singleHoleM k)
	beforeM (RHole hole) = RevMap (afterM hole)
	beforeWithM a (RHole hole) = RevMap (afterWithM a hole)
	afterM (RHole hole) = RevMap (beforeM hole)
	afterWithM a (RHole hole) = RevMap (beforeWithM a hole)
	searchMC (Rev k) (RevMap m) = mapSearch RHole (searchMC k m)
	indexM i (RevMap m) = case indexM (revIndex i m) m of
		(# i', a, hole #) -> (# revIndex i' a, a, RHole hole #)
	  where	revIndex :: Sized a => Int -> a -> Int
		revIndex i a = getSize a - 1 - i
	
	extractHoleM (RevMap m) = fmap RHole <$> runDualPlus (extractHoleM m)
	firstHoleM (RevMap m) = First (fmap RHole <$> getLast (lastHoleM m))
	lastHoleM (RevMap m) = Last (fmap RHole <$> getFirst (firstHoleM m))
	
	assignM v (RHole m) = RevMap (assignM v m)
	clearM (RHole m) = RevMap (clearM m)
	
	insertWithM f (Rev k) a (RevMap m) = RevMap (insertWithM f k a m)
	
	fromListFold f = RevMap <$> mapFoldlKey getRev (fromListFold f)
	fromAscListFold f = RevMap <$> mapFoldlKey getRev (reverseFold (fromAscListFold f))
	fromDistAscListFold = RevMap <$> mapFoldlKey getRev (reverseFold fromDistAscListFold)
	
	fromListM f xs = RevMap (fromListM f [(k, a) | (Rev k, a) <- xs])
	fromAscListM f xs = RevMap (fromAscListM (flip f) [(k, a) | (Rev k, a) <- reverse xs])
	fromDistAscListM xs = RevMap (fromDistAscListM [(k, a) | (Rev k, a) <- reverse xs])
	
	unifierM (Rev k') (Rev k) a = RHole <$> unifierM k' k a

{-# INLINE reverseFold #-}
reverseFold :: Foldl k a z -> Foldl k a z
reverseFold Foldl{snoc = snoc0, begin = begin0, zero, done = done0}
  = Foldl {..} where
  snoc g k a Nothing = g (Just $ begin0 k a)
  snoc g k a (Just m) = g (Just $ snoc0 m k a)
  
  begin = snoc (maybe zero done0)
  
  done g = g Nothing