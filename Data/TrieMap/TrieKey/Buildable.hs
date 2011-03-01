{-# LANGUAGE TypeFamilies, NamedFieldPuns, RecordWildCards, FunctionalDependencies, BangPatterns, MultiParamTypeClasses, ViewPatterns #-}
module Data.TrieMap.TrieKey.Buildable (
  Buildable(..),
  Foldl(..),
  mapFoldlKeys,
  runFoldl,
  defaultUFold,
  Distinct,
  combineFold) where

import Data.TrieMap.Sized

class Buildable f k | f -> k where
  type UStack f :: * -> *
  uFold :: Sized a => (a -> a -> a) -> Foldl (UStack f) k a (f a)
  type AStack f :: * -> *
  aFold :: Sized a => (a -> a -> a) -> Foldl (AStack f) k a (f a)
  type DAStack f :: * -> *
  daFold :: Sized a => Foldl (DAStack f) k a (f a)

data Foldl stack k a result =
  Foldl {snoc :: stack a -> k -> a -> stack a,
	  begin :: k -> a -> stack a,
	  done :: stack a -> result}

instance Functor (Foldl stack k a) where
  fmap f Foldl{..} = Foldl{done = f . done, ..}

{-# INLINE runFoldl #-}
runFoldl :: Foldl stack k a result -> [(k, a)] -> Maybe result
runFoldl Foldl{..} = fL where
  fL [] = Nothing
  fL ((k, a):xs) = fL' (begin k a) xs
  
  fL' !s ((k, a):xs) = fL' (snoc s k a) xs
  fL' s [] = Just $ done s

{-# INLINE mapFoldlKeys #-}
mapFoldlKeys :: (k -> k') -> Foldl stack k' a result -> Foldl stack k a result
mapFoldlKeys f Foldl{..} = Foldl{snoc = \ z k a -> snoc z (f k) a, begin = begin . f, ..}

{-# INLINE defaultUFold #-}
defaultUFold :: (k -> a -> f a) -> ((a -> a) -> k -> a -> f a -> f a) -> 
  (a -> a -> a) -> Foldl f k a (f a)
defaultUFold single insert f = Foldl{
  begin = single,
  snoc = \ m k a -> insert (f a) k a m,
  done = id}

data Distinct k z a = Begin k a | Dist k a (z a)

{-# INLINE combineFold #-}
combineFold :: Eq k => Foldl stack k a result -> (a -> a -> a) -> Foldl (Distinct k stack) k a result
combineFold Foldl{..} f = Foldl{snoc = snoc', begin = Begin, done = done'} where
    snoc' (Begin k a) k' a'
      | k == k'	= Begin k (f a' a)
    snoc' (Dist k a stk) k' a'
      | k == k'	= Dist k (f a' a) stk
    snoc' stk k a = Dist k a (collapse stk)
    
    done' = done . collapse
    
    collapse (Begin k a) = begin k a
    collapse (Dist k a stk) = snoc stk k a