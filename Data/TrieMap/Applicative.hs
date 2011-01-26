{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module Data.TrieMap.Applicative where

import Control.Applicative
import Control.Monad

import Data.Monoid hiding (Dual)

instance Functor First where
	fmap f (First m) = First (fmap f m)

instance Functor Last where
	fmap f (Last m) = Last (fmap f m)

instance Monad First where
	return = First . return
	First m >>= k = First (m >>= getFirst . k)

instance Monad Last where
	return = Last . return
	Last m >>= k = Last (m >>= getLast . k)

instance MonadPlus First where
	mzero = mempty
	mplus = mappend

instance MonadPlus Last where
	mzero = mempty
	mplus = mappend

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)

(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
f <.> g = fmap f . g

(<.:>) :: Functor f => (c -> d) -> (a -> b -> f c) -> a -> b -> f d
(f <.:> g) x y = f <$> g x y

instance Applicative First where
	pure = return
	(<*>) = ap

instance Alternative First where
	empty = mempty
	(<|>) = mplus

instance Applicative Last where
	pure = return
	(<*>) = ap

instance Alternative Last where
	empty = mempty
	(<|>) = mplus

newtype DualPlus f a = DualPlus {runDualPlus :: f a} deriving (Functor, Applicative, Monad)
newtype Dual f a = Dual {runDual :: f a} deriving (Functor)

instance Applicative f => Applicative (Dual f) where
	pure = Dual . pure
	Dual f <*> Dual a = Dual (a <**> f)
	Dual f *> Dual g = Dual (g <* f)
	Dual f <* Dual g = Dual (g *> f)

instance MonadPlus m => MonadPlus (DualPlus m) where
	mzero = DualPlus mzero
	DualPlus m `mplus` DualPlus k = DualPlus (k `mplus` m)