{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Ends where

import Control.Monad

newtype First a = First {getFirst :: Maybe a} deriving (Functor, Monad)
newtype Last a = Last {getLast :: Maybe a} deriving (Functor, Monad)

instance MonadPlus First where
  mzero = First Nothing
  First Nothing `mplus` m	= m
  m `mplus` _			= m

instance MonadPlus Last where
  mzero = Last Nothing
  m `mplus` Last Nothing	= m
  _ `mplus` m			= m