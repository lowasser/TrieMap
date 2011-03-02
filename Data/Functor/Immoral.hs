{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Data.Functor.Immoral where

class ImmoralMap a b where
  castMap :: Functor f => f a -> f b

instance ImmoralMap a a where
  castMap = id