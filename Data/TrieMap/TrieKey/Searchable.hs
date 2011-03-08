{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Data.TrieMap.TrieKey.Searchable where

import Control.Monad.Option

import Data.TrieMap.TrieKey.Zippable
import Data.TrieMap.TrieKey.Splittable
import Data.TrieMap.Sized

type Search f a r = (Zipper f a -> r) -> (a -> Zipper f a -> r) -> r

class Zippable f => Searchable f k | f -> k where
  search :: k -> f a -> Search f a r
  singleZip :: k -> Zipper f a
  singleZip k = search k empty id undefined
  
  singleton :: Sized a => k -> a -> f a
  singleton k a = assign a (singleZip k)
  
  lookup :: k -> f a -> Option a
  lookup k m = option $ \ no yes -> search k m (\ _ -> no) (\ a _ -> yes a)
  
  insertWith :: Sized a => (a -> a) -> k -> a -> f a -> f a
  insertWith f k a m = search k m nomatch match where
    nomatch hole = assign a hole
    match a0 hole = assign (f a0) hole
  
  alter :: Sized a => (Maybe a -> Maybe a) -> k -> f a -> f a
  alter f k m = search k m nomatch match where
    nomatch hole = case f Nothing of
      Nothing	-> m
      Just a	-> assign a hole
    match a0 hole = fill (f (Just a0)) hole

searchM :: Searchable f k => k -> Maybe (f a) -> Search f a r
searchM k m nomatch match = case m of
  Nothing	-> nomatch (singleZip k)
  Just m	-> search k m nomatch match

insertWithM :: (Searchable f k, Sized a) => (a -> a) -> k -> a -> Maybe (f a) -> f a
insertWithM _ k a Nothing = singleton k a
insertWithM f k a (Just m) = insertWith f k a m

splitLookup :: (Splittable f, Searchable f k, Sized a) => k -> f a -> (f a, Maybe a, f a)
splitLookup k m = search k m nomatch match where
  nomatch hole = (before hole, Nothing, after hole)
  match a hole = (before hole, Just a, after hole)

mapHole :: (Zipper f a -> Zipper g a) -> Search f a r -> Search g a r
mapHole f run notfound found = run notfound' found' where
  notfound' z = notfound (f z)
  found' a z = found a (f z)