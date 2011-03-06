{-# LANGUAGE CPP, FlexibleInstances #-}
module Data.TrieMap.WordMap.SetOp () where

import Control.Monad.Option

import Data.TrieMap.TrieKey

import Data.Word

import Data.TrieMap.WordMap.Base
import Data.TrieMap.WordMap.Searchable ()

import Prelude hiding (lookup)

#define SETOP(op) op f (WordMap m1) (WordMap m2) = WordMap (op f m1 m2)

instance SetOp (TrieMap Word) where
  SETOP(union)
  SETOP(isect)
  SETOP(diff)

instance SetOp SNode where
  union f = (\/) where
    n1@(SNode _ t1) \/ n2@(SNode _ t2) = case (t1, t2) of
      (Nil, _)	-> n2
      (_, Nil)	-> n1
      (Tip k x, _)	-> alter (maybe (Just x) (f x)) k n2
      (_, Tip k x)	-> alter (maybe (Just x) (`f` x)) k n1
      (Bin p1 m1 l1 r1, Bin p2 m2 l2 r2)
	| shorter m1 m2  -> union1
	| shorter m2 m1  -> union2
	| p1 == p2       -> bin p1 m1 (l1 \/ l2) (r1 \/ r2)
	| otherwise      -> join p1 n1 p2 n2
	where
	  union1  | nomatch p2 p1 m1  = join p1 n1 p2 n2
		  | mask0 p2 m1       = bin p1 m1 (l1 \/ n2) r1
		  | otherwise         = bin p1 m1 l1 (r1 \/ n2)

	  union2  | nomatch p1 p2 m2  = join p1 n1 p2 n2
		  | mask0 p1 m2       = bin p2 m2 (n1 \/ l2) r2
		  | otherwise         = bin p2 m2 l2 (n1 \/ r2)
  isect f = (/\) where
    n1@(SNode _ t1) /\ n2@(SNode _ t2) = case (t1, t2) of
      (Nil, _)	-> nil
      (Tip{}, Nil)	-> nil
      (Bin{}, Nil)	-> nil
      (Tip k x, _)	-> runOption (lookup k n2) nil (singletonMaybe k . f x)
      (_, Tip k y)	-> runOption (lookup k n1) nil (singletonMaybe k . flip f y)
      (Bin p1 m1 l1 r1, Bin p2 m2 l2 r2)
	| shorter m1 m2  -> intersection1
	| shorter m2 m1  -> intersection2
	| p1 == p2       -> bin p1 m1 (l1 /\ l2) (r1 /\ r2)
	| otherwise      -> nil
	where
	  intersection1 | nomatch p2 p1 m1  = nil
			| mask0 p2 m1       = l1 /\ n2
			| otherwise         = r1 /\ n2

	  intersection2 | nomatch p1 p2 m2  = nil
			| mask0 p1 m2       = n1 /\ l2
			| otherwise         = n1 /\ r2
  diff f = (\\) where
    n1@(SNode _ t1) \\ n2@(SNode _ t2) = case (t1, t2) of
      (Nil, _)	-> nil
      (_, Nil)	-> n1
      (Tip k x, _)	-> runOption (lookup k n2) n1 (singletonMaybe k . f x)
      (_, Tip k y)	-> alter (>>= flip f y) k n1
      (Bin p1 m1 l1 r1, Bin p2 m2 l2 r2)
	| shorter m1 m2  -> difference1
	| shorter m2 m1  -> difference2
	| p1 == p2       -> bin p1 m1 (l1 \\ l2) (r1 \\ r2)
	| otherwise      -> n1
	where
	  difference1 | nomatch p2 p1 m1  = n1
		      | mask0 p2 m1       = bin p1 m1 (l1 \\ n2) r1
		      | otherwise         = bin p1 m1 l1 (r1 \\ n2)

	  difference2 | nomatch p1 p2 m2  = n1
		      | mask0 p1 m2       = n1 \\ l2
		      | otherwise         = n1 \\ r2
