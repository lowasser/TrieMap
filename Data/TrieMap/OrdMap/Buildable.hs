{-# LANGUAGE BangPatterns, RecordWildCards, TypeSynonymInstances, MultiParamTypeClasses, TypeFamilies #-}
module Data.TrieMap.OrdMap.Buildable () where

import Data.TrieMap.OrdMap.Base
import Data.TrieMap.OrdMap.Searchable ()

import Data.Functor.Immoral

instance Ord k => Buildable (OrdMap k) (Ordered k) where
  type UStack (OrdMap k) = OrdMap k
  uFold = defaultUFold
  type AStack (OrdMap k) = Distinct (Ordered k) (Stack k)
  aFold = combineFold daFold
  type DAStack (OrdMap k) = Stack k
  daFold = castMap $ mapFoldlKeys unOrd fromDistAscList

fromDistAscList :: Sized a => Foldl (Stack k) k a (SNode k a)
fromDistAscList = Foldl{zero = tip, ..} where
  incr !t (Yes t' stk) = No (incr (t' `glue` t) stk)
  incr !t (No stk) = Yes t stk
  incr !t End = Yes t End
  
  begin k a = Yes (single k a) End
  
  snoc stk k a = incr (single k a) stk
  
  roll !t End = t
  roll !t (No stk) = roll t stk
  roll !t (Yes t' stk) = roll (t' `glue` t) stk
  
  done = roll tip

data Stack k a = No (Stack k a) | Yes !(SNode k a) (Stack k a) | End
