{-# LANGUAGE BangPatterns, UnboxedTuples, MagicHash, NamedFieldPuns, RecordWildCards, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Data.TrieMap.WordMap.Buildable () where

import Data.TrieMap.TrieKey

import Data.Word

import Data.TrieMap.WordMap.Base
import Data.TrieMap.WordMap.Searchable ()

import GHC.Exts

instance Buildable (TrieMap Word) Word where
  type UStack (TrieMap Word) = SNode
  {-# INLINE uFold #-}
  uFold = fmap WordMap . defaultUFold
  type AStack (TrieMap Word) = WordStack
  {-# INLINE aFold #-}
  aFold = fmap WordMap . fromAscList
  type DAStack (TrieMap Word) = WordStack
  {-# INLINE daFold #-}
  daFold = aFold const

{-# INLINE fromAscList #-}
fromAscList :: Sized a => (a -> a -> a) -> Foldl WordStack Key a (SNode a)
fromAscList f = Foldl{zero = nil, ..} where
  begin kx vx = WordStack kx vx Nada

  snoc (WordStack kx vx stk) kz vz
    | kx == kz	= WordStack kx (f vz vx) stk
    | otherwise	= WordStack kz vz $ reduce (branchMask kx kz) kx (singleton kx vx) stk
  
--   reduce :: Mask -> Prefix -> SNode a -> Stack a -> Stack a
  reduce !m !px !tx (Push py ty stk')
    | shorter m mxy	= reduce m pxy (bin' pxy mxy ty tx) stk'
    where mxy = branchMask px py; pxy = mask px mxy
  reduce _ px tx stk	= Push px tx stk

  done (WordStack kx vx stk) = case finish kx (singleton kx vx) stk of
    (# sz#, node #) -> SNode {sz = I# sz#, node}
  
  finish !px !tx (Push py ty stk) = finish p (join' py ty px tx) stk
    where m = branchMask px py; p = mask px m
  finish _ SNode{sz, node} Nada = (# unbox sz, node #)
  
  join' p1 t1 p2 t2
  	= SNode{sz = sz t1 + sz t2, node = Bin p m t1 t2}
    where
      m = branchMask p1 p2
      p = mask p1 m

data WordStack a = WordStack !Key a (Stack a)
data Stack a = Push !Prefix !(SNode a) !(Stack a) | Nada