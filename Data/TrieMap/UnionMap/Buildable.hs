{-# LANGUAGE RecordWildCards, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
module Data.TrieMap.UnionMap.Buildable () where

import Data.TrieMap.UnionMap.Base
import Data.TrieMap.UnionMap.Searchable ()

instance (TrieKey k1, TrieKey k2) => Buildable (TrieMap (Either k1 k2)) (Either k1 k2) where
  type UStack (TrieMap (Either k1 k2)) = TrieMap (Either k1 k2)
  uFold = defaultUFold
  type AStack (TrieMap (Either k1 k2)) = Stack (AMStack k1) (AMStack k2)
  aFold f = unionFold (aFold f) (aFold f)
  type DAStack (TrieMap (Either k1 k2)) = Stack (DAMStack k1) (DAMStack k2)
  daFold = unionFold daFold daFold

{-# INLINE unionFold #-}
unionFold :: (TrieKey k1, TrieKey k2, Sized a) =>
  FromList z1 k1 a -> FromList z2 k2 a -> FromList (Stack z1 z2) (Either k1 k2) a
unionFold Foldl{snoc = snocL, begin = beginL, done = doneL}
	    Foldl{snoc = snocR, begin = beginR, done = doneR}
  = Foldl{zero = Empty, ..}
  where	snoc (JustL s1)	(Left k) a = JustL (snocL s1 k a)
	snoc (JustL s1)	(Right k) a = Both s1 (beginR k a)
	snoc (JustR s2) (Left k) a = Both (beginL k a) s2
	snoc (JustR s2) (Right k) a = JustR (snocR s2 k a)
	snoc (Both s1 s2) (Left k) a = Both (snocL s1 k a) s2
	snoc (Both s1 s2) (Right k) a = Both s1 (snocR s2 k a)
	
	begin (Left k) a = JustL (beginL k a)
	begin (Right k) a = JustR (beginR k a)
	
	done (JustL sL) = MapL (doneL sL)
	done (JustR sR) = MapR (doneR sR)
	done (Both sL sR) = doneL sL `mapLR` doneR sR

data Stack s1 s2 a =
  JustL (s1 a)
  | JustR (s2 a)
  | Both (s1 a) (s2 a)