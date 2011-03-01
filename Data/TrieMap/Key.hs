{-# LANGUAGE TypeFamilies, CPP, FlexibleInstances, FlexibleContexts, NamedFieldPuns, RecordWildCards, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses, UnboxedTuples #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.TrieMap.Key () where

import Data.TrieMap.Class
import Data.TrieMap.TrieKey
import Data.TrieMap.Representation.Class
import Data.TrieMap.Modifiers

import Prelude hiding (foldr, foldl, foldr1, foldl1)

type RepMap k = TrieMap (Rep k)

keyMap :: (Repr k, TrieKey (Rep k), Sized a) => TrieMap (Rep k) a -> TrieMap (Key k) a
keyMap m = KeyMap (sizeM m) m

#define KMAP(m) KeyMap{tMap = m}
#define CONTEXT(cl) (Repr k, TrieKey (Rep k), cl (RepMap k))

instance CONTEXT(Foldable) => Foldable (TrieMap (Key k)) where
  foldMap f KMAP(m) = foldMap f m
  foldr f z KMAP(m) = foldr f z m
  foldl f z KMAP(m) = foldl f z m

instance CONTEXT(Functor) => Functor (TrieMap (Key k)) where
  fmap f KeyMap{..} = KeyMap{sz, tMap = f <$> tMap}

instance CONTEXT(Traversable) => Traversable (TrieMap (Key k)) where
  traverse f KeyMap{..} = KeyMap sz <$> traverse f tMap

instance CONTEXT(Subset) => Subset (TrieMap (Key k)) where
  KMAP(m1) <=? KMAP(m2) = m1 <=? m2

instance (Repr k, TrieKey (Rep k), Buildable (RepMap k) (Rep k)) => Buildable (TrieMap (Key k)) (Key k) where
  type UStack (TrieMap (Key k)) = UMStack (Rep k)
  uFold = fmap keyMap . mapFoldlKeys keyRep . uFold
  type AStack (TrieMap (Key k)) = AMStack (Rep k)
  aFold = fmap keyMap . mapFoldlKeys keyRep . aFold
  type DAStack (TrieMap (Key k)) = DAMStack (Rep k)
  daFold = keyMap <$> mapFoldlKeys keyRep daFold

#define SETOP(op) op f KMAP(m1) KMAP(m2) = keyMap <$> op f m1 m2
instance CONTEXT(SetOp) => SetOp (TrieMap (Key k)) where
  SETOP(union)
  SETOP(isect)
  SETOP(diff)

instance CONTEXT(Project) => Project (TrieMap (Key k)) where
  mapMaybe f KMAP(m) = keyMap <$> mapMaybe f m
  mapEither f KMAP(m) = case mapEither f m of
    (# mL, mR #) -> (# keyMap <$> mL, keyMap <$> mR #)

-- | @'TrieMap' ('Key' k) a@ is a wrapper around a @TrieMap (Rep k) a@.
instance TKey k => TrieKey (Key k) where
	data TrieMap (Key k) a = KeyMap {sz :: !Int, tMap :: !(TrieMap (Rep k) a)}
	newtype Hole (Key k) a = KeyHole (Hole (Rep k) a)
	
	singletonM (Key k) a = KeyMap (getSize a) (singletonM (toRep k) a)
	getSimpleM KMAP(m) = getSimpleM m
	sizeM = sz
	lookupMC (Key k) KMAP(m) = lookupMC (toRep k) m

	singleHoleM (Key k) = KeyHole (singleHoleM (toRep k))
	beforeM (KeyHole hole) = keyMap <$> beforeM hole
	beforeWithM a (KeyHole hole) = keyMap (beforeWithM a hole)
	afterM (KeyHole hole) = keyMap <$> afterM hole
	afterWithM a (KeyHole hole) = keyMap (afterWithM a hole)
	searchMC (Key k) KMAP(m) = mapSearch KeyHole (searchMC (toRep k) m)
	indexM KMAP(m) i = case indexM m i of
	  (# i', a, hole #) -> (# i', a, KeyHole hole #)
	extractHoleM KMAP(m) = fmap KeyHole <$> extractHoleM m
	assignM v (KeyHole hole) = keyMap (assignM v hole)
	clearM (KeyHole hole) = keyMap <$> clearM hole
	
	insertWithM f (Key k) a KMAP(m) = keyMap (insertWithM f (toRep k) a m)
	
keyRep :: (Repr k, TrieKey (Rep k)) => Key k -> Rep k
keyRep (Key k) = toRep k