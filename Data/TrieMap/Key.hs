{-# LANGUAGE TypeFamilies, CPP, FlexibleInstances, FlexibleContexts, NamedFieldPuns, RecordWildCards, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses, UnboxedTuples, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.TrieMap.Key () where

import Data.TrieMap.Class
import Data.TrieMap.TrieKey
import Data.TrieMap.Representation.Class
import Data.TrieMap.Modifiers

import Prelude hiding (foldr, foldl, foldr1, foldl1, lookup)

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

#define SETOP(op) op f KMAP(m1) KMAP(m2) = keyMap (op f m1 m2)
instance CONTEXT(SetOp) => SetOp (TrieMap (Key k)) where
  SETOP(union)
  SETOP(isect)
  SETOP(diff)

instance CONTEXT(Project) => Project (TrieMap (Key k)) where
  mapMaybe f KMAP(m) = keyMap $ mapMaybe f m
  mapEither f KMAP(m) = both keyMap (mapEither f) m

instance CONTEXT(Zippable) => Zippable (TrieMap (Key k)) where
  empty = KeyMap 0 empty
  clear (KeyHole hole) = keyMap (clear hole)
  assign a (KeyHole hole) = keyMap (assign a hole)

#define SPLITOP(op) op (KeyHole hole) = keyMap (op hole)
instance CONTEXT(Splittable) => Splittable (TrieMap (Key k)) where
  SPLITOP(before)
  SPLITOP(beforeWith a)
  SPLITOP(after)
  SPLITOP(afterWith a)

instance CONTEXT(Indexable) => Indexable (TrieMap (Key k)) where
  index i KMAP(m) = case index i m of
    (# i', a, hole #) -> (# i', a, KeyHole hole #)

instance TKey k => Searchable (TrieMap (Key k)) (Key k) where
  search k KMAP(m) = mapHole KeyHole (search (keyRep k) m)
  singleZip k = KeyHole (singleZip (keyRep k))
  
  lookup k KMAP(m) = lookup (keyRep k) m
  insertWith f k a KMAP(m) = keyMap (insertWith f (keyRep k) a m)
  alter f k KMAP(m) = keyMap (alter f (keyRep k) m)

data instance TrieMap (Key k) a = KeyMap {sz :: !Int, tMap :: !(TrieMap (Rep k) a)}
newtype instance Zipper (TrieMap (Key k)) a = KeyHole (Hole (Rep k) a)

-- | @'TrieMap' ('Key' k) a@ is a wrapper around a @TrieMap (Rep k) a@.
instance TKey k => TrieKey (Key k) where
  getSimpleM KMAP(m) = getSimpleM m
  sizeM = sz
  
  unifierM (Key k1) (Key k2) a = KeyHole <$> unifierM (toRep k1) (toRep k2) a
  unifyM (Key k1) a1 (Key k2) a2 = keyMap <$> unifyM (toRep k1) a1 (toRep k2) a2

keyRep :: (Repr k, TrieKey (Rep k)) => Key k -> Rep k
keyRep (Key k) = toRep k