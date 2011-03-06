{-# LANGUAGE CPP, TupleSections, FlexibleInstances, TypeSynonymInstances #-}
module Data.TrieMap.OrdMap.SetOp () where

import Control.Monad.Option
import Data.Functor.Immoral

import Data.TrieMap.OrdMap.Base
import Data.TrieMap.OrdMap.Traversable ()
import Data.TrieMap.OrdMap.Searchable ()
import Data.TrieMap.OrdMap.Splittable ()

import Prelude hiding (lookup)

#define SETOP(op) op f (OrdMap m1) (OrdMap m2) = OrdMap (op f m1 m2)
#define TIP SNode{node=Tip}
#define BIN(args) SNode{node=Bin args}

instance Ord k => SetOp (OrdMap k) where
  SETOP(union)
  SETOP(isect)
  SETOP(diff)

instance Ord k => SetOp (SNode k) where
  union f = hedgeUnion f (const LT) (const GT)
  diff f = hedgeDiff f (const LT) (const GT)
  isect f m1 m2 = castMap m1 `intersection` m2 where
    t1@BIN(_ _ _ _) `intersection` BIN(k2 x2 l2 r2) = case splitLookup k2 t1 of
      (tl, found, tr) -> joinMaybe k2 (found >>= \ (Elem x1') -> f x1' x2) (tl `intersection` l2) (tr `intersection` r2)
    _ `intersection` _ = tip

hedgeUnion :: (Ord k, Sized a)
                  => (a -> a -> Maybe a)
                  -> (k -> Ordering) -> (k -> Ordering)
                  -> SNode k a -> SNode k a -> SNode k a
hedgeUnion _ _     _     t1 TIP
  = t1
hedgeUnion _ cmplo cmphi TIP BIN(kx x l r)
  = join kx x (filterGt  cmplo l) (filterLt  cmphi r)
hedgeUnion f cmplo cmphi BIN(kx x l r) t2
  = joinMaybe  kx newx (hedgeUnion  f cmplo cmpkx l lt) 
                (hedgeUnion  f cmpkx cmphi r gt)
  where
    cmpkx k     = compare kx k
    lt          = trim cmplo cmpkx t2
    (found,gt)  = trimLookupLo kx cmphi t2
    newx        = case found of
                    Nothing -> Just x
                    Just (_,y) -> f x y

filterGt :: (Ord k, Sized a) => (k -> Ordering) -> SNode k a -> SNode k a
filterGt _   TIP = tip
filterGt cmp BIN(kx x l r)
  = case cmp kx of
      LT -> join kx x (filterGt  cmp l) r
      GT -> filterGt  cmp r
      EQ -> r

filterLt :: (Ord k, Sized a) => (k -> Ordering) -> SNode k a -> SNode k a
filterLt _   TIP = tip
filterLt cmp BIN(kx x l r)
  = case cmp kx of
      LT -> filterLt cmp l
      GT -> join kx x l (filterLt  cmp r)
      EQ -> l

trim :: (k -> Ordering) -> (k -> Ordering) -> SNode k a -> SNode k a
trim cmplo cmphi = trimmer where
  trimmer TIP	= tip
  trimmer t@BIN(kx _ l r) = case (cmplo kx, cmphi kx) of
    (LT, GT)	-> t
    (LT, _)	-> trimmer l
    _		-> trimmer r
              
trimLookupLo :: Ord k => k -> (k -> Ordering) -> SNode k a -> (Maybe (k,a), SNode k a)
trimLookupLo _  _     TIP = (Nothing,tip)
trimLookupLo lo cmphi t@BIN(kx x l r)
  = case compare lo kx of
      LT -> case cmphi kx of
              GT -> (liftOption (fmap (lo,) (lookup lo t)), t)
              _  -> trimLookupLo lo cmphi l
      GT -> trimLookupLo lo cmphi r
      EQ -> (Just (kx,x),trim (compare lo) cmphi r)

hedgeDiff :: (Ord k, Sized a)
                 => (a -> b -> Maybe a)
                 -> (k -> Ordering) -> (k -> Ordering)
                 -> SNode k a -> SNode k b -> SNode k a
hedgeDiff _ _     _     TIP _
  = tip
hedgeDiff _ cmplo cmphi BIN(kx x l r) TIP
  = join kx x (filterGt  cmplo l) (filterLt  cmphi r)
hedgeDiff  f cmplo cmphi t BIN(kx x l r) 
  = case found of
      Nothing -> merge  tl tr
      Just (ky,y) -> 
          case f y x of
            Nothing -> merge tl tr
            Just z  -> join ky z tl tr
  where
    cmpkx k     = compare kx k   
    lt          = trim cmplo cmpkx t
    (found,gt)  = trimLookupLo kx cmphi t
    tl          = hedgeDiff f cmplo cmpkx lt l
    tr          = hedgeDiff f cmpkx cmphi gt r