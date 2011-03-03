{-# LANGUAGE TypeFamilies, FlexibleInstances, CPP, BangPatterns, ScopedTypeVariables, UndecidableInstances, FlexibleContexts #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.TrieMap.Representation.Instances.Vectors () where

import Data.Vector.Generic (stream)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U

import Data.TrieMap.Representation.Class

#define VEC_REP_INST(vec) \
  type Rep ((vec) k) = RepStream k;	\
  {-# INLINE toRep #-};			\
  toRep xs = toRepStream (stream xs);	\
  type RepStream ((vec) k) = DRepStream ((vec) k);\
  toRepStreamM = dToRepStreamM

instance Repr k => Repr (V.Vector k) where
  VEC_REP_INST(V.Vector)

instance (U.Unbox k, Repr k) => Repr (U.Vector k) where
  VEC_REP_INST(U.Vector)

instance (P.Prim k, Repr k) => Repr (P.Vector k) where
  VEC_REP_INST(P.Vector)

instance (S.Storable k, Repr k) => Repr (S.Vector k) where
  VEC_REP_INST(S.Vector)