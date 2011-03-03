{-# LANGUAGE TypeFamilies, FlexibleInstances, CPP, BangPatterns, ScopedTypeVariables, UndecidableInstances, FlexibleContexts #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.TrieMap.Representation.Instances.Vectors () where

import Data.Word
import Data.Int

import Data.Vector.Generic (stream)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U

import Data.TrieMap.Representation.Class
import Data.TrieMap.Representation.Instances.Prim ()
import Data.TrieMap.Representation.Instances.Foreign ()

import Foreign.C.Types

#include "MachDeps.h"

#define DefStream(ty) \
  type RepStream (ty) = DRepStream (ty); \
  toRepStream = dToRepStream

#define VEC_INSTANCE(vec, ty)	\
instance Repr (vec ty) where {	\
  type Rep (vec ty) = RepStream ty;	\
  {-# INLINE toRep #-};			\
  toRep xs = toRepStream (stream xs);	\
  DefStream(vec ty)}

#define SPEC_INSTANCE(vec, ty) {-# SPECIALIZE instance Repr ((vec) (ty)) #-}
#define SPEC_INT_INSTANCES(vec) \
SPEC_INSTANCE(vec,Int);		\
SPEC_INSTANCE(vec,Int8);	\
SPEC_INSTANCE(vec,Int16);	\
SPEC_INSTANCE(vec,Int32);	\
SPEC_INSTANCE(vec,Int64)

#define SPEC_WORD_INSTANCES(vec) \
SPEC_INSTANCE(vec,Word);	\
SPEC_INSTANCE(vec,Word8);	\
SPEC_INSTANCE(vec,Word16);	\
SPEC_INSTANCE(vec,Word32);	\
SPEC_INSTANCE(vec,Word64)

#define SPEC_PRIM_INSTANCES(vec) \
SPEC_INT_INSTANCES(vec);	\
SPEC_WORD_INSTANCES(vec);	\
SPEC_INSTANCE(vec,Char);	\
SPEC_INSTANCE(vec,Double);	\
SPEC_INSTANCE(vec,Float)

#define SPEC_UNBOX_INSTANCES(vec) \
SPEC_PRIM_INSTANCES(vec);	\
SPEC_INSTANCE(vec,Bool);	\
SPEC_INSTANCE(vec,())

#define SPEC_C_INSTANCES(vec) \
SPEC_INSTANCE(vec,CChar);	\
SPEC_INSTANCE(vec,CSChar);	\
SPEC_INSTANCE(vec,CUChar);	\
SPEC_INSTANCE(vec,CShort);	\
SPEC_INSTANCE(vec,CUShort);	\
SPEC_INSTANCE(vec,CInt);	\
SPEC_INSTANCE(vec,CUInt);	\
SPEC_INSTANCE(vec,CLong);	\
SPEC_INSTANCE(vec,CULong);	\
SPEC_INSTANCE(vec,CLLong);	\
SPEC_INSTANCE(vec,CULLong);	\
SPEC_INSTANCE(vec,CFloat);	\
SPEC_INSTANCE(vec,CDouble)

instance Repr k => Repr (V.Vector k) where
  SPEC_UNBOX_INSTANCES(V.Vector)
  type Rep (V.Vector k) = RepStream k
  {-# INLINE toRep #-}
  toRep xs = toRepStream (stream xs)
  DefStream(V.Vector k)

instance (U.Unbox k, Repr k) => Repr (U.Vector k) where
  SPEC_UNBOX_INSTANCES(U.Vector)
  type Rep (U.Vector k) = RepStream k
  {-# INLINE toRep #-}
  toRep xs = toRepStream (stream xs)
  DefStream(U.Vector k)

instance (P.Prim k, Repr k) => Repr (P.Vector k) where
  SPEC_PRIM_INSTANCES(P.Vector)
  type Rep (P.Vector k) = RepStream k
  {-# INLINE toRep #-}
  toRep xs = toRepStream (stream xs)
  DefStream(P.Vector k)

instance (S.Storable k, Repr k) => Repr (S.Vector k) where
  SPEC_PRIM_INSTANCES(S.Vector)
  SPEC_INSTANCE(S.Vector,Bool)
  SPEC_C_INSTANCES(S.Vector)
  type Rep (S.Vector k) = RepStream k
  {-# INLINE toRep #-}
  toRep xs = toRepStream (stream xs)
  DefStream(S.Vector k)