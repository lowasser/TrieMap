{-# LANGUAGE TypeFamilies, FlexibleInstances, CPP, BangPatterns, ScopedTypeVariables, UndecidableInstances, FlexibleContexts #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.TrieMap.Representation.Instances.Vectors () where

import Data.Vector.Generic (Vector, stream, unstream)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U

import Data.TrieMap.Representation.Class
import Data.TrieMap.Representation.Instances.Prim ()

import Data.Word
import Data.Int

import GHC.Exts

#define SPECV(vec,ty) (((vec) (ty)) -> RepStream (ty))
#define VSPECV(ty) SPECV(V.Vector,ty)
#define PSPECV(ty) SPECV(P.Vector,ty)
#define USPECV(ty) SPECV(U.Vector,ty)
#define SSPECV(ty) SPECV(S.Vector,ty)
#define APSPECV(ty) USPECV(ty), PSPECV(ty), SSPECV(ty)

{-# SPECIALIZE directToRep ::
      APSPECV(Int),
      APSPECV(Int8),
      APSPECV(Int16),
      APSPECV(Int32),
      APSPECV(Int64),
      APSPECV(Word),
      APSPECV(Word8),
      APSPECV(Word16),
      APSPECV(Word32),
      APSPECV(Word64),
      APSPECV(Char),
      APSPECV(Float),
      APSPECV(Double),
      USPECV(Bool),
      SSPECV(Bool),
      USPECV(())
 #-}
directToRep :: (Repr a, Vector v a) => v a -> RepStream a
directToRep xs = toRepStream (stream xs)

{-# INLINE [0] toRepImpl #-}
toRepImpl :: (Repr a, Vector v a) => v a -> RepStream a
toRepImpl xs = directToRep xs

#define VEC_REP_INST(vec) \
  type Rep ((vec) k) = RepStream k;	\
  {-# INLINE toRep #-};	\
  toRep xs = toRepImpl xs;	\
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

{-# RULES
      "toRepImpl/unstream" forall stream . toRepImpl (unstream stream) = inline toRepStream stream;
      #-}