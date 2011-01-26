{-# LANGUAGE TypeFamilies, FlexibleInstances, CPP, BangPatterns, UndecidableInstances, TemplateHaskell, ScopedTypeVariables #-}
module Data.TrieMap.Representation.Instances.Vectors () where

import Control.Monad.Primitive

import Data.Word
import Data.Int
import Data.Bits

import Foreign.Storable (Storable)
import Foreign.Ptr
import Foreign.ForeignPtr

import Data.Vector.Generic (convert)
import qualified Data.Vector.Generic as G
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U

import Data.TrieMap.Representation.Class
import Data.TrieMap.Representation.Instances.Prim

import Language.Haskell.TH.Syntax

#include "MachDeps.h"

instance Repr a => Repr (V.Vector a) where
	type Rep (V.Vector a) = V.Vector (Rep a)
	toRep = V.map toRep

instance Repr (S.Vector Word) where
	type Rep (S.Vector Word) = S.Vector Word
	toRep = id

type Overhang = Word
-- When storing a vector of WordNs, we view it as a vector of Words plus an overhang.
-- We store the length of the overhang (which can be up to (WORD_SIZE_IN_BITS / N - 1)) in the top
-- N bits of the Overhang, and k leftover WordNs (however large k is) in the low kN bits of the Overhang.

-- Just a version of 'quot' for dividing by powers of 2.
quoPow :: Int -> Int -> Int
quoPow n d = $(foldr ($) [| n `quot` d |] 
		[\ other -> [| if d == $(lift (bit i :: Int)) then n `shiftR` $(lift i) else $other |]
			| i <- [0..6]])

-- Just a version of 'rem' for modding by powers of 2.
remPow :: Int -> Int -> Int
remPow n d = n .&. (d - 1)

unsafeToPtr :: Storable a => S.Vector a -> (Ptr a, Int, ForeignPtr a)
unsafeToPtr xs = unsafeInlineST $ do
	S.MVector ptr n fp <- S.unsafeThaw xs
	return (ptr, n, fp)

unsafeFromPtr :: Storable a => Ptr b -> Int -> ForeignPtr b -> S.Vector a
unsafeFromPtr ptr n fp = unsafeInlineST $ S.unsafeFreeze (S.MVector (castPtr ptr) n (castForeignPtr fp))

#define HANGINSTANCE(wTy)					\
    instance Repr (S.Vector wTy) where				\
    	type Rep (S.Vector wTy) = (S.Vector Word);		\
    	toRep = S.map fromIntegral

HANGINSTANCE(Word8)
HANGINSTANCE(Word16)
#if WORD_SIZE_IN_BITS == 32
instance Repr (S.Vector Word32) where
	type Rep (S.Vector Word32) = S.Vector Word
	toRep xs = case unsafeToPtr xs of
		(p, n, fp) -> unsafeFromPtr p n fp
#elif WORD_SIZE_IN_BITS > 32
HANGINSTANCE(Word32)
#endif

#if WORD_SIZE_IN_BITS == 32
-- | @'Rep' ('S.Vector' 'Word64') = 'S.Vector' 'Word'@, by viewing each 'Word64' as two 'Word's.
#else
-- | @'Rep' ('S.Vector' 'Word64') = 'S.Vector' 'Word'@
#endif
instance Repr (S.Vector Word64) where
	type Rep (S.Vector Word64) = S.Vector Word
	toRep xs = case unsafeToPtr xs of
		(p, n, fp) -> unsafeFromPtr p (n * ratio) fp
		where !wordBits = bitSize (0 :: Word); ratio = quoPow 64 wordBits

#define VEC_WORD_DOC(vec, wTy) {-| @'Rep' ('vec' 'wTy') = 'Rep' ('S.Vector' 'wTy')@ -}
#define VEC_WORD_INST(vec,wTy)				\
  instance Repr (vec wTy) where {			\
	type Rep (vec wTy) = Rep (S.Vector wTy);	\
	toRep = (toRep :: S.Vector wTy -> Rep (S.Vector wTy)) . convert}

VEC_WORD_DOC(U.Vector,Word8)
VEC_WORD_INST(U.Vector,Word8)
VEC_WORD_DOC(P.Vector,Word8)
VEC_WORD_INST(P.Vector,Word8)
VEC_WORD_DOC(U.Vector,Word16)
VEC_WORD_INST(U.Vector,Word16)
VEC_WORD_DOC(P.Vector,Word16)
VEC_WORD_INST(P.Vector,Word16)
VEC_WORD_DOC(U.Vector,Word32)
VEC_WORD_INST(U.Vector,Word32)
VEC_WORD_DOC(P.Vector,Word32)
VEC_WORD_INST(P.Vector,Word32)
VEC_WORD_DOC(U.Vector,Word64)
VEC_WORD_INST(U.Vector,Word64)
VEC_WORD_DOC(P.Vector,Word64)
VEC_WORD_INST(P.Vector,Word64)
VEC_WORD_DOC(U.Vector,Word)
VEC_WORD_INST(U.Vector,Word)
VEC_WORD_DOC(P.Vector,Word)
VEC_WORD_INST(P.Vector,Word)

#define VEC_INT_INST(vec,iTy,wTy)			\
  instance Repr (vec iTy) where {			\
  	type Rep (vec iTy) = Rep (S.Vector wTy);	\
  	toRep = (toRep :: S.Vector wTy -> Rep (S.Vector wTy)) . convert . G.map (i2w :: iTy -> wTy)}
#define VEC_INT_INSTANCES(iTy,wTy)	\
	VEC_INT_INST(S.Vector,iTy,wTy); \
	VEC_INT_INST(P.Vector,iTy,wTy); \
	VEC_INT_INST(U.Vector,iTy,wTy)

VEC_INT_INSTANCES(Int8, Word8)
VEC_INT_INSTANCES(Int16, Word16)
VEC_INT_INSTANCES(Int32, Word32)
VEC_INT_INSTANCES(Int64, Word64)
VEC_INT_INSTANCES(Int, Word)

#define VEC_ENUM_INST(ty, vec)				\
  instance Repr (vec ty) where {			\
  	type Rep (vec ty) = S.Vector Word;		\
  	toRep = convert . G.map (fromIntegral . fromEnum)}
#define VEC_ENUM_INSTANCES(ty)	\
	VEC_ENUM_INST(ty,S.Vector);	\
	VEC_ENUM_INST(ty,P.Vector);	\
	VEC_ENUM_INST(ty,U.Vector)

VEC_ENUM_INSTANCES(Char)