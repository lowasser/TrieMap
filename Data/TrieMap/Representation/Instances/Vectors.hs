{-# LANGUAGE TypeFamilies, FlexibleInstances, CPP, BangPatterns, UndecidableInstances, ScopedTypeVariables #-}
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

import Data.TrieMap.Utils
import Data.TrieMap.Representation.Class
import Data.TrieMap.Representation.Instances.Prim

#include "MachDeps.h"

instance Repr a => Repr (V.Vector a) where
	type Rep (V.Vector a) = V.Vector (Rep a)
	toRep = V.map toRep

instance Repr (S.Vector Word) where
	type Rep (S.Vector Word) = S.Vector Word
	toRep = id

{-# INLINE unsafeCastStorable #-}
unsafeCastStorable :: (Storable a, Storable b) => (Int -> Int) -> S.Vector a -> S.Vector b
unsafeCastStorable f xs = unsafeInlineST $ do
  S.MVector ptr n fp <- S.unsafeThaw xs
  let n' = f n
  S.unsafeFreeze (S.MVector (castPtr ptr) n' (castForeignPtr fp))

wordSize :: Int
wordSize = bitSize (0 :: Word)

{-# INLINE getWord #-}
getWord :: forall w . (Storable w, Bits w, Integral w) => S.Vector w -> Word
getWord = S.foldl' (\ x w -> (x `shiftL` bitSize (0 :: w)) .|. fromIntegral w) 0

{-# INLINE toWordVector #-}
toWordVector :: forall w . (Storable w, Integral w, Bits w) => S.Vector w -> S.Vector Word
toWordVector !xs = let
  !n = S.length xs
  wSize = bitSize (0 :: w)
  ratio = wordSize `quoPow` wSize
  n' = n `quoPow` ratio
  in case n `remPow` ratio of
    0	-> S.map (\ i -> getWord (S.unsafeSlice i ratio xs)) (S.enumFromStepN 0 ratio n')
    r	-> S.map (\ i -> getWord (S.unsafeSlice i ratio xs)) (S.enumFromStepN 0 ratio n')
	      `S.snoc` (getWord (S.unsafeDrop (n' * ratio) xs) .<<. (wSize * (ratio - r)))

#define HANGINSTANCE(wTy)					\
    instance Repr (S.Vector wTy) where				\
    	type Rep (S.Vector wTy) = (S.Vector Word);		\
    	{-# NOINLINE toRep #-};					\
    	toRep = toWordVector

-- | @'Rep' ('S.Vector' 'Word8') = 'S.Vector' 'Word'@, by packing multiple 'Word8's into each 'Word' for space efficiency.
HANGINSTANCE(Word8)
-- | @'Rep' ('S.Vector' 'Word16') = 'S.Vector' 'Word'@, by packing multiple 'Word16's into each 'Word' for space efficiency.
HANGINSTANCE(Word16)
#if WORD_SIZE_IN_BITS == 32
instance Repr (S.Vector Word32) where
	type Rep (S.Vector Word32) = S.Vector Word
	toRep xs = unsafeCastStorable id xs
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
	toRep xs = unsafeCastStorable (ratio *) xs
		where !wordBits = bitSize (0 :: Word); ratio = quoPow 64 wordBits

#define VEC_WORD_DOC(vec, wTy) {-| @'Rep' ('vec' 'wTy') = 'Rep' ('S.Vector' 'wTy')@ -}
#define VEC_WORD_INST(vec,wTy)				\
  instance Repr (vec wTy) where {			\
	type Rep (vec wTy) = Rep (S.Vector wTy);	\
	toRep = (toRep :: S.Vector wTy -> Rep (S.Vector wTy)) . convert}

VEC_WORD_INST(U.Vector,Word8)
VEC_WORD_INST(P.Vector,Word8)
VEC_WORD_INST(U.Vector,Word16)
VEC_WORD_INST(P.Vector,Word16)
VEC_WORD_INST(U.Vector,Word32)
VEC_WORD_INST(P.Vector,Word32)
VEC_WORD_INST(U.Vector,Word64)
VEC_WORD_INST(P.Vector,Word64)
VEC_WORD_INST(U.Vector,Word)
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

-- | @'Rep' ('S.Vector' 'Char') = 'S.Vector' 'Word'@
VEC_ENUM_INSTANCES(Char)