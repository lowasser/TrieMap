{-# LANGUAGE TypeFamilies, FlexibleInstances, CPP, BangPatterns, ScopedTypeVariables, UndecidableInstances #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.TrieMap.Representation.Instances.Vectors (i2w) where

import Control.Monad.Primitive

import Data.Word
import Data.Int
import Data.Bits

import Foreign.Storable (Storable)
import Foreign.Ptr
import Foreign.ForeignPtr

import Data.Vector.Generic (convert, stream, unstream)
import qualified Data.Vector.Generic as G
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U

import Data.Vector.Fusion.Stream.Monadic
import Data.Vector.Fusion.Stream.Size

import Data.TrieMap.Utils
import Data.TrieMap.Representation.Class

import Prelude hiding (length)

#include "MachDeps.h"

#define DefList(ty) \
  type RepList (ty) = DRepList (ty); \
  toRepList = dToRepList

instance Repr a => Repr (V.Vector a) where
	type Rep (V.Vector a) = V.Vector (Rep a)
	toRep = V.map toRep
	DefList(V.Vector a)

instance Repr (S.Vector Word) where
	type Rep (S.Vector Word) = S.Vector Word
	toRep = id
	DefList(S.Vector Word)

{-# INLINE unsafeCastStorable #-}
unsafeCastStorable :: (Storable a, Storable b) => (Int -> Int) -> S.Vector a -> S.Vector b
unsafeCastStorable f xs = unsafeInlineST $ do
  S.MVector ptr n fp <- S.unsafeThaw xs
  let n' = f n
  S.unsafeFreeze (S.MVector (castPtr ptr) n' (castForeignPtr fp))

wordSize :: Int
wordSize = bitSize (0 :: Word)

#define VEC_WORD_INST(vec,wTy)			\
  instance Repr (vec wTy) where {		\
	type Rep (vec wTy) = Rep (S.Vector wTy);	\
	toRep xs = toHangingVector xs;\
	DefList(vec wTy)}
#define HANGINSTANCE(wTy)			\
    instance Repr (S.Vector wTy) where {	\
    	type Rep (S.Vector wTy) = (S.Vector Word, Word);\
    	{-# INLINE toRep #-};			\
    	toRep xs = toHangingVector xs;		\
    	DefList(S.Vector wTy) };		\
    VEC_WORD_INST(P.Vector,wTy);		\
    VEC_WORD_INST(U.Vector,wTy)

{-# INLINE toHangingVector #-}
toHangingVector :: (G.Vector v w, Bits w, Integral w, Storable w) => v w -> (S.Vector Word, Word)
toHangingVector !xs = (unstream (packStream (stream xs)), fromIntegral (G.length xs))

-- | @'Rep' ('S.Vector' 'Word8') = 'S.Vector' 'Word'@, by packing multiple 'Word8's into each 'Word' for space efficiency.
HANGINSTANCE(Word8)
-- | @'Rep' ('S.Vector' 'Word16') = 'S.Vector' 'Word'@, by packing multiple 'Word16's into each 'Word' for space efficiency.
HANGINSTANCE(Word16)
#if WORD_SIZE_IN_BITS == 32
instance Repr (S.Vector Word32) where
	type Rep (S.Vector Word32) = S.Vector Word
	toRep xs = unsafeCastStorable id xs
	DefList (S.Vector Word32)
instance Repr (U.Vector Word32) where
	type Rep (U.Vector Word32) = S.Vector Word
	toRep xs = unsafeCastStorable id (convert xs)
	DefList (U.Vector Word32)
instance Repr (P.Vector Word32) where
	type Rep (P.Vector Word32) = S.Vector Word
	toRep xs = unsafeCastStorable id (convert xs)
	DefList (P.Vector Word32)
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
	DefList(S.Vector Word64)

#define VEC_INT_INST(vec,iTy,wTy)		\
  instance Repr (vec iTy) where {		\
  	type Rep (vec iTy) = Rep (S.Vector wTy);	\
  	toRep = (toRep :: S.Vector wTy -> Rep (S.Vector wTy)) . convert . G.map (i2w :: iTy -> wTy); \
  	DefList(vec iTy)}
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
  	{-# INLINE toRep #-};				\
  	toRep xs = convert (G.map (fromIntegral . fromEnum) xs);\
  	DefList(vec ty)}
#define VEC_ENUM_INSTANCES(ty)	\
	VEC_ENUM_INST(ty,S.Vector);	\
	VEC_ENUM_INST(ty,P.Vector);	\
	VEC_ENUM_INST(ty,U.Vector)

-- | @'Rep' ('S.Vector' 'Char') = 'S.Vector' 'Word'@
VEC_ENUM_INSTANCES(Char)

-- | We embed IntN into WordN, but we have to be careful about overflow.
{-# INLINE [1] i2w #-}
i2w :: forall i w . (Integral i, Bits w, Bits i, Integral w) => i -> w
i2w !i	| i < 0		= mB - fromIntegral (-i)
	| otherwise	= mB + fromIntegral i
	where mB = bit (bitSize (0 :: i) - 1) :: w

data PackState s = PackState !Word !Int s | End

{-# INLINE packStream #-}
packStream :: forall m w . (Bits w, Integral w, Storable w, Monad m) => Stream m w -> Stream m Word
packStream (Stream step s0 size) = Stream step' s0' size'
  where	!ratio = wordSize `quoPow` bitSize (0 :: w)
	size' = case size of
	  Exact n	-> Exact $ (n + ratio - 1) `quoPow` ratio
	  Max n		-> Max $ (n + ratio - 1) `quoPow` ratio
	  Unknown	-> Unknown
	s0' = PackState 0 ratio s0
	step' End = return Done
	step' (PackState w 0 s) = return $ Yield w (PackState 0 ratio s)
	step' (PackState w i s) = do
	  s' <- step s
	  case s' of
	    Done  | i == ratio	-> return Done
		  | otherwise	-> return $ Yield (w .<<. (i * bitSize (0 :: w))) End
	    Skip s'		-> return $ Skip (PackState w i s')
	    Yield ww s'		-> return $ Skip (PackState ((w .<<. bitSize (0 :: w)) .|. fromIntegral ww) (i-1) s')