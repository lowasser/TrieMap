{-# LANGUAGE ScopedTypeVariables, BangPatterns, TypeFamilies, UndecidableInstances, CPP #-}
module Data.TrieMap.Representation.Instances.Prim (i2w) where

#include "MachDeps.h"

import Data.TrieMap.Representation.Class
import Data.Word
import Data.Int
import Data.Char
import Data.Bits

#define WDOC(ty) {-| @'Rep' 'ty' = 'Word'@ -}
WDOC(Char)
instance Repr Char where
	type Rep Char = Word
	toRep = fromIntegral . ord

#define WREPR(wTy) \
instance Repr wTy where { \
	type Rep wTy = Word; \
	toRep = fromIntegral}

WREPR(Word)
WDOC(Word8)
WREPR(Word8)
WDOC(Word16)
WREPR(Word16)
WDOC(Word32)
WREPR(Word32)

#if WORD_SIZE_IN_BITS < 64
-- | @'Rep' 'Word64' = ('Word', 'Word')@
instance Repr Word64 where
	type Rep Word64 = (Rep Word32, Rep Word32)
	toRep w = (toRep pre, toRep suf)
		where	pre = fromIntegral (w `shiftR` 32) :: Word32
			suf = fromIntegral w :: Word32
#else
WDOC(Word64)
WREPR(Word64)
#endif

-- | We embed IntN into WordN, but we have to be careful about overflow.
{-# INLINE [1] i2w #-}
i2w :: forall i w . (Integral i, Bits w, Bits i, Integral w) => i -> w
i2w !i	| i < 0		= mB - fromIntegral (-i)
	| otherwise	= mB + fromIntegral i
	where mB = bit (bitSize (0 :: i) - 1) :: w

#define IREPR(iTy,wTy) \
instance Repr iTy where { \
	type Rep iTy = Rep wTy; \
	toRep = toRep . (i2w :: iTy -> wTy)}

IREPR(Int8,Word8)
IREPR(Int16,Word16)
IREPR(Int32,Word32)
IREPR(Int64,Word64)
-- | @'Rep' 'Int' = 'Word'@, by way of a careful translation of their domains to avoid overflow.
IREPR(Int,Word)