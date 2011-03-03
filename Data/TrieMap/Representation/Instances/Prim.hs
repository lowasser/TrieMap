{-# LANGUAGE ScopedTypeVariables, BangPatterns, TypeFamilies, UndecidableInstances, CPP #-}
module Data.TrieMap.Representation.Instances.Prim () where

#include "MachDeps.h"

import Data.TrieMap.Representation.Class
import Data.TrieMap.Representation.Instances.Vectors
import Data.TrieMap.Representation.Instances.Basic ()
import Data.Word
import Data.Int
import Data.Char
import Data.Bits
import Data.Vector.Primitive
import qualified Data.Vector.Unboxed as U
import Data.Vector.Generic (unstream)
import Prelude hiding (map)

#define WDOC(ty) {-| @'Rep' 'ty' = 'Word'@ -}

WDOC(Char)
instance Repr Char where
	type Rep Char = Word
	type RepStream Char = Vector Word
	toRep = fromIntegral . ord
	toRepStream xs = toRepStream (fmap toRep xs)

#define WREPR(wTy) \
instance Repr wTy where { \
	type Rep wTy = Word; \
	toRep = fromIntegral; \
	type RepStream wTy = Rep (Vector wTy);\
	{-# INLINE toRepStream #-};\
	toRepStream xs = toRep (unstream xs :: Vector wTy)}

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
	type RepStream Word64 = Vector Word
	toRepStream xs = toRep (unstream xs :: Vector Word64)
#else
WDOC(Word64)
WREPR(Word64)
#endif

#define IREPR(iTy,wTy) \
instance Repr iTy where { \
	type Rep iTy = Rep wTy; \
	toRep = toRep . (i2w :: iTy -> wTy); \
	type RepStream iTy = Rep (Vector wTy); \
	toRepStream xs = toRep (unstream xs :: Vector iTy)}

IREPR(Int8,Word8)
IREPR(Int16,Word16)
IREPR(Int32,Word32)
IREPR(Int64,Word64)
-- | @'Rep' 'Int' = 'Word'@, by way of a careful translation of their domains to avoid overflow.
IREPR(Int,Word)

instance Repr Bool where
  type Rep Bool = Either () ()
  toRep False = Left ()
  toRep True = Right ()
  type RepStream Bool = (Vector Word, Word)
  toRepStream xs = toRep (unstream xs :: U.Vector Bool)