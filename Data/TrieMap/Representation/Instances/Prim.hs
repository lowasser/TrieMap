{-# LANGUAGE ScopedTypeVariables, BangPatterns, TypeFamilies, UndecidableInstances, CPP, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.TrieMap.Representation.Instances.Prim () where

import Data.TrieMap.Representation.Class
import Data.TrieMap.Representation.Instances.Basic ()
import Data.TrieMap.Representation.Instances.Prim.Bool
import Data.TrieMap.Utils

import Data.Word
import Data.Int
import Data.Char
import Data.Bits

import Data.Vector.Primitive
import Data.Vector.Generic (unstream)
import Data.Vector.Fusion.Util
import Data.Vector.Fusion.Stream.Size
import Data.Vector.Fusion.Stream.Monadic (Stream(..), Step(..))

import Prelude hiding (map)
import GHC.Exts

#define WDOC(ty) {-| @'Rep' 'ty' = 'Word'@ -}

WDOC(Char)
instance Repr Char where
	type Rep Char = Word
	type RepStream Char = RepStream Word
	toRep = fromIntegral . ord
	{-# INLINE toRepStream #-}
	toRepStream xs = toRepStream (fmap toRep xs)

#define WREPR(wTy) \
instance Repr wTy where { \
	type Rep wTy = Word; \
	toRep = fromIntegral; \
	type RepStream wTy = (Vector Word, Word);\
	{-# INLINE toRepStream #-};\
	toRepStream xs = wStreamToRep xs}

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
	type RepStream Word64 = RepStream Word32
	{-# INLINE toRepStream #-}
	toRepStream xs = toRepStream (split64Stream xs)

data Word64S s = S0 s | S1 !Word32 s

{-# INLINE split64Stream #-}
split64Stream :: Monad m => Stream m Word64 -> Stream m Word32
split64Stream (Stream step s0 size) = Stream step' (S0 s0) (2 * size) where
  step' (S1 w32 s) = return (Yield w32 (S0 s))
  step' (S0 s) = do
    st <- step s
    case st of
      Yield w64 s' -> let
	!hi = fromIntegral (w64 `shiftR` 32) :: Word32
	!lo = fromIntegral w64 :: Word32
	in return (Yield hi (S1 lo s'))
      Skip s' -> return (Skip (S0 s'))
      Done -> return Done

#else
WDOC(Word64)
WREPR(Word64)
#endif

#define IREPR(iTy,wTy) \
instance Repr iTy where { \
	type Rep iTy = Rep wTy; \
	toRep x = toRep ((i2w :: iTy -> wTy) x); \
	type RepStream iTy = RepStream wTy; \
	{-# INLINE toRepStream #-};	\
	toRepStream xs = toRepStream (fmap (i2w :: iTy -> wTy) xs)}

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
  {-# INLINE toRepStream #-}
  toRepStream xs = wStreamToRep (fmap BBool xs)

-- | We embed IntN into WordN in an order-preserving fashion.
{-# INLINE i2w #-}
i2w :: forall i w . (Integral i, Bounded i, Bits w, Integral w) => i -> w
i2w !i = fromIntegral i `xor` fromIntegral (minBound :: i)

data PackState s = PackState !Word !Int s | Last !Int | End
{-# ANN type PackState ForceSpecConstr #-}

{-# INLINE wStreamToRep #-}
wStreamToRep :: (Bits w, Integral w) => Stream Id w -> (Vector Word, Word)
wStreamToRep xs = let !ys = unstream (packStream xs) in (unsafeInit ys, unsafeLast ys)

{-# INLINE packStream #-}
packStream :: forall m w . (Bits w, Integral w, Monad m) => Stream m w -> Stream m Word
packStream (Stream step s0 size) = Stream step' s0' size'
  where	!ratio = wordSize `quoPow` bitSize (0 :: w)
	size' = 1 + case size of
	  Exact n	-> Exact $ (n + ratio - 1) `quoPow` ratio
	  Max n		-> Max $ (n + ratio - 1) `quoPow` ratio
	  Unknown	-> Unknown
	s0' = PackState 0 ratio s0
	step' End = return Done
	step' (Last i) = return $ Yield (fromIntegral i) End
	step' (PackState w 0 s) = return $ Yield w (PackState 0 ratio s)
	step' (PackState w i s) = do
	  s' <- step s
	  case s' of
	    Done  | i == ratio	-> return $ Skip (Last 0)
		  | otherwise	-> return $ Yield (w .<<. (i * bitSize (0 :: w))) (Last (ratio - i))
	    Skip s'		-> return $ Skip (PackState w i s')
	    Yield ww s'		-> return $ Skip (PackState ((w .<<. bitSize (0 :: w)) .|. fromIntegral ww) (i-1) s')

wordSize :: Int
wordSize = bitSize (0 :: Word)