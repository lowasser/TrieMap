{-# LANGUAGE ScopedTypeVariables, BangPatterns, TypeFamilies, UndecidableInstances, CPP, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS -funbox-strict-fields #-}
#if __GLASGOW_HASKELL__ >= 700
{-# OPTIONS -fllvm #-}
#endif
module Data.TrieMap.Representation.Instances.Prim () where

import Control.Monad.Primitive

import Data.TrieMap.Representation.Class
-- import Data.TrieMap.Representation.Instances.Basic ()
import Data.TrieMap.Representation.Instances.Prim.Bool
import Data.TrieMap.Utils

import Data.Word
import Data.Int
import Data.Char
import Data.Bits

import Data.Vector.Primitive
import Data.Vector.Fusion.Stream.Size
import Data.Vector.Fusion.Stream.Monadic (Stream(..), Step(..))

import Prelude hiding (map)
-- import GHC.Exts

instance Repr Word where
  type Rep Word = Word
  toRep = id
  type RepStream Word = Vector Word
  {-# INLINE toRepStreamM #-}
  toRepStreamM strm = unstreamM strm

#define WDOC(ty) {-| @'Rep' 'ty' = 'Word'@ -}

WDOC(Char)
instance Repr Char where
	type Rep Char = Word
	type RepStream Char = RepStream Word
	toRep = fromIntegral . ord
	{-# INLINE toRepStreamM #-}
	toRepStreamM xs = toRepStreamM (fmap toRep xs)

#define WREPR(wTy) \
instance Repr wTy where { \
	type Rep wTy = Word; \
	toRep = fromIntegral; \
	type RepStream wTy = (Vector Word, Word);\
	{-# INLINE toRepStreamM #-};\
	toRepStreamM xs = wStreamToRep xs}

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
	{-# INLINE toRepStreamM #-}
	toRepStreamM xs = toRepStreamM (split64Stream xs)

data Word64S s = S0 s | S1 !Word32 s

{-# INLINE split64Stream #-}
split64Stream :: Monad m => Stream m Word64 -> Stream m Word32
split64Stream (Stream step s0 size) = Stream step' (S0 s0) size' where
  size' = case size of
    Max n -> Max (2 * n)
    Exact n -> Exact (2 * n)
    Unknown -> Unknown
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
	{-# INLINE toRepStreamM #-};	\
	toRepStreamM xs = toRepStreamM (fmap (i2w :: iTy -> wTy) xs)}

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
  {-# INLINE toRepStreamM #-}
  toRepStreamM xs = wStreamToRep (fmap BBool xs)

-- | We embed IntN into WordN in an order-preserving fashion.
{-# INLINE i2w #-}
i2w :: forall i w . (Integral i, Bounded i, Bits w, Integral w) => i -> w
i2w !i = fromIntegral i `xor` fromIntegral (minBound :: i)

{-# INLINE wStreamToRep #-}
wStreamToRep :: (Bits w, Integral w, PrimMonad m) => Stream m w -> m (Vector Word, Word)
wStreamToRep xs = do
  !ys <- unstreamM (packStream xs)
  return (unsafeInit ys, unsafeLast ys)

wordSize :: Int
wordSize = bitSize (0 :: Word)

data State s = Start !s |
  Normal !Int !Word !s | End !Int | Stop

{-# INLINE packStream #-}
packStream :: forall m w . (Bits w, Integral w, Monad m) => Stream m w -> Stream m Word
packStream (Stream step s0 size) = Stream step' s0' size' where
  wSize = bitSize (0 :: w)
  
  ratio = wordSize `quoPow` wSize
  
  s0' = Start s0
  size' = 2 + case size of
    Exact n -> Exact (( n) `quoPow` ratio)
    Max n -> Max ((n) `quoPow` ratio)
    Unknown -> Unknown
  
  step' Stop = return Done
  step' (End i) = return (Yield (fromIntegral i) Stop)
  step' (Start s) = do
    st <- step s
    case st of
      Skip s' -> return $ Skip $ Start s'
      Done -> return (Yield (fromIntegral ratio) Stop)
      Yield ww s' -> return $ Skip (Normal 1 (fromIntegral ww) s')
  step' (Normal i w s)
    | i < ratio =  do
	st <- step s
	case st of
	  Skip s' -> return $ Skip (Normal i w s')
	  Yield ww s' -> return $ Skip (Normal (i+1) ((w .<<. wSize) .|. fromIntegral ww) s')
	  Done  -> return $ Yield (w .<<. (wSize * (ratio - i))) (End i)
    | otherwise = return (Yield w (Start s))