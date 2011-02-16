{-# LANGUAGE UndecidableInstances, TypeFamilies, BangPatterns, CPP #-}
module Data.TrieMap.Representation.Instances.ByteString () where

#include "MachDeps.h"

import Data.TrieMap.Representation.Class
import Data.TrieMap.Utils

import Control.Monad
import Data.Primitive.ByteArray

import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Array

import Data.Bits
import Data.Word

import Data.ByteString.Internal
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Data.Vector.Primitive

import Prelude

-- | @'Rep' 'ByteString' = 'Rep' ('Vector' 'Word8')@
instance Repr ByteString where
	type Rep ByteString = (Vector Word, Word)
	toRep !bs = (bsToRep bs, fromIntegral (B.length bs))
	type RepList ByteString = DRepList ByteString
	toRepList = dToRepList

bsToRep :: ByteString -> Vector Word
bsToRep (PS fp off n) = if n <= 0 then empty else inlinePerformIO $ withForeignPtr fp $ \ p0 -> 
  let !src = p0 `advancePtr` off :: Ptr Word8 in do
    !dest <- newByteArray (n' * bytesPerWord)
    let go !i = if ii < n' then (readWordAt src i >>= out >> go ii) else readLastWordAt n i src >>= out
	  where !ii = i + 1
		out = writeByteArray dest i
    go 0
    unsafeFreeze (MVector 0 n' dest)
  where n' = (n + (bytesPerWord - 1)) `quoPow` bytesPerWord

bytesPerWord :: Int
bytesPerWord = sizeOf (0 :: Word)

readWordAt :: Ptr Word8 -> Int -> IO Word
readWordAt ptr off = 
#if WORD_SIZE_IN_BITS == 32
  accum 3 $ accum 2 $ accum 1 $ accum 0 $ return 0
#else
  accum 7 $ accum 6 $ accum 5 $ accum 4 $ accum 3 $ accum 2 $ accum 1 $ accum 0 $ return 0
#endif
  where !off' = off * bytesPerWord
	accum x w = let s = 8 * (bytesPerWord - 1 - x) in
	  liftM2 (.|.) w $ liftM (\ w -> fromIntegral w .<<. s) $ peekElemOff ptr (x + off')

readLastWordAt :: Int -> Int -> Ptr Word8 -> IO Word
readLastWordAt !n !off !ptr =
  let	w0 = accum 0 (return 0)
	w1 = accum 1 w0
	w2 = accum 2 w1
	w3 = accum 3 w2
	w4 = accum 4 w3
	w5 = accum 5 w4
	w6 = accum 6 w5
	w7 = accum 7 w6
    in case n `remPow` bytesPerWord of
      1	-> w0
      2	-> w1
      3	-> w2
#if WORD_SIZE_IN_BITS > 32
      4	-> w3
      5	-> w4
      6	-> w5
      7	-> w6
      _	-> w7
#else
      _	-> w3
#endif
  where	!off' = off * bytesPerWord
	{-# INLINE accum #-}
	accum x w = let s = 8 * (bytesPerWord - 1 - x) in
	  liftM2 (.|.) w $ liftM (\ w -> fromIntegral w .<<. s) $ peekElemOff ptr (x + off')

instance Repr L.ByteString where
	type Rep L.ByteString = Rep ByteString
	toRep = toRep . B.concat . L.toChunks
	type RepList L.ByteString = DRepList L.ByteString
	toRepList = dToRepList