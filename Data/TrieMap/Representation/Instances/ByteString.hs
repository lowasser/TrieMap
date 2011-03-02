{-# LANGUAGE UndecidableInstances, TypeFamilies, BangPatterns #-}
module Data.TrieMap.Representation.Instances.ByteString () where

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

import Data.Vector.Primitive

import Prelude hiding (foldr)

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
    let go !i = if ii < n' then (readWordAt src i (\ w -> out w >> go ii)) else readLastWordAt n i src out
	  where !ii = i + 1
		out = writeByteArray dest i
    go 0
    unsafeFreeze (MVector 0 n' dest)
  where n' = (n + (bytesPerWord - 1)) `quoPow` bytesPerWord

bytesPerWord :: Int
bytesPerWord = sizeOf (0 :: Word)

{-# INLINE readWordAt #-}
readWordAt :: Ptr Word8 -> Int -> (Word -> IO a) -> IO a
readWordAt !ptr !off ret = accum 0 0 where
  !off' = off * bytesPerWord
  accum i w
    | i < bytesPerWord
      = do	let !s = 8 * (bytesPerWord - 1 - i)
		!w8 <- peekElemOff ptr (i + off')
		accum (i+1) (w .|. (fromIntegral w8 .<<. s))
    | otherwise
      = ret w

{-# INLINE readLastWordAt #-}
readLastWordAt :: Int -> Int -> Ptr Word8 -> (Word -> IO a) -> IO a
readLastWordAt !n !off !ptr ret = case n `remPow` bytesPerWord of
  0	-> readWordAt ptr off ret
  r	-> run 0 0 where
    run !i !w
      | i < r	= do
	  let s = 8 * (bytesPerWord - 1 - i) 
	  !w8 <- peekElemOff ptr (i + off')
	  run (i+1) (w .|. (fromIntegral w8 .<<. s))
      | otherwise = ret w
    !off' = off * bytesPerWord