{-# LANGUAGE UndecidableInstances, TypeFamilies, BangPatterns #-}
module Data.TrieMap.Representation.Instances.ByteString () where

import Data.TrieMap.Representation.Class
import Data.TrieMap.Representation.Instances.Prim ()

import Data.Vector.Fusion.Stream
import Data.Vector.Fusion.Stream.Monadic (generateM, trans)

import Data.Word
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Array

import Data.ByteString.Internal

instance Repr ByteString where
  type Rep ByteString = RepStream Word8
  toRep !str = inlinePerformIO $ withByteString str $ \ ptr len ->
    return $! toRepStream (inlineIOStream (streamWord8Ptr ptr len))
  type RepStream ByteString = DRepStream ByteString
  toRepStream = dToRepStream

{-# INLINE withByteString #-}
withByteString :: ByteString -> (Ptr Word8 -> Int -> IO a) -> IO a
withByteString (PS fp off len) action = withForeignPtr fp $ \ ptr -> action (ptr `advancePtr` off) len

{-# INLINE inlineIOStream #-}
inlineIOStream :: MStream IO a -> Stream a
inlineIOStream stream = trans (return . inlinePerformIO) stream

{-# INLINE streamWord8Ptr #-}
streamWord8Ptr :: Ptr Word8 -> Int -> MStream IO Word8
streamWord8Ptr !ptr !len = generateM len (peekElemOff ptr)