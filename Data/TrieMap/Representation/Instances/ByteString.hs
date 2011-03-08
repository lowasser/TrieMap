{-# LANGUAGE UndecidableInstances, TypeFamilies, BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.TrieMap.Representation.Instances.ByteString () where

import Control.Monad.Primitive

import Data.TrieMap.Representation.Class
import Data.TrieMap.Representation.Instances.Prim ()

import Data.Vector.Fusion.Stream.Monadic (trans, generateM)

import Data.Word
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Array

import Data.Vector.Primitive
import Data.ByteString.Internal

instance Repr ByteString where
  type Rep ByteString = (Vector Word, Word)
  toRep !str = inlinePerformIO $ withByteString str $ \ ptr len ->
    primToIO $ toRepStreamM (trans primToPrim $ generateM len (peekElemOff ptr))
  type RepStream ByteString = DRepStream ByteString
  toRepStreamM = dToRepStreamM

{-# INLINE withByteString #-}
withByteString :: ByteString -> (Ptr Word8 -> Int -> IO a) -> IO a
withByteString (PS fp off len) action = withForeignPtr fp $ \ ptr -> action (ptr `advancePtr` off) len
