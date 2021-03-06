{-# LANGUAGE UndecidableInstances, TypeFamilies, BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.TrieMap.Representation.Instances.ByteString () where

import Control.Monad.Primitive

import Data.TrieMap.Representation.Class

import Data.Vector.Fusion.Stream.Monadic (Stream(..), Step(..), trans)
import Data.Vector.Fusion.Stream.Size

import Data.Word
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Array

import Data.TrieMap.Utils
import  Data.Bits

import Data.Vector.Primitive
import Data.ByteString.Internal

instance Repr ByteString where
  type Rep ByteString = (Vector Word, Word)
  toRep !str = inlinePerformIO $ withByteString str $ \ ptr len -> do
--     primToIO $ toRepStreamM (trans primToPrim $ generateM len (peekElemOff ptr))
      !xs <- unstreamM $ trans primToPrim $ streamBS ptr len
      return (xs, fromIntegral len)
  type RepStream ByteString = DRepStream ByteString
  toRepStreamM = dToRepStreamM

{-# INLINE withByteString #-}
withByteString :: ByteString -> (Ptr Word8 -> Int -> IO a) -> IO a
withByteString (PS fp off len) action = withForeignPtr fp $ \ ptr -> action (ptr `advancePtr` off) len

data State = Normal {iIn :: !Int, 
		iBlock :: !Int, tmp :: !Word}
	      | Start {iIn :: !Int}
	      | Ending {iIn :: !Int, tmp :: !Word}
	      | Stop

streamBS :: Ptr Word8 -> Int -> Stream IO Word
streamBS !src !n = Stream step Start{iIn = 0} (Exact n')
  where	!ratio = bitSize (0 :: Word) `quoPow` wSize
	!wSize = bitSize (0 :: Word8)
	!n' = (ratio - 1 + n) `quoPow` ratio

	step Stop = return Done
	step Start{iIn}
	  | iIn < 1 - ratio + n	= return (Skip Normal{iIn, iBlock = 0, tmp = 0})
	  | n `remPow` ratio == 0
	  			= return Done
	  | otherwise		= return (Skip Ending{iIn, tmp = 0})
	step Normal{iIn, iBlock, tmp}
	  | iBlock < ratio	= do
	      ww <- peekElemOff src iIn
	      return $ Skip Normal{iIn = iIn + 1, iBlock = iBlock + 1, tmp = (tmp .<<. wSize) .|. fromIntegral ww}
	  | otherwise		= return $ Yield tmp Start{iIn}
	step Ending{iIn, tmp}
	  | iIn < n	= do
	      ww <- peekElemOff src iIn
	      return $ Skip Ending{iIn = iIn + 1, tmp = (tmp .<<. wSize) .|. fromIntegral ww}
	  | otherwise	= return $ Yield (tmp .<<. (wSize * (ratio - (n `remPow` ratio)))) Stop