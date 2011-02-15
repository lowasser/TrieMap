{-# LANGUAGE UndecidableInstances, TypeFamilies, BangPatterns #-}
module Data.TrieMap.Representation.Instances.ByteString () where

import Data.TrieMap.Representation.Class
import Data.TrieMap.Utils

import Control.Monad

import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr

import Data.Bits
import Data.Word

import Data.ByteString.Internal
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Data.Vector.Storable

import Prelude

-- | @'Rep' 'ByteString' = 'Rep' ('Vector' 'Word8')@
instance Repr ByteString where
	type Rep ByteString = (Vector Word, Word)
	toRep !bs = (bsToRep bs, fromIntegral (B.length bs))
	type RepList ByteString = DRepList ByteString
	toRepList = dToRepList

bsToRep :: ByteString -> Vector Word
bsToRep (PS fp off n) = inlinePerformIO $ withForeignPtr fp $ \ p0 -> let !src = p0 `plusPtr` off in do
  !destFP <- mallocForeignPtrArray n'
  withForeignPtr destFP $ \ dest -> do
    let finish = do
	  let !i' = (n' - 1) `shiftL` 2
	  let {-# INLINE w0 #-}
	      w0 = liftM (\ w -> fromIntegral w .<<. 24) (peekElemOff src i')
	      {-# INLINE w1 #-}
	      w1 = liftM2 (.|.) w0 $ liftM (\ w -> fromIntegral w .<<. 16) (peekElemOff src (i' + 1))
	      {-# INLINE w2 #-}
	      w2 = liftM2 (.|.) w1 $ liftM (\ w -> fromIntegral w .<<. 8) (peekElemOff src (i' + 2))
	      {-# INLINE w3 #-}
	      w3 = liftM2 (.|.) w2 $ liftM fromIntegral (peekElemOff src (i' + 3))
	      out = pokeElemOff dest (n' - 1)
	  case n .&. 3 of
	    1	-> out =<< w0
	    2	-> out =<< w1
	    3	-> out =<< w2
	    _	-> out =<< w3
    let go !i = let !ii = i + 1 in if ii < n' then (let !i' = i `shiftL` 2 in do
	  w0 <- peekElemOff src i' :: IO Word8
	  w1 <- peekElemOff src (i' + 1)
	  w2 <- peekElemOff src (i' + 2)
	  w3 <- peekElemOff src (i' + 3)
	  let w = fromIntegral w3 .|. 
		  (fromIntegral w2 .<<. 8) .|. 
		  (fromIntegral w1 .<<. 16) .|. 
		  (fromIntegral w0 .<<. 24)
	  pokeElemOff dest i (w :: Word)
	  go ii) else finish
    go 0
    unsafeFreeze (MVector dest n' destFP)
  where n' = (n + 3) `shiftR` 2

instance Repr L.ByteString where
	type Rep L.ByteString = Rep ByteString
	toRep = toRep . B.concat . L.toChunks
	type RepList L.ByteString = DRepList L.ByteString
	toRepList = dToRepList