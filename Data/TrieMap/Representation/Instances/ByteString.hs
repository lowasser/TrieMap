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
bsToRep (PS fp off n) = inlinePerformIO $ withForeignPtr fp $ \ p0 -> let !src = p0 `plusPtr` off :: Ptr Word8 in do
  !destFP <- mallocForeignPtrArray n'
  withForeignPtr destFP $ \ dest -> do
    let {-# INLINE accum #-}
      	accum :: Int -> Int -> IO Word -> IO Word
	accum x s w = liftM2 (.|.) w (liftM (\ w -> fromIntegral w .<<. s) (peekElemOff src x))
    let finish = do
	  let !i' = (n' - 1) `shiftL` 2; accum' = accum . (i' +)
	  let w0 = accum' 0 24 (return 0)
	      w1 = accum' 1 16 w0
	      w2 = accum' 2 8 w1
	      w3 = accum' 3 0 w2
	      out = pokeElemOff dest (n' - 1)
	  case n .&. 3 of
	    1	-> out =<< w0
	    2	-> out =<< w1
	    3	-> out =<< w2
	    _	-> out =<< w3
    let go !i = let !ii = i + 1 in if ii < n' then (let !i' = i `shiftL` 2; accum' = accum . (i' +) in do
	  w <- accum' 3 0 $ accum' 2 8 $ accum' 1 16 $ accum' 0 24 $ return 0
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