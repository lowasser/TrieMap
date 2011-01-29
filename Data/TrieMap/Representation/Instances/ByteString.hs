{-# LANGUAGE UndecidableInstances, TypeFamilies #-}
module Data.TrieMap.Representation.Instances.ByteString () where

import Data.TrieMap.Representation.Class
import Data.TrieMap.Representation.Instances.Vectors ()

import Data.Word

import Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Data.Vector.Storable

-- | @'Rep' 'ByteString' = 'Rep' ('Vector' 'Word8')@
instance Repr ByteString where
	type Rep ByteString = Rep (Vector Word8)
	toRep (PS fp off len) = toRep (unsafeFromForeignPtr fp off len)
	type RepList ByteString = DRepList ByteString
	toRepList = dToRepList

-- | @'Rep' 'L.ByteString' = 'Rep' ('Vector' 'Word8')@
instance Repr L.ByteString where
	type Rep L.ByteString = Rep (Vector Word8)
	toRep = toRep . B.concat . L.toChunks
	type RepList L.ByteString = DRepList L.ByteString
	toRepList = dToRepList