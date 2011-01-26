{-# LANGUAGE UndecidableInstances, TypeFamilies #-}
module Data.TrieMap.Representation.Instances.ByteString () where

import Data.TrieMap.Representation.Class
import Data.TrieMap.Representation.Instances.Vectors ()

import Data.Word

import Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Data.Vector.Storable

instance Repr ByteString where
	type Rep ByteString = (Vector Word, Word)
	toRep (PS fp off len) = toRep (unsafeFromForeignPtr fp off len)

instance Repr L.ByteString where
	type Rep L.ByteString = (Vector Word, Word)
	toRep = toRep . B.concat . L.toChunks