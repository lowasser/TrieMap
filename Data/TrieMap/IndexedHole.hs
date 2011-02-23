{-# LANGUAGE TypeOperators, TypeFamilies, DeriveFunctor #-}
module Data.TrieMap.IndexedHole where

import Control.Monad.Trans.Reader
import Control.Monad.Unpack

import Data.TrieMap.Utils

data IndexedHole a h = Indexed !Int a h deriving (Functor)
type IndexCont h a r = (IndexedHole a h :~> r) -> r

instance Unpackable (IndexedHole a h) where
  newtype UnpackedReaderT (IndexedHole a h) m r =
    IndexedReaderT {runIndexedReaderT :: UnpackedReaderT Int (ReaderT a (ReaderT h m)) r}
  runUnpackedReaderT func (Indexed i a hole) =
    runIndexedReaderT func `runUnpackedReaderT` i `runReaderT` a `runReaderT` hole
  unpackedReaderT func = IndexedReaderT $ unpackedReaderT $ \ i ->
    ReaderT $ \ a -> ReaderT $ \ h -> func (Indexed i a h)

indexFail :: a
indexFail = error "Error: not a valid index"

mapIndex :: (hole -> hole') -> IndexCont hole a r -> IndexCont hole' a r
mapIndex f run = run . mapInput (fmap f)