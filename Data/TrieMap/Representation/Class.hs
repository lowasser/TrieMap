{-# LANGUAGE TypeFamilies #-}
module Data.TrieMap.Representation.Class where

import Data.Vector

-- | The @Repr@ type class denotes that a type can be decomposed to a representation
-- built out of pieces for which the 'TrieKey' class defines a generalized trie structure.
-- 
-- It is required that, if @('Repr' a, 'Eq' a)@, and @x, y :: a@, then @x '==' y@
-- if and only if @'toRep' x '==' 'toRep' y@.  It is typically the case that
-- @'compare' x y == 'compare' ('toRep' x) ('toRep' y)@, as well, but this is not
-- strictly required.  (It is, however, the case for all instances built into the package.)
-- 
-- As an additional note, the 'Key' modifier is used for \"bootstrapping\" 'Repr' instances,
-- allowing a type to be used in its own 'Repr' definition when wrapped in a 'Key' modifier.
class Repr a where
  type Rep a
  type RepList a
  toRep :: a -> Rep a
  toRepList :: [a] -> RepList a

-- | A default implementation of @'RepList' a@.
type DRepList a = Vector (Rep a)

-- | A default implementation of 'toRepList'.
dToRepList :: Repr a => [a] -> DRepList a
dToRepList = fromList . Prelude.map toRep

-- | Uses the 'RepList' instance of @a@.  (This allows for efficient and automatic implementations of e.g. @Rep String@.)
instance Repr a => Repr [a] where
  type Rep [a] = RepList a
  type RepList [a] = Vector (RepList a)
  toRep = toRepList
  toRepList = dToRepList