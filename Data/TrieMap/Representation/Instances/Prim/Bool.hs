{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | A wrapper around 'Bool' that provides basic 'Integral' and 'Bits' instances.
module Data.TrieMap.Representation.Instances.Prim.Bool (BBool(..)) where

import Data.Bits

newtype BBool = BBool Bool deriving (Show, Eq, Ord, Enum)

instance Bits BBool where
  BBool p .&. BBool q = BBool (p && q)
  BBool p .|. BBool q = BBool (p || q)
  BBool p `xor` BBool q = BBool (p /= q)
  complement (BBool p) = BBool (not p)
  shift p 0 = p
  shift _ _ = BBool False
  rotate p _ = p
  isSigned _ = False
  bitSize _ = 1

instance Num BBool where
  (+) = (.|.)
  (*) = (.&.)
  negate = id
  abs = id
  signum = id
  fromInteger i = BBool (i /= 0)

{-# INLINE toNum #-}
toNum :: Num a => BBool -> a
toNum (BBool p) = if p then 1 else 0

instance Real BBool where
  toRational = toNum

instance Integral BBool where
  quotRem x _ = (BBool False, x)
  toInteger = toNum

{-# RULES
    "fromIntegral/BBool" fromIntegral = toNum
    #-}