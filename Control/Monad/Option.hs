{-# LANGUAGE Rank2Types #-}
module Control.Monad.Option (Option, option, runOption, none, maybeToOption, liftOption, isSome) where

import Control.Monad
import Control.Applicative

newtype Option a = Option {execOption :: forall r . r -> (a -> r) -> r}

option :: (forall r . r -> (a -> r) -> r) -> Option a
option = Option

runOption :: Option a -> r -> (a -> r) -> r
runOption = execOption

none :: Option a
none = Option const

maybeToOption :: Maybe a -> Option a
maybeToOption m = Option $ \ no yes -> maybe no yes m

{-# SPECIALIZE liftOption :: Option a -> Maybe a #-}
liftOption :: MonadPlus m => Option a -> m a
liftOption m = runOption m mzero return

isSome :: Option a -> Bool
isSome m = runOption m False (const True)

instance Functor Option where
  fmap f m = Option $ \ no yes -> execOption m no (yes . f)

instance Applicative Option where
  pure = return
  (<*>) = ap

instance Monad Option where
  return a = Option $ \ _ yes -> yes a
  m >>= k = Option $ \ no yes -> execOption m no (\ a -> execOption (k a) no yes)

instance Alternative Option where
  empty = mzero
  (<|>) = mplus

instance MonadPlus Option where
  mzero = none
  m `mplus` k = Option $ \ no yes -> execOption m (execOption k no yes) yes