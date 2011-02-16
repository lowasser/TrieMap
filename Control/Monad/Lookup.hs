module Control.Monad.Lookup where

import Control.Monad

newtype Lookup r a = Lookup {runLookup :: r -> (a -> r) -> r}

instance Functor (Lookup r) where
  fmap f m = Lookup $ \ no yes -> runLookup m no (yes . f)

instance Monad (Lookup r) where
  return a = Lookup $ \ _ yes -> yes a
  m >>= k = Lookup $ \ no yes ->
    runLookup m no (\ a -> runLookup (k a) no yes)
  fail _ = mzero

instance MonadPlus (Lookup r) where
  mzero = Lookup $ \ no _ -> no
  m `mplus` k = Lookup $ \ no yes -> runLookup m (runLookup k no yes) yes