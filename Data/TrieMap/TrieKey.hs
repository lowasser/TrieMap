{-# LANGUAGE TypeFamilies, UnboxedTuples, MagicHash, FlexibleContexts, TupleSections, Rank2Types, ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Data.TrieMap.TrieKey where

import Data.TrieMap.Sized
import Data.TrieMap.Utils

import Control.Applicative (Applicative)
import Control.Monad
import Control.Monad.Ends

import Data.Foldable hiding (foldrM, foldlM)

import Prelude hiding (foldr, foldl)

import GHC.Exts

type LEq a b = a -> b -> Bool
type SearchCont h a r = (h -> r) -> (a -> h -> r) -> r
type IndexCont h a r = (Int# -> a -> h -> r) -> r
type LookupCont a r = r -> (a -> r) -> r

data Foldl k a z = forall z0 . 
  Foldl {snoc :: z0 -> k -> a -> z0,
	  begin :: k -> a -> z0,
	  zero :: z,
	  done :: z0 -> z}
type FromList k a = Foldl k a (TrieMap k a)

instance Functor (Foldl k a) where
  fmap f Foldl{..} = Foldl{zero = f zero, done = f . done, ..}

{-# INLINE runFoldl #-}
runFoldl :: Foldl k a z -> [(k, a)] -> z
runFoldl Foldl{zero} [] = zero
runFoldl Foldl{..} ((k,a):xs) = run (begin k a) xs where
  run z [] = done z
  run z ((k, a):xs) = let z' = snoc z k a in z' `seq` run z' xs 

{-# INLINE mapFoldlKey #-}
mapFoldlKey :: (k -> k') -> Foldl k' a z -> Foldl k a z
mapFoldlKey f Foldl{..} = Foldl{snoc = \ z k a -> snoc z (f k) a, begin = begin . f, ..}

data Distinct k a z = Begin k a | Dist k a z

{-# INLINE combineKeys #-}
combineKeys :: Eq k => (a -> a -> a) -> Foldl k a z -> Foldl k a z
combineKeys f Foldl{..} = Foldl{snoc = snoc', begin = Begin, zero, done = done'} where
  snoc' (Begin k a) k' a'
    | k == k'	= Begin k (f a' a)
  snoc' (Dist k a stk) k' a'
    | k == k'	= Dist k (f a' a) stk
  snoc' stk k a = Dist k a (collapse stk)
  
  done' = done . collapse
  
  collapse (Begin k a) = begin k a
  collapse (Dist k a stk) = snoc stk k a

data Simple a = Null | Singleton a | NonSimple

lookupYes :: a -> LookupCont a r
lookupYes a _ yes = yes a

lookupNo :: LookupCont a r
lookupNo no _ = no

instance Monad Simple where
	return = Singleton
	Null >>= _ = Null
	Singleton a >>= k = k a
	NonSimple >>= _ = NonSimple

instance MonadPlus Simple where
	mzero = Null
	Null `mplus` simple	= simple
	simple `mplus` Null	= simple
	_ `mplus` _		= NonSimple

{-# INLINE onSnd #-}
onSnd :: (c -> d) -> (a -> (# b, c #)) -> a -> (# b, d #)
onSnd g f a = case f a of
	(# b, c #) -> (# b, g c #)

{-# INLINE onThird #-}
onThird :: (d -> e) -> (a -> (# Int, c, d #)) -> a -> (# Int, c, e #)
onThird g f a = case f a of
	(# b, c, d #) -> (# b, c, g d #)

-- | A @TrieKey k@ instance implies that @k@ is a standardized representation for which a
-- generalized trie structure can be derived.
class (Ord k, Foldable (TrieMap k)) => TrieKey k where
	data TrieMap k :: * -> *
	emptyM :: TrieMap k a
	singletonM :: Sized a => k -> a -> TrieMap k a
	getSimpleM :: TrieMap k a -> Simple a
	sizeM# :: Sized a => TrieMap k a -> Int#
	sizeM :: Sized a => TrieMap k a -> Int
	lookupMC :: k -> TrieMap k a -> r -> (a -> r) -> r
	fmapM :: Sized b => (a -> b) -> TrieMap k a -> TrieMap k b
	traverseM :: (Applicative f, Sized b) =>
		(a -> f b) -> TrieMap k a -> f (TrieMap k b)
	mapMaybeM :: Sized b => (a -> Maybe b) -> TrieMap k a -> TrieMap k b
	mapEitherM :: (Sized b, Sized c) => (a -> (# Maybe b, Maybe c #)) -> TrieMap k a -> (# TrieMap k b, TrieMap k c #)
	unionM :: Sized a => (a -> a -> Maybe a) -> TrieMap k a -> TrieMap k a -> TrieMap k a
	isectM :: (Sized a, Sized b, Sized c) =>
		(a -> b -> Maybe c) -> TrieMap k a -> TrieMap k b -> TrieMap k c
	diffM :: Sized a => (a -> b -> Maybe a) -> TrieMap k a -> TrieMap k b -> TrieMap k a
	isSubmapM :: (Sized a, Sized b) => LEq a b -> LEq (TrieMap k a) (TrieMap k b)
	
	fromListFold, fromAscListFold :: Sized a => (a -> a -> a) -> FromList k a
	fromDistAscListFold :: Sized a => FromList k a
	insertWithM :: (TrieKey k, Sized a) => (a -> a) -> k -> a -> TrieMap k a -> TrieMap k a
	
	data Hole k :: * -> *
	singleHoleM :: k -> Hole k a
	beforeM, afterM :: Sized a => Hole k a -> TrieMap k a
	beforeWithM, afterWithM :: Sized a => a -> Hole k a -> TrieMap k a
	searchMC :: k -> TrieMap k a -> SearchCont (Hole k a) a r
	indexMC :: Sized a => Int# -> TrieMap k a -> IndexCont (Hole k a) a r

	-- By combining rewrite rules and these NOINLINE pragmas, we automatically derive
	-- specializations of functions for every instance of TrieKey.
	extractHoleM :: (Functor m, MonadPlus m) => Sized a => TrieMap k a -> m (a, Hole k a)
	{-# NOINLINE firstHoleM #-}
	{-# NOINLINE lastHoleM #-}
	{-# NOINLINE sizeM# #-}
	sizeM# m = unbox (inline sizeM m)
	firstHoleM :: Sized a => TrieMap k a -> First (a, Hole k a)
	firstHoleM m = inline extractHoleM m
	lastHoleM :: Sized a => TrieMap k a -> Last (a, Hole k a)
	lastHoleM m = inline extractHoleM m
	
	insertWithM f k a m = inline searchMC k m (assignM a) (assignM . f)
	
	assignM :: Sized a => a -> Hole k a -> TrieMap k a
	clearM :: Sized a => Hole k a -> TrieMap k a
	unifierM :: Sized a => k -> k -> a -> Maybe (Hole k a)
	
	fromListM, fromAscListM :: Sized a => (a -> a -> a) -> [(k, a)] -> TrieMap k a
	fromListM f xs = runFoldl (fromListFold f) xs
	fromAscListM f xs = runFoldl (fromAscListFold f) xs
	fromDistAscListM :: Sized a => [(k, a)] -> TrieMap k a
	fromDistAscListM xs = runFoldl fromDistAscListFold xs
	
	{-# INLINE fromListFold #-}
	fromListFold f = Foldl 
	  {snoc = \ m k a -> insertWithM (f a) k a m,
	   begin = singletonM,
	   zero = emptyM,
	   done = id}
	{-# INLINE fromAscListFold #-}
	fromAscListFold = fromListFold
	{-# INLINE fromDistAscListFold #-}
	fromDistAscListFold = fromAscListFold const
	
	unifierM k' k a = searchMC k' (singletonM k a) Just (\ _ _ -> Nothing)

instance (TrieKey k, Sized a) => Sized (TrieMap k a) where
	getSize# = sizeM#

foldl1Empty :: a
foldl1Empty = error "Error: cannot call foldl1 on an empty map"

foldr1Empty :: a
foldr1Empty = error "Error: cannot call foldr1 on an empty map"

{-# INLINE fillHoleM #-}
fillHoleM :: (TrieKey k, Sized a) => Maybe a -> Hole k a -> TrieMap k a
fillHoleM = maybe clearM assignM

{-# INLINE mapSearch #-}
mapSearch :: (hole -> hole') -> SearchCont hole a r -> SearchCont hole' a r
mapSearch f run nomatch match = run nomatch' match' where
  nomatch' hole = nomatch (f hole)
  match' a hole = match a (f hole)

mapIndex :: (hole -> hole') -> IndexCont hole a r -> IndexCont hole' a r
mapIndex f run res = run res' where
  res' i# a hole = res i# a (f hole)

{-# INLINE lookupM #-}
lookupM :: TrieKey k => k -> TrieMap k a -> Maybe a
lookupM k m = lookupMC k m Nothing Just

{-# INLINE unifyM #-}
unifyM :: (TrieKey k, Sized a) => k -> a -> k -> a -> Maybe (TrieMap k a)
unifyM k1 a1 k2 a2 = case unifierM k1 k2 a2 of
  Nothing	-> Nothing
  Just hole	-> Just $ inline assignM a1 hole

insertWithM' :: (TrieKey k, Sized a) => (a -> a) -> k -> a -> Maybe (TrieMap k a) -> TrieMap k a
insertWithM' f k a = maybe (singletonM k a) (insertWithM f k a)

mapMaybeM' :: (TrieKey k, Sized b) => (a -> Maybe b) -> TrieMap k a -> Maybe (TrieMap k b)
mapMaybeM' = guardNullM .: mapMaybeM

mapEitherM' :: (TrieKey k, Sized b, Sized c) => (a -> (# Maybe b, Maybe c #)) -> TrieMap k a ->
	(# Maybe (TrieMap k b), Maybe (TrieMap k c) #)
mapEitherM' = both guardNullM guardNullM . mapEitherM

mapEitherM'' :: (TrieKey k, Sized b, Sized c) => (a -> (# Maybe b, Maybe c #)) -> Maybe (TrieMap k a) ->
	(# Maybe (TrieMap k b), Maybe (TrieMap k c) #)
mapEitherM'' = mapEitherMaybe . mapEitherM'

unionM' :: (TrieKey k, Sized a) => (a -> a -> Maybe a) -> TrieMap k a -> TrieMap k a -> Maybe (TrieMap k a)
unionM' f m1 m2 = guardNullM (unionM f m1 m2)

isectM' :: (TrieKey k, Sized a, Sized b, Sized c) => (a -> b -> Maybe c) -> TrieMap k a -> TrieMap k b -> Maybe (TrieMap k c)
isectM' f m1 m2 = guardNullM (isectM f m1 m2)

diffM' :: (TrieKey k, Sized a) => (a -> b -> Maybe a) -> TrieMap k a -> TrieMap k b -> Maybe (TrieMap k a)
diffM' f m1 m2 = guardNullM (diffM f m1 m2)

{-# INLINE beforeMM #-}
beforeMM :: (TrieKey k, Sized a) => Maybe a -> Hole k a -> TrieMap k a
beforeMM = maybe beforeM beforeWithM

{-# INLINE afterMM #-}
afterMM :: (TrieKey k, Sized a) => Maybe a -> Hole k a -> TrieMap k a
afterMM = maybe afterM afterWithM

clearM' :: (TrieKey k, Sized a) => Hole k a -> Maybe (TrieMap k a)
clearM' hole = guardNullM (clearM hole)

{-# INLINE alterM #-}
alterM :: (TrieKey k, Sized a) => (Maybe a -> Maybe a) -> k -> TrieMap k a -> TrieMap k a
alterM f k m = searchMC k m g h where
  g hole = case f Nothing of
    Nothing	-> m
    Just a	-> assignM a hole
  h = fillHoleM . f . Just

{-# INLINE searchMC' #-}
searchMC' :: TrieKey k => k -> Maybe (TrieMap k a) -> (Hole k a -> r) -> (a -> Hole k a -> r) -> r
searchMC' k Nothing f _ = f (singleHoleM k)
searchMC' k (Just m) f g = searchMC k m f g

nullM :: TrieKey k => TrieMap k a -> Bool
nullM m = case getSimpleM m of
	Null	-> True
	_	-> False

guardNullM :: TrieKey k => TrieMap k a -> Maybe (TrieMap k a)
guardNullM m
	| nullM m	= Nothing
	| otherwise	= Just m

sides :: (b -> d) -> (a -> (# b, c, b #)) -> a -> (# d, c, d #)
sides g f a = case f a of
	(# x, y, z #) -> (# g x, y, g z #)

both :: (b -> b') -> (c -> c') -> (a -> (# b, c #)) -> a -> (# b', c' #)
both g1 g2 f a = case f a of
	(# x, y #) -> (# g1 x, g2 y #)

elemsM :: TrieKey k => TrieMap k a -> [a]
elemsM m = build (\ f z -> foldr f z m)

mapEitherMaybe :: (a -> (# Maybe b, Maybe c #)) -> Maybe a -> (# Maybe b, Maybe c #)
mapEitherMaybe f (Just a) = f a
mapEitherMaybe _ _ = (# Nothing, Nothing #)

{-# INLINE unionMaybe #-}
unionMaybe :: (a -> a -> Maybe a) -> Maybe a -> Maybe a -> Maybe a
unionMaybe f (Just x) (Just y) = f x y
unionMaybe _ Nothing y = y
unionMaybe _ x Nothing = x

isectMaybe :: (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
isectMaybe f (Just x) (Just y) = f x y
isectMaybe _ _ _ = Nothing

diffMaybe :: (a -> b -> Maybe a) -> Maybe a -> Maybe b -> Maybe a
diffMaybe _ Nothing _ = Nothing
diffMaybe _ (Just x) Nothing = Just x
diffMaybe f (Just x) (Just y) = f x y

subMaybe :: (a -> b -> Bool) -> Maybe a -> Maybe b -> Bool
subMaybe _ Nothing _ = True
subMaybe (<=) (Just a) (Just b) = a <= b
subMaybe _ _ _ = False

indexFail :: IndexCont hole a r
indexFail _ = error "Error: not a valid index"

{-# RULES
  "extractHoleM/First" [0] extractHoleM = firstHoleM;
  "extractHoleM/Last" [0] extractHoleM = lastHoleM;
  "sizeM" [0] forall m . sizeM m = I# (sizeM# m);
  "getSimpleM/emptyM" getSimpleM emptyM = Null;
  "getSimpleM/singletonM" forall k a . getSimpleM (singletonM k a) = Singleton a;
  #-}