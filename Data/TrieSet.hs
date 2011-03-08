{-# LANGUAGE UnboxedTuples, ImplicitParams, RecordWildCards, FlexibleContexts #-}
module Data.TrieSet (
	-- * Set type
	TSet,
	-- * Operators
	(\\),
	-- * Query
	null,
	size,
	member,
	notMember,
	isSubsetOf,
	isProperSubsetOf,
	-- * Construction
	empty,
	singleton,
	insert,
	delete,
	-- * Combine
	union,
	symmetricDifference,
	intersection,
	difference,
	-- * Filter
	filter,
	partition,
	split,
	splitMember,
	-- * Map
	map,
	mapMonotonic,
	-- * Fold
	foldl,
	foldr,
	-- * Min/Max
	findMin,
	findMax,
	deleteMin,
	deleteMax,
	deleteFindMin,
	deleteFindMax,
	minView,
	maxView,
	-- * Index
	elemAt,
	deleteAt,
	lookupIndex,
	-- * Conversion
	-- ** Map
	mapSet,
	-- ** List
	elems,
	toList,
	fromList,
	-- ** Vector
	toVector,
	fromVector,
	-- ** Ordered lists
	toAscList,
	fromAscList,
	fromDistinctAscList,
	-- ** Ordered vectors
	fromAscVector,
	fromDistinctAscVector)
	where

import Control.Monad
import Control.Monad.Ends
import Control.Monad.Option

import Data.TrieMap.Class
import Data.TrieMap.Class.Instances ()
import Data.TrieMap.TrieKey hiding (foldr, foldl, toList, union, diff, isect, empty, singleton, index)
import qualified Data.TrieMap.TrieKey as TK
import qualified Data.TrieMap.TrieKey.SetOp as Set
import Data.TrieMap.Representation.Class

import Data.Vector.Build
import qualified Data.Vector.Generic as G
import Data.Vector.Fusion.Util (unId)
import Data.Vector.Fusion.Stream.Monadic (Stream(..), Step(..))
import qualified Data.Vector.Fusion.Stream.Monadic as S

import Data.Maybe(fromJust)

import qualified Data.Foldable as F
import Data.Monoid (Monoid (..))

import GHC.Exts
import Prelude hiding (foldr, foldl, map, filter, null, lookup)

instance TKey a => Eq (TSet a) where
	s1 == s2 = size s1 == size s2 && s1 `isSubsetOf` s2

instance (TKey a, Ord a) => Ord (TSet a) where
	s1 `compare` s2 = elems s1 `compare` elems s2

instance (TKey a, Show a) => Show (TSet a) where
	show s = "fromList " ++ show (elems s)

instance TKey a => Monoid (TSet a) where
	mempty = empty
	mappend = union

-- | The empty 'TSet'.
empty :: TKey a => TSet a
empty = TSet TK.empty

-- | Insert an element into the 'TSet'.
insert :: TKey a => a -> TSet a -> TSet a
insert a (TSet s) = TSet (insertWith (const (Elem a)) (toRep a) (Elem a) s)

-- | Delete an element from the 'TSet'.
delete :: TKey a => a -> TSet a -> TSet a
delete a (TSet s) = TSet (alter (\ _ -> Nothing) (toRep a) s)

-- | /O(1)/. Create a singleton set.
singleton :: TKey a => a -> TSet a
singleton a = TSet (TK.singleton (toRep a) (Elem a))

-- | The union of two 'TSet's, preferring the first set when
-- equal elements are encountered.
union :: TKey a => TSet a -> TSet a -> TSet a
TSet s1 `union` TSet s2 = TSet (Set.union (const . Just) s1 s2)

-- | The symmetric difference of two 'TSet's.
symmetricDifference :: TKey a => TSet a -> TSet a -> TSet a
TSet s1 `symmetricDifference` TSet s2 = TSet (Set.union (\ _ _ -> Nothing) s1 s2)

-- | Difference of two 'TSet's.
difference :: TKey a => TSet a -> TSet a -> TSet a
TSet s1 `difference` TSet s2 = TSet (Set.diff (\ _ _ -> Nothing) s1 s2)

-- | Intersection of two 'TSet's.  Elements of the result come from the first set.
intersection :: TKey a => TSet a -> TSet a -> TSet a
TSet s1 `intersection` TSet s2 = TSet (Set.isect (const . Just) s1 s2)

-- | Filter all elements that satisfy the predicate.
filter :: TKey a => (a -> Bool) -> TSet a -> TSet a
filter p (TSet s) = TSet (mapMaybe (\ (Elem a) -> if p a then return (Elem a) else mzero) s)

-- | Partition the set into two sets, one with all elements that satisfy
-- the predicate and one with all elements that don't satisfy the predicate.
-- See also 'split'.
partition :: TKey a => (a -> Bool) -> TSet a -> (TSet a, TSet a)
partition p (TSet s) = case mapEither f s of
	  (# s1, s2 #) -> (TSet s1, TSet s2)
  where f e@(Elem a)
	  | p a		= (# Just e, Nothing #)
	  | otherwise	= (# Nothing, Just e #)

-- | The expression (@'split' x set@) is a pair @(set1,set2)@
-- where @set1@ comprises the elements of @set@ less than @x@ and @set2@
-- comprises the elements of @set@ greater than @x@.
split :: TKey a => a -> TSet a -> (TSet a, TSet a)
split a s = case splitMember a s of
	(sL, _, sR) -> (sL, sR)

-- | Performs a 'split' but also returns whether the pivot
-- element was found in the original set.
splitMember :: TKey a => a -> TSet a -> (TSet a, Bool, TSet a)
splitMember a (TSet s) = case splitLookup (toRep a) s of
  (sL, Nothing, sR) -> (TSet sL, False, TSet sR)
  (sL, Just{}, sR) -> (TSet sL, True, TSet sR)

-- |
-- @'map' f s@ is the set obtained by applying @f@ to each element of @s@.
-- 
-- It's worth noting that the size of the result may be smaller if,
-- for some @(x,y)@, @x \/= y && f x == f y@
map :: (TKey a, TKey b) => (a -> b) -> TSet a -> TSet b
map f s = fromList [f x | x <- elems s]

-- | 
-- @'mapMonotonic' f s == 'map' f s@, but works only when @f@ is strictly monotonic.
-- /The precondition is not checked./
-- Semi-formally, we have:
-- 
-- > and [x < y ==> f x < f y | x <- ls, y <- ls] 
-- >                     ==> mapMonotonic f s == map f s
-- >     where ls = toList s
mapMonotonic :: (TKey a, TKey b) => (a -> b) -> TSet a -> TSet b
mapMonotonic f s = fromDistinctAscList [f x | x <- toAscList s]

-- | Post-order fold.
foldr :: TKey a => (a -> b -> b) -> b -> TSet a -> b
foldr f z (TSet s) = F.foldr (flip $ F.foldr f) z s

-- | Pre-order fold.
foldl :: TKey b => (a -> b -> a) -> a -> TSet b -> a
foldl f z (TSet s) = F.foldl (F.foldl f) z s

-- | The minimal element of the set.
findMin :: TKey a => TSet a -> a
findMin = fst . deleteFindMin

-- | The maximal element of the set.
findMax :: TKey a => TSet a -> a
findMax = fst . deleteFindMax

-- | Delete the minimal element.
deleteMin :: TKey a => TSet a -> TSet a
deleteMin s = maybe s snd (minView s)

-- |  Delete the maximal element.
deleteMax :: TKey a => TSet a -> TSet a
deleteMax s = maybe s snd (maxView s)

-- | Delete and find the minimal element.
-- 
-- > 'deleteFindMin' set = ('findMin' set, 'deleteMin' set)
deleteFindMin :: TKey a => TSet a -> (a, TSet a)
deleteFindMin = fromJust . minView

-- | Delete and find the maximal element.
-- 
-- > 'deleteFindMax' set = ('findMax' set, 'deleteMax' set)
deleteFindMax :: TKey a => TSet a -> (a, TSet a)
deleteFindMax = fromJust . maxView

-- | Retrieves the minimal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
minView :: TKey a => TSet a -> Maybe (a, TSet a)
minView (TSet s) = case getFirst (alternate s) of
  Nothing	-> Nothing
  Just (Elem a, hole) -> Just (a, TSet (after hole))

-- | Retrieves the maximal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
maxView :: TKey a => TSet a -> Maybe (a, TSet a)
maxView (TSet s) = case getLast (alternate s) of
  Nothing	-> Nothing
  Just (Elem a, hole) -> Just (a, TSet (before hole))

{-# INLINE elems #-}
-- | See 'toAscList'.
elems :: TKey a => TSet a -> [a]
elems = toAscList
{-# INLINE toList #-}
-- | See 'toAscList'.
toList :: TKey a => TSet a -> [a]
toList = toAscList
{-# INLINE toAscList #-}
-- | Convert the set to an ascending list of elements.
toAscList :: TKey a => TSet a -> [a]
toAscList s = build (\ c n -> foldr c n s)

{-# INLINE fromFoldStream #-}
fromFoldStream :: (Monad m, Repr a, TrieKey (Rep a)) => FromList z (Rep a) (Elem a) -> Stream m a -> m (TSet a)
fromFoldStream Foldl{..} (Stream suc s0 _) = run s0 where
  run s = do
    step <- suc s
    case step of
      Done -> return empty
      Skip s' -> run s'
      Yield x s' -> run' (begin (toRep x) (Elem x)) s'
  run' stack s = do
    step <- suc s
    case step of
      Done -> return (TSet (done stack))
      Skip s' -> run' stack s'
      Yield x s' -> run' (snoc stack (toRep x) (Elem x)) s'

{-# INLINE fromList #-}
-- | Create a set from a list of elements.
fromList :: TKey a => [a] -> TSet a
fromList xs = unId (fromFoldStream (uFold const) (S.fromList xs))

{-# INLINE fromVector #-}
-- | Create a set from a vector of elements.
fromVector :: (TKey a, G.Vector v a) => v a -> TSet a
fromVector xs = unId (fromFoldStream (uFold const) (G.stream xs))

{-# INLINE fromAscList #-}
-- | Build a set from an ascending list in linear time.
-- /The precondition (input list is ascending) is not checked./
fromAscList :: TKey a => [a] -> TSet a
fromAscList xs = unId (fromFoldStream (aFold const) (S.fromList xs))

{-# INLINE fromAscVector #-}
-- | Build a set from an ascending vector in linear time.
-- /The precondition (input vector is ascending) is not checked./
fromAscVector :: (TKey a, G.Vector v a) => v a -> TSet a
fromAscVector xs = unId (fromFoldStream (aFold const) (G.stream xs))

{-# INLINE fromDistinctAscList #-}
-- | /O(n)/. Build a set from an ascending list of distinct elements in linear time.
-- /The precondition (input list is strictly ascending) is not checked./
fromDistinctAscList :: TKey a => [a] -> TSet a
fromDistinctAscList xs = unId (fromFoldStream daFold (S.fromList xs))

{-# INLINE fromDistinctAscVector #-}
-- | /O(n)/. Build a set from an ascending vector of distinct elements in linear time.
-- /The precondition (input vector is strictly ascending) is not checked./
fromDistinctAscVector :: (TKey a, G.Vector v a) => v a -> TSet a
fromDistinctAscVector xs = unId (fromFoldStream daFold (G.stream xs))

{-# INLINE toVector #-}
-- | /O(n)/.  Construct a vector from the elements of this set.  Does not currently fuse.
toVector :: (TKey a, G.Vector v a) => TSet a -> v a
toVector (TSet s) = toVectorMapN (sizeM s) getElem s
-- If we want this to fuse, our best bet is probably a method to iterate a hole to the next key...or something.
-- This seems difficult, but perhaps not impossible.

-- | /O(1)/. Is this the empty set?
null :: TKey a => TSet a -> Bool
null (TSet s) = isNull s

-- | /O(1)/. The number of elements in the set.
size :: TKey a => TSet a -> Int
size (TSet s) = getSize s

-- | Is the element in the set?
member :: TKey a => a -> TSet a -> Bool
member a (TSet s) = isSome (lookup (toRep a) s)

-- | Is the element not in the set?
notMember :: TKey a => a -> TSet a -> Bool
notMember = not .: member

-- | Is this a subset? @(s1 `isSubsetOf` s2)@ tells whether @s1@ is a subset of @s2@.
isSubsetOf :: TKey a => TSet a -> TSet a -> Bool
TSet s1 `isSubsetOf` TSet s2 = let ?le = \ _ _ -> True in s1 <=? s2

-- | Is this a proper subset? (ie. a subset but not equal).
isProperSubsetOf :: TKey a => TSet a -> TSet a -> Bool
s1 `isProperSubsetOf` s2 = size s1 < size s2 && s1 `isSubsetOf` s2

-- | See 'difference'.
(\\) :: TKey a => TSet a -> TSet a -> TSet a
(\\) = difference

{-# INLINE [1] mapSet #-}
-- | Generate a 'TMap' by mapping on the elements of a 'TSet'.
mapSet :: TKey a => (a -> b) -> TSet a -> TMap a b
mapSet f (TSet s) = TMap (fmap (\ (Elem a) -> Assoc a (f a)) s)

-- | Returns the element at the specified index.  Throws an error if an invalid index is specified.
elemAt :: TKey a => Int -> TSet a -> a
elemAt i (TSet s) = case TK.index (unbox i) s of
  (# _, Elem a, _ #) -> a

-- | Deletes the element at the specified index.  Throws an error if an invalid index is specified.
deleteAt :: TKey a => Int -> TSet a -> TSet a
deleteAt i (TSet s) = case TK.index (unbox i) s of
  (# _, _, hole #) -> TSet (clear hole)

-- | If the specified element is in the set, returns 'Just' the index of the element, otherwise returns 'Nothing'.
lookupIndex :: TKey a => a -> TSet a -> Maybe Int
lookupIndex a (TSet s) = search (toRep a) s (\ _ -> Nothing) (\ _ hole -> Just $ sizeM (before hole))