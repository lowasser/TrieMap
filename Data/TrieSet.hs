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
	-- * Conversion
	-- ** Map
	mapSet,
	-- ** List
	elems,
	toList,
	fromList,
	-- ** Ordered lists
	toAscList,
	fromAscList,
	fromDistinctAscList)
 		where

import qualified Data.TrieMap as M
import qualified Data.Foldable as F
import Data.TrieMap.Class

import Control.Applicative hiding (empty)
import Control.Arrow

import Data.Maybe
import Data.Monoid

import Prelude hiding (foldr, foldl, map, filter, null)

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
empty = TSet M.empty

-- | Insert an element into the 'TSet'.
insert :: TKey a => a -> TSet a -> TSet a
insert a (TSet s) = TSet (M.insert a a s)

-- | Delete an element from the 'TSet'.
delete :: TKey a => a -> TSet a -> TSet a
delete a (TSet s) = TSet (M.delete a s)

-- | /O(1)/. Create a singleton set.
singleton :: TKey a => a -> TSet a
singleton a = TSet (M.singleton a a)

-- | The union of two 'TSet's, preferring the first set when
-- equal elements are encountered.
union :: TKey a => TSet a -> TSet a -> TSet a
TSet s1 `union` TSet s2 = TSet (s1 `M.union` s2)

-- | The symmetric difference of two 'TSet's.
symmetricDifference :: TKey a => TSet a -> TSet a -> TSet a
TSet s1 `symmetricDifference` TSet s2 = TSet (M.unionMaybeWith (\ _ _ -> Nothing) s1 s2)

-- | Difference of two 'TSet's.
difference :: TKey a => TSet a -> TSet a -> TSet a
TSet s1 `difference` TSet s2 = TSet (s1 `M.difference` s2)

-- | Intersection of two 'TSet's.  Elements of the result come from the first set.
intersection :: TKey a => TSet a -> TSet a -> TSet a
TSet s1 `intersection` TSet s2 = TSet (s1 `M.intersection` s2)

-- | Filter all elements that satisfy the predicate.
filter :: TKey a => (a -> Bool) -> TSet a -> TSet a
filter p (TSet s) = TSet (M.filter p s)

-- | Partition the set into two sets, one with all elements that satisfy
-- the predicate and one with all elements that don't satisfy the predicate.
-- See also 'split'.
partition :: TKey a => (a -> Bool) -> TSet a -> (TSet a, TSet a)
partition p (TSet s) = (TSet *** TSet) (M.partition p s)

-- | The expression (@'split' x set@) is a pair @(set1,set2)@
-- where @set1@ comprises the elements of @set@ less than @x@ and @set2@
-- comprises the elements of @set@ greater than @x@.
split :: TKey a => a -> TSet a -> (TSet a, TSet a)
split a s = case splitMember a s of
	(sL, _, sR) -> (sL, sR)

-- | Performs a 'split' but also returns whether the pivot
-- element was found in the original set.
splitMember :: TKey a => a -> TSet a -> (TSet a, Bool, TSet a)
splitMember a (TSet s) = case M.splitLookup a s of
	(sL, x, sR) -> (TSet sL, isJust x, TSet sR)

-- |
-- @'map' f s@ is the set obtained by applying @f@ to each element of @s@.
-- 
-- It's worth noting that the size of the result may be smaller if,
-- for some @(x,y)@, @x \/= y && f x == f y@
map :: (TKey a, TKey b) => (a -> b) -> TSet a -> TSet b
map f s = fromList [f x | x <- elems s]

-- | 
-- @'mapMonotonic' f s == 'map' f s@, but works only when @f@ is monotonic.
-- /The precondition is not checked./
-- Semi-formally, we have:
-- 
-- > and [x < y ==> f x < f y | x <- ls, y <- ls] 
-- >                     ==> mapMonotonic f s == map f s
-- >     where ls = toList s
mapMonotonic :: (TKey a, TKey b) => (a -> b) -> TSet a -> TSet b
mapMonotonic f s = fromAscList [f x | x <- toAscList s]

-- | Post-order fold.
foldr :: TKey a => (a -> b -> b) -> b -> TSet a -> b
foldr f z (TSet s) = F.foldr f z s

-- | Pre-order fold.
foldl :: TKey b => (a -> b -> a) -> a -> TSet b -> a
foldl f z (TSet s) = F.foldl f z s

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
minView (TSet s) = (fst *** TSet) <$> M.minViewWithKey s

-- | Retrieves the maximal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
maxView :: TKey a => TSet a -> Maybe (a, TSet a)
maxView (TSet s) = (fst *** TSet) <$> M.maxViewWithKey s

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
toAscList (TSet s) = M.keys s

-- | Create a set from a list of elements.
fromList :: TKey a => [a] -> TSet a
fromList xs = TSet (M.fromList [(x, x) | x <- xs])

-- | Build a set from an ascending list in linear time.
-- /The precondition (input list is ascending) is not checked./
fromAscList :: TKey a => [a] -> TSet a
fromAscList xs = TSet (M.fromAscList [(x, x) | x <- xs])

-- | /O(n)/. Build a set from an ascending list of distinct elements in linear time.
-- /The precondition (input list is strictly ascending) is not checked./
fromDistinctAscList :: TKey a => [a] -> TSet a
fromDistinctAscList xs = TSet (M.fromDistinctAscList [(x, x) | x <- xs])

-- | /O(1)/. Is this the empty set?
null :: TKey a => TSet a -> Bool
null (TSet s) = M.null s

-- | /O(1)/. The number of elements in the set.
size :: TKey a => TSet a -> Int
size (TSet s) = M.size s

-- | Is the element in the set?
member :: TKey a => a -> TSet a -> Bool
member a (TSet s) = a `M.member` s

-- | Is the element not in the set?
notMember :: TKey a => a -> TSet a -> Bool
notMember a = not . member a

-- | Is this a subset? @(s1 `isSubsetOf` s2)@ tells whether @s1@ is a subset of @s2@.
isSubsetOf :: TKey a => TSet a -> TSet a -> Bool
TSet s1 `isSubsetOf` TSet s2 = M.isSubmapOfBy (\ _ _ -> True) s1 s2

-- | Is this a proper subset? (ie. a subset but not equal).
isProperSubsetOf :: TKey a => TSet a -> TSet a -> Bool
s1 `isProperSubsetOf` s2 = size s1 < size s2 && s1 `isSubsetOf` s2

-- | See 'difference'.
(\\) :: TKey a => TSet a -> TSet a -> TSet a
(\\) = difference

{-# INLINE [1] mapSet #-}
-- | Generate a 'TMap' by mapping on the elements of a 'TSet'.
mapSet :: TKey a => (a -> b) -> TSet a -> TMap a b
mapSet f (TSet a) = M.map f a

{-# RULES
	"mapSet/id" forall s . mapSet id s = let TSet m = s in m;
	#-}