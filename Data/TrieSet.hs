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
	fold,
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
import Data.TrieMap.Class

import Control.Applicative hiding (empty)
import Control.Arrow

import Data.Maybe
import Data.Monoid

import Prelude hiding (foldr, foldl, map, filter, null)

instance TKey a => Eq (TSet a) where
	s1 == s2 = s1 `isSubsetOf` s2 && size s1 == size s2

instance (TKey a, Ord a) => Ord (TSet a) where
	s1 `compare` s2 = elems s1 `compare` elems s2

instance (TKey a, Show a) => Show (TSet a) where
	show s = "fromList " ++ show (elems s)

instance TKey a => Monoid (TSet a) where
	mempty = empty
	mappend = union

empty :: TKey a => TSet a
empty = TSet M.empty

insert :: TKey a => a -> TSet a -> TSet a
insert a (TSet s) = TSet (M.insert a () s)

delete :: TKey a => a -> TSet a -> TSet a
delete a (TSet s) = TSet (M.delete a s)

singleton :: TKey a => a -> TSet a
singleton a = insert a empty

union :: TKey a => TSet a -> TSet a -> TSet a
TSet s1 `union` TSet s2 = TSet (s1 `M.union` s2)

symmetricDifference :: TKey a => TSet a -> TSet a -> TSet a
TSet s1 `symmetricDifference` TSet s2 = TSet (M.unionMaybeWith (\ _ _ -> Nothing) s1 s2)

difference :: TKey a => TSet a -> TSet a -> TSet a
TSet s1 `difference` TSet s2 = TSet (s1 `M.difference` s2)

intersection :: TKey a => TSet a -> TSet a -> TSet a
TSet s1 `intersection` TSet s2 = TSet (s1 `M.intersection` s2)

filter :: TKey a => (a -> Bool) -> TSet a -> TSet a
filter p (TSet s) = TSet (M.filterWithKey (\ k _ -> p k) s)

partition :: TKey a => (a -> Bool) -> TSet a -> (TSet a, TSet a)
partition p (TSet s) = (TSet *** TSet) (M.partitionWithKey (\ k _ -> p k) s)

split :: TKey a => a -> TSet a -> (TSet a, TSet a)
split a s = case splitMember a s of
	(sL, _, sR) -> (sL, sR)

splitMember :: TKey a => a -> TSet a -> (TSet a, Bool, TSet a)
splitMember a (TSet s) = case M.splitLookup a s of
	(sL, x, sR) -> (TSet sL, isJust x, TSet sR)

map :: (TKey a, TKey b) => (a -> b) -> TSet a -> TSet b
map f (TSet s) = TSet (M.mapKeys f s)

mapMonotonic :: (TKey a, TKey b) => (a -> b) -> TSet a -> TSet b
mapMonotonic f (TSet s) = TSet (M.mapKeysMonotonic f s)

fold, foldr :: TKey a => (a -> b -> b) -> b -> TSet a -> b
fold = foldr
foldr f z (TSet s) = M.foldrWithKey (const . f) z s

foldl :: TKey b => (a -> b -> a) -> a -> TSet b -> a
foldl f z (TSet s) = M.foldlWithKey (\ z a _ -> f z a) z s

findMin, findMax :: TKey a => TSet a -> a
findMin = fst . deleteFindMin
findMax = fst . deleteFindMax

deleteMin, deleteMax :: TKey a => TSet a -> TSet a
deleteMin s = maybe s snd (minView s)
deleteMax s = maybe s snd (maxView s)

deleteFindMin, deleteFindMax :: TKey a => TSet a -> (a, TSet a)
deleteFindMin = fromJust . minView
deleteFindMax = fromJust . maxView

minView, maxView :: TKey a => TSet a -> Maybe (a, TSet a)
minView (TSet s) = (fst *** TSet) <$> M.minViewWithKey s
maxView (TSet s) = (fst *** TSet) <$> M.maxViewWithKey s

elems, toList, toAscList :: TKey a => TSet a -> [a]
elems (TSet s) = M.keys s
toList = elems
toAscList = toList

fromList, fromAscList, fromDistinctAscList :: TKey a => [a] -> TSet a
fromList xs = TSet (M.fromList [(x, ()) | x <- xs])
fromAscList xs = TSet (M.fromAscList [(x, ()) | x <- xs])
fromDistinctAscList xs = TSet (M.fromDistinctAscList [(x, ()) | x <- xs])

null :: TKey a => TSet a -> Bool
null (TSet s) = M.null s

size :: TKey a => TSet a -> Int
size (TSet s) = M.size s

member :: TKey a => a -> TSet a -> Bool
member a (TSet s) = a `M.member` s

notMember :: TKey a => a -> TSet a -> Bool
notMember a = not . member a

isSubsetOf, isProperSubsetOf :: TKey a => TSet a -> TSet a -> Bool
TSet s1 `isSubsetOf` TSet s2 = M.isSubmapOfBy (\ _ _ -> True) s1 s2
s1 `isProperSubsetOf` s2 = size s1 < size s2 && s1 `isSubsetOf` s2

(\\) :: TKey a => TSet a -> TSet a -> TSet a
(\\) = difference