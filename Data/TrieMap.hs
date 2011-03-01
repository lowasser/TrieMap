{-# LANGUAGE UnboxedTuples, ImplicitParams, RecordWildCards, FlexibleContexts, BangPatterns #-}

module Data.TrieMap (
	-- * Map type
	TKey,
	TMap,
	-- * Location type
	TLocation,
	-- ** Components
	key,
	before,
	beforeWith,
	after,
	afterWith,
	-- ** Locations in maps
	search,
	minLocation,
	maxLocation,
	-- ** Building maps
	assign,
	clear,
	-- * Operators
	(!),
	(\\),
	-- * Query
	null,
	size,
	member,
	notMember,
	lookup,
	findWithDefault,
	-- * Construction
	empty,
	singleton,
	-- ** Insertion
	insert,
	insertWith,
	insertWithKey,
	insertLookupWithKey,
	-- ** Delete/Update
	delete,
	adjust,
	adjustWithKey,
	update,
	updateWithKey,
	alter,
	-- * Combine
	-- ** Union
	union,
	unionWith,
	unionWithKey,
	unionMaybeWith,
	unionMaybeWithKey,
	symmetricDifference,
	-- ** Difference
	difference,
	differenceWith,
	differenceWithKey,
	-- ** Intersection
	intersection,
	intersectionWith,
	intersectionWithKey,
	intersectionMaybeWith,
	intersectionMaybeWithKey,
	-- * Traversal
	-- ** Map
	map,
	mapWithKey,
	mapKeys,
	mapKeysWith,
	mapKeysMonotonic,
	-- ** Traverse
	traverseWithKey,
	-- ** Fold
-- 	fold,
	foldrWithKey,
	foldlWithKey,
	-- * Conversion
	keysSet,
	-- ** Lists
	elems,
	keys,
	assocs,
	fromList,
	fromListWith,
	fromListWithKey,
	-- ** Vectors
	elemsVector,
	keysVector,
	assocsVector,
	fromVector,
	fromVectorWith,
	fromVectorWithKey,
	-- ** Ordered lists
	fromAscList,
	fromAscListWith,
	fromAscListWithKey,
	fromDistinctAscList,
	-- ** Ordered vectors
	fromAscVector,
	fromAscVectorWith,
	fromAscVectorWithKey,
	fromDistinctAscVector,
	-- * Filter
	filter,
	filterWithKey,
	partition,
	partitionWithKey,
	mapMaybe,
	mapMaybeWithKey,
	mapEither,
	mapEitherWithKey,
	split,
	splitLookup,
	-- * Submap
	isSubmapOf,
	isSubmapOfBy,
	-- * Indexed
	lookupIndex,
	findIndex,
	-- * Min/Max
	findMin,
	findMax,
	deleteMin,
	deleteMax,
	deleteFindMin,
	deleteFindMax,
	updateMin,
	updateMax,
	updateMinWithKey,
	updateMaxWithKey,
	minView,
	maxView,
	minViewWithKey,
	maxViewWithKey
	) where

import Control.Monad
import Control.Monad.Ends
import Control.Monad.Lookup

import Data.TrieMap.Class
import Data.TrieMap.Class.Instances()
import Data.TrieMap.TrieKey hiding (union, isect, diff, mapMaybe, mapEither)
import qualified Data.TrieMap.TrieKey.Projection as Proj
import qualified Data.TrieMap.TrieKey.SetOp as Set
import Data.TrieMap.Representation
import Data.TrieMap.Representation.Instances ()

import qualified Data.Foldable as F
import Data.Maybe hiding (mapMaybe)

import Data.Vector.Build
import qualified Data.Vector.Generic as G
import Data.Vector.Fusion.Util (unId)
import Data.Vector.Fusion.Stream.Monadic (Stream(..), Step(..))
import qualified Data.Vector.Fusion.Stream.Monadic as S

import GHC.Exts (build)

import Prelude hiding (lookup, foldr, null, map, filter, reverse)

instance (Show k, Show a, TKey k) => Show (TMap k a) where
	show m = "fromList " ++ show (assocs m)

instance (Eq k, TKey k, Eq a) => Eq (TMap k a) where
	m1 == m2 = assocs m1 == assocs m2

instance (Ord k, TKey k, Ord a) => Ord (TMap k a) where
	m1 `compare` m2 = assocs m1 `compare` assocs m2

instance TKey k => Monoid (TMap k a) where
	mempty = empty
	mappend = union

-- | A 'TLocation' represents a 'TMap' with a \"hole\" at a particular key position.
-- 
-- 'TLocation's are used for element-wise operations on maps (insertion, deletion and update) in a two-stage process:
-- 
-- 1. A 'TLocation' (and the value at that position, if any) is obtained from a 'TMap' by searching or indexing.
-- 2. A new 'TMap' is made from a 'TLocation' by either filling the hole with a value ('assign') or erasing it ('clear').
data TLocation k a = TLoc {-# UNPACK #-} !Int k (Hole (Rep k) (Assoc k a))

{-# INLINE empty #-}
-- | /O(1)/. The empty map.
empty :: TKey k => TMap k a
empty = TMap 0 emptyM

-- | /O(1)/. A map with a single element.
{-# INLINE singleton #-}
singleton :: TKey k => k -> a -> TMap k a
singleton k a = TMap 1 (singletonM (toRep k) (Assoc k a))

-- | /O(1)/. Is the map empty?
{-# INLINE null #-}
null :: TKey k => TMap k a -> Bool
null (TMap 0 _) = True
null _ = False

-- | Lookup the value at a key in the map.
-- 
-- The function will return the corresponding value as @('Just' value)@, or 'Nothing' if the key isn't in the map.
{-# INLINE lookup #-}
lookup :: TKey k => k -> TMap k a -> Maybe a
lookup k (TMap _ m) = runLookup (lookupMC (toRep k) m) Nothing (Just . getValue)

-- | The expression @('findWithDefault' def k map)@ returns the value at key @k@ or returns default value @def@
-- when the key is not in the map.
{-# INLINE findWithDefault #-}
findWithDefault :: TKey k => a -> k -> TMap k a -> a
findWithDefault a = fromMaybe a .: lookup

-- | Find the value at a key. Calls 'error' when the element can not be found.
{-# INLINE (!) #-}
(!) :: TKey k => TMap k a -> k -> a
(!) m k = fromMaybe (error "Element not found") (lookup k m)

-- | The expression @('alter' f k map)@ alters the value @x@ at @k@, or absence thereof. 
-- 'alter' can be used to insert, delete, or update a value in a 'TMap'. In short:
-- @'lookup' k ('alter' f k m) = f ('lookup' k m)@.
{-# INLINE alter #-}
alter :: TKey k => (Maybe a -> Maybe a) -> k -> TMap k a -> TMap k a
alter f k (TMap sz m) = searchMC (toRep k) m nomatch match where
  nomatch hole = case f Nothing of
      Nothing	-> TMap sz m
      Just a'	-> TMap (sz + 1) (assignM (Assoc k a') hole)
  match (Assoc _ a) hole = case f (Just a) of
      Nothing	-> TMap (sz - 1) (clearM hole)
      Just a'	-> TMap sz (assignM (Assoc k a') hole)

-- | Insert a new key and value in the map.
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value. 'insert' is equivalent to
-- @'insertWith' 'const'@.
--
-- > insert 5 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'x')]
-- > insert 7 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'a'), (7, 'x')]
-- > insert 5 'x' empty                         == singleton 5 'x'
{-# INLINE insert #-}
insert :: TKey k => k -> a -> TMap k a -> TMap k a
insert = insertWith const

-- | Insert with a function, combining new value and old value.
-- @'insertWith' f key value mp@ 
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the pair @(key, f new_value old_value)@.
--
-- > insertWith (++) 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "xxxa")]
-- > insertWith (++) 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWith (++) 5 "xxx" empty                         == singleton 5 "xxx"
{-# INLINE insertWith #-}
insertWith :: TKey k => (a -> a -> a) -> k -> a -> TMap k a -> TMap k a
insertWith = insertWithKey . const

-- | Insert with a function, combining key, new value and old value.
-- @'insertWithKey' f key value mp@ 
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the pair @(key,f key new_value old_value)@.
-- Note that the key passed to f is the same key passed to 'insertWithKey'.
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:xxx|a")]
-- > insertWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWithKey f 5 "xxx" empty                         == singleton 5 "xxx"
{-# INLINE insertWithKey #-}
insertWithKey :: TKey k => (k -> a -> a -> a) -> k -> a -> TMap k a -> TMap k a
insertWithKey f k a' = alter g k where
  g Nothing = Just a'
  g (Just a) = Just (f k a' a)

-- | Combines insert operation with old value retrieval.
-- The expression (@'insertLookupWithKey' f k x map@)
-- is a pair where the first element is equal to (@'lookup' k map@)
-- and the second element equal to (@'insertWithKey' f k x map@).
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertLookupWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "5:xxx|a")])
-- > insertLookupWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a"), (7, "xxx")])
-- > insertLookupWithKey f 5 "xxx" empty                         == (Nothing,  singleton 5 "xxx")
{-# INLINE insertLookupWithKey #-}
insertLookupWithKey :: TKey k => (k -> a -> a -> a) -> k -> a -> TMap k a -> (Maybe a, TMap k a)
insertLookupWithKey f k a m = case search k m of
	(a', hole)	-> (a', assign (maybe a (f k a) a') hole)

-- | Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
--
-- > delete 5 (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > delete 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > delete 5 empty                         == empty
{-# INLINE delete #-}
delete :: TKey k => k -> TMap k a -> TMap k a
delete k m = case search k m of
	(Nothing, _)	-> m
	(Just{}, hole)	-> clear hole

-- | Update a value at a specific key with the result of the provided function.
-- When the key is not a member of the map, the original map is returned.
--
-- > adjust ("new " ++) 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > adjust ("new " ++) 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjust ("new " ++) 7 empty                         == empty
{-# INLINE adjust #-}
adjust :: TKey k => (a -> a) -> k -> TMap k a -> TMap k a
adjust = adjustWithKey . const

-- | Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
--
-- > let f key x = (show key) ++ ":new " ++ x
-- > adjustWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > adjustWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjustWithKey f 7 empty                         == empty
{-# INLINE adjustWithKey #-}
adjustWithKey :: TKey k => (k -> a -> a) -> k -> TMap k a -> TMap k a
adjustWithKey f k m = case search k m of
	(Nothing, _)	-> m
	(Just a, hole)	-> assign (f k a) hole

-- | The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > update f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > update f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > update f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
{-# INLINE update #-}
update :: TKey k => (a -> Maybe a) -> k -> TMap k a -> TMap k a
update f = updateWithKey (const f)

-- | The expression (@'updateWithKey' f k map@) updates the
-- value @x@ at @k@ (if it is in the map). If (@f k x@) is 'Nothing',
-- the element is deleted. If it is (@'Just' y@), the key @k@ is bound
-- to the new value @y@.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > updateWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > updateWithKey f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
{-# INLINE updateWithKey #-}
updateWithKey :: TKey k => (k -> a -> Maybe a) -> k -> TMap k a -> TMap k a
updateWithKey f k m = case search k m of
	(Nothing, _)	-> m
	(Just a, hole)	-> fillHole (f k a) hole

-- | Post-order fold.  The function will be applied from the lowest
-- value to the highest.
{-# INLINE foldrWithKey #-}
foldrWithKey :: TKey k => (k -> a -> b -> b) -> b -> TMap k a -> b
foldrWithKey f z (TMap _ m) = F.foldr (\ (Assoc k a) -> f k a) z m

-- | Pre-order fold.  The function will be applied from the highest
-- value to the lowest.
{-# INLINE foldlWithKey #-}
foldlWithKey :: TKey k => (b -> k -> a -> b) -> b -> TMap k a -> b
foldlWithKey f z (TMap _ m) = F.foldl (\ z (Assoc k a) -> f z k a) z m

-- | Map each key\/element pair to an action, evaluate these actions from left to right, and collect the results.
{-# INLINE traverseWithKey #-}
traverseWithKey :: (TKey k, Applicative f) => (k -> a -> f b) -> TMap k a -> f (TMap k b)
traverseWithKey f (TMap sz m) = TMap sz <$> traverse (\ (Assoc k a) -> Assoc k <$> f k a) m

-- | Map a function over all values in the map.
--
-- > map (++ "x") (fromList [(5,"a"), (3,"b")]) == fromList [(3, "bx"), (5, "ax")]
{-# INLINE map #-}
map :: TKey k => (a -> b) -> TMap k a -> TMap k b
map = fmap

-- | Map a function over all values in the map.
--
-- > let f key x = (show key) ++ ":" ++ x
-- > mapWithKey f (fromList [(5,"a"), (3,"b")]) == fromList [(3, "3:b"), (5, "5:a")]
{-# INLINEABLE mapWithKey #-}
mapWithKey :: TKey k => (k -> a -> b) -> TMap k a -> TMap k b
mapWithKey f (TMap sz m) = TMap sz (fmap (\ (Assoc k a) -> Assoc k (f k a)) m)

-- |
-- @'mapKeys' f s@ is the map obtained by applying @f@ to each key of @s@.
-- 
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the value at the smallest of
-- these keys is retained.
--
-- > mapKeys (+ 1) (fromList [(5,"a"), (3,"b")])                        == fromList [(4, "b"), (6, "a")]
-- > mapKeys (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "c"
-- > mapKeys (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "c"
{-# INLINE mapKeys #-}
mapKeys :: (TKey k, TKey k') => (k -> k') -> TMap k a -> TMap k' a
mapKeys = mapKeysWith const

-- |
-- @'mapKeysWith' c f s@ is the map obtained by applying @f@ to each key of @s@.
-- 
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the associated values will be
-- combined using @c@.
--
-- > mapKeysWith (++) (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "cdab"
-- > mapKeysWith (++) (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "cdab"
{-# INLINE mapKeysWith #-}
mapKeysWith :: (TKey k, TKey k') => (a -> a -> a) -> (k -> k') -> TMap k a -> TMap k' a
mapKeysWith g f m = fromListWith g [(f k, a) | (k, a) <- assocs m]

-- |
-- @'mapKeysMonotonic' f s == 'mapKeys' f s@, but works only when @f@
-- is strictly monotonic.
-- That is, for any values @x@ and @y@, if @x@ < @y@ then @f x@ < @f y@.
-- /The precondition is not checked./
-- Semi-formally, we have:
-- 
-- > and [x < y ==> f x < f y | x <- ls, y <- ls] 
-- >                     ==> mapKeysMonotonic f s == mapKeys f s
-- >     where ls = keys s
--
-- This means that @f@ maps distinct original keys to distinct resulting keys.
-- This function has better performance than 'mapKeys'.
--
-- > mapKeysMonotonic (\ k -> k * 2) (fromList [(5,"a"), (3,"b")]) == fromList [(6, "b"), (10, "a")]
{-# INLINE mapKeysMonotonic #-}
mapKeysMonotonic :: (TKey k, TKey k') => (k -> k') -> TMap k a -> TMap k' a
mapKeysMonotonic f m = fromDistinctAscList [(f k, a) | (k, a) <- assocs m]

-- |
-- The expression (@'union' t1 t2@) takes the left-biased union of @t1@ and @t2@. 
-- It prefers @t1@ when duplicate keys are encountered,
-- i.e. (@'union' == 'unionWith' 'const'@).
-- The implementation uses the efficient /hedge-union/ algorithm.
-- Hedge-union is more efficient on (bigset \``union`\` smallset).
--
-- > union (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "a"), (7, "C")]
{-# INLINE union #-}
union :: TKey k => TMap k a -> TMap k a -> TMap k a
union = unionWith const

-- | /O(n+m)/. Union with a combining function. The implementation uses the efficient /hedge-union/ algorithm.
--
-- > unionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "aA"), (7, "C")]
{-# INLINE unionWith #-}
unionWith :: TKey k => (a -> a -> a) -> TMap k a -> TMap k a -> TMap k a
unionWith = unionWithKey . const

-- Union with a combining function. The implementation uses the efficient /hedge-union/ algorithm.
-- Hedge-union is more efficient on (bigset \``union`\` smallset).
--
-- > let f key left_value right_value = (show key) ++ ":" ++ left_value ++ "|" ++ right_value
-- > unionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "5:a|A"), (7, "C")]
{-# INLINE unionWithKey #-}
unionWithKey :: TKey k => (k -> a -> a -> a) -> TMap k a -> TMap k a -> TMap k a
unionWithKey f = unionMaybeWithKey (\ k a b -> Just (f k a b))

-- | Union with a combining function. The implementation uses the efficient /hedge-union/ algorithm.
{-# INLINE unionMaybeWith #-}
unionMaybeWith :: TKey k => (a -> a -> Maybe a) -> TMap k a -> TMap k a -> TMap k a
unionMaybeWith = unionMaybeWithKey . const

-- | Union with a combining function. The implementation uses the efficient /hedge-union/ algorithm.
-- Hedge-union is more efficient on (bigset \``union`\` smallset).
--
-- > let f key left_value right_value = Just ((show key) ++ ":" ++ left_value ++ "|" ++ right_value)
-- > unionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "5:a|A"), (7, "C")]
{-# INLINEABLE unionMaybeWithKey #-}
unionMaybeWithKey :: TKey k => (k -> a -> a -> Maybe a) -> TMap k a -> TMap k a -> TMap k a
unionMaybeWithKey _ m1 m2
  | null m1	= m2
  | null m2	= m1
unionMaybeWithKey f (TMap _ m1) (TMap _ m2) = tMap (Set.union f' m1 m2) where
	f' (Assoc k a) (Assoc _ b) = Assoc k <$> f k a b

-- | 'symmetricDifference' is equivalent to @'unionMaybeWith' (\ _ _ -> Nothing)@.
{-# INLINE symmetricDifference #-}
symmetricDifference :: TKey k => TMap k a -> TMap k a -> TMap k a
symmetricDifference = unionMaybeWith (\ _ _ -> Nothing)

-- | Intersection of two maps.
-- Return data in the first map for the keys existing in both maps.
-- (@'intersection' m1 m2 == 'intersectionWith' 'const' m1 m2@).
--
-- > intersection (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "a"
{-# INLINE intersection #-}
intersection :: TKey k => TMap k a -> TMap k b -> TMap k a
intersection = intersectionWith const

-- | Intersection with a combining function.
--
-- > intersectionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "aA"
{-# INLINE intersectionWith #-}
intersectionWith :: TKey k => (a -> b -> c) -> TMap k a -> TMap k b -> TMap k c
intersectionWith = intersectionWithKey . const

-- | Intersection with a combining function.
-- Intersection is more efficient on (bigset \``intersection`\` smallset).
--
-- > let f k al ar = (show k) ++ ":" ++ al ++ "|" ++ ar
-- > intersectionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "5:a|A"
{-# INLINE intersectionWithKey #-}
intersectionWithKey :: TKey k => (k -> a -> b -> c) -> TMap k a -> TMap k b -> TMap k c
intersectionWithKey f = intersectionMaybeWithKey (\ k a b -> Just (f k a b))

-- | @'intersectionMaybeWith' f m1 m2@ is equivalent to
-- @'mapMaybe' 'id' ('intersectionWith' f m1 m2)@.
{-# INLINE intersectionMaybeWith #-}
intersectionMaybeWith :: TKey k => (a -> b -> Maybe c) -> TMap k a -> TMap k b -> TMap k c
intersectionMaybeWith = intersectionMaybeWithKey . const

-- | @'intersectionMaybeWithKey' f m1 m2@ is equivalent to
-- @'mapMaybe' 'id' ('intersectionWithKey' f m1 m2)@.
{-# INLINEABLE intersectionMaybeWithKey #-}
intersectionMaybeWithKey :: TKey k => (k -> a -> b -> Maybe c) -> TMap k a -> TMap k b -> TMap k c
intersectionMaybeWithKey f (TMap _ m1) (TMap _ m2) = tMap (Set.isect f' m1 m2) where
	f' (Assoc k a) (Assoc _ b) = Assoc k <$> f k a b

-- | Difference of two maps. 
-- Return elements of the first map not existing in the second map.
-- The implementation uses an efficient /hedge/ algorithm comparable with /hedge-union/.
--
-- > difference (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 3 "b"
{-# INLINE difference #-}
difference :: TKey k => TMap k a -> TMap k b -> TMap k a
difference = differenceWith (\ _ _ -> Nothing)

-- | Same as 'difference'.
(\\) :: TKey k => TMap k a -> TMap k b -> TMap k a
(\\) = difference

-- | Difference with a combining function. 
-- When two equal keys are
-- encountered, the combining function is applied to the values of these keys.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@. 
-- The implementation uses an efficient /hedge/ algorithm comparable with /hedge-union/.
--
-- > let f al ar = if al == "b" then Just (al ++ ":" ++ ar) else Nothing
-- > differenceWith f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (7, "C")])
-- >     == singleton 3 "b:B"
{-# INLINE differenceWith #-}
differenceWith :: TKey k => (a -> b -> Maybe a) -> TMap k a -> TMap k b -> TMap k a
differenceWith = differenceWithKey . const

-- | Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the key and both values.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@. 
-- The implementation uses an efficient /hedge/ algorithm comparable with /hedge-union/.
--
-- > let f k al ar = if al == "b" then Just ((show k) ++ ":" ++ al ++ "|" ++ ar) else Nothing
-- > differenceWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (10, "C")])
-- >     == singleton 3 "3:b|B"
{-# INLINEABLE differenceWithKey #-}
differenceWithKey :: TKey k => (k -> a -> b -> Maybe a) -> TMap k a -> TMap k b -> TMap k a
differenceWithKey _ m1 m2
  | null m1 || null m2	= m1
differenceWithKey f (TMap _ m1) (TMap _ m2) = tMap (Set.diff f' m1 m2) where
	f' (Assoc k a) (Assoc _ b) = Assoc k <$> f k a b

-- | Retrieves the value associated with minimal key of the
-- map, and the map stripped of that element, or 'Nothing' if passed an
-- empty map.
--
-- > minView (fromList [(5,"a"), (3,"b")]) == Just ("b", singleton 5 "a")
-- > minView empty == Nothing
{-# INLINE minView #-}
minView :: TKey k => TMap k a -> Maybe (a, TMap k a)
minView = fmap (fmap after) . minLocation

-- | Retrieves the value associated with maximal key of the
-- map, and the map stripped of that element, or 'Nothing' if passed an
--
-- > maxView (fromList [(5,"a"), (3,"b")]) == Just ("a", singleton 3 "b")
-- > maxView empty == Nothing
{-# INLINE maxView #-}
maxView :: TKey k => TMap k a -> Maybe (a, TMap k a)
maxView = fmap (fmap before) . maxLocation

-- | The minimal key of the map. Calls 'error' if the map is empty.
--
-- > findMin (fromList [(5,"a"), (3,"b")]) == (3,"b")
-- > findMin empty                            Error: empty map has no minimal element
{-# INLINE findMin #-}
findMin :: TKey k => TMap k a -> (k, a)
findMin = maybe (error "empty map has no minimal element") fst . minViewWithKey

-- | The maximal key of the map. Calls 'error' if the map is empty.
--
-- > findMax (fromList [(5,"a"), (3,"b")]) == (5,"a")
-- > findMax empty                            Error: empty map has no maximal element
{-# INLINE findMax #-}
findMax :: TKey k => TMap k a -> (k, a)
findMax = maybe (error "empty map has no maximal element") fst . maxViewWithKey

-- | Delete the minimal key. Returns an empty map if the map is empty.
--
-- > deleteMin (fromList [(5,"a"), (3,"b"), (7,"c")]) == fromList [(5,"a"), (7,"c")]
-- > deleteMin empty == empty
{-# INLINE deleteMin #-}
deleteMin :: TKey k => TMap k a -> TMap k a
deleteMin m = maybe m snd (minViewWithKey m)

-- | Delete the maximal key. Returns an empty map if the map is empty.
--
-- > deleteMax (fromList [(5,"a"), (3,"b"), (7,"c")]) == fromList [(3,"b"), (5,"a")]
-- > deleteMax empty == empty
{-# INLINE deleteMax #-}
deleteMax :: TKey k => TMap k a -> TMap k a
deleteMax m = maybe m snd (maxViewWithKey m)

-- | Update the value at the minimal key.
--
-- > updateMin (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "Xb"), (5, "a")]
-- > updateMin (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
{-# INLINE updateMin #-}
updateMin :: TKey k => (a -> Maybe a) -> TMap k a -> TMap k a
updateMin = updateMinWithKey . const

-- | Update the value at the maximal key.
--
-- > updateMax (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "Xa")]
-- > updateMax (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
{-# INLINE updateMax #-}
updateMax :: TKey k => (a -> Maybe a) -> TMap k a -> TMap k a
updateMax = updateMaxWithKey . const

-- | Update the value at the minimal key.
--
-- > updateMinWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"3:b"), (5,"a")]
-- > updateMinWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
{-# INLINEABLE updateMinWithKey #-}
updateMinWithKey :: TKey k => (k -> a -> Maybe a) -> TMap k a -> TMap k a
updateMinWithKey f map0@(TMap sz m) = case getFirst $ firstHoleM m of
  Nothing	-> map0
  Just (Assoc k a, hole) -> case f k a of
    Nothing	-> TMap (sz - 1) (afterM hole)
    Just a'	-> TMap sz (afterWithM (Assoc k a') hole)

-- | Update the value at the maximal key.
--
-- > updateMaxWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"b"), (5,"5:a")]
-- > updateMaxWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
{-# INLINEABLE updateMaxWithKey #-}
updateMaxWithKey :: TKey k => (k -> a -> Maybe a) -> TMap k a -> TMap k a
updateMaxWithKey f map0@(TMap sz m) = case getLast $ lastHoleM m of
  Nothing	-> map0
  Just (Assoc k a, hole) -> case f k a of
    Nothing	-> TMap (sz - 1) (beforeM hole)
    Just a'	-> TMap sz (beforeWithM (Assoc k a') hole)

-- | Delete and find the minimal element.
--
-- > deleteFindMin (fromList [(5,"a"), (3,"b"), (10,"c")]) == ((3,"b"), fromList[(5,"a"), (10,"c")]) 
-- > deleteFindMin                                            Error: can not return the minimal element of an empty map
{-# INLINEABLE deleteFindMin #-}
deleteFindMin :: TKey k => TMap k a -> ((k, a), TMap k a)
deleteFindMin m = fromMaybe (error "Cannot return the minimal element of an empty map") (minViewWithKey m)

-- | Delete and find the maximal element.
--
-- > deleteFindMax (fromList [(5,"a"), (3,"b"), (10,"c")]) == ((10,"c"), fromList[(3,"b"),(5,"a")]) 
-- > deleteFindMax                                            Error: can not return the maximal element of an empty map
{-# INLINEABLE deleteFindMax #-}
deleteFindMax :: TKey k => TMap k a -> ((k, a), TMap k a)
deleteFindMax m = fromMaybe (error "Cannot return the maximal element of an empty map") (maxViewWithKey m)

-- | Retrieves the minimal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
--
-- > minViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((3,"b"), singleton 5 "a")
-- > minViewWithKey empty == Nothing
minViewWithKey :: TKey k => TMap k a -> Maybe ((k, a), TMap k a)
{-# INLINE minViewWithKey #-}
minViewWithKey m = do
	(a, loc) <- minLocation m
	return ((key loc, a), after loc)

-- | /O(log n)/. Retrieves the maximal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
--
-- > maxViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((5,"a"), singleton 3 "b")
-- > maxViewWithKey empty == Nothing
{-# INLINE maxViewWithKey #-}
maxViewWithKey :: TKey k => TMap k a -> Maybe ((k, a), TMap k a)
maxViewWithKey m = do
	(a, loc) <- maxLocation m
	return ((key loc, a), before loc)

-- |
-- Return all elements of the map in the ascending order of their keys.
--
-- > elems (fromList [(5,"a"), (3,"b")]) == ["b","a"]
-- > elems empty == []
{-# INLINE elems #-}
elems :: TKey k => TMap k a -> [a]
elems m = build (\ c n -> foldrWithKey (\ _ a -> c a) n m)

{-# INLINE elemsVector #-}
-- |
-- Return all elements of the map in the ascending order of their keys.
-- Does not currently fuse.
elemsVector :: (TKey k, G.Vector v a) => TMap k a -> v a
elemsVector (TMap sz m) = toVectorMapN sz (\ (Assoc _ a) -> a) m

-- | Return all keys of the map in ascending order.
--
-- > keys (fromList [(5,"a"), (3,"b")]) == [3,5]
-- > keys empty == []
{-# INLINE keys #-}
keys :: TKey k => TMap k a -> [k]
keys m = build (\ c n -> foldrWithKey (\ k _ -> c k) n m)

-- | Return all keys of the map in ascending order.
-- Does not currently fuse.
keysVector :: (TKey k, G.Vector v k) => TMap k a -> v k
keysVector (TMap sz m) = toVectorMapN sz (\ (Assoc k _) -> k) m

-- | Return all key\/value pairs in the map in ascending key order.
--
-- > assocs (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
-- > assocs empty == []
{-# INLINE assocs #-}
assocs :: TKey k => TMap k a -> [(k, a)]
assocs m = build (\ c n -> foldrWithKey (curry c) n m)

{-# INLINE assocsVector #-}
-- | Return all key\/value pairs in the map in ascending key order.
-- Does not currently fuse.
assocsVector :: (TKey k, G.Vector v (k, a)) => TMap k a -> v (k, a)
assocsVector (TMap sz m) = toVectorMapN sz (\ (Assoc k a) -> (k, a)) m

-- | Map values and separate the 'Left' and 'Right' results.
--
-- > let f a = if a < "c" then Left a else Right a
-- > mapEither f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(3,"b"), (5,"a")], fromList [(1,"x"), (7,"z")])
-- >
-- > mapEither (\ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
{-# INLINE mapEither #-}
mapEither :: TKey k => (a -> Either b c) -> TMap k a -> (TMap k b, TMap k c)
mapEither = mapEitherWithKey . const

-- | Map keys\/values and separate the 'Left' and 'Right' results.
--
-- > let f k a = if k < 5 then Left (k * 2) else Right (a ++ a)
-- > mapEitherWithKey f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(1,2), (3,6)], fromList [(5,"aa"), (7,"zz")])
-- >
-- > mapEitherWithKey (\_ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(1,"x"), (3,"b"), (5,"a"), (7,"z")])
{-# INLINEABLE mapEitherWithKey #-}
mapEitherWithKey :: TKey k => (k -> a -> Either b c) -> TMap k a -> (TMap k b, TMap k c)
mapEitherWithKey f (TMap _ m) = case Proj.mapEither f' m of
	(# mL, mR #) -> (tMap mL, tMap mR) 
	where	f' (Assoc k a) = case f k a of
			Left b	-> (# Just (Assoc k b), Nothing #)
			Right c	-> (# Nothing, Just (Assoc k c) #)

-- | /O(n)/. Map values and collect the 'Just' results.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > mapMaybe f (fromList [(5,"a"), (3,"b")]) == singleton 5 "new a"
{-# INLINE mapMaybe #-}
mapMaybe :: TKey k => (a -> Maybe b) -> TMap k a -> TMap k b
mapMaybe = mapMaybeWithKey . const

-- | Map keys\/values and collect the 'Just' results.
--
-- > let f k _ = if k < 5 then Just ("key : " ++ (show k)) else Nothing
-- > mapMaybeWithKey f (fromList [(5,"a"), (3,"b")]) == singleton 3 "key : 3"
{-# INLINEABLE mapMaybeWithKey #-}
mapMaybeWithKey :: TKey k => (k -> a -> Maybe b) -> TMap k a -> TMap k b
mapMaybeWithKey f (TMap _ m) = tMap (Proj.mapMaybe (\ (Assoc k a) -> Assoc k <$> f k a) m)

-- | Partition the map according to a predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partition (> "a") (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > partition (< "x") (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partition (> "x") (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])
{-# INLINE partition #-}
partition :: TKey k => (a -> Bool) -> TMap k a -> (TMap k a, TMap k a)
partition = partitionWithKey . const

-- | Partition the map according to a predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partition (> "a") (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > partition (< "x") (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partition (> "x") (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])
{-# INLINE partitionWithKey #-}
partitionWithKey :: TKey k => (k -> a -> Bool) -> TMap k a -> (TMap k a, TMap k a)
partitionWithKey p = mapEitherWithKey (\ k a -> (if p k a then Left else Right) a)

-- | Filter all values that satisfy the predicate.
--
-- > filter (> "a") (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > filter (> "x") (fromList [(5,"a"), (3,"b")]) == empty
-- > filter (< "a") (fromList [(5,"a"), (3,"b")]) == empty
{-# INLINE filter #-}
filter :: TKey k => (a -> Bool) -> TMap k a -> TMap k a
filter p = mapMaybeWithKey (\ _ a -> mfilter p (Just a))

-- | Filter all keys\/values that satisfy the predicate.
--
-- > filterWithKey (\k _ -> k > 4) (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
{-# INLINE filterWithKey #-}
filterWithKey :: TKey k => (k -> a -> Bool) -> TMap k a -> TMap k a
filterWithKey p = mapMaybeWithKey (\ k a -> mfilter (p k) (Just a))

-- | The expression (@'split' k map@) is a pair @(map1,map2)@ where
-- the keys in @map1@ are smaller than @k@ and the keys in @map2@ larger than @k@.
-- Any key equal to @k@ is found in neither @map1@ nor @map2@.
--
-- > split 2 (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3,"b"), (5,"a")])
-- > split 3 (fromList [(5,"a"), (3,"b")]) == (empty, singleton 5 "a")
-- > split 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > split 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", empty)
-- > split 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], empty)
{-# INLINE split #-}
split :: TKey k => k -> TMap k a -> (TMap k a, TMap k a)
split k m = case splitLookup k m of
	(mL, _, mR) -> (mL, mR)

-- | The expression (@'splitLookup' k map@) splits a map just
-- like 'split' but also returns @'lookup' k map@.
--
-- > splitLookup 2 (fromList [(5,"a"), (3,"b")]) == (empty, Nothing, fromList [(3,"b"), (5,"a")])
-- > splitLookup 3 (fromList [(5,"a"), (3,"b")]) == (empty, Just "b", singleton 5 "a")
-- > splitLookup 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Nothing, singleton 5 "a")
-- > splitLookup 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Just "a", empty)
-- > splitLookup 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], Nothing, empty)
{-# INLINE splitLookup #-}
splitLookup :: TKey k => k -> TMap k a -> (TMap k a, Maybe a, TMap k a)
splitLookup k m = case search k m of
	(x, hole) -> (before hole, x, after hole)

-- | 
-- This function is defined as (@'isSubmapOf' = 'isSubmapOfBy' (==)@).
{-# INLINE isSubmapOf #-}
isSubmapOf :: (TKey k, Eq a) => TMap k a -> TMap k a -> Bool
isSubmapOf = isSubmapOfBy (==)

{- |
 The expression (@'isSubmapOfBy' f t1 t2@) returns 'True' if
 all keys in @t1@ are in tree @t2@, and when @f@ returns 'True' when
 applied to their respective values. For example, the following 
 expressions are all 'True':
 
 > isSubmapOfBy (==) (fromList [('a',1)]) (fromList [('a',1),('b',2)])
 > isSubmapOfBy (<=) (fromList [('a',1)]) (fromList [('a',1),('b',2)])
 > isSubmapOfBy (==) (fromList [('a',1),('b',2)]) (fromList [('a',1),('b',2)])

 But the following are all 'False':
 
 > isSubmapOfBy (==) (fromList [('a',2)]) (fromList [('a',1),('b',2)])
 > isSubmapOfBy (<)  (fromList [('a',1)]) (fromList [('a',1),('b',2)])
 > isSubmapOfBy (==) (fromList [('a',1),('b',2)]) (fromList [('a',1)])
 
-}
{-# INLINEABLE isSubmapOfBy #-}
isSubmapOfBy :: TKey k => (a -> b -> Bool) -> TMap k a -> TMap k b -> Bool
isSubmapOfBy (<?) (TMap n1 m1) (TMap n2 m2) = let ?le = \ (Assoc _ a) (Assoc _ b) -> a <? b in n1 <= n2 && m1 <=? m2

{-# INLINE fromFoldStream #-}
fromFoldStream :: (Repr k, TrieKey (Rep k), Monad m) => 
  FromList z (Rep k) (Assoc k a) -> Stream m (k, a) -> m (TMap k a)
fromFoldStream Foldl{..} (Stream suc s0 _) = run s0 where
  run s = do
    step <- suc s
    case step of
      Done	-> return empty
      Skip s'	-> run s'
      Yield (k, a) s' -> run' 1 (begin (toRep k) (Assoc k a)) s'
  run' !n stack s = do
    step <- suc s
    case step of
      Done	-> return (TMap n (done stack))
      Skip s'	-> run' n stack s'
      Yield (k, a) s' -> run' (n+1) (snoc stack (toRep k) (Assoc k a)) s'

-- | Build a map from a list of key\/value pairs. See also 'fromAscList'.
-- If the list contains more than one value for the same key, the last value
-- for the key is retained.
--
-- > fromList [] == empty
-- > fromList [(5,"a"), (3,"b"), (5, "c")] == fromList [(5,"c"), (3,"b")]
-- > fromList [(5,"c"), (3,"b"), (5, "a")] == fromList [(5,"a"), (3,"b")]
{-# INLINE fromList #-}
fromList :: TKey k => [(k, a)] -> TMap k a
fromList = fromListWith const

{-# INLINE fromVector #-}
-- | Equivalent to @'fromList' ('G.toList' xs)@.
fromVector :: (TKey k, G.Vector v (k, a)) => v (k, a) -> TMap k a
fromVector = fromVectorWith const

-- | Build a map from an ascending list in linear time.
-- /The precondition (input list is ascending) is not checked./
--
-- > fromAscList [(3,"b"), (5,"a")]          == fromList [(3, "b"), (5, "a")]
-- > fromAscList [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "b")]
{-# INLINE fromAscList #-}
fromAscList :: TKey k => [(k, a)] -> TMap k a
fromAscList = fromAscListWith const

{-# INLINE fromAscVector #-}
-- | Equivalent to @'fromAscList' ('G.toList' xs)@.
fromAscVector :: (TKey k, G.Vector v (k, a)) => v (k, a) -> TMap k a
fromAscVector = fromAscVectorWith const

-- | Build a map from a list of key\/value pairs with a combining function. See also 'fromAscListWith'.
--
-- > fromListWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] == fromList [(3, "ab"), (5, "aba")]
-- > fromListWith (++) [] == empty
{-# INLINE fromListWith #-}
fromListWith :: TKey k => (a -> a -> a) -> [(k, a)] -> TMap k a
fromListWith = fromListWithKey . const

{-# INLINE fromVectorWith #-}
-- | Equivalent to @'fromListWith' f ('G.toList' xs)@.
fromVectorWith :: (TKey k, G.Vector v (k, a)) => (a -> a -> a) -> v (k, a) -> TMap k a
fromVectorWith = fromVectorWithKey . const

-- | Build a map from an ascending list in linear time with a combining function for equal keys.
-- /The precondition (input list is ascending) is not checked./
--
-- > fromAscListWith (++) [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "ba")]
{-# INLINE fromAscListWith #-}
fromAscListWith :: TKey k => (a -> a -> a) -> [(k, a)] -> TMap k a
fromAscListWith = fromAscListWithKey . const

{-# INLINE fromAscVectorWith #-}
-- | Equivalent to @'fromAscListWith' f ('G.toList' xs)@.
fromAscVectorWith :: (TKey k, G.Vector v (k, a)) => (a -> a -> a) -> v (k, a) -> TMap k a
fromAscVectorWith = fromAscVectorWithKey . const

-- | Build a map from a list of key\/value pairs with a combining function. See also 'fromAscListWith'.
--
-- > fromListWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] == fromList [(3, "ab"), (5, "aba")]
-- > fromListWith (++) [] == empty
{-# INLINEABLE fromListWithKey #-}
fromListWithKey :: TKey k => (k -> a -> a -> a) -> [(k, a)] -> TMap k a
fromListWithKey f xs = unId $ fromFoldStream (uFold f') (S.fromList xs)
	where f' (Assoc k a) (Assoc _ b) = Assoc k (f k a b)

{-# INLINE fromVectorWithKey #-}
-- | Equivalent to @'fromListWithKey' f ('G.toList' xs)@.
fromVectorWithKey :: (TKey k, G.Vector v (k, a)) => (k -> a -> a -> a) -> v (k, a) -> TMap k a
fromVectorWithKey f xs = unId $ fromFoldStream (uFold f') (G.stream xs)
	where f' (Assoc k a) (Assoc _ b) = Assoc k (f k a b)

-- | Build a map from an ascending list in linear time.
-- /The precondition (input list is ascending) is not checked./
--
-- > fromAscList [(3,"b"), (5,"a")]          == fromList [(3, "b"), (5, "a")]
-- > fromAscList [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "b")]
{-# INLINEABLE fromAscListWithKey #-}
fromAscListWithKey :: TKey k => (k -> a -> a -> a) -> [(k, a)] -> TMap k a
fromAscListWithKey f xs = unId $ fromFoldStream (aFold f') (S.fromList xs)
	where f' (Assoc k a) (Assoc _ b) = Assoc k (f k a b)

{-# INLINE fromAscVectorWithKey #-}
-- | Equivalent to @'fromAscListWithKey' f ('G.toList' xs)@.
fromAscVectorWithKey :: (TKey k, G.Vector v (k, a)) => (k -> a -> a -> a) -> v (k, a) -> TMap k a
fromAscVectorWithKey f xs = unId $ fromFoldStream (aFold f') (G.stream xs)
	where f' (Assoc k a) (Assoc _ b) = Assoc k (f k a b)

-- | Build a map from an ascending list of distinct elements in linear time.
-- /The precondition is not checked./
--
-- > fromDistinctAscList [(3,"b"), (5,"a")] == fromList [(3, "b"), (5, "a")]
{-# INLINEABLE fromDistinctAscList #-}
fromDistinctAscList :: TKey k => [(k, a)] -> TMap k a
fromDistinctAscList xs = unId $ fromFoldStream daFold (S.fromList xs)

{-# INLINE fromDistinctAscVector #-}
-- | Equivalent to @'fromDistinctAscList' ('G.toList' xs)@.
fromDistinctAscVector :: (TKey k, G.Vector v (k, a)) => v (k, a) -> TMap k a
fromDistinctAscVector xs = unId $ fromFoldStream daFold (G.stream xs)

-- | /O(1)/. The number of elements in the map.
--
-- > size empty                                   == 0
-- > size (singleton 1 'a')                       == 1
-- > size (fromList([(1,'a'), (2,'c'), (3,'b')])) == 3
{-# INLINE size #-}
size :: TKey k => TMap k a -> Int
size (TMap sz _) = sz

-- | Is the key a member of the map? See also 'notMember'.
--
-- > member 5 (fromList [(5,'a'), (3,'b')]) == True
-- > member 1 (fromList [(5,'a'), (3,'b')]) == False
{-# INLINE member #-}
member :: TKey k => k -> TMap k a -> Bool
member = isJust .: lookup

-- | Is the key not a member of the map? See also 'member'.
--
-- > notMember 5 (fromList [(5,'a'), (3,'b')]) == False
-- > notMember 1 (fromList [(5,'a'), (3,'b')]) == True
{-# INLINE notMember #-}
notMember :: TKey k => k -> TMap k a -> Bool
notMember = not .: member

-- | The set of all keys of the map.
--
-- > keysSet (fromList [(5,"a"), (3,"b")]) == Data.TrieSet.fromList [3,5]
-- > keysSet empty == Data.TrieSet.empty
{-# INLINE keysSet #-}
keysSet :: TKey k => TMap k a -> TSet k
keysSet (TMap sz m) = TSet sz (fmap (\ (Assoc k _) -> Elem k) m)

-- | /O(1)/.  The key marking the position of the \"hole\" in the map.
{-# INLINE key #-}
key :: TKey k => TLocation k a -> k
key (TLoc _ k _) = k

-- | @'before' loc@ is the submap with keys less than @'key' loc@.
{-# INLINE before #-}
before :: TKey k => TLocation k a -> TMap k a
before (TLoc _ _ hole) = tMap (beforeM hole)

-- | @'after' loc@ is the submap with keys greater than @'key' loc@.
{-# INLINE after #-}
after :: TKey k => TLocation k a -> TMap k a
after (TLoc _ _ hole) = tMap (afterM hole)

beforeWith :: TKey k => a -> TLocation k a -> TMap k a
beforeWith a (TLoc k hole) = TMap (beforeWithM (Assoc k a) hole)

-- | @'afterWith' a loc@ is equivalent to @'insert' ('key' loc) a ('after' loc)@.
afterWith :: TKey k => a -> TLocation k a -> TMap k a
afterWith a (TLoc k hole) = TMap (afterWithM (Assoc k a) hole)

-- | Search the map for the given key, returning the
-- corresponding value (if any) and an updatable location for that key.
--
-- Properties:
--
-- @
-- case 'search' k m of
--     (Nothing, loc) -> 'key' loc == k && 'clear' loc == m
--     (Just v,  loc) -> 'key' loc == k && 'assign' v loc == m
-- @
--
-- @'lookup' k m == 'fst' ('search' k m)@
{-# INLINE search #-}
search :: TKey k => k -> TMap k a -> (Maybe a, TLocation k a)
search k (TMap sz m) = searchMC (toRep k) m nomatch match where
  nomatch hole = (Nothing, TLoc sz k hole)
  match (Assoc k a) hole = (Just a, TLoc (sz - 1) k hole)

{-# INLINE extract #-}
extract :: (TKey k, Functor m, MonadPlus m) => TMap k a -> m (a, TLocation k a)
extract (TMap sz m) = fmap (\ (Assoc k a, hole) -> (a, TLoc (sz - 1) k hole)) (extractHoleM m)

-- | /O(log n)/. Return the value and an updatable location for the
-- least key in the map, or 'Nothing' if the map is empty.
--
-- Properties:
--
-- @
-- 'size' m > 0 ==>
--     let Just (v, loc) = 'minLocation' i m in
--         'size' (`before` loc) == 0 && 'assign' v loc == m
-- @
--
-- @'findMin' m == let Just (v, loc) = 'minLocation' i m in ('key' loc, v)@
{-# INLINEABLE minLocation #-}
minLocation :: TKey k => TMap k a -> Maybe (a, TLocation k a)
minLocation = getFirst . extract

-- | Return the value and an updatable location for the
-- greatest key in the map, or 'Nothing' if the map is empty.
--
-- Properties:
--
-- @
-- 'size' m > 0 ==>
--     let Just (v, loc) = 'maxLocation' i m in
--         'size' (`after` loc) == 0 && 'assign' v loc == m
-- @
--
-- @'findMax' m == let Just (v, loc) = 'maxLocation' i m in ('key' loc, v)@
{-# INLINEABLE maxLocation #-}
maxLocation :: TKey k => TMap k a -> Maybe (a, TLocation k a)
maxLocation = getLast . extract

-- | Return a map obtained by placing the given value
-- at the location (replacing an existing value, if any).
--
-- @'assign' v loc == 'before' loc `union` 'singleton' ('key' loc) v `union` 'after' loc@
{-# INLINE assign #-}
assign :: TKey k => a -> TLocation k a -> TMap k a
assign a (TLoc sz k hole) = TMap (sz + 1) (assignM (Assoc k a) hole)

-- | Return a map obtained by erasing the location.
--
-- @'clear' loc == 'before' loc `union` 'after' loc@
{-# INLINE clear #-}
clear :: TKey k => TLocation k a -> TMap k a
clear (TLoc sz _ hole) = TMap sz (clearM hole)

{-# INLINE fillHole #-}
fillHole :: TKey k => Maybe a -> TLocation k a -> TMap k a
fillHole = maybe clear assign

-- | Return the /index/ of a key. The index is a number from
-- /0/ up to, but not including, the 'size' of the map. Calls 'error' when
-- the key is not a 'member' of the map.
--
-- > findIndex 2 (fromList [(5,"a"), (3,"b")])    Error: element is not in the map
-- > findIndex 3 (fromList [(5,"a"), (3,"b")]) == 0
-- > findIndex 5 (fromList [(5,"a"), (3,"b")]) == 1
-- > findIndex 6 (fromList [(5,"a"), (3,"b")])    Error: element is not in the map
{-# INLINEABLE findIndex #-}
findIndex :: TKey k => k -> TMap k a -> Int
findIndex k m = fromMaybe (error "TrieMap.findIndex: key is not in the map") (lookupIndex k m)

-- | Lookup the /index/ of a key. The index is a number from
-- /0/ up to, but not including, the 'size' of the map.
--
-- > lookupIndex 2 (fromList [(5,"a"), (3,"b")]) == Nothing
-- > lookupIndex 3 (fromList [(5,"a"), (3,"b")]) == Just 0
-- > lookupIndex 5 (fromList [(5,"a"), (3,"b")]) == Just 1
-- > lookupIndex 6 (fromList [(5,"a"), (3,"b")]) == Nothing
{-# INLINEABLE lookupIndex #-}
lookupIndex :: TKey k => k -> TMap k a -> Maybe Int
lookupIndex k m = case search k m of
	(Nothing, _)	-> Nothing
	(_, hole)	-> Just $ size (before hole)