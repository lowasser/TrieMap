{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module IntBench.Trie (main) where

import Criterion.Main
import Control.DeepSeq
import Data.TrieSet
import IntBench.Base
import qualified Data.Vector.Primitive as P
import qualified Progression.Main as Prog
import Prelude hiding (filter)

tSortBench TestCase{vector1} = toList (fromVector vector1)

tIntersectBench (set1, set2) = size (intersection set1 set2)

tLookupBench (set, m, nm) = (m `member` set, nm `member` set)

tUnionBench (set1, set2) = size set1 + size set2 - size (union set1 set2)

tDiffBench (set1, set2) = size set1 - size (difference set1 set2)

tFilterBench set1 = size (filter even set1)

tSplitBench (set1, nm) = case split nm set1 of
  (l, r) -> size l - size r

tEnds set = case deleteFindMin set of
  (l, set') -> case deleteFindMax set' of
    (r, set'') -> size set'' + l - r

tFromList TestCase{vector1} = size (fromVector vector1)
tToList set = sum (toList set)

tFAL TestCase{sortedVector} = size (fromAscVector sortedVector)

tInsert (set, nm) = size (insert nm set)
tIndex set = elemAt (31415926 `rem` size set) set

tNeighborhood (set, i) = case splitMember i set of
  (l, x, r) -> (findMax l, x, findMin r)
nf' f a = f a `deepseq` nf f a

tBenches = bgroup ""
  [bench "Lookup" (nf' tLookupBench (set1, m, nm)),
    set2 `seq` bench "Intersect" (nf' tIntersectBench (set1, set2)),
    bench "Sort" (nf' tSortBench tc),
    bench "Union" (nf' tUnionBench (set1, set2)),
    bench "Difference" (nf' tDiffBench (set1, set2)),
    bench "Filter" (nf' tFilterBench set1),
    bench "Split" (nf' tSplitBench (set1, nm)),
    bench "Neighborhood" (nf' tNeighborhood (set1, nm)),
    bench "Index" (nf' tIndex set1),
    bench "Min/Max" (nf' tEnds set1),
    bench "ToList" (nf' tToList set1),
    bench "Insert" (nf' tInsert (set1, nm)),
    bench "FromList" (nf' tFromList tc),
    bench "FromAscList" (nf' tFAL tc)]
  where tc@TestCase{..} = testCase; !set1 = fromList (P.toList vector1); !set2 = fromList (P.toList vector2)
	!nm = nonMemberVal; !m = memberVal
main :: IO ()
main = Prog.defaultMain tBenches
