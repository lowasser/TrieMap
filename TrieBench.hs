{-# OPTIONS -fasm #-}
module TrieBench (main) where

import Criterion.Main

import Data.TrieSet
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Control.Monad.Primitive
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Random (getRandomR, RandT, StdGen, evalRandT, mkStdGen)
import qualified Data.ByteString.Char8 as BS
import qualified Progression.Main as P
import Control.DeepSeq
import Data.List (sort)
import Prelude hiding (filter)

instance NFData BS.ByteString where
  rnf xs = xs `seq` ()

shuffle :: V.Vector a -> V.Vector a
shuffle = V.modify (\ mv -> evalRandT (shuffleM mv) (mkStdGen 0))

shuffleM :: PrimMonad m => VM.MVector (PrimState m) a -> RandT StdGen m ()
shuffleM xs = forM_ [0..VM.length xs - 1] $ \ i -> do
  j <- getRandomR (0, VM.length xs - 1)
  lift $ VM.swap xs i j

tSortBench strings = toList (fromList strings)

tIntersectBench (strings, revs) = size (intersection strings revs)

tLookupBench (strings, s1, s2) = (s1 `member` strings, s2 `member` strings)

tUnionBench (strings, revs) = size strings + size revs - size (union strings revs)

tDiffBench (strings, revs) = size strings - size (difference strings revs)

tFilterBench strings = size (filter (\ str -> not (BS.null str) && BS.last str /= 's') strings)

tSplitBench strings = case split (BS.pack "logical") strings of
  (l, r) -> size l - size r

tEnds strings = case deleteFindMin strings of
  (l, strs') -> case deleteFindMax strs' of
    (r, strs'') -> size strs'' + BS.length l - BS.length r

tFromList strings = size (fromList strings)
tToList strs = sum [BS.length str | str <- toList strs]

tFDAL strings = size (fromDistinctAscList strings)

tInsert strs = size (insert (BS.pack "scientifitude") strs)

tNeighborhood (strs, str) = case splitMember str strs of
  (l, x, r) -> (findMax l, x, findMin r)

nf' f a = f a `deepseq` nf f a

tBenches strings revs = bgroup ""
  [bench "Lookup" (nf' tLookupBench (strSet, someStr1, someStr2)),
    revSet `seq` bench "Intersect" (nf' tIntersectBench (strSet, revSet)),
    bench "Sort" (nf' tSortBench strings),
    bench "Union" (nf' tUnionBench (strSet, revSet)),
    bench "Difference" (nf' tDiffBench (strSet, revSet)),
    bench "Filter" (nf' tFilterBench strSet),
    bench "Split" (nf' tSplitBench strSet),
    bench "Neighborhood" (nf' tNeighborhood (strSet, someStr2)),
    bench "Index" (nf' tIndex strSet),
    bench "Min/Max" (nf' tEnds strSet),
    bench "ToList" (nf' tToList strSet),
    bench "Insert" (nf' tInsert strSet),
    bench "FromList" (nf' tFromList strings),
    bench "FromDistinctAscList" (nf' tFDAL strSort)]
  where !strSet = fromList strings; !revSet = fromList revs; !strSort = sort strings
	someStr1 = strings !! (314159 `rem` n); someStr2 = revs !! (314159 `rem` n)
	n = length strings

main :: IO ()
main = do
  strings <- liftM BS.lines (BS.readFile "dictionary.txt")
  let !strings' = V.toList (shuffle (V.fromList strings))
  let !revs' = Prelude.map BS.reverse strings'
  let benches = tBenches strings' revs'
  strings' `deepseq` revs' `deepseq` P.defaultMain benches
