module Benchmark (main) where

import Criterion.Main

import qualified Data.TrieSet as T
import qualified Data.Set as S
import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Control.Monad.Primitive
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Random
import qualified Data.ByteString.Char8 as BS
import qualified Progression.Main as P
import Control.DeepSeq

instance NFData BS.ByteString where
  rnf xs = xs `seq` ()

shuffle :: V.Vector a -> V.Vector a
shuffle = V.modify (\ mv -> evalRandT (shuffleM mv) (mkStdGen 0))

shuffleM :: PrimMonad m => VM.MVector (PrimState m) a -> RandT StdGen m ()
shuffleM xs = forM_ [0..VM.length xs - 1] $ \ i -> do
  j <- getRandomR (0, VM.length xs - 1)
  lift $ VM.swap xs i j

tSortBench strings = T.toList (T.fromList strings)

sSortBench strings = S.toList (S.fromList strings)

sortBenches strings =  bgroup "Sort" 
  [bench "Data.TrieSet" (nf tSortBench strings),
   bench "Data.Set" (nf sSortBench strings)]

tIntersectBench (strings, revs) = T.size (T.intersection (T.fromList strings) (T.fromList revs))
sIntersectBench (strings, revs) = S.size (S.intersection (S.fromList strings) (S.fromList revs))

intersectBenches strev = bgroup "Intersect"
  [bench "Data.TrieSet" (whnf tIntersectBench strev),
    bench "Data.Set" (whnf sIntersectBench strev)]

main :: IO ()
main = do
  strings <- liftM BS.lines (BS.readFile "dictionary.txt")
  let !strings' = V.toList (shuffle (V.fromList strings))
  let !revs' = Prelude.map BS.reverse strings'
  let benches = bgroup "" [sortBenches strings', intersectBenches (strings', revs')]
  strings' `deepseq` revs' `deepseq` P.defaultMain benches
