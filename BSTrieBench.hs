{-# OPTIONS -fasm #-}
module BSTrieBench (main) where

import Criterion.Main

import Data.Trie
import Data.Trie.Convenience
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

tSortBench strings = keys (fromList strings)

tLookupBench (strings, s1, s2) = (s1 `member` strings, s2 `member` strings)

tUnionBench (strings, revs) = unionL strings revs `seq` ()

tFilterBench strings = filterMap (\ str -> if not (BS.null str) && BS.last str /= 's' then Just str else Nothing) strings `seq` ()

tFromList strings = fromList strings `seq` ()
tToList strs = sum [BS.length str | (str, _) <- toList strs]

tInsert strs = insert s s strs `seq` ()
  where !s = BS.pack "scientifitude"

nf' f a = f a `deepseq` nf f a

tBenches strings revs = bgroup ""
  [bench "Lookup" (nf' tLookupBench (strSet, someStr1, someStr2)),
    bench "Sort" (nf' tSortBench (map dup strings)),
    bench "Union" (nf' tUnionBench (strSet, revSet)),
    bench "Filter" (nf' tFilterBench strSet),
    bench "ToList" (nf' tToList strSet),
    bench "Insert" (nf' tInsert strSet),
    bench "FromList" (nf' tFromList (map dup strings))]
  where !strSet = fromList (map dup strings); !revSet = fromList (map dup revs)
	dup :: a -> (a, a)
	dup x = (x, x)
	someStr1 = strings !! (314159 `rem` n); someStr2 = revs !! (314159 `rem` n)
	n = length strings

main :: IO ()
main = do
  strings <- liftM BS.lines (BS.readFile "dictionary.txt")
  let !strings' = V.toList (shuffle (V.fromList strings))
  let !revs' = Prelude.map BS.reverse strings'
  let benches = tBenches strings' revs'
  strings' `deepseq` revs' `deepseq` P.defaultMain benches
