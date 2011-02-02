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
import Control.DeepSeq

trieLongestPalindrome :: [String] -> [String] -> String
trieLongestPalindrome strings revs =
	 T.foldl (\ bs1 bs2 -> if length bs1 >= length bs2 then bs1 else bs2) ""
	    $ T.intersection (T.fromList strings) (T.fromList revs)

setLongestPalindrome :: [String] -> [String] -> String
setLongestPalindrome strings revs =
	F.foldl (\ bs1 bs2 -> if length bs1 >= length bs2 then bs1 else bs2) "" $ 
		S.intersection (S.fromList strings) (S.fromList revs)

shuffle :: V.Vector a -> V.Vector a
shuffle = V.modify (\ mv -> evalRandT (shuffleM mv) (mkStdGen 0))

shuffleM :: PrimMonad m => VM.MVector (PrimState m) a -> RandT StdGen m ()
shuffleM xs = forM_ [0..VM.length xs - 1] $ \ i -> do
  j <- getRandomR (0, VM.length xs - 1)
  lift $ VM.swap xs i j

main :: IO ()
main = do
  strings <- liftM lines (readFile "dictionary.txt")
  let !strings' = V.toList (shuffle (V.fromList strings))
  let !revs' = map reverse strings'
  let trieBench = bench "Data.TrieSet" (whnf (trieLongestPalindrome strings') revs')
  let setBench = bench "Data.Set" (whnf (setLongestPalindrome strings') revs')
  revs' `deepseq` defaultMain [setBench, trieBench]
