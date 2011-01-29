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

mkTrie  :: [String] -> T.TSet String
mkTrie = T.fromList

intersect :: T.TSet String -> T.TSet String -> T.TSet String
intersect = T.intersection

tFold :: T.TSet String -> String
tFold = T.foldl (\ bs1 bs2 -> if length bs1 >= length bs2 then bs1 else bs2) ""

{-# NOINLINE trieLongestPalindrome #-}
trieLongestPalindrome :: [String] -> String
trieLongestPalindrome strings = 
	 tFold $ intersect (mkTrie strings) (mkTrie (Prelude.map reverse strings))

setLongestPalindrome :: [String] -> String
setLongestPalindrome strings = 
	F.foldl (\ bs1 bs2 -> if length bs1 >= length bs2 then bs1 else bs2) "" $ 
		S.intersection (S.fromList strings) (S.fromList (Prelude.map reverse strings))

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
  let trieBench = bench "Data.TrieSet" (whnf trieLongestPalindrome strings')
  let setBench = bench "Data.Set" (whnf setLongestPalindrome strings')
  defaultMain [setBench, trieBench]
