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

trieLongestPalindrome strings revs =
	 T.foldl (\ bs1 bs2 -> if BS.length bs1 >= BS.length bs2 then bs1 else bs2) BS.empty
	    $ T.intersection (T.fromList strings) (T.fromList revs)

{-# INLINE setLongestPalindrome #-}
setLongestPalindrome strings revs =
	F.foldl (\ bs1 bs2 -> if BS.length bs1 >= BS.length bs2 then bs1 else bs2) BS.empty $ 
		S.intersection (S.fromList strings) (S.fromList revs)

shuffle :: V.Vector a -> V.Vector a
shuffle = V.modify (\ mv -> evalRandT (shuffleM mv) (mkStdGen 0))

shuffleM :: PrimMonad m => VM.MVector (PrimState m) a -> RandT StdGen m ()
shuffleM xs = forM_ [0..VM.length xs - 1] $ \ i -> do
  j <- getRandomR (0, VM.length xs - 1)
  lift $ VM.swap xs i j

main :: IO ()
main = do
  strings <- liftM BS.lines (BS.readFile "dictionary.txt")
  let !strings' = V.toList (shuffle (V.fromList strings))
  let !revs' = Prelude.map BS.reverse strings'
  let trieBench = bench "Data.TrieSet" (whnf (trieLongestPalindrome strings') revs')
  let setBench = bench "Data.Set" (whnf (setLongestPalindrome strings') revs')
  let benches = bgroup "" [trieBench,setBench]
  strings' `deepseq` revs' `deepseq` P.defaultMain benches
