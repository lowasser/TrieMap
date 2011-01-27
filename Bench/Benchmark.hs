-- module TrieBench where

import Criterion.Main

import qualified Data.TrieSet as T
import qualified Data.Set as S
import qualified Data.Foldable as F
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Control.Monad.Primitive
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Random

-- mkTrie :: [ByteString] -> ByteString
mkTrie = T.fromList

intersect :: T.TSet ByteString -> T.TSet ByteString -> T.TSet ByteString
intersect = T.intersection

tFold = T.foldl (\ bs1 bs2 -> if BS.length bs1 >= BS.length bs2 then bs1 else bs2) BS.empty

{-# NOINLINE trieLongestPalindrome #-}
trieLongestPalindrome :: [ByteString] -> ByteString
trieLongestPalindrome strings = 
	 tFold $ intersect (mkTrie strings) (mkTrie (Prelude.map BS.reverse strings))

setLongestPalindrome :: [ByteString] -> ByteString
setLongestPalindrome strings = 
	F.foldl (\ bs1 bs2 -> if BS.length bs1 >= BS.length bs2 then bs1 else bs2) BS.empty $ 
		S.intersection (S.fromList strings) (S.fromList (Prelude.map BS.reverse strings))

shuffle :: V.Vector a -> V.Vector a
shuffle = V.modify (\ mv -> evalRandT (shuffleM mv) (mkStdGen 0))

shuffleM :: PrimMonad m => VM.MVector (PrimState m) a -> RandT StdGen m ()
shuffleM xs = forM_ [0..VM.length xs - 1] $ \ i -> do
  j <- getRandomR (0, VM.length xs - 1)
  lift $ VM.swap xs i j

main = do
  strings <- liftM BS.lines (BS.readFile "dictionary.txt")
  let !strings' = V.toList (shuffle (V.fromList strings))
  let trieBench = bench "Data.TrieSet" (whnf trieLongestPalindrome strings')
  let setBench = bench "Data.Set" (whnf setLongestPalindrome strings')
  defaultMain [trieBench]