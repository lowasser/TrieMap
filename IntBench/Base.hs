{-# LANGUAGE BangPatterns, RecordWildCards #-}
{-# OPTIONS -fasm -funbox-strict-fields #-}
module IntBench.Base where

import Control.Monad.ST
import qualified System.Random.MWC as R
import Data.Vector.Primitive
import qualified Data.Vector.Primitive.Mutable as M
import Data.Vector.Algorithms.Radix

import Data.Bits
import Prelude hiding (elem)

minInt, maxInt :: Int
minInt = - bit 20
maxInt = bit 20

count :: Int
count = bit 15

data TestCase = TestCase {
  memberVal :: !Int,
  nonMemberVal :: !Int,
  vector1 :: !(Vector Int),
  vector2 :: !(Vector Int),
  sortedVector :: !(Vector Int)}

testCase :: TestCase
testCase = runST $ do
  gen <- R.create
  !mv1 <- M.new count
  !mv2 <- M.new count
  !mv3 <- M.new count
  forM_ (enumFromN 0 count) (\ i -> do
    !r1 <- R.uniformR (minInt, maxInt) gen
    !r2 <- R.uniformR (minInt, maxInt) gen
    !r3 <- R.uniformR (minInt, maxInt) gen
    M.write mv1 i r1
    M.write mv2 i r2
    M.write mv3 i r3)
  sort mv3
  vector1 <- unsafeFreeze mv1
  vector2 <- unsafeFreeze mv2
  sortedVector <- unsafeFreeze mv3
  let memberVal = vector1 ! 0
  let findNonMember = do
	i <- R.uniformR (minInt, maxInt) gen
	if i `elem` vector1 then findNonMember else return i
  nonMemberVal <- findNonMember
  return TestCase{..}
  
  