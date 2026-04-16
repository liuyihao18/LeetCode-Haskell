{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.List
import Data.Map qualified as M
import Data.Maybe
import Data.Vector qualified as V
import IO
import Util

solveQueries :: [Int] -> [Int] -> [Int]
solveQueries nums = map (findClosest _nums m)
  where
    _nums = V.fromList nums
    m = makeIndex _nums

makeIndex :: V.Vector Int -> M.Map Int [Int]
makeIndex _nums = foldl' (\m i -> emplaceValue (_nums V.! i) i m) M.empty index
  where
    n = length _nums
    index = reverse [0 .. n - 1]

emplaceValue :: Int -> Int -> M.Map Int [Int] -> M.Map Int [Int]
emplaceValue k v m = M.insert k (v : originValue) m
  where
    originValue = fromMaybe [] (M.lookup k m)

findClosest :: V.Vector Int -> M.Map Int [Int] -> Int -> Int
findClosest nums m query
  | nIndex <= 1 = -1
  | otherwise = minDistance1 `min` minDistance2
  where
    nNums = length nums
    index = V.fromList $ fromMaybe [] $ M.lookup (nums V.! query) m
    nIndex = length index
    i = lowerBound index query
    minDistance1 =
      if i == 0
        then query + nNums - index V.! (length index - 1)
        else query - index V.! (i - 1)
    minDistance2 =
      if i + 1 == nIndex
        then index V.! 0 + nNums - query
        else index V.! (i + 1) - query

nums1 :: [Int]
nums1 = [1, 3, 1, 4, 1, 3, 2]

queries1 :: [Int]
queries1 = [0, 3, 5]

nums2 :: [Int]
nums2 = [1, 2, 3, 4]

queries2 :: [Int]
queries2 = [0, 1, 2, 3]

input :: [Int] -> [Int] -> IO ()
input = input2 "nums" "queries"

output :: [Int] -> IO ()
output = output1

main :: IO ()
main = do
  input nums1 queries1
  output (solveQueries nums1 queries1)
  input nums2 queries2
  output (solveQueries nums2 queries2)
