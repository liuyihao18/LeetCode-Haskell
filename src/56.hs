{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.List
import IO

merge :: [[Int]] -> [[Int]]
merge = _merge . sort

_merge :: [[Int]] -> [[Int]]
_merge [] = []
_merge [x] = [x]
_merge (x : y : z)
  | last x < head y = x : _merge (y : z)
  | otherwise = _merge ([head x, last x `max` last y] : z)

intervals1 :: [[Int]]
intervals1 = [[1, 3], [2, 6], [8, 10], [15, 18]]

intervals2 :: [[Int]]
intervals2 = [[1, 4], [4, 5]]

intervals3 :: [[Int]]
intervals3 = [[1, 4], [2, 3]]

input :: [[Int]] -> IO ()
input = input1 "intervals"

output :: [[Int]] -> IO ()
output = output1

main :: IO ()
main = do
  input intervals1
  output (merge intervals1)
  input intervals2
  output (merge intervals2)
  input intervals3
  output (merge intervals3)
