{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.List
import IO

countWays :: [Int] -> Int
countWays nums = foldr (\i res -> if _nums !! i > i && _nums !! (i - 1) < i then res + 1 else res) val [1 .. n - 1] + 1
  where
    _nums = sort nums
    n = length _nums
    val = if head _nums > 0 then 1 else 0

nums1 :: [Int]
nums1 = [1, 1]

nums2 :: [Int]
nums2 = [6, 0, 3, 3, 6, 7, 2, 7]

input :: [Int] -> IO ()
input = input1 "nums"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  input nums1
  output (countWays nums1)
  input nums2
  output (countWays nums2)
