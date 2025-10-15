{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO

maxIncreasingSubarrays :: [Int] -> Int
maxIncreasingSubarrays nums = _maxIncreasingSubarrays nums 0 1 0

_maxIncreasingSubarrays :: [Int] -> Int -> Int -> Int -> Int
_maxIncreasingSubarrays (num1 : remain@(num2 : _)) prev cnt res
  | num1 < num2 = _maxIncreasingSubarrays remain prev (cnt + 1) (combine prev (cnt + 1) res)
  | otherwise = _maxIncreasingSubarrays remain cnt 1 (combine cnt 1 res)
_maxIncreasingSubarrays [_] _ _ res = res
_maxIncreasingSubarrays [] _ _ res = res

combine :: Int -> Int -> Int -> Int
combine prev cnt res = res `max` (prev `min` cnt) `max` (cnt `div` 2)

nums1 :: [Int]
nums1 = [2, 5, 7, 8, 9, 2, 3, 4, 3, 1]

nums2 :: [Int]
nums2 = [1, 2, 3, 4, 4, 4, 4, 5, 6, 7]

nums3 :: [Int]
nums3 = [-15, 19]

nums4 :: [Int]
nums4 = [5, 8, -2, -1]

nums5 :: [Int]
nums5 = [-17, -20, -3, -12, -16]

input :: [Int] -> IO ()
input = input1 "nums1"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  input nums1
  output (maxIncreasingSubarrays nums1)
  input nums2
  output (maxIncreasingSubarrays nums2)
  input nums3
  output (maxIncreasingSubarrays nums3)
  input nums4
  output (maxIncreasingSubarrays nums4)
  input nums5
  output (maxIncreasingSubarrays nums5)
