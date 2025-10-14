{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO

hasIncreasingSubarrays :: [Int] -> Int -> Bool
hasIncreasingSubarrays nums k = maxIncreasingSubarrays >= k
  where
    maxIncreasingSubarrays = computeMaxIncreasingSubarrays nums 0 1 0

computeMaxIncreasingSubarrays :: [Int] -> Int -> Int -> Int -> Int
computeMaxIncreasingSubarrays (num1 : remain@(num2 : _)) prev cnt res
  | num1 < num2 = computeMaxIncreasingSubarrays remain prev (cnt + 1) (combine prev (cnt + 1) res)
  | otherwise = computeMaxIncreasingSubarrays remain cnt 1 (combine cnt 1 res)
computeMaxIncreasingSubarrays [_] _ _ res = res
computeMaxIncreasingSubarrays [] _ _ res = res

combine :: Int -> Int -> Int -> Int
combine prev cnt res = res `max` (prev `min` cnt) `max` (cnt `div` 2)

nums1 :: [Int]
nums1 = [2, 5, 7, 8, 9, 2, 3, 4, 3, 1]

k1 :: Int
k1 = 3

nums2 :: [Int]
nums2 = [1, 2, 3, 4, 4, 4, 4, 5, 6, 7]

k2 :: Int
k2 = 5

num3 :: [Int]
num3 = [-15, 19]

k3 :: Int
k3 = 1

nums4 :: [Int]
nums4 = [5, 8, -2, -1]

k4 :: Int
k4 = 2

nums5 :: [Int]
nums5 = [-17, -20, -3, -12, -16]

k5 :: Int
k5 = 2

input :: [Int] -> Int -> IO ()
input = input2 "nums1" "k1"

output :: Bool -> IO ()
output = output1

main :: IO ()
main = do
  input nums1 k1
  output (hasIncreasingSubarrays nums1 k1)
  input nums2 k2
  output (hasIncreasingSubarrays nums2 k2)
  input num3 k3
  output (hasIncreasingSubarrays num3 k3)
  input nums4 k4
  output (hasIncreasingSubarrays nums4 k4)
  input nums5 k5
  output (hasIncreasingSubarrays nums5 k5)
