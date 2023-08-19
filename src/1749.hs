{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO

maxAbsoluteSum :: [Int] -> Int
maxAbsoluteSum nums = max (abs (maxSum nums 0 minBound)) (abs (maxSum (map negate nums) 0 minBound))

maxSum :: [Int] -> Int -> Int -> Int
maxSum [] dp res = max dp res
maxSum (x : xs) dp res = maxSum xs (max x (dp + x)) (max dp res)

input :: [Int] -> IO ()
input = input1 "nums"

output :: Int -> IO ()
output = output1

nums1 :: [Int]
nums1 = [1, -3, 2, 3, -4]

nums2 :: [Int]
nums2 = [2, -5, 1, -4, 3, -2]

nums3 :: [Int]
nums3 = [-7, -1, 0, -2, 1, 3, 8, -2, -6, -1, -10, -6, -6, 8, -4, -9, -4, 1, 4, -9]

main :: IO ()
main = do
  input nums1
  output (maxAbsoluteSum nums1)
  input nums2
  output (maxAbsoluteSum nums2)
  input nums3
  output (maxAbsoluteSum nums3)
