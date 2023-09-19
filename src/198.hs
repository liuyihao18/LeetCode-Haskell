{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO

rob :: [Int] -> Int
rob = _rob 0 0

_rob :: Int -> Int -> [Int] -> Int
_rob prev curr [] = prev `max` curr
_rob prev curr (num : nums) = _rob curr ((prev + num) `max` curr) nums

nums1 :: [Int]
nums1 = [1, 2, 3, 1]

nums2 :: [Int]
nums2 = [2, 7, 9, 3, 1]

input :: [Int] -> IO ()
input = input1 "nums"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  input nums1
  output (rob nums1)
  input nums2
  output (rob nums2)
