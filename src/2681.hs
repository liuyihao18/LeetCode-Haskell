{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.List
import IO

data Result = Result {preSum :: Integer, power :: Integer}

sumOfPower :: [Integer] -> Integer
sumOfPower nums = power $ foldl' travel (Result 0 0) sortedNums
  where
    sortedNums = sort nums

remainder :: Integer
remainder = 1000000007

travel :: Result -> Integer -> Result
travel (Result preSum power) num = Result newPreSum newPower
  where
    newDp = (num + preSum) `mod` remainder
    newPreSum = (preSum + newDp) `mod` remainder
    newPower = (power + num * num `mod` remainder * newDp) `mod` remainder

nums1 :: [Integer]
nums1 = [2, 1, 4]

nums2 :: [Integer]
nums2 = [1, 1, 1]

nums3 :: [Integer]
nums3 = [658, 489, 777, 2418, 1893, 130, 2448, 178, 1128, 2149, 1059, 1495, 1166, 608, 2006, 713, 1906, 2108, 680, 1348, 860, 1620, 146, 2447, 1895, 1083, 1465, 2351, 1359, 1187, 906, 533, 1943, 1814, 1808, 2065, 1744, 254, 1988, 1889, 1206]

input :: [Integer] -> IO ()
input = input1 "nums"

output :: Integer -> IO ()
output = output1

main :: IO ()
main = do
  input nums1
  output (sumOfPower nums1)
  input nums2
  output (sumOfPower nums2)
  input nums3
  output (sumOfPower nums3)
