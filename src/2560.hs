{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.List
import IO

minCapability :: [Int] -> Int -> Int
minCapability nums = _minCapability m1 m2 nums
  where
    m1 = minimum nums
    m2 = maximum nums + 1

_minCapability :: Int -> Int -> [Int] -> Int -> Int
_minCapability left right nums target
  | left >= right = left
  | stolen < target = _minCapability (median + 1) right nums target
  | otherwise = _minCapability left median nums target
  where
    median = (left + right) `div` 2
    stolen = maxSteal nums median

maxSteal :: [Int] -> Int -> Int
maxSteal nums capability = fst $ foldl' (\(cnt, flag) num -> if flag || num > capability then (cnt, False) else (cnt + 1, True)) (0, False) nums

nums1 :: [Int]
nums1 = [2, 3, 5, 9]

k1 :: Int
k1 = 2

nums2 :: [Int]
nums2 = [2, 7, 9, 3, 1]

k2 :: Int
k2 = 2

input :: [Int] -> Int -> IO ()
input = input2 "nums" "k"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  input nums1 k1
  output (minCapability nums1 k1)
  input nums2 k2
  output (minCapability nums2 k2)
