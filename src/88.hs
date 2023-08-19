{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO

merge :: [Int] -> Int -> [Int] -> Int -> [Int]
merge nums1 _ _ 0 = nums1
merge nums1 0 nums2 j = take j nums2 ++ drop j nums1
merge nums1 m nums2 n
  | nums1 !! (m - 1) < nums2 !! (n - 1) = merge (take (m + n - 1) nums1 ++ [nums2 !! (n - 1)] ++ drop (m + n) nums1) m nums2 (n - 1)
  | otherwise = merge (take (m + n - 1) nums1 ++ [nums1 !! (m - 1)] ++ drop (m + n) nums1) (m - 1) nums2 n

nums11 :: [Int]
nums11 = [1, 2, 3, 0, 0, 0]

m1 :: Int
m1 = 3

nums21 :: [Int]
nums21 = [2, 5, 6]

n1 :: Int
n1 = 3

nums12 :: [Int]
nums12 = [1]

m2 :: Int
m2 = 1

nums22 :: [Int]
nums22 = []

n2 :: Int
n2 = 0

nums13 :: [Int]
nums13 = [0]

m3 :: Int
m3 = 0

nums23 :: [Int]
nums23 = [1]

n3 :: Int
n3 = 1

nums14 :: [Int]
nums14 = [4, 0, 0, 0, 0, 0]

m4 :: Int
m4 = 1

nums24 :: [Int]
nums24 = [1, 2, 3, 5, 6]

n4 :: Int
n4 = 5

input :: [Int] -> Int -> [Int] -> Int -> IO ()
input = input4 "nums1" "m" "nums2" "n"

output :: [Int] -> IO ()
output = output1

main :: IO ()
main = do
  input nums11 m1 nums21 n1
  output (merge nums11 m1 nums21 n1)
  input nums12 m2 nums22 n2
  output (merge nums12 m2 nums22 n2)
  input nums13 m3 nums23 n3
  output (merge nums13 m3 nums23 n3)
  input nums14 m4 nums24 n4
  output (merge nums14 m4 nums24 n4)
