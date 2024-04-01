{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.List
import IO

minimumSum :: [Int] -> Int
minimumSum nums = case sums of
  [] -> -1
  _ -> minimum sums
  where
    n = length nums
    left = getLeft nums
    right = getRight nums
    index = filter (\i -> left !! (i - 1) < nums !! i && right !! (i + 1) < nums !! i) [1 .. n - 2]
    sums = map (\i -> left !! i + nums !! i + right !! i) index

getLeft :: [Int] -> [Int]
getLeft = reverse . foldl' nextSmall []

getRight :: [Int] -> [Int]
getRight = reverse . getLeft . reverse

nextSmall :: [Int] -> Int -> [Int]
nextSmall [] y = [y]
nextSmall left@(x : _) y
  | y < x = y : left
  | otherwise = x : left

nums1 :: [Int]
nums1 = [8, 6, 1, 5, 3]

nums2 :: [Int]
nums2 = [5, 4, 8, 7, 10, 2]

nums3 :: [Int]
nums3 = [6, 5, 4, 3, 4, 5]

input :: [Int] -> IO ()
input = input1 "nums"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  input nums1
  output (minimumSum nums1)
  input nums2
  output (minimumSum nums2)
  input nums3
  output (minimumSum nums3)