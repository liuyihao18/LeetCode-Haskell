{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.List
import IO

sortedSquares :: [Int] -> [Int]
sortedSquares = sort . map (\x -> x * x)

nums1 :: [Int]
nums1 = [-4, -1, 0, 3, 10]

nums2 :: [Int]
nums2 = [-7, -3, 2, 3, 11]

input :: [Int] -> IO ()
input = input1 "nums"

output :: [Int] -> IO ()
output = output1

main :: IO ()
main = do
  input nums1
  output (sortedSquares nums1)
  input nums2
  output (sortedSquares nums2)