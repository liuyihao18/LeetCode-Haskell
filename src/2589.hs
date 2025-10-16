{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.List
import Data.Maybe

import IO

findSmallestInteger :: [Int] -> Int -> Int
findSmallestInteger nums value = minElem * value + fromMaybe 0 minIdx
    where mods = map (`mod` value) nums
          counts = map (\n -> length . filter (== n) $ mods) [0..value-1]
          minElem = minimum counts
          minIdx = elemIndex minElem counts

nums1 :: [Int]
nums1 = [1,-10,7,13,6,8]

value1 :: Int
value1 = 5

nums2 :: [Int]
nums2 = [1,-10,7,13,6,8]

value2 :: Int
value2 = 7

nums3 :: [Int]
nums3 = [3,0,3,2,4,2,1,1,0,4]

value3 :: Int
value3 = 5

input :: [Int] -> Int -> IO ()
input = input2 "nums" "value"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
    input nums1 value1
    output (findSmallestInteger nums1 value1)
    input nums2 value2
    output (findSmallestInteger nums2 value2)
    input nums3 value3
    output (findSmallestInteger nums3 value3)
