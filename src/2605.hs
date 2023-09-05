{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.Set qualified as S
import IO

minNumber :: [Int] -> [Int] -> Int
minNumber nums1 nums2 = if S.null is then 10 * (s1m `min` s2m) + (s1m `max` s2m) else S.elemAt 0 is
  where
    s1 = S.fromList nums1
    s2 = S.fromList nums2
    is = s1 `S.intersection` s2
    s1m = S.elemAt 0 s1
    s2m = S.elemAt 0 s2

nums11 :: [Int]
nums11 = [4, 1, 3]

nums21 :: [Int]
nums21 = [5, 7]

nums12 :: [Int]
nums12 = [3, 5, 2, 6]

nums22 :: [Int]
nums22 = [3, 1, 7]

input :: [Int] -> [Int] -> IO ()
input = input2 "nums1" "nums2"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  input nums11 nums21
  output (minNumber nums11 nums21)
  input nums12 nums22
  output (minNumber nums12 nums22)