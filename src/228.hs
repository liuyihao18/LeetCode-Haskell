{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO

type Range = (Int, Int)

summaryRanges :: [Int] -> [String]
summaryRanges [] = []
summaryRanges (x : xs) =
  ( if left == right
      then show left
      else show left ++ "->" ++ show right
  )
    : summaryRanges r
  where
    ((left, right), r) = _summaryRanges ((x, x), xs)

_summaryRanges :: (Range, [Int]) -> (Range, [Int])
_summaryRanges res@(_, []) = res
_summaryRanges res@((left, right), x : xs)
  | x == right + 1 = _summaryRanges ((left, x), xs)
  | otherwise = res

nums1 :: [Int]
nums1 = [0, 1, 2, 4, 5, 7]

nums2 :: [Int]
nums2 = [0, 2, 3, 4, 6, 8, 9]

nums3 :: [Int]
nums3 = [-2147483648, -2147483647, 2147483647]

input :: [Int] -> IO ()
input = input1 "nums"

output :: [String] -> IO ()
output = output1

main :: IO ()
main = do
  input nums1
  output (summaryRanges nums1)
  input nums2
  output (summaryRanges nums2)
  input nums3
  output (summaryRanges nums3)
