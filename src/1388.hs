{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.Array
import IO

maxSizeSlices :: [Int] -> Int
maxSizeSlices slices = max (calculate $ take (n - 1) slices) (calculate $ drop 1 slices)
  where
    n = length slices

calculate :: [Int] -> Int
calculate slices = dp ! (n - 1, k)
  where
    n = length slices
    k = (n + 1) `div` 3
    dp =
      -- dp[i][j] indicates that j non-adjacent numbers are selected among the first (i + 1) numbers
      array ((0, 0), (n - 1, k)) $
        [((i, 0), 0) | i <- [0 .. n - 1]]
          ++ [((0, 1), head slices)]
          ++ [((1, 1), maximum $ take 2 slices)]
          ++ [((i, j), minBound) | i <- [0, 1], j <- [2 .. k]]
          ++ [((i, j), max (dp ! (i - 1, j)) (dp ! (i - 2, j - 1) + slices !! i)) | i <- [2 .. n - 1], j <- [1 .. k]]

slices1 :: [Int]
slices1 = [1, 2, 3, 4, 5, 6]

slices2 :: [Int]
slices2 = [8, 9, 8, 6, 1, 1]

input :: [Int] -> IO ()
input = input1 "slices"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  input slices1
  output (maxSizeSlices slices1)
  input slices2
  output (maxSizeSlices slices2)
