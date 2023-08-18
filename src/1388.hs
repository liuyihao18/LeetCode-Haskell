{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.Array
import IO

maxSizeSlices :: [Integer] -> Integer
maxSizeSlices slices = max (calculate $ take (n - 1) slices) (calculate $ drop 1 slices)
  where
    n = length slices

calculate :: [Integer] -> Integer
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
          ++ [((i, j), intMin) | i <- [0, 1], j <- [2 .. k]]
          ++ [((i, j), max (dp ! (i - 1, j)) (dp ! (i - 2, j - 1) + slices !! i)) | i <- [2 .. n - 1], j <- [1 .. k]]

intMin :: Integer
intMin = toInteger (minBound :: Int)

slices1 :: [Integer]
slices1 = [1, 2, 3, 4, 5, 6]

slices2 :: [Integer]
slices2 = [8, 9, 8, 6, 1, 1]

input :: [Integer] -> IO ()
input = input1 "slices"

output :: Integer -> IO ()
output = output1

main :: IO ()
main = do
  input slices1
  output (maxSizeSlices slices1)
  input slices2
  output (maxSizeSlices slices2)
