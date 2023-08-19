{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.Array
import Data.List
import IO

minFallingPathSum :: [[Int]] -> Int
minFallingPathSum grid = getRowMin dp (n - 1) (-1)
  where
    n = length grid
    dp =
      array ((0, 0), (n - 1, n - 1)) $
        [((0, j), head grid !! j) | j <- [0 .. n - 1]]
          ++ [((i, j), grid !! i !! j + getRowMin dp (i - 1) j) | i <- [1 .. n - 1], j <- [0 .. n - 1]]

-- getRowMin arr row col / Choose the minimum in the given row of the given array except in the given col
getRowMin :: Array (Int, Int) Int -> Int -> Int -> Int
getRowMin arr = chooseRowMin . getRowInfo arr

getRowInfo :: Array (Int, Int) Int -> Int -> (Int, Int, Int, Int)
getRowInfo arr row =
  foldl'
    (\info@(fstCol, fstMin, _, sndMin) col -> if arr ! (row, col) < fstMin then (col, arr ! (row, col), fstCol, fstMin) else if arr ! (row, col) < sndMin then (fstCol, fstMin, col, arr ! (row, col)) else info)
    (0, maxBound, 0, maxBound)
    [minColIdx .. maxColIdx]
  where
    ((_, minColIdx), (_, maxColIdx)) = bounds arr

chooseRowMin :: (Int, Int, Int, Int) -> Int -> Int
chooseRowMin (fstCol, fstMin, _, sndMin) col = if col == fstCol then sndMin else fstMin

grid1 :: [[Int]]
grid1 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

grid2 :: [[Int]]
grid2 = [[7]]

grid3 :: [[Int]]
grid3 = [[1, 4, 3, 5, 1], [4, 5, 6, 2, 7], [7, 8, 9, 1, 25], [6, 16, 7, 13, 2], [61, 6, 7, 8, 3]]

input :: [[Int]] -> IO ()
input = input1 "grid"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  input grid1
  output (minFallingPathSum grid1)
  input grid2
  output (minFallingPathSum grid2)
  input grid3
  output (minFallingPathSum grid3)