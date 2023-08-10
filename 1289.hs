{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.Array
import Data.List
import IO

solute :: [[Integer]] -> Integer
solute grid = getRowMin dp (n - 1) (-1)
  where
    n = length grid
    dp =
      array ((0, 0), (n - 1, n - 1)) $
        [((0, j), head grid !! j) | j <- [0 .. n - 1]]
          ++ [((i, j), grid !! i !! j + getRowMin dp (i - 1) j) | i <- [1 .. n - 1], j <- [0 .. n - 1]]

-- getRowMin arr row col / Choose the minimum in the given row of the given array except in the given col
getRowMin :: Array (Int, Int) Integer -> Int -> Int -> Integer
getRowMin arr = chooseRowMin . getRowInfo arr

getRowInfo :: Array (Int, Int) Integer -> Int -> (Int, Integer, Int, Integer)
getRowInfo arr row =
  foldl'
    (\info@(fstCol, fstMin, _, sndMin) col -> if arr ! (row, col) < fstMin then (col, arr ! (row, col), fstCol, fstMin) else if arr ! (row, col) < sndMin then (fstCol, fstMin, col, arr ! (row, col)) else info)
    (0, intMax, 0, intMax)
    [minColIdx .. maxColIdx]
  where
    ((_, minColIdx), (_, maxColIdx)) = bounds arr

chooseRowMin :: (Int, Integer, Int, Integer) -> Int -> Integer
chooseRowMin (fstCol, fstMin, _, sndMin) col = if col == fstCol then sndMin else fstMin

intMax :: Integer
intMax = toInteger (maxBound :: Int)

grid1 :: [[Integer]]
grid1 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

grid2 :: [[Integer]]
grid2 = [[7]]

grid3 :: [[Integer]]
grid3 = [[1, 4, 3, 5, 1], [4, 5, 6, 2, 7], [7, 8, 9, 1, 25], [6, 16, 7, 13, 2], [61, 6, 7, 8, 3]]

input :: [[Integer]] -> IO ()
input = input1 "grid"

output :: Integer -> IO ()
output = output1

main :: IO ()
main = do
  input grid1
  output (solute grid1)
  input grid2
  output (solute grid2)
  input grid3
  output (solute grid3)