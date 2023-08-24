{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.List
import IO

countServers :: [[Int]] -> Int
countServers grid = length $ filter (\(i, j) -> grid !! i !! j == 1 && (rowCnt !! i > 1 || colCnt !! j > 1)) [(i, j) | i <- [0 .. m - 1], j <- [0 .. n - 1]]
  where
    m = length grid
    n = length $ head grid
    rowCnt = countEveryRow grid
    colCnt = countEveryRow $ transpose grid

countEveryRow :: [[Int]] -> [Int]
countEveryRow = map (length . filter (== 1))

grid1 :: [[Int]]
grid1 = [[1, 0], [0, 1]]

grid2 :: [[Int]]
grid2 = [[1, 0], [1, 1]]

grid3 :: [[Int]]
grid3 = [[1, 1, 0, 0], [0, 0, 1, 0], [0, 0, 1, 0], [0, 0, 0, 1]]

input :: [[Int]] -> IO ()
input = input1 "grid"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  input grid1
  output (countServers grid1)
  input grid2
  output (countServers grid2)
  input grid3
  output (countServers grid3)
