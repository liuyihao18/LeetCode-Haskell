{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO

satisfiesConditions :: [[Int]] -> Bool
satisfiesConditions grid = flag1 && flag2
  where
    firstRow = head grid
    flag1 = all (== firstRow) grid
    flag2 = adjacentDistinct firstRow

adjacentDifferences :: [Int] -> [Int]
adjacentDifferences = zipWith (-) <*> tail

adjacentDistinct :: [Int] -> Bool
adjacentDistinct [] = True
adjacentDistinct [_] = True
adjacentDistinct (a : b : xs) = (a /= b) && adjacentDistinct (b : xs)

grid1 :: [[Int]]
grid1 = [[1, 0, 2], [1, 0, 2]]

grid2 :: [[Int]]
grid2 = [[1, 1, 1], [0, 0, 0]]

grid3 :: [[Int]]
grid3 = [[1], [2], [3]]

input :: [[Int]] -> IO ()
input = input1 "grid"

output :: Bool -> IO ()
output = output1

main :: IO ()
main = do
  input grid1
  output (satisfiesConditions grid1)
  input grid2
  output (satisfiesConditions grid2)
  input grid3
  output (satisfiesConditions grid3)