{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO

diagonalSum :: [[Int]] -> Int
diagonalSum mat = sum $ map (extractSum mat . generate n) [0 .. n - 1]
  where
    n = length mat

-- Two indics in a row.
generate :: Int -> Int -> ((Int, Int), (Int, Int))
generate n i = ((i, i), (i, n - i - 1))

-- Remove the duplicate one.
extractSum :: [[Int]] -> ((Int, Int), (Int, Int)) -> Int
extractSum mat ((i1, j1), (i2, j2))
  | j1 /= j2 = mat !! i1 !! j1 + mat !! i2 !! j2
  | otherwise = mat !! i1 !! j1

mat1 :: [[Int]]
mat1 =
  [ [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
  ]

mat2 :: [[Int]]
mat2 =
  [ [1, 1, 1, 1],
    [1, 1, 1, 1],
    [1, 1, 1, 1],
    [1, 1, 1, 1]
  ]

mat3 :: [[Int]]
mat3 = [[5]]

input :: [[Int]] -> IO ()
input = input1 "mat"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  input mat1
  output (diagonalSum mat1)
  input mat2
  output (diagonalSum mat2)
  input mat3
  output (diagonalSum mat3)
