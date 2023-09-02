{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.Array
import Data.List
import IO

minTrioDegree :: Int -> [[Int]] -> Int
minTrioDegree n edges =
  let trioDegree = map (\(i, j, k) -> degree !! i + degree !! j + degree !! k - 6) $ filter (\(i, j, k) -> graph ! (i, j) == 1 && graph ! (i, k) == 1 && graph ! (j, k) == 1) $ [(i, j, k) | i <- [0 .. n - 1], j <- [0 .. n - 1], k <- [0 .. n - 1]]
   in if null trioDegree then -1 else minimum trioDegree
  where
    us = map ((\x -> x - 1) . head) edges
    vs = map ((\x -> x - 1) . last) edges
    degree = map (\x -> length (filter (== x) us) + length (filter (== x) vs)) [0 .. n - 1]
    graph =
      array ((0, 0), (n - 1, n - 1)) $
        [(uv, 1) | uv <- zip us vs]
          ++ [(uv, 0) | uv <- [(i, j) | i <- [0 .. n - 1], j <- [0 .. n - 1]] \\ zip us vs] ::
        Array (Int, Int) Int

n1 :: Int
n1 = 6

edges1 :: [[Int]]
edges1 = [[1, 2], [1, 3], [3, 2], [4, 1], [5, 2], [3, 6]]

n2 :: Int
n2 = 7

edges2 :: [[Int]]
edges2 = [[1, 3], [4, 1], [4, 3], [2, 5], [5, 6], [6, 7], [7, 5], [2, 6]]

n3 :: Int
n3 = 3

edges3 :: [[Int]]
edges3 = [[3, 2], [2, 1]]

input :: Int -> [[Int]] -> IO ()
input = input2 "n" "edges"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  input n1 edges1
  output (minTrioDegree n1 edges1)
  input n2 edges2
  output (minTrioDegree n2 edges2)
  input n3 edges3
  output (minTrioDegree n3 edges3)
