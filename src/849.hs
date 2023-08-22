{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO

maxDistToClosest :: [Int] -> Int
maxDistToClosest seats = lMax `max` mMax `max` rMax
  where
    n = length seats
    index = filter (\i -> seats !! i == 1) [0 .. n - 1]
    mMax = case getMedian index of
      [] -> 0
      median -> maximum median
    (lMax, rMax) = case index of
      [] -> (0, 0)
      _ -> (head index, n - 1 - last index)

getMedian :: [Int] -> [Int]
getMedian index = [(index !! (i + 1) - index !! i) `div` 2 | i <- [0 .. n - 2]]
  where
    n = length index

seats1 :: [Int]
seats1 = [1, 0, 0, 0, 1, 0, 1]

seats2 :: [Int]
seats2 = [1, 0, 0, 0]

seats3 :: [Int]
seats3 = [0, 1]

input :: [Int] -> IO ()
input = input1 "seats"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  input seats1
  output (maxDistToClosest seats1)
  input seats2
  output (maxDistToClosest seats2)
  input seats3
  output (maxDistToClosest seats3)
