{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO

maxDistance :: [Int] -> Int
maxDistance colors = ans1 `max` ans2
  where
    n = length colors
    rColors = reverse colors
    h = head colors
    t = head rColors
    check1 = zipWith (\i v -> (if v /= h then i else 0)) [0 .. n - 1] colors
    check2 = zipWith (\i v -> (if v /= t then i else 0)) (reverse [0 .. n - 1]) colors
    ans1 = foldr max 0 check1
    ans2 = foldr max 0 check2

colors1 :: [Int]
colors1 = [1, 1, 1, 6, 1, 1, 1]

colors2 :: [Int]
colors2 = [1, 8, 3, 8, 3]

colors3 :: [Int]
colors3 = [0, 1]

input :: [Int] -> IO ()
input = input1 "colors"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  input colors1
  output (maxDistance colors1)
  input colors2
  output (maxDistance colors2)
  input colors3
  output (maxDistance colors3)
