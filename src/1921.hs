{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.List
import IO

eliminateMaximum :: [Int] -> [Int] -> Int
eliminateMaximum dist speed = length $ takeWhile id $ zipWith (<=) [0 ..] $ sort $ zipWith (\x y -> (x - 1) `div` y) dist speed

dist1 :: [Int]
dist1 = [1, 3, 4]

speed1 :: [Int]
speed1 = [1, 1, 1]

dist2 :: [Int]
dist2 = [1, 1, 2, 3]

speed2 :: [Int]
speed2 = [1, 1, 1, 1]

dist3 :: [Int]
dist3 = [3, 2, 4]

speed3 :: [Int]
speed3 = [5, 3, 2]

input :: [Int] -> [Int] -> IO ()
input = input2 "dist" "speed"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  input dist1 speed1
  output (eliminateMaximum dist1 speed1)
  input dist2 speed2
  output (eliminateMaximum dist2 speed2)
  input dist3 speed3
  output (eliminateMaximum dist3 speed3)
