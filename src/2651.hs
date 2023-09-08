{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO

findDelayedArrivalTime :: Int -> Int -> Int
findDelayedArrivalTime arrivalTime delayedTime = (arrivalTime + delayedTime) `mod` 24

arrivalTime1 :: Int
arrivalTime1 = 15

delayedTime1 :: Int
delayedTime1 = 5

arrivalTime2 :: Int
arrivalTime2 = 13

delayedTime2 :: Int
delayedTime2 = 11

input :: Int -> Int -> IO ()
input = input2 "arrivalTime" "delayedTime"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  input arrivalTime1 delayedTime1
  output (findDelayedArrivalTime arrivalTime1 delayedTime1)
  input arrivalTime2 delayedTime2
  output (findDelayedArrivalTime arrivalTime2 delayedTime2)
