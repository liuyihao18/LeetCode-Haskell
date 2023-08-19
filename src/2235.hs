{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO

sum :: Int -> Int -> Int
sum = (+)

num11 :: Int
num11 = 12

num21 :: Int
num21 = 5

num12 :: Int
num12 = -10

num22 :: Int
num22 = 4

input :: Int -> Int -> IO ()
input = input2 "num1" "num2"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  input num11 num21
  output (Solution.sum num11 num21)
  input num12 num22
  output (Solution.sum num12 num22)
