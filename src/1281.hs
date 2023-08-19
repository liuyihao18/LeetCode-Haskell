{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO

subtractProductAndSum :: Int -> Int
subtractProductAndSum n = product digits - sum digits
  where
    digits = toDigits n

toDigits :: Int -> [Int]
toDigits 0 = []
toDigits n = n `mod` 10 : toDigits (n `div` 10)

input :: Int -> IO ()
input = input1 "n"

output :: Int -> IO ()
output = output1

n1 :: Int
n1 = 234

n2 :: Int
n2 = 4421

main :: IO ()
main = do
  input n1
  output (subtractProductAndSum n1)
  input n2
  output (subtractProductAndSum n2)
