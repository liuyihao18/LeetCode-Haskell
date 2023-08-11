{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO

alternateDigitSum :: Integer -> Integer
alternateDigitSum = sum . addSign . reverse . toDigits

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = n `mod` 10 : toDigits (n `div` 10)

addSign :: [Integer] -> [Integer]
addSign [] = []
addSign [x] = [x]
addSign (x : y : ys) = x : (-y) : addSign ys

n1 :: Integer
n1 = 521

n2 :: Integer
n2 = 111

n3 :: Integer
n3 = 886996

input :: Integer -> IO ()
input = input1 "n"

output :: Integer -> IO ()
output = output1

main :: IO ()
main = do
  input n1
  output (alternateDigitSum n1)
  input n2
  output (alternateDigitSum n2)
  input n3
  output (alternateDigitSum n3)
