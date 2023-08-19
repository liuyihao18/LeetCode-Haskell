{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO

alternateDigitSum :: Int -> Int
alternateDigitSum = sum . addSign . reverse . toDigits

toDigits :: Int -> [Int]
toDigits 0 = []
toDigits n = n `mod` 10 : toDigits (n `div` 10)

addSign :: [Int] -> [Int]
addSign [] = []
addSign [x] = [x]
addSign (x : y : ys) = x : (-y) : addSign ys

n1 :: Int
n1 = 521

n2 :: Int
n2 = 111

n3 :: Int
n3 = 886996

input :: Int -> IO ()
input = input1 "n"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  input n1
  output (alternateDigitSum n1)
  input n2
  output (alternateDigitSum n2)
  input n3
  output (alternateDigitSum n3)
