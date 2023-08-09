{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO

solute :: Integer -> Integer
solute n = product digits - sum digits
  where
    digits = toDigits n

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = n `mod` 10 : toDigits (n `div` 10)

input :: Integer -> IO ()
input = input1 "n"

output :: Integer -> IO ()
output = output1

n1 :: Integer
n1 = 234

n2 :: Integer
n2 = 4421

main :: IO ()
main = do
  input n1
  output (solute n1)
  input n2
  output (solute n2)
