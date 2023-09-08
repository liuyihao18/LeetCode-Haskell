{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO

isPalindrome :: Int -> Bool
isPalindrome x = x == y
  where
    y = flipInt x 0

flipInt :: Int -> Int -> Int
flipInt x y
  | x <= 0 = y
  | otherwise = flipInt (x `div` 10) (y * 10 + x `mod` 10)

x1 :: Int
x1 = 121

x2 :: Int
x2 = -121

x3 :: Int
x3 = 10

input :: Int -> IO ()
input = input1 "x"

output :: Bool -> IO ()
output = output1

main :: IO ()
main = do
  input x1
  output (isPalindrome x1)
  input x2
  output (isPalindrome x2)
  input x3
  output (isPalindrome x3)
