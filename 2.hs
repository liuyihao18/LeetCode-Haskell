{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO

addTwoNumbers :: [Integer] -> [Integer] -> [Integer]
addTwoNumbers l1 l2 = carry 0 $ add l1 l2

add :: [Integer] -> [Integer] -> [Integer]
add xs [] = xs
add [] ys = ys
add (x : xs) (y : ys) = (x + y) : add xs ys

carry :: Integer -> [Integer] -> [Integer]
carry c []
  | c == 0 = []
  | otherwise = [1]
carry c (x : xs)
  | x + c >= 10 = x + c - 10 : carry 1 xs
  | otherwise = x + c : carry 0 xs

l11 :: [Integer]
l11 = [2, 4, 3]

l21 :: [Integer]
l21 = [5, 6, 4]

l12 :: [Integer]
l12 = [0]

l22 :: [Integer]
l22 = [0]

l13 :: [Integer]
l13 = [9, 9, 9, 9, 9, 9, 9]

l23 :: [Integer]
l23 = [9, 9, 9, 9]

input :: [Integer] -> [Integer] -> IO ()
input = input2 "l1" "l2"

output :: [Integer] -> IO ()
output = output1

main :: IO ()
main = do
  input l11 l21
  output (addTwoNumbers l11 l12)
  input l12 l22
  output (addTwoNumbers l12 l22)
  input l13 l23
  output (addTwoNumbers l13 l23)
