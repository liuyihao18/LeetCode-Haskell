{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO

solute :: [Integer] -> Integer
solute nums = max (abs (maxSum nums 0 intMin)) (abs (maxSum (map negate nums) 0 intMin))

maxSum :: [Integer] -> Integer -> Integer -> Integer
maxSum [] dp res = max dp res
maxSum (x : xs) dp res = maxSum xs (max x (dp + x)) (max dp res)

intMin :: Integer
intMin = toInteger (minBound :: Int)

input :: [Integer] -> IO ()
input = input1 "nums"

output :: Integer -> IO ()
output = output1

nums1 :: [Integer]
nums1 = [1, -3, 2, 3, -4]

nums2 :: [Integer]
nums2 = [2, -5, 1, -4, 3, -2]

nums3 :: [Integer]
nums3 = [-7, -1, 0, -2, 1, 3, 8, -2, -6, -1, -10, -6, -6, 8, -4, -9, -4, 1, 4, -9]

main :: IO ()
main = do
  input nums1
  output (solute nums1)
  input nums2
  output (solute nums2)
  input nums3
  output (solute nums3)
