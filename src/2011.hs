{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO

finalValueAfterOperations :: [String] -> Int
finalValueAfterOperations = foldr compute 0

compute :: String -> Int -> Int
compute op value
  | op == "++X" = value + 1
  | op == "X++" = value + 1
  | op == "--X" = value - 1
  | op == "X--" = value - 1
  | otherwise = value

operations1 :: [String]
operations1 = ["--X", "X++", "X++"]

operations2 :: [String]
operations2 = ["++X", "++X", "X++"]

operations3 :: [String]
operations3 = ["X++", "++X", "--X", "X--"]

input :: [String] -> IO ()
input = input1 "operations"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  input operations1
  output (finalValueAfterOperations operations1)
  input operations2
  output (finalValueAfterOperations operations2)
  input operations3
  output (finalValueAfterOperations operations3)
