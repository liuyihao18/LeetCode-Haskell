{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO

solute :: [String] -> [String]
solute = reverse

s1 :: [String]
s1 = ["h", "e", "l", "l", "o"]

s2 :: [String]
s2 = ["H", "a", "n", "n", "a", "h"]

input :: [String] -> IO ()
input = input1 "s"

output :: [String] -> IO ()
output = output1

main :: IO ()
main = do
  input s1
  output (solute s1)
  input s2
  output (solute s2)