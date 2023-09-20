{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO

minCount :: [Int] -> Int
minCount = sum . map ((`div` 2) . (+ 1))

coins1 :: [Int]
coins1 = [4, 2, 1]

coins2 :: [Int]
coins2 = [2, 3, 10]

input :: [Int] -> IO ()
input = input1 "coins"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  input coins1
  output (minCount coins1)
  input coins2
  output (minCount coins2)
