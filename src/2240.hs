{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO

waysToBuyPensPencils :: Int -> Int -> Int -> Int
waysToBuyPensPencils total cost1 cost2 = sum . map (\cnt1 -> (total - cost1 * cnt1) `div` cost2 + 1) . takeWhile (\cnt1 -> cost1 * cnt1 <= total) $ [0 ..]

total1 :: Int
total1 = 20

cost11 :: Int
cost11 = 10

cost21 :: Int
cost21 = 5

total2 :: Int
total2 = 5

cost12 :: Int
cost12 = 10

cost22 :: Int
cost22 = 10

input :: Int -> Int -> Int -> IO ()
input = input3 "total" "cost1" "cost2"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  input total1 cost11 cost21
  output (waysToBuyPensPencils total1 cost11 cost21)
  input total2 cost12 cost22
  output (waysToBuyPensPencils total2 cost12 cost22)
