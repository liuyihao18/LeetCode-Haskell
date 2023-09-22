{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO

distMoney :: Int -> Int -> Int
distMoney money children
  | temp == 4 = children - 2
  | temp < 0 = children - 1
  | otherwise = (money - children) `div` 7
  where
    temp = children * 8 - money

money1 :: Int
money1 = 20

children1 :: Int
children1 = 3

money2 :: Int
money2 = 16

children2 :: Int
children2 = 2

input :: Int -> Int -> IO ()
input = input2 "money" "children"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  input money1 children1
  output (distMoney money1 children1)
  input money2 children2
  output (distMoney money2 children2)
