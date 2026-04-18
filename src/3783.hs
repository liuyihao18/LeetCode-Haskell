{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO
import Util

mirrorDistance :: Int -> Int
mirrorDistance n = abs (n - flipInt n)

n1 :: Int
n1 = 25

n2 :: Int
n2 = 10

n3 :: Int
n3 = 7

input :: Int -> IO ()
input = input1 "n"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  input n1
  output (mirrorDistance n1)
  input n2
  output (mirrorDistance n2)
  input n3
  output (mirrorDistance n3)
