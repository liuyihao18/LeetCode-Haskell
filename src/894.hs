{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO
import Tree

allPossibleFBT :: Int -> [Tree Int]
allPossibleFBT n
  | n == 1 = [singleton 0]
  | even n = []
  | otherwise = concat [combine (allPossibleFBT i) (allPossibleFBT (n - 1 - i)) | i <- [1, 3 .. n - 1]]

combine :: [Tree Int] -> [Tree Int] -> [Tree Int]
combine left right = [TreeNode 0 l r | l <- left, r <- right]

n1 :: Int
n1 = 7

n2 :: Int
n2 = 3

input :: Int -> IO ()
input = input1 "n"

output :: [Tree Int] -> IO ()
output = output1

main :: IO ()
main = do
  input n1
  output (allPossibleFBT n1)
  input n2
  output (allPossibleFBT n2)
