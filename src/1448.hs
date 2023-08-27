{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO
import Tree

type State = (Tree Int, Int)

goodNodes :: Tree Int -> Int
goodNodes root = dfs (root, minBound)

dfs :: State -> Int
dfs (Empty, _) = 0
dfs (TreeNode v l r, m)
  | v >= m = 1 + dfs (l, mm) + dfs (r, mm)
  | otherwise = dfs (l, mm) + dfs (r, mm)
  where
    mm = max m v

root1 :: Tree Int
root1 = fromString "[3,1,4,3,null,1,5]"

root2 :: Tree Int
root2 = fromString "[3, 3, null, 4, 2]"

root3 :: Tree Int
root3 = fromString "[1]"

input :: Tree Int -> IO ()
input = input1 "root"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  input root1
  output (goodNodes root1)
  input root2
  output (goodNodes root2)
  input root3
  output (goodNodes root3)
