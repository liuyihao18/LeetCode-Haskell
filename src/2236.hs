{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO
import Tree

checkTree :: Tree Int -> Bool
checkTree Empty = False
checkTree (TreeNode _ Empty _) = False
checkTree (TreeNode _ _ Empty) = False
checkTree (TreeNode v l r) = v == val l + val r

root1 :: Tree Int
root1 = fromString "[10, 4, 6]"

root2 :: Tree Int
root2 = fromString "[5,3,1]"

input :: Tree Int -> IO ()
input = input1 "root"

output :: Bool -> IO ()
output = output1

main :: IO ()
main = do
  input root1
  output (checkTree root1)
  input root2
  output (checkTree root2)
