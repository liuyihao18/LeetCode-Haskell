{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO
import Tree

mergeTrees :: Tree Integer -> Tree Integer -> Tree Integer
mergeTrees Empty root2 = root2
mergeTrees root1 Empty = root1
mergeTrees (TreeNode val1 left1 right1) (TreeNode val2 left2 right2) =
  TreeNode (val1 + val2) (mergeTrees left1 left2) (mergeTrees right1 right2)

root11 :: Tree Integer
root11 = fromString "[1,3,2,5]"

root21 :: Tree Integer
root21 = fromString "[2,1,3,null,4,null,7]"

root12 :: Tree Integer
root12 = fromString "[1]"

root22 :: Tree Integer
root22 = fromString "[1,2]"

input :: Tree Integer -> Tree Integer -> IO ()
input = input2 "root1" "root2"

output :: Tree Integer -> IO ()
output = output1

main :: IO ()
main = do
  input root11 root21
  output (mergeTrees root11 root21)
  input root12 root22
  output (mergeTrees root12 root22)
