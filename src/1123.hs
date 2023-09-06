{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO
import Tree

lcaDeepestLeaves :: Tree Int -> Tree Int
lcaDeepestLeaves = fst . _lcaDeepestLeaves

_lcaDeepestLeaves :: Tree Int -> (Tree Int, Int)
_lcaDeepestLeaves Empty = (Empty, -1)
_lcaDeepestLeaves root@(TreeNode _ left right)
  | lDepth > rDepth = (lAncester, lDepth + 1)
  | lDepth < rDepth = (rAncester, rDepth + 1)
  | otherwise = (root, lDepth + 1)
  where
    (lAncester, lDepth) = _lcaDeepestLeaves left
    (rAncester, rDepth) = _lcaDeepestLeaves right

root1 :: Tree Int
root1 = fromString "[3, 5, 1, 6, 2, 0, 8, null, null, 7, 4]"

root2 :: Tree Int
root2 = fromString "[1]"

root3 :: Tree Int
root3 = fromString "[0, 1, 3, null, 2]"

input :: Tree Int -> IO ()
input = input1 "root"

output :: Tree Int -> IO ()
output = output1

main :: IO ()
main = do
  input root1
  output (lcaDeepestLeaves root1)
  input root2
  output (lcaDeepestLeaves root2)
  input root3
  output (lcaDeepestLeaves root3)