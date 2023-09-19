{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO
import Tree

rob :: Tree Int -> Int
rob root = w `max` wo
  where
    (w, wo) = _rob root

_rob :: Tree Int -> (Int, Int)
_rob Empty = (0, 0)
_rob (TreeNode v l r) = (v + lwo + rwo, lw `max` lwo + rw `max` rwo)
  where
    (lw, lwo) = _rob l
    (rw, rwo) = _rob r

root1 :: Tree Int
root1 = fromString "[3, 2, 3, null, 3, null, 1]"

root2 :: Tree Int
root2 = fromString "[3, 4, 5, 1, 3, null, 1]"

input :: Tree Int -> IO ()
input = input1 "root"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  input root1
  output (rob root1)
  input root2
  output (rob root2)
