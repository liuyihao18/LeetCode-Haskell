{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.Char (isSpace)
import IO
import Tree
import Util

serialize :: Tree Int -> String
serialize Empty = ""
serialize (TreeNode val left right) = show val ++ " " ++ serialize left ++ " " ++ serialize right

deserialize :: String -> Tree Int
deserialize d = _deserialize nd
  where
    nd = map read $ wordsWhen isSpace d

_deserialize :: [Int] -> Tree Int
_deserialize [] = Empty
_deserialize (x : xs) = TreeNode x left right
  where
    (ls, rs) = break (> x) xs
    left = _deserialize ls
    right = _deserialize rs

root1 :: Tree Int
root1 = fromString "[2, 1, 3]"

root2 :: Tree Int
root2 = fromString ""

root3 :: Tree Int
root3 = fromString "[4,2,5,1,3,null,6]"

input :: Tree Int -> IO ()
input = input1 "root"

output :: Tree Int -> IO ()
output = output1

main :: IO ()
main = do
  input root1
  output (deserialize . serialize $ root1)
  input root2
  output (deserialize . serialize $ root2)
  input root3
  output (deserialize . serialize $ root3)
