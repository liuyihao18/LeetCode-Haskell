{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.List
import Data.Set qualified as Set
import IO

flipgame :: [Int] -> [Int] -> Int
flipgame fronts backs = minimum (filter (`Set.notMember` same) fronts) `min` minimum (filter (`Set.notMember` same) backs)
  where
    same = foldl' findSame Set.empty (zip fronts backs)

findSame :: Set.Set Int -> (Int, Int) -> Set.Set Int
findSame s (front, back)
  | front == back = Set.insert front s
  | otherwise = s

fronts1 :: [Int]
fronts1 = [1, 2, 4, 4, 7]

backs1 :: [Int]
backs1 = [1, 3, 4, 1, 3]

input :: [Int] -> [Int] -> IO ()
input = input2 "fronts" "backs"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  input fronts1 backs1
  output (flipgame fronts1 backs1)
