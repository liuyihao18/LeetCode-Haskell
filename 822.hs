{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.List
import Data.Set qualified as Set
import IO

solute :: [Integer] -> [Integer] -> Integer
solute fronts backs = minimum (filter (`Set.notMember` same) fronts) `min` minimum (filter (`Set.notMember` same) backs)
  where
    same = foldl' findSame Set.empty (zip fronts backs)

findSame :: Set.Set Integer -> (Integer, Integer) -> Set.Set Integer
findSame s (front, back)
  | front == back = Set.insert front s
  | otherwise = s

fronts1 :: [Integer]
fronts1 = [1, 2, 4, 4, 7]

backs1 :: [Integer]
backs1 = [1, 3, 4, 1, 3]

input :: [Integer] -> [Integer] -> IO ()
input = input2 "fronts" "backs"

output :: Integer -> IO ()
output = output1

main :: IO ()
main = do
  input fronts1 backs1
  output (solute fronts1 backs1)
