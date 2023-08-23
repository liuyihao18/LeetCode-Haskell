{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.List
import Data.Set qualified as S
import IO

flipgame :: [Int] -> [Int] -> Int
flipgame fronts backs = minimum (filter (`S.notMember` same) fronts) `min` minimum (filter (`S.notMember` same) backs)
  where
    same = foldl' findSame S.empty (zip fronts backs)

findSame :: S.Set Int -> (Int, Int) -> S.Set Int
findSame s (front, back)
  | front == back = S.insert front s
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
