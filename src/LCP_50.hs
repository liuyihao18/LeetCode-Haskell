{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.List
import Data.Vector qualified as V
import IO

giveGem :: [Int] -> [[Int]] -> Int
giveGem gem operations = maximum gem' - minimum gem'
  where
    gem' =
      foldl'
        ( \g op ->
            let x = head op
                y = last op
                xHave = g V.! x
                yHave = g V.! y
                g' = V.update_ g (V.singleton y) (V.singleton (yHave + xHave `div` 2))
                g'' = V.update_ g' (V.singleton x) (V.singleton (xHave - xHave `div` 2))
             in g''
        )
        (V.fromList gem)
        operations

gem1 :: [Int]
gem1 = [3, 1, 2]

operations1 :: [[Int]]
operations1 = [[0, 2], [2, 1], [2, 0]]

gem2 :: [Int]
gem2 = [100, 0, 50, 100]

operations2 :: [[Int]]
operations2 = [[0, 2], [0, 1], [3, 0], [3, 0]]

gem3 :: [Int]
gem3 = [0, 0, 0, 0]

operations3 :: [[Int]]
operations3 = [[1, 2], [3, 1], [1, 2]]

input :: [Int] -> [[Int]] -> IO ()
input = input2 "gems" "operations"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  input gem1 operations1
  output (giveGem gem1 operations1)
  input gem2 operations2
  output (giveGem gem2 operations2)
  input gem3 operations3
  output (giveGem gem3 operations3)
