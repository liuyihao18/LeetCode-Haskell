{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.Array
import Data.List
import IO

queensAttacktheKing :: [[Int]] -> [Int] -> [[Int]]
queensAttacktheKing queens king = map (\(x, y) -> [x, y]) $ filter (/= (-1, -1)) $ map (search chess kingPosition) directions
  where
    queenPositions = map (\queen -> (head queen, last queen)) queens
    kingPosition = (head king, last king)
    chess = array ((0, 0), (7, 7)) $ map (,True) queenPositions ++ map (,False) ([(i, j) | i <- [0 .. 7], j <- [0 .. 7]] \\ queenPositions)
    directions = [(-1, 0), (1, 0), (0, -1), (0, 1), (-1, -1), (-1, 1), (1, -1), (1, 1)]

search :: Array (Int, Int) Bool -> (Int, Int) -> (Int, Int) -> (Int, Int)
search chess (x, y) (dx, dy) = case res of
  [] -> (-1, -1)
  (q : _) -> q
  where
    candidate = takeWhile (\(nx, ny) -> nx >= 0 && nx < 8 && ny >= 0 && ny < 8) $ map (\i -> (x + dx * i, y + dy * i)) [1 ..]
    res = take 1 $ dropWhile (not . (chess !)) candidate

queens1 :: [[Int]]
queens1 = [[0, 1], [1, 0], [4, 0], [0, 4], [3, 3], [2, 4]]

king1 :: [Int]
king1 = [0, 0]

queens2 :: [[Int]]
queens2 = [[0, 0], [1, 1], [2, 2], [3, 4], [3, 5], [4, 4], [4, 5]]

king2 :: [Int]
king2 = [3, 3]

queens3 :: [[Int]]
queens3 = [[5, 6], [7, 7], [2, 1], [0, 7], [1, 6], [5, 1], [3, 7], [0, 3], [4, 0], [1, 2], [6, 3], [5, 0], [0, 4], [2, 2], [1, 1], [6, 4], [5, 4], [0, 0], [2, 6], [4, 5], [5, 2], [1, 4], [7, 5], [2, 3], [0, 5], [4, 2], [1, 0], [2, 7], [0, 1], [4, 6], [6, 1], [0, 6], [4, 3], [1, 7]]

king3 :: [Int]
king3 = [3, 4]

input :: [[Int]] -> [Int] -> IO ()
input = input2 "queens" "king"

output :: [[Int]] -> IO ()
output = output1

main :: IO ()
main = do
  input queens1 king1
  output (queensAttacktheKing queens1 king1)
  input queens2 king2
  output (queensAttacktheKing queens2 king2)
  input queens3 king3
  output (queensAttacktheKing queens3 king3)
