{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.Map qualified as M
import IO

checkValidGrid :: [[Int]] -> Bool
checkValidGrid [] = False
checkValidGrid [[]] = False
checkValidGrid ((x : _) : _)
  | x /= 0 = False
checkValidGrid grid =
  all
    (\(dx, dy) -> abs dx * abs dy == 2)
    $ diff2
    $ map snd
    $ M.toList
    $ foldr (\pos@(i, j) -> M.insert (grid !! i !! j) pos) M.empty [(i, j) | i <- [0 .. n - 1], j <- [0 .. n - 1]]
  where
    n = length grid

diff2 :: [(Int, Int)] -> [(Int, Int)]
diff2 [] = []
diff2 [_] = []
diff2 ((x1, y1) : r@((x2, y2) : _)) = (x1 - x2, y1 - y2) : diff2 r

grid1 :: [[Int]]
grid1 = [[0, 11, 16, 5, 20], [17, 4, 19, 10, 15], [12, 1, 8, 21, 6], [3, 18, 23, 14, 9], [24, 13, 2, 7, 22]]

grid2 :: [[Int]]
grid2 = [[0, 3, 6], [5, 8, 1], [2, 7, 4]]

grid3 :: [[Int]]
grid3 = [[24, 11, 22, 17, 4], [21, 16, 5, 12, 9], [6, 23, 10, 3, 18], [15, 20, 1, 8, 13], [0, 7, 14, 19, 2]]

input :: [[Int]] -> IO ()
input = input1 "grid"

output :: Bool -> IO ()
output = output1

main :: IO ()
main = do
  input grid1
  output (checkValidGrid grid1)
  input grid2
  output (checkValidGrid grid2)
  input grid3
  output (checkValidGrid grid3)
