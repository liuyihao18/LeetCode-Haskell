{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.Heap qualified as H
import IO

mergeKLists :: [[Int]] -> [Int]
mergeKLists lists = merge heap []
  where
    heap = H.fromList $ map reverse lists

merge :: H.MaxHeap [Int] -> [Int] -> [Int]
merge heap list = case top of
  (Just ([], newHeap)) -> merge newHeap list
  (Just (x : xs, newHeap)) -> merge (H.insert xs newHeap) (x : list)
  Nothing -> list
  where
    top = H.view heap

lists1 :: [[Int]]
lists1 = [[1, 4, 5], [], [1, 3, 4], [2, 6], [], []]

lists2 :: [[Int]]
lists2 = []

lists3 :: [[Int]]
lists3 = [[]]

input :: [[Int]] -> IO ()
input = input1 "lists"

output :: [Int] -> IO ()
output = output1

main :: IO ()
main = do
  input lists1
  output (mergeKLists lists1)
  input lists2
  output (mergeKLists lists2)
  input lists3
  output (mergeKLists lists3)