{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.Map qualified as M
import Data.Maybe
import IO
import Queue qualified as Q

canFinish :: Int -> [[Int]] -> Bool
canFinish numCourses prerequisites = numCourses == cnt
  where
    inDegree = calInDegree numCourses prerequisites
    edges = calEdges prerequisites
    queue = M.foldrWithKey (\k a q -> if a == 0 then Q.insert k q else q) Q.empty inDegree
    cnt = tSortNum inDegree edges queue 0

tSortNum :: M.Map Int Int -> M.Map Int [Int] -> Q.Queue Int -> Int -> Int
tSortNum inDegree edges queue num
  | Q.null queue = num
  | otherwise = tSortNum newInDegree edges queue'' (num + 1)
  where
    (u, queue') = fromJust (Q.view queue)
    vs = fromMaybe [] (M.lookup u edges)
    newInDegree = foldr (\v -> M.insertWith (+) v (-1)) inDegree vs
    queue'' = foldr (\v q -> if fromJust (M.lookup v newInDegree) == 0 then Q.insert v q else q) queue' vs

calInDegree :: Int -> [[Int]] -> M.Map Int Int
calInDegree numCourses =
  foldr
    ( \edge ->
        let v = last edge
         in M.adjust (+ 1) v
    )
    inDegree0
  where
    inDegree0 = foldr (`M.insert` 0) M.empty [0 .. (numCourses - 1)]

calEdges :: [[Int]] -> M.Map Int [Int]
calEdges =
  foldr
    ( \edge ->
        let u = head edge
            v = last edge
         in M.insertWith (++) u [v]
    )
    M.empty

numCourses1 :: Int
numCourses1 = 2

prerequisites1 :: [[Int]]
prerequisites1 = [[1, 0]]

numCourses2 :: Int
numCourses2 = 2

prerequisites2 :: [[Int]]
prerequisites2 = [[1, 0], [0, 1]]

numCourses3 :: Int
numCourses3 = 5

prerequisites3 :: [[Int]]
prerequisites3 = [[1, 0], [1, 3], [2, 4], [4, 3]]

input :: Int -> [[Int]] -> IO ()
input = input2 "numCourses" "prerequisites"

output :: Bool -> IO ()
output = output1

main :: IO ()
main = do
  input numCourses1 prerequisites1
  output (canFinish numCourses1 prerequisites1)
  input numCourses2 prerequisites2
  output (canFinish numCourses2 prerequisites2)
  input numCourses3 prerequisites3
  output (canFinish numCourses3 prerequisites3)
