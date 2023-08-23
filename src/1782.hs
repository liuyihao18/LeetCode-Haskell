{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.List
import Data.Map qualified as M
import Data.Vector qualified as V
import IO
import Util

countPairs :: Int -> [[Int]] -> [Int] -> [Int]
countPairs n edges = map (\bound -> getCountedPairs1 n bound sortedDegree - getCountedPairs2 n bound degree cnt)
  where
    degree = getDegree n edges
    cnt = getCnt n edges
    sortedDegree = V.fromList $ sort $ V.toList degree

getDegree :: Int -> [[Int]] -> V.Vector Int
getDegree n =
  foldr
    ( \edge degree ->
        let (u, v) = (head edge - 1, last edge - 1)
         in V.update_ degree (V.fromList [u, v]) (V.fromList [degree V.! u + 1, degree V.! v + 1])
    )
    degree0
  where
    degree0 = V.generate n (const 0)

getCnt :: Int -> [[Int]] -> M.Map Int Int
getCnt n =
  foldr
    ( \edge cnt ->
        let (u, v) = (head edge - 1, last edge - 1)
            k = u `max` v * n + u `min` v
         in if M.member k cnt
              then M.update (\a -> Just (a + 1)) k cnt
              else M.insert k 1 cnt
    )
    cnt0
  where
    cnt0 = M.empty

getCountedPairs1 :: Int -> Int -> V.Vector Int -> Int
getCountedPairs1 n bound degree =
  foldr (\i e -> let j = upperBound' degree (bound - degree V.! i) (i + 1) n in e + n - j) 0 [0 .. n - 1]

getCountedPairs2 :: Int -> Int -> V.Vector Int -> M.Map Int Int -> Int
getCountedPairs2 n bound degree cnt =
  foldr (\(val, freq) e -> let (u, v) = (val `div` n, val `mod` n) in if degree V.! u + degree V.! v > bound && degree V.! u + degree V.! v - freq <= bound then e + 1 else e) 0 list
  where
    list = M.toList cnt

n1 :: Int
n1 = 4

edges1 :: [[Int]]
edges1 = [[1, 2], [2, 4], [1, 3], [2, 3], [2, 1]]

queries1 :: [Int]
queries1 = [2, 3]

n2 :: Int
n2 = 5

edges2 :: [[Int]]
edges2 = [[1, 5], [1, 5], [3, 4], [2, 5], [1, 3], [5, 1], [2, 3], [2, 5]]

queries2 :: [Int]
queries2 = [1, 2, 3, 4, 5]

input :: Int -> [[Int]] -> [Int] -> IO ()
input = input3 "n" "edges" "queries"

output :: [Int] -> IO ()
output = output1

main :: IO ()
main = do
  input n1 edges1 queries1
  output (countPairs n1 edges1 queries1)
  input n2 edges2 queries2
  output (countPairs n2 edges2 queries2)
