{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.Set qualified as S
import IO

type Players = S.Set Int

data GameStatus = GameStatus
  { n :: Int,
    k :: Int,
    nowPlayer :: Int,
    epoch :: Int,
    winners :: Players
  }

circularGameLosers :: Int -> Int -> [Int]
circularGameLosers n k = map (+ 1) $ S.toList $ getLosers (winners gameOver) n
  where
    gameStart = GameStatus n k 0 1 S.empty
    gameOver = simulateGame gameStart

simulateGame :: GameStatus -> GameStatus
simulateGame game@(GameStatus n k nowPlayer epoch winners)
  | S.member nowPlayer winners = game
  | otherwise = simulateGame $ GameStatus n k ((nowPlayer + epoch * k) `mod` n) (epoch + 1) (S.insert nowPlayer winners)

getLosers :: Players -> Int -> Players
getLosers winners n = S.fromList [0 .. n - 1] S.\\ winners

input :: Int -> Int -> IO ()
input = input2 "n" "k"

output :: [Int] -> IO ()
output = output1

n1 :: Int
n1 = 5

k1 :: Int
k1 = 2

n2 :: Int
n2 = 4

k2 :: Int
k2 = 4

main :: IO ()
main = do
  input n1 k1
  output (circularGameLosers n1 k1)
  input n2 k2
  output (circularGameLosers n2 k2)
