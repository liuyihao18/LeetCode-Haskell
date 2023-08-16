{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.Set qualified as Set
import IO

type Players = Set.Set Integer

data GameStatus = GameStatus
  { n :: Integer,
    k :: Integer,
    nowPlayer :: Integer,
    epoch :: Integer,
    winners :: Players
  }

circularGameLosers :: Integer -> Integer -> [Integer]
circularGameLosers n k = map (+ 1) $ Set.toList $ getLosers (winners gameOver) n
  where
    gameStart = GameStatus n k 0 1 Set.empty
    gameOver = simulateGame gameStart

simulateGame :: GameStatus -> GameStatus
simulateGame game@(GameStatus n k nowPlayer epoch winners)
  | Set.member nowPlayer winners = game
  | otherwise = simulateGame $ GameStatus n k ((nowPlayer + epoch * k) `mod` n) (epoch + 1) (Set.insert nowPlayer winners)

getLosers :: Players -> Integer -> Players
getLosers winners n = Set.fromList [0 .. n - 1] Set.\\ winners

input :: Integer -> Integer -> IO ()
input = input2 "n" "k"

output :: [Integer] -> IO ()
output = output1

n1 :: Integer
n1 = 5

k1 :: Integer
k1 = 2

n2 :: Integer
n2 = 4

k2 :: Integer
k2 = 4

main :: IO ()
main = do
  input n1 k1
  output (circularGameLosers n1 k1)
  input n2 k2
  output (circularGameLosers n2 k2)
