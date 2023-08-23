{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.List
import Data.Set qualified as S
import IO

numJewelsInStones :: String -> String -> Int
numJewelsInStones jewels = foldl' (count s) 0
  where
    s = S.fromList jewels

count :: S.Set Char -> Int -> Char -> Int
count js cnt s
  | S.member s js = cnt + 1
  | otherwise = cnt

jewels1 :: String
jewels1 = "aA"

stones1 :: String
stones1 = "aAAbbbb"

jewels2 :: String
jewels2 = "z"

stones2 :: String
stones2 = "ZZ"

input :: String -> String -> IO ()
input = input2 "jewels" "stones"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  input jewels1 stones1
  output (numJewelsInStones jewels1 stones1)
  input jewels2 stones2
  output (numJewelsInStones jewels2 stones2)
