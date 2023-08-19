{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.List
import Data.Set qualified as Set
import IO

numJewelsInStones :: String -> String -> Integer
numJewelsInStones jewels = foldl' (count s) 0
  where
    s = Set.fromList jewels

count :: Set.Set Char -> Integer -> Char -> Integer
count js cnt s
  | Set.member s js = cnt + 1
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

output :: Integer -> IO ()
output = output1

main :: IO ()
main = do
  input jewels1 stones1
  output (numJewelsInStones jewels1 stones1)
  input jewels2 stones2
  output (numJewelsInStones jewels2 stones2)