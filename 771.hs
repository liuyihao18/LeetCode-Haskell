{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.List
import Data.Set qualified as Set

solute :: String -> String -> Integer
solute jewels = foldl' (count s) 0
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

main :: IO ()
main = do
  putStrLn ("Input:\tjewels = " ++ show jewels1 ++ ", " ++ "stones = " ++ show stones1)
  putStrLn ("Output:\t" ++ show (solute jewels1 stones1))
  putStrLn ("Input:\tjewels = " ++ show jewels2 ++ ", " ++ "stones = " ++ show stones2)
  putStrLn ("Output:\t" ++ show (solute jewels2 stones2))
