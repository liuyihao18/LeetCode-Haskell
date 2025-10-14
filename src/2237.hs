{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.List
import IO

removeAnagrams :: [String] -> [String]
removeAnagrams (word1 : remains@(word2 : _))
  | sort word1 == sort word2 = removeAnagrams remains
  | otherwise = word1 : removeAnagrams remains
removeAnagrams [word] = [word]
removeAnagrams [] = []

words1 :: [String]
words1 = ["abba", "baba", "bbaa", "cd", "cd"]

words2 :: [String]
words2 = ["a", "b", "c", "d", "e"]

input :: [String] -> IO ()
input = input1 "words"

output :: [String] -> IO ()
output = output1

main :: IO ()
main = do
  input words1
  output (removeAnagrams words1)
  input words2
  output (removeAnagrams words2)