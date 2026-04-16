{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.Vector qualified as V
import IO

closestTarget :: [String] -> String -> Int -> Int
closestTarget wordList = check 0 _words
  where
    _words = V.fromList wordList

check :: Int -> V.Vector String -> String -> Int -> Int
check i candidates target startIndex
  | i >= n = -1
  | candidates V.! forwardIndex == target = i
  | candidates V.! backwardIndex == target = i
  | otherwise = check (i + 1) candidates target startIndex
  where
    n = length candidates
    forwardIndex = (startIndex + i) `mod` n
    backwardIndex = (startIndex - i) `mod` n

words1 :: [String]
words1 = ["hello", "i", "am", "leetcode", "hello"]

target1 :: String
target1 = "hello"

startIndex1 :: Int
startIndex1 = 1

words2 :: [String]
words2 = ["a", "b", "leetcode"]

target2 :: String
target2 = "leetcode"

startIndex2 :: Int
startIndex2 = 0

words3 :: [String]
words3 = ["i", "eat", "leetcode"]

target3 :: String
target3 = "ate"

startIndex3 :: Int
startIndex3 = 0

input :: [String] -> String -> Int -> IO ()
input = input3 "words" "target" "startIndex"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  input words1 target1 startIndex1
  output (closestTarget words1 target1 startIndex1)
  input words2 target2 startIndex2
  output (closestTarget words2 target2 startIndex2)
  input words3 target3 startIndex3
  output (closestTarget words3 target3 startIndex3)
