{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.List
import Data.Map qualified as M
import Data.Maybe
import IO
import Util

-- index: the index in s
-- i: the index in indexes, sources and targets
findReplaceString :: String -> [Int] -> [String] -> [String] -> String
findReplaceString s indexes sources targets =
  foldl'
    ( \res index ->
        res
          ++ if index `M.member` m
            then
              let i = fromJust (M.lookup index m)
               in findReplaceSubString s index (sources !! i) (targets !! i)
            else [s !! index]
    )
    ""
    ([0 .. length s - 1] \\ ignoreIndexes)
  where
    n = length indexes
    m = foldr (\i -> M.insert (indexes !! i) i) M.empty [0 .. n - 1]
    ignoreIndexes = genIgnoreIndexes indexes sources

findReplaceSubString :: String -> Int -> String -> String -> String
findReplaceSubString s index source target
  | origin == source = target
  | otherwise = origin
  where
    n = length source
    origin = takeNAtP index n s

genIgnoreIndexes :: [Int] -> [String] -> [Int]
genIgnoreIndexes indexes sources =
  mconcat
    ( map
        (\i -> [indexes !! i + 1 .. indexes !! i + length (sources !! i) - 1])
        [0 .. n - 1]
    )
    \\ indexes
  where
    n = length indexes

s1 :: String
s1 = "abcd"

indexes1 :: [Int]
indexes1 = [0, 2]

sources1 :: [String]
sources1 = ["a", "cd"]

targets1 :: [String]
targets1 = ["eee", "ffff"]

input :: String -> [Int] -> [String] -> [String] -> IO ()
input = input4 "s" "indexes" "sources" "targets"

output :: String -> IO ()
output = output1

s2 :: String
s2 = "abcd"

indexes2 :: [Int]
indexes2 = [0, 2]

sources2 :: [String]
sources2 = ["ab", "ec"]

targets2 :: [String]
targets2 = ["eee", "ffff"]

s3 :: String
s3 = "vmokgggqzp"

indexes3 :: [Int]
indexes3 = [3, 5, 1]

sources3 :: [String]
sources3 = ["kg", "ggq", "mo"]

targets3 :: [String]
targets3 = ["s", "so", "bfr"]

s4 :: String
s4 = "abcde"

indexes4 :: [Int]
indexes4 = [2, 2]

sources4 :: [String]
sources4 = ["cdef", "bc"]

targets4 :: [String]
targets4 = ["f", "fe"]

main :: IO ()
main = do
  input s1 indexes1 sources1 targets1
  output (findReplaceString s1 indexes1 sources1 targets1)
  input s2 indexes2 sources2 targets2
  output (findReplaceString s2 indexes2 sources2 targets2)
  input s3 indexes3 sources3 targets3
  output (findReplaceString s3 indexes3 sources3 targets3)
  input s4 indexes4 sources4 targets4
  output (findReplaceString s4 indexes4 sources4 targets4)
