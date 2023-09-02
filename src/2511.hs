{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.List
import IO

data Status = Status {pre :: Int, forts :: Int, maxForts :: Int}

captureForts1 :: [Int] -> Int
captureForts1 =
  maxForts
    . foldl'
      ( \(Status pre fs mf) f ->
          if f == 0
            then Status pre (fs + 1) mf
            else
              if pre + f == 0
                then Status f 0 (mf `max` fs)
                else Status f 0 mf
      )
      initial
  where
    initial = Status (-2) 0 0

captureForts2 :: [Int] -> Int
captureForts2 [] = 0
captureForts2 (pre : forts)
  | pre == 0 = captureForts2 forts'
  | otherwise = case forts' of
      [] -> 0
      (post : _) -> if pre + post == 0 then cnt `max` captureForts2 forts' else captureForts2 forts'
  where
    forts' = dropWhile (== 0) forts
    cnt = length $ takeWhile (== 0) forts

forts1 :: [Int]
forts1 = [1, 0, 0, -1, 0, 0, 0, 0, 1]

forts2 :: [Int]
forts2 = [0, 0, 1, -1]

input :: [Int] -> IO ()
input = input1 "forts"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  putStrLn "-------- Method 1 --------"
  input forts1
  output (captureForts1 forts1)
  input forts2
  output (captureForts1 forts2)
  putStrLn "-------- Method 1 --------"
  input forts1
  output (captureForts2 forts1)
  input forts2
  output (captureForts2 forts2)
