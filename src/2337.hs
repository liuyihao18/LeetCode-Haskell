{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO

canChange :: String -> String -> Bool
canChange = _canChange 0 0

_canChange :: Int -> Int -> String -> String -> Bool
_canChange i j start target
  | i >= n || j >= n = True
  | sc == '_' = _canChange (i + 1) j start target
  | tc == '_' = _canChange i (j + 1) start target
  | sc /= tc = False
  | sc == 'L' && i < j = False
  | sc == 'R' && i > j = False
  | otherwise = _canChange (i + 1) (j + 1) start target
  where
    n = length start
    sc = start !! i
    tc = target !! j

start1 :: String
start1 = "_L__R__R_"

target1 :: String
target1 = "L______RR"

start2 :: String
start2 = "R_L_"

target2 :: String
target2 = "__LR"

start3 :: String
start3 = "_R"

target3 :: String
target3 = "R_"

start4 :: String
start4 = "__R_R_R_L"

target4 :: String
target4 = "____RRR_L"

input :: String -> String -> IO ()
input = input2 "start" "target"

output :: Bool -> IO ()
output = output1

main :: IO ()
main = do
  input start1 target1
  output (canChange start1 target1)
  input start2 target2
  output (canChange start2 target2)
  input start3 target3
  output (canChange start3 target3)
  input start4 target4
  output (canChange start4 target4)
