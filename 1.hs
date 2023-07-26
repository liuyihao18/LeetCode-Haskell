{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.List
import Data.Map qualified as Map
import IO

data Result = Result Bool Int Int
  deriving (Show)

solute :: [Integer] -> Integer -> [Integer]
solute nums target = toIndex (foldl' (check nums target) result index)
  where
    index = [(i, j) | i <- [0 .. length nums - 1], j <- [0 .. length nums - 1]]
    result = Result False (-1) (-1)

toIndex :: Result -> [Integer]
toIndex (Result True i1 i2) = [toInteger i1, toInteger i2]
toIndex (Result False _ _) = [-1, -1]

check :: [Integer] -> Integer -> Result -> (Int, Int) -> Result
check _ _ res@(Result True _ _) _ = res
check nums target _ (i, j)
  | i /= j && nums !! i + nums !! j == target = Result True i j
  | otherwise = Result False (-1) (-1)

solute2 :: [Integer] -> Integer -> [Integer]
solute2 nums target = toIndex2 (foldl' (check2 nums target) result index)
  where
    index = [0 .. length nums - 1]
    result = Result2 False Map.empty (-1) (-1)

data Result2 = Result2 Bool (Map.Map Integer Int) Int Int
  deriving (Show)

toIndex2 :: Result2 -> [Integer]
toIndex2 (Result2 True _ i1 i2) = [toInteger i1, toInteger i2]
toIndex2 (Result2 False _ _ _) = [-1, -1]

check2 :: [Integer] -> Integer -> Result2 -> Int -> Result2
check2 _ _ res@(Result2 True _ _ _) _ = res
check2 nums target (Result2 _ m _ _) i =
  case v of
    (Just j) -> Result2 True m i j
    Nothing -> Result2 False (Map.insert (nums !! i) i m) (-1) (-1)
  where
    v = Map.lookup (target - nums !! i) m

nums1 :: [Integer]
nums1 = [2, 7, 11, 15]

target1 :: Integer
target1 = 9

nums2 :: [Integer]
nums2 = [3, 2, 4]

target2 :: Integer
target2 = 6

nums3 :: [Integer]
nums3 = [3, 3]

target3 :: Integer
target3 = 6

input :: [Integer] -> Integer -> IO ()
input = input2 "nums" "target"

output :: [Integer] -> IO ()
output = output1

main :: IO ()
main = do
  putStrLn "-------- Method 1 --------"
  input nums1 target1
  output (solute nums1 target1)
  input nums2 target2
  output (solute nums2 target2)
  input nums3 target3
  output (solute nums3 target3)
  putStrLn "-------- Method 2 --------"
  input nums1 target1
  output (solute2 nums1 target1)
  input nums2 target2
  output (solute2 nums2 target2)
  input nums3 target3
  output (solute2 nums3 target3)
