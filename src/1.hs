{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.List
import Data.Map qualified as Map
import IO

data Result = Result Bool Int Int
  deriving (Show)

twoSum :: [Int] -> Int -> [Int]
twoSum nums target = toIndex (foldl' (check nums target) result index)
  where
    index = [(i, j) | i <- [0 .. length nums - 1], j <- [0 .. length nums - 1]]
    result = Result False (-1) (-1)

toIndex :: Result -> [Int]
toIndex (Result True i1 i2) = [i1, i2]
toIndex (Result False _ _) = [-1, -1]

check :: [Int] -> Int -> Result -> (Int, Int) -> Result
check _ _ res@(Result True _ _) _ = res
check nums target _ (i, j)
  | i /= j && nums !! i + nums !! j == target = Result True i j
  | otherwise = Result False (-1) (-1)

twoSum2 :: [Int] -> Int -> [Int]
twoSum2 nums target = toIndex2 (foldl' (check2 nums target) result index)
  where
    index = [0 .. length nums - 1]
    result = Result2 False Map.empty (-1) (-1)

data Result2 = Result2 Bool (Map.Map Int Int) Int Int
  deriving (Show)

toIndex2 :: Result2 -> [Int]
toIndex2 (Result2 True _ i1 i2) = [i1, i2]
toIndex2 (Result2 False _ _ _) = [-1, -1]

check2 :: [Int] -> Int -> Result2 -> Int -> Result2
check2 _ _ res@(Result2 True _ _ _) _ = res
check2 nums target (Result2 _ m _ _) i =
  case v of
    (Just j) -> Result2 True m i j
    Nothing -> Result2 False (Map.insert (nums !! i) i m) (-1) (-1)
  where
    v = Map.lookup (target - nums !! i) m

nums1 :: [Int]
nums1 = [2, 7, 11, 15]

target1 :: Int
target1 = 9

nums2 :: [Int]
nums2 = [3, 2, 4]

target2 :: Int
target2 = 6

nums3 :: [Int]
nums3 = [3, 3]

target3 :: Int
target3 = 6

input :: [Int] -> Int -> IO ()
input = input2 "nums" "target"

output :: [Int] -> IO ()
output = output1

main :: IO ()
main = do
  putStrLn "-------- Method 1 --------"
  input nums1 target1
  output (twoSum nums1 target1)
  input nums2 target2
  output (twoSum nums2 target2)
  input nums3 target3
  output (twoSum nums3 target3)
  putStrLn "-------- Method 2 --------"
  input nums1 target1
  output (twoSum2 nums1 target1)
  input nums2 target2
  output (twoSum2 nums2 target2)
  input nums3 target3
  output (twoSum2 nums3 target3)
