{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.Map qualified as M
import Data.Maybe
import IO

data Data = Data {m :: M.Map Int Int, index :: Int, minDistance :: Int}

minMirrorPairDistance :: [Int] -> Int
minMirrorPairDistance nums = if r == maxBound then (-1) else r
  where
    d = Data M.empty 0 maxBound
    r = _minMirrorPairDistance d nums

_minMirrorPairDistance :: Data -> [Int] -> Int
_minMirrorPairDistance (Data _ _ minDistance) [] = minDistance
_minMirrorPairDistance (Data m index minDistance) (num : nums)
  | M.member num m = _minMirrorPairDistance (Data newM newIndex newMinDistance) nums
  | otherwise = _minMirrorPairDistance (Data newM newIndex minDistance) nums
  where
    newM = M.insert (flipInt num) index m
    lastIndex = fromJust (M.lookup num m)
    newDistance = index - lastIndex
    newMinDistance = minDistance `min` newDistance
    newIndex = index + 1

flipInt :: Int -> Int
flipInt num = _flipInt num 0

_flipInt :: Int -> Int -> Int
_flipInt x y
  | x <= 0 = y
  | otherwise = _flipInt (x `div` 10) (y * 10 + x `mod` 10)

nums1 :: [Int]
nums1 = [12, 21, 45, 33, 54]

nums2 :: [Int]
nums2 = [120, 21]

nums3 :: [Int]
nums3 = [21, 120]

input :: [Int] -> IO ()
input = input1 "nums"

output :: Int -> IO ()
output = output1

main :: IO ()
main = do
  input nums1
  output (minMirrorPairDistance nums1)
  input nums2
  output (minMirrorPairDistance nums2)
  input nums3
  output (minMirrorPairDistance nums3)
