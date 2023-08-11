{-# OPTIONS_GHC  #-}

module Solution where

import Data.Heap qualified as Heap
import Data.Maybe
import IO

halveArray :: [Integer] -> Integer
halveArray nums = divide2 s s heap 0
  where
    s = sum (map fromInteger nums)
    heap = Heap.fromList (map fromInteger nums)

divide2 ::
  Double -> -- Origin Sum
  Double -> -- Current Sum
  Heap.MaxHeap Double -> -- Priority Queue
  Integer -> -- Last Operation Count
  Integer -- New Operation Count
divide2 os cs heap cnt
  | cs * 2 <= os = cnt
  | otherwise = divide2 os (cs - res + res / 2) (Heap.insert (res / 2) newHeap) (cnt + 1)
  where
    (res, newHeap) = fromMaybe (0, Heap.empty) (Heap.view heap)

nums1 :: [Integer]
nums1 = [5, 19, 8, 1]

nums2 :: [Integer]
nums2 = [3, 8, 20]

nums3 :: [Integer]
nums3 = [1, 5, 1, 7, 1, 33, 123, 5, 6, 12, 3, 5, 1, 53, 6, 6, 5, 1, 2, 5, 6, 1, 3, 5, 46, 1, 56, 1, 23, 6, 34, 7, 8, 2, 8, 68, 68, 45, 235]

input :: [Integer] -> IO ()
input = input1 "nums"

output :: Integer -> IO ()
output = output1

main :: IO ()
main = do
  input nums1
  output (halveArray nums1)
  input nums2
  output (halveArray nums2)
  input nums3
  output (halveArray nums3)
