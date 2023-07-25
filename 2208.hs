{-# OPTIONS_GHC  #-}

module Solution where

import Data.Heap qualified as Heap
import Data.Maybe

solute :: [Integer] -> Integer
solute nums = divide2 s s heap 0
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

main :: IO ()
main = do
  putStrLn ("Input:\tnums = " ++ show nums1)
  putStrLn ("Output:\t" ++ show (solute nums1))
  putStrLn ("Input:\tjnums = " ++ show nums2)
  putStrLn ("Output:\t" ++ show (solute nums2))
  putStrLn ("Input:\tjnums = " ++ show nums3)
  putStrLn ("Output:\t" ++ show (solute nums3))
