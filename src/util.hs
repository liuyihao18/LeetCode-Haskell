{-# OPTIONS_GHC -Wall #-}

module Util where

strip :: String -> String
strip = takeWhile (/= ' ') . dropWhile (== ' ')

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

unWordsWith :: Char -> [String] -> String
unWordsWith _ [] = ""
unWordsWith _ [w] = w
unWordsWith d (w : ws) = w ++ [d] ++ unWordsWith d ws

removeBrackets :: String -> String
removeBrackets = takeWhile (/= ']') . dropWhile (== '[')

addBrackets :: String -> String
addBrackets str = "[" ++ str ++ "]"

takeRange :: Int -> Int -> [a] -> [a]
takeRange l r = take (r - l) . drop l

takeNAtP :: Int -> Int -> [a] -> [a]
takeNAtP p n = take n . drop p

bsearch :: [Int] -> Int -> Bool
bsearch [] _ = False
bsearch list target
  | target > element = bsearch list2 target
  | target < element = bsearch list1 target
  | otherwise = True
  where
    middle = length list `div` 2
    element = list !! middle
    list1 = take middle list
    list2 = drop (middle + 1) list

lowerBound :: [Int] -> Int -> Int
lowerBound list target = _lowerBound list target 0 n
  where
    n = length list

_lowerBound :: [Int] -> Int -> Int -> Int -> Int
_lowerBound [] _ _ _ = -1
_lowerBound list target left right
  | left >= right = left
  | target > element = _lowerBound list target (middle + 1) right
  | otherwise = _lowerBound list target left middle
  where
    middle = (left + right) `div` 2
    element = list !! middle

upperBound :: [Int] -> Int -> Int
upperBound list target = _upperBound list target 0 n
  where
    n = length list

_upperBound :: [Int] -> Int -> Int -> Int -> Int
_upperBound [] _ _ _ = -1
_upperBound list target left right
  | left >= right = left
  | target >= element = _upperBound list target (middle + 1) right
  | otherwise = _upperBound list target left middle
  where
    middle = (left + right) `div` 2
    element = list !! middle
