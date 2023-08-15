{-# OPTIONS_GHC -Wall #-}

module Tree where

import Data.List
import Util

data Tree a
  = TreeNode {val :: a, left :: Tree a, right :: Tree a}
  | Empty

instance Show a => Show (Tree a) where
  show = toString

fromString :: Read a => String -> Tree a
fromString str = fromList $ map strip $ wordsWhen (== ',') $ removeBrackets str

fromList :: Read a => [String] -> Tree a
fromList list = _fromList list 0

-- Index
_fromList :: Read a => [String] -> Int -> Tree a
_fromList list index
  | index < 0 || index >= n = Empty
  | otherwise = let val = (list !! index) in if val == "null" then Empty else TreeNode (read val) (_fromList list (2 * index + 1)) (_fromList list (2 * (index + 1)))
  where
    n = length list

toString :: Show a => Tree a -> String
toString = addBrackets . unWordsWith ',' . toList

toList :: Show a => Tree a -> [String]
toList = dropWhileEnd (== "null") . _toList . singleton

-- BFS
_toList :: Show a => [Tree a] -> [String]
_toList [] = []
_toList (Empty : ts) = "null" : _toList ts
_toList (TreeNode val left right : ts) = show val : _toList (ts ++ [left, right])
