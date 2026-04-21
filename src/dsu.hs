{-# OPTIONS_GHC -Wall #-}

module Dsu where

import Data.Vector qualified as V

newtype Dsu = Dsu (V.Vector Int)
  deriving (Show, Eq)

create :: Int -> Dsu
create n = Dsu . V.fromList $ [0 .. n - 1]

find :: Dsu -> Int -> (Dsu, Int)
find dsu@(Dsu parents) x
  | parents V.! x == x = (dsu, x)
  | otherwise = (Dsu $ newParents V.// [(x, newParent)], newParent)
  where
    (Dsu newParents, newParent) = find dsu (parents V.! x)

unite :: Dsu -> Int -> Int -> Dsu
unite dsu x y
  | xRoot == yRoot = dsu
  | otherwise = Dsu $ parents V.// [(xRoot, yRoot)]
  where
    (dsu1, xRoot) = find dsu x
    (Dsu parents, yRoot) = find dsu1 y
