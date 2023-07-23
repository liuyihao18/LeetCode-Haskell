{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.List

type Status = Bool

data Cashier = Cashier Status Integer Integer Integer
  deriving (Show)

solute :: [Integer] -> Bool
solute bills = result
  where
    (Cashier result _ _ _) = foldl' buy (Cashier True 0 0 0) bills

buy :: Cashier -> Integer -> Cashier
buy (Cashier False a b c) _ = Cashier False a b c
buy (Cashier True a b c) 5 = Cashier True (a + 1) b c
buy (Cashier True a b c) 10
  | a > 0 = Cashier True (a - 1) (b + 1) c
  | otherwise = Cashier False a b c
buy (Cashier True a b c) 20
  | b > 0 && a > 0 = Cashier True (a - 1) (b - 1) (c + 1)
  | a > 2 = Cashier True (a - 3) b (c + 1)
  | otherwise = Cashier False a b c
buy (Cashier _ a b c) _ = Cashier False a b c

bills1 :: [Integer]
bills1 = [5, 5, 5, 10, 20]

bills2 :: [Integer]
bills2 = [5, 5, 10, 10, 20]

main :: IO ()
main = do
  putStrLn ("Input:\tbills = " ++ show bills1)
  putStrLn ("Output:\t" ++ show (solute bills1))
  putStrLn ("Input:\tbills = " ++ show bills2)
  putStrLn ("Output:\t" ++ show (solute bills2))
