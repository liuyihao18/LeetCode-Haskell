{-# OPTIONS_GHC -Wall #-}

module Solution where

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = n `mod` 10 : toDigits (n `div` 10)

addSign :: [Integer] -> [Integer]
addSign [] = []
addSign [x] = [x]
addSign (x : y : ys) = x : (-y) : addSign ys

solute :: Integer -> Integer
solute = sum . addSign . reverse . toDigits

n1 :: Integer
n1 = 521

n2 :: Integer
n2 = 111

n3 :: Integer
n3 = 886996

main :: IO ()
main = do
  putStrLn ("Input:\tn = " ++ show n1)
  putStrLn ("Output:\t" ++ show (solute n1))
  putStrLn ("Input:\tn = " ++ show n2)
  putStrLn ("Output:\t" ++ show (solute n2))
  putStrLn ("Input:\tn = " ++ show n3)
  putStrLn ("Output:\t" ++ show (solute n3))