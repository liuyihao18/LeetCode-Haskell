{-# OPTIONS_GHC -Wall #-}

module Solution where

solute :: [Integer] -> [Integer] -> [Integer]
solute l1 l2 = carry 0 $ add l1 l2

add :: [Integer] -> [Integer] -> [Integer]
add xs [] = xs
add [] ys = ys
add (x : xs) (y : ys) = (x + y) : add xs ys

carry :: Integer -> [Integer] -> [Integer]
carry c []
  | c == 0 = []
  | otherwise = [1]
carry c (x : xs)
  | x + c >= 10 = x + c - 10 : carry 1 xs
  | otherwise = x + c : carry 0 xs

l11 :: [Integer]
l11 = [2, 4, 3]

l21 :: [Integer]
l21 = [5, 6, 4]

l12 :: [Integer]
l12 = [0]

l22 :: [Integer]
l22 = [0]

l13 :: [Integer]
l13 = [9, 9, 9, 9, 9, 9, 9]

l23 :: [Integer]
l23 = [9, 9, 9, 9]

main :: IO ()
main = do
  putStrLn ("Input:\tl1 = " ++ show l11 ++ ", " ++ "l1 = " ++ show l21)
  putStrLn ("Output:\t" ++ show (solute l11 l21))
  putStrLn ("Input:\tl1 = " ++ show l12 ++ ", " ++ "l1 = " ++ show l22)
  putStrLn ("Output:\t" ++ show (solute l12 l22))
  putStrLn ("Input:\tl1 = " ++ show l13 ++ ", " ++ "l1 = " ++ show l23)
  putStrLn ("Output:\t" ++ show (solute l13 l23))
