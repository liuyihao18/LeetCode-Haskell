{-# OPTIONS_GHC -Wall #-}

module IO where

input1 :: (Show a) => String -> a -> IO ()
input1 name1 x1 = putStrLn ("Input:\t" ++ name1 ++ " = " ++ show x1)

input2 :: (Show a, Show b) => String -> String -> a -> b -> IO ()
input2 name1 name2 x1 x2 = putStrLn ("Input:\t" ++ name1 ++ " = " ++ show x1 ++ ", " ++ name2 ++ " = " ++ show x2)

input3 :: (Show a, Show b, Show c) => String -> String -> String -> a -> b -> c -> IO ()
input3 name1 name2 name3 x1 x2 x3 = putStrLn ("Input:\t" ++ name1 ++ " = " ++ show x1 ++ ", " ++ name2 ++ " = " ++ show x2 ++ ", " ++ name3 ++ " = " ++ show x3)

output1 :: (Show a) => a -> IO ()
output1 y = putStrLn ("Output:\t" ++ show y)
