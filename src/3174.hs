{-# OPTIONS_GHC -Wall #-}

module Solution where

import Data.Char
import Data.List
import Data.Maybe
import IO
import Stack

clearDigits :: String -> String
clearDigits = reverse . toString . foldl' (\st c -> if isDigit c then snd . fromJust . Stack.view $ st else Stack.insert c st) Stack.empty

toString :: Stack Char -> String
toString st = case Stack.view st of
  Nothing -> ""
  Just (c, st') -> c : toString st'

s1 :: String
s1 = "abc"

s2 :: String
s2 = "cb34"

input :: String -> IO ()
input = input1 "s"

output :: String -> IO ()
output = output1

main :: IO ()
main = do
  input s1
  output (clearDigits s1)
  input s2
  output (clearDigits s2)
