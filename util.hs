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
