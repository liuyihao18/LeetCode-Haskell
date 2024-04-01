{-# OPTIONS_GHC -Wall #-}

module Solution where

import IO

finalString :: String -> String
finalString str = _finalString str []

_finalString :: String -> String -> String
_finalString [] = reverse
_finalString ('i' : cs) = _finalString cs . reverse
_finalString (c : cs) = _finalString cs . (:) c

s1 :: String
s1 = "string"

s2 :: String
s2 = "poiinter"

input :: String -> IO ()
input = input1 "s"

output :: String -> IO ()
output = output1

main :: IO ()
main = do
  input s1
  output (finalString s1)
  input s2
  output (finalString s2)
