{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Solution where

import Data.List
import Data.Map qualified as Map
import Data.Maybe
import IO

data Person = Person {isLiving :: Bool, children :: [String]}
  deriving (Show)

type Family = Map.Map String Person

data Data = Data
  { family :: Family,
    kingName :: String,
    result :: [[String]]
  }
  deriving (Show)

emptyData :: Data
emptyData = Data Map.empty "" []

throneInheritance :: [String] -> [[String]] -> [[String]]
throneInheritance cmds params = result $ foldl' _throneInheritance emptyData $ zip cmds params

_throneInheritance :: Data -> (String, [String]) -> Data
_throneInheritance (Data family kingName result) (cmd, param)
  | cmd == "ThroneInheritance" = Data (Map.insert name (Person True []) family) name (result ++ [[]])
  | cmd == "birth" = Data (Map.insert child (Person True []) (Map.update (\(Person isLiving children) -> Just (Person isLiving (children ++ [last param]))) parent family)) kingName (result ++ [[]])
  | cmd == "death" = Data (Map.update (\(Person _ children) -> Just (Person False children)) name family) kingName (result ++ [[]])
  | otherwise = Data family kingName (result ++ [getSuccessors kingName family []])
  where
    name = head param
    parent = head param
    child = last param

getSuccessors :: String -> Family -> [String] -> [String]
getSuccessors name family res
  | isLiving person = res ++ [name] ++ concatMap (\child -> getSuccessors child family []) (children person)
  | otherwise = res ++ concatMap (\child -> getSuccessors child family []) (children person)
  where
    person = fromMaybe (Person False []) (Map.lookup name family)

inputs1 :: [String]
inputs1 = ["ThroneInheritance", "birth", "birth", "birth", "birth", "birth", "birth", "getInheritanceOrder", "death", "getInheritanceOrder"]

inputs2 :: [[String]]
inputs2 = [["king"], ["king", "andy"], ["king", "bob"], ["king", "catherine"], ["andy", "matthew"], ["bob", "alex"], ["bob", "asha"], [], ["bob"], []]

input :: [String] -> [[String]] -> IO ()
input = input2 "inputs1" "inputs2"

output :: [[String]] -> IO ()
output = output1

main :: IO ()
main = do
  input inputs1 inputs2
  output (throneInheritance inputs1 inputs2)