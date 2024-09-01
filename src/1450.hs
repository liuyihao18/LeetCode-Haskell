{-# OPTIONS_GHC -Wall #-}

module Solution where 

import IO

busyStudent :: [Int] -> [Int] -> Int -> Int
busyStudent startTime endTime queryTime = (length . filter (<= queryTime) $ startTime) `min` (length . filter (>= queryTime) $ endTime)

startTime1 :: [Int]
startTime1 = [1,2,3]

endTime1 :: [Int]
endTime1 = [3,2,7]

queryTime1 :: Int
queryTime1 = 4

startTime2 :: [Int]
startTime2 = [4]

endTime2 :: [Int]
endTime2 = [4]

queryTime2 :: Int
queryTime2 = 4

startTime3 :: [Int]
startTime3 = [4]

endTime3 :: [Int]
endTime3 = [4]

queryTime3 :: Int
queryTime3 = 5

startTime4 :: [Int]
startTime4 = [1,1,1,1]

endTime4 :: [Int]
endTime4 = [1,3,2,4]

queryTime4 :: Int
queryTime4 = 7

startTime5 :: [Int]
startTime5 = [9,8,7,6,5,4,3,2,1]

endTime5 :: [Int]
endTime5 = [10,10,10,10,10,10,10,10,10]

queryTime5 :: Int
queryTime5 = 5

input :: [Int] -> [Int] -> Int -> IO()
input = input3 "startTime" "endTime" "queryTime"

output :: Int -> IO()
output = output1

main :: IO()
main = do
    input startTime1 endTime1 queryTime1
    output (busyStudent startTime1 endTime1 queryTime1)
    input startTime2 endTime2 queryTime2
    output (busyStudent startTime2 endTime2 queryTime2)
    input startTime3 endTime3 queryTime3
    output (busyStudent startTime3 endTime3 queryTime3)
    input startTime4 endTime4 queryTime4
    output (busyStudent startTime4 endTime4 queryTime4)
    input startTime5 endTime5 queryTime5
    output (busyStudent startTime5 endTime5 queryTime5)
    