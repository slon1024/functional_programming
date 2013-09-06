module ChangeCount where
import Data.List(sort)

changeCount                           :: Int -> [Int] -> Int
changeCount money coins  | money <= 0 = 0
                         | otherwise  = changeCount' money  (reverse . sort $ coins) 

changeCount' money coins | null coins = 0
                         | money == 0 = 1
                         | money < 0  = 0
                         | otherwise  = (changeCount' (money - (head coins)) coins) + (changeCount' money (tail coins))