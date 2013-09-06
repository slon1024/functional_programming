module ChangeCount_Test where

import ChangeCount(changeCount)
import Test.HUnit

testWithEmptyCoinsList = TestCase $ assertEqual
    "changeCount 4 [] == 0" 0 (changeCount 4 [])

testWithZeroMoney = TestCase $ assertEqual
    "changeCount 0 [1, 2, 5] == 18" 0 (changeCount 0 [1, 2, 5])

testWithUnSortedCoins = TestCase $ assertEqual
    "changeCount 15 [1, 2, 5] == 18" 18 (changeCount 15 [2, 5, 1])    

main = runTestTT $ TestList [testWithEmptyCoinsList, testWithZeroMoney, testWithUnSortedCoins]