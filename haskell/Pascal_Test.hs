module Pascal_Test where 

import Pascal(pascal)
import Test.HUnit


testForOne = TestCase $ assertEqual
    "Should be equal [1,1]" [1, 1] (pascal !! 1)

testSumForOne = TestCase $ assertEqual
    "Sum should be equal 2^1 = 2" 2 (sum $ pascal !! 1)

testSumForTen = TestCase $ assertEqual
    "Sum should be equal 2^10 = 1024" 1024 (sum $ pascal !! 10)


main = runTestTT $ TestList [testForOne, testSumForOne, testSumForTen]
