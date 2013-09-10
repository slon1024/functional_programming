module PeanoNumber_Test where


import PeanoNumber(Peano(..))
import Test.HUnit

testZero = TestCase $ assertEqual
    "0 is (Zero)" (0::Peano) (Zero)

testOne = TestCase $ assertEqual
    "1 is (Succ Zero)" (1::Peano) (Succ Zero)

testFive = TestCase $ assertEqual
    "5 is (Succ (Succ (Succ (Succ (Succ Zero)))))" (5::Peano) (Succ $ Succ $ Succ $ Succ $ Succ Zero)

testZeroPlusZeroShouldBeZero = TestCase $ assertEqual
    "0 + 0 = 0" (0::Peano) (Zero + Zero)

testOnePlusZeroShouldBeOne = TestCase $ assertEqual
    "1 + 0 = 0" (1::Peano) ((Succ Zero) + Zero)

testOnePlusTwoShouldBeThree = TestCase $ assertEqual
    "1 + 2 = 3" (3::Peano) ((Succ Zero) + (Succ $ Succ Zero))

testTwoMultiplyThreeShouldBeSix = TestCase $ assertEqual
    "2 * 3 = 6" (6::Peano) ((Succ $ Succ Zero) * (Succ $ Succ $ Succ Zero))


simpleTests = TestLabel "Check simple value" (TestList [
        testZero,
        testOne, 
        testFive
    ])

operatorPlus = TestLabel "Checking adding numbers" (TestList [
        testZeroPlusZeroShouldBeZero, 
        testOnePlusZeroShouldBeOne, 
        testOnePlusTwoShouldBeThree
     ])

operatorMultiply = TestLabel "Checking the multiplication of numbers" (TestList [
        testTwoMultiplyThreeShouldBeSix
     ])

tests = TestList [
        simpleTests, 
        operatorPlus,
        operatorMultiply
    ]
main = runTestTT $ tests