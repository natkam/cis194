module TestCardNumbers where

{-
1. Install the test libraries; the "--lib" flag makes them available in ghci.
cabal install --lib HUnit
cabal install --lib tasty
-}

--import Test.Tasty
--import Test.Tasty.HUnit
import Test.HUnit
import CardNumbers
  
{-
main = defaultMain tests

tests = testGroup "Exercise 1 tests" [
  testCase "Convert integer to a list of digits" $
    toDigits 1234 @?= [1, 2, 3, 4],
  testCase "Convert 0 to an empty list" $
    toDigits 0 @?= [],
  testCase "Convert negative integer to an empty list",
    toDigits (-17) @?= []]
-}

testToDigits :: Test
testToDigits = 
  TestCase $ assertEqual "Convert integer to a list of digits"
                         (toDigits 1234) [1, 2, 3, 4]

testToDigitsZero :: Test
testToDigitsZero = 
  TestCase $ assertEqual "Convert 0 to an empty list"
                         (toDigits 0) []

testToDigitsNegative :: Test
testToDigitsNegative = 
  TestCase $ assertEqual "Convert negative integer to an empty list"
                         (toDigits (-17)) []

main :: IO Counts
main = runTestTT $ TestList [testToDigits, testToDigitsZero, testToDigitsNegative]


