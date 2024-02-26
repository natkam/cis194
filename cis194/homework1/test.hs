module TestCardNumbers where

{-
1. Install the test libraries; the "--lib" flag makes them available in ghci.
cabal install --lib HUnit
2. Run the tests:
runghc test.hs
-}

import CardNumbers
import Test.HUnit

testToDigits :: Test
testToDigits =
  TestCase $
    assertEqual
      "Convert integer to a list of digits"
      (toDigits 1234)
      [1, 2, 3, 4]

testToDigitsZero :: Test
testToDigitsZero =
  TestCase $
    assertEqual
      "Convert 0 to an empty list"
      (toDigits 0)
      []

testToDigitsNegative :: Test
testToDigitsNegative =
  TestCase $
    assertEqual
      "Convert negative integer to an empty list"
      (toDigits (-17))
      []

main :: IO Counts
main = runTestTT $ TestList [testToDigits, testToDigitsZero, testToDigitsNegative]
