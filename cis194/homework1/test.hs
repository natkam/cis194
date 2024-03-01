module TestCardNumbers where

{-
1. Install the test libraries; the "--lib" flag makes them available in ghci.
cabal install --lib HUnit
2. Run the tests:
runghc test.hs
-}

import CardNumbers
import Test.HUnit

{- Unit tests for toDigits -}

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

{- Unit tests for toDigitsRev -}

testToDigitsRev :: Test
testToDigitsRev =
  TestCase $
    assertEqual
      "Convert integer to a reversed list of digits"
      (toDigitsRev 1234)
      [4, 3, 2, 1]

{- Unit tests for doubleEveryOther -}

testDoubleEveryOther :: Test
testDoubleEveryOther =
  TestCase $
    assertEqual
      "Double every other digit in an integer list"
      (doubleEveryOther [8, 7, 6, 5])
      [16, 7, 12, 5]

testDoubleEveryOtherOddLenth :: Test
testDoubleEveryOtherOddLenth =
  TestCase $
    assertEqual
      "Double every other digit in an list of odd lenth"
      (doubleEveryOther [1, 2, 3])
      [1, 4, 3]

main :: IO Counts
main =
  runTestTT $
    TestList
      [ testToDigits,
        testToDigitsZero,
        testToDigitsNegative,
        testToDigitsRev,
        testDoubleEveryOther,
        testDoubleEveryOtherOddLenth
      ]
