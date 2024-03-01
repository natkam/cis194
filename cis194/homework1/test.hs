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
      [1, 2, 3, 4]
      (toDigits 1234)

testToDigitsZero :: Test
testToDigitsZero =
  TestCase $
    assertEqual
      "Convert 0 to an empty list"
      []
      (toDigits 0)

testToDigitsNegative :: Test
testToDigitsNegative =
  TestCase $
    assertEqual
      "Convert negative integer to an empty list"
      []
      (toDigits (-17))

{- Unit tests for toDigitsRev -}

testToDigitsRev :: Test
testToDigitsRev =
  TestCase $
    assertEqual
      "Convert integer to a reversed list of digits"
      [4, 3, 2, 1]
      (toDigitsRev 1234)

{- Unit tests for doubleEveryOther -}

testDoubleEveryOther :: Test
testDoubleEveryOther =
  TestCase $
    assertEqual
      "Double every other digit in an integer list"
      [16, 7, 12, 5]
      (doubleEveryOther [8, 7, 6, 5])

testDoubleEveryOtherOddLenth :: Test
testDoubleEveryOtherOddLenth =
  TestCase $
    assertEqual
      "Double every other digit in an list of odd lenth"
      [1, 4, 3]
      (doubleEveryOther [1, 2, 3])

testSumDigits :: Test
testSumDigits =
  TestCase $
    assertEqual
      "Sum all digits of the numbers in the list"
      22
      (sumDigits [16, 7, 12, 5])

testValidateTrue :: Test
testValidateTrue =
  TestCase $
    assertEqual
      "Confirm that the number is valid"
      True
      (validate 4012888888881881)

testValidateFalse :: Test
testValidateFalse =
  TestCase $
    assertEqual
      "Confirm that the number is invalid"
      False
      (validate 4012888888881882)

main :: IO Counts
main =
  runTestTT $
    TestList
      [ testToDigits,
        testToDigitsZero,
        testToDigitsNegative,
        testToDigitsRev,
        testDoubleEveryOther,
        testDoubleEveryOtherOddLenth,
        testSumDigits,
        testValidateTrue,
        testValidateFalse
      ]
