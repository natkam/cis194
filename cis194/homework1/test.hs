module TestCardNumbers where

{-
1. Install the test libraries:
cabal install HUnit
cabal install tasty
cabal install tasty-hunit
2. Run the tests:
runghc test.hs
-}

import CardNumbers
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests =
  testGroup
    "Exercise 1 tests"
    [ testCase "Convert integer to a list of digits" $
        toDigits 1234 @?= [1, 2, 3, 4],
      testCase "Convert 0 to an empty list" $
        toDigits 0 @?= [],
      testCase "Convert negative integer to an empty list" $
        toDigits (-17) @?= []
    ]
