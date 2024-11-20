module TestWholemealN where
{-
1. Install the test libraries; the "--lib" flag makes them available in ghci.
cabal install --lib HUnit
cabal install --lib tasty
cabal install --lib tasty-hunit
2. Run the tests:
runghc test.hs
-}

import Test.Tasty.HUnit
import WholemealN

main = defaultMain tests

tests =
  testGroup
    "Homework 04 tests"
    [ testCase "fun1: Base case" $
        fun1' [] @?= fun1 [],
      testCase "fun1: Only even numbers" $
        fun1' [7, 13, 3, 9] @?= fun1 [7, 13, 3, 9],
      testCase "fun1: Even and odd numbers" $
        fun1' [9, 4, 123, 55, 6] @?= fun1 [9, 4, 123, 55, 6],
      testCase "fun1: Even and odd numbers, including 2" $
        fun1' [2, 1024, 88, 97, 137, -513]
          @?= fun1 [2, 1024, 88, 97, 137, -513],
      testCase "fun2: Check all inputs in range [1 .. 50]" $
        map fun2' [1 .. 50] @?= map fun2 [1 .. 50]
    ]
