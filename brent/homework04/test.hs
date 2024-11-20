module TestWholemealN where

{-
1. Install the test libraries; the "--lib" flag makes them available in ghci.
cabal install --lib HUnit
2. Run the tests:
runghc test.hs
-}

import WholemealN
import Test.HUnit

testFun1BaseCase :: Test
testFun1BaseCase = TestCase $ assertEqual
  "Base case: 1"
  (fun1 [])
  (fun1' [])

testFun1Case1 :: Test
testFun1Case1 = TestCase $ assertEqual
  "Case 1: only even numbers: 1"
  (fun1 [7, 13, 3, 9])
  (fun1' [7, 13, 3, 9])

testFun1Case2 :: Test
testFun1Case2 = TestCase $ assertEqual
  "Case 2: even and odd numbers: ((4 - 2) * (6 - 2))"
  (fun1 [9, 4, 123, 55, 6])
  (fun1' [9, 4, 123, 55, 6])

testFun1Case3 :: Test
testFun1Case3 = TestCase $ assertEqual
  "Case 3: even and odd numbers, including a 2: 0"
  (fun1 [2, 1024, 88, 97, 137, -513])
  (fun1' [2, 1024, 88, 97, 137, -513])


testFun2 :: Test
testFun2 = TestCase $ assertEqual
  "Check all inputs in range [1 .. 50]"
  [0, 2, 40, 6, 30, 46, 234, 14, 276, 40, 212, 58, 100, 248, 562,
  30, 178, 294, 424, 60, 126, 234, 516, 82, 538, 126, 81178, 276, 
  366, 592, 80910, 62, 688, 212, 446, 330, 444, 462, 1894, 100, 
  81096, 168, 1090, 278, 416, 562, 80816, 130, 666, 588]
  (map fun2 [1 .. 50])
--  (map fun2' [1 .. 50])


main :: IO Counts
main = runTestTT $ TestList 
  [ testFun1BaseCase,
    testFun1Case1,
    testFun1Case2,
    testFun1Case3,
    testFun2
  ]

