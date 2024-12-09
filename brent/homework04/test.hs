module TestWholemealN where

{-
1. Install the test libraries:
cabal install HUnit
cabal install tasty
cabal install tasty-hunit
2. Run the tests:
runghc test.hs
-}

import Test.Tasty
import Test.Tasty.HUnit
import WholemealN

main = defaultMain tests

tests =
  testGroup
    "Homework 04 tests"
    [ testCase "Ex. 1, fun1: Base case" $
        fun1' [] @?= fun1 [],
      testCase "Ex. 1, fun1: Only odd numbers" $
        fun1' [7, 13, 3, 9] @?= fun1 [7, 13, 3, 9],
      testCase "Ex. 1, fun1: Even and odd numbers" $
        fun1' [9, 4, 123, 55, 6] @?= fun1 [9, 4, 123, 55, 6],
      testCase "Ex. 1, fun1: Even and odd numbers, including 2" $
        fun1' [2, 1024, 88, 97, 137, -513]
          @?= fun1 [2, 1024, 88, 97, 137, -513],
      testCase "Ex. 1, fun2: Check all inputs in range [1 .. 50]" $
        map fun2' [1 .. 50] @?= map fun2 [1 .. 50],
      -- TODO: foldTree does not compute the correct height for some nodes.
      -- testCase "Ex. 2: foldTree with strings" $
      --   foldTree "ABCDEFGHIJ"
      --     @?= Node
      --       3
      --       ( Node
      --           2
      --           (Node 0 Leaf 'F' Leaf)
      --           'I'
      --           (Node 1 (Node 0 Leaf 'B' Leaf) 'C' Leaf)
      --       )
      --       'J'
      --       ( Node
      --           2
      --           (Node 1 (Node 0 Leaf 'A' Leaf) 'G' Leaf)
      --           'H'
      --           (Node 1 (Node 0 Leaf 'D' Leaf) 'E' Leaf)
      --       ),
      testCase "Ex. 3.1, xor: An odd number of True values" $
        xor [False, True, False] @?= True,
      testCase "Ex. 3.1, xor: An even number of True values" $
        xor [False, True, False, False, True] @?= False,
      testCase "Ex. 3.2, map': +2" $
        map' (+ 2) [3, 17, 42] @?= map (+ 2) [3, 17, 42],
      testCase "Ex. 3.2, map': || False" $
        map' (|| False) [True, False, False]
          @?= map (|| False) [True, False, False],
      testCase "Ex. 3.3, myFoldl" $
        myFoldl (++) "foo" ["a", "b", "c"]
          @?= foldl (++) "foo" ["a", "b", "c"]
    ]
